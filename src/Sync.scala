/* vim: noai:ts=3:sw=3
 */
package me.phh.imap

import collection.JavaConversions._
import java.util.concurrent.{LinkedBlockingQueue, BlockingQueue}
import scala.collection.immutable.Set

import scala.concurrent.ExecutionContext.Implicits.global

object Sync {
	type MailOperation = Tuple2[Int,Symbol]
	val fifo = new FIFOStream[MailOperation]()

	val idUidMap = new scala.collection.mutable.HashMap[Int, Int]
	class FIFOStream[A]( private val queue: BlockingQueue[Option[A]] = new LinkedBlockingQueue[Option[A]]() ) {
		import scala.language.postfixOps
		def toStream: Stream[A] = queue take match {
			case Some(a) => Stream cons ( a, toStream )
			case None    => Stream empty
		}
		def close() = queue add None
		def enqueue( as: A* ) = queue addAll as.map( Some(_) )
	}

	object ImapConnectionThread {
		def syncMail(uid: Integer, i: Imaps) {
		}

		def downloadMail(uid: Integer, i: Imaps) = {
			i.downloadMail(uid.toString)
		}

		def initSync(uid: Integer, i: Imaps, knownIds: Set[Int]) {
			if(knownIds.contains(uid)) {
				//Sync
				syncMail(uid, i)
			} else {
				//Download
				downloadMail(uid, i)
			}
		}
		def idle(conn: Imaps, folder: Imaps.FolderInfos) {
			var nextId = folder._4.head+1
			var nextUid = folder._6.head
			while(true) {
				for(line <- conn.idle(25*60*1000, false)) {
					if(line._1 == "EXISTS") {
						//It is possible to receive more than one mail at a time here
						val id = line._2.toInt
						val newNextId = line._2.toInt+1
						val nNew = newNextId - nextId
						println(s"Got $nNew new mails")
						for(i <- nextId to id) {
							val v = i.toString
							val r = conn.fetch(v, "(UID)").toList
							val getUid = raw"\(UID ([0-9]*)\)".r
							val uid = r.flatMap { line =>
								getUid.findFirstMatchIn(line)
									.map( _.group(1))
							}.head


							println(s"Guessed UID was $nextUid, got $uid")
							if(nextUid != uid.toInt) println("-----------------------------")
							nextUid = uid.toInt+1
							fifo.enqueue( (uid.toInt, 'Download) )
							idUidMap(i) = uid.toInt
						}
						nextId = newNextId
					} else if(line._1 == "EXPUNGE") {
						//Expunge always only delete one mail at a time
						val id = line._2.toInt
						val deadUid = idUidMap(id)
						fifo.enqueue( (deadUid, 'Remove) )

						//This shit is expensive...
						for(i <- (nextId-1) to (id+1) by -1) {
							idUidMap(i-1) = idUidMap(i)
						}
						nextId-=1
						idUidMap -= nextId
					} else if(line._1 == "FETCH") {
						scala.util.Try {
							fifo.enqueue( (idUidMap(line._2.toInt), 'Sync) )
						}
					}
					println(line)
				}
			}
		}

		def downloadMails(ops: FIFOStream[MailOperation], i: Imaps, knownIds: Set[Int], folder: Imaps.FolderInfos) {
			val threadID = Thread.currentThread().getId()

			ops.toStream.map{ op =>
				try {
					op match {
						case (uid, 'InitSync) =>
							initSync(uid, i, knownIds)
						case (uid, 'Download) =>
							val res = downloadMail(uid, i)
							//Block thread for 500ms and retry
							if(res.isEmpty) {
								println(s"Couldn't get mail $uid, retrying...")
								Thread.sleep(500)
								ops.enqueue(op)
							} else {
								//Sync mu DB
							}
						case (uid, 'Remove) =>
						case (uid, 'Sync) =>
							syncMail(uid, i)
						case (_, 'Idle) => idle(i, folder)
					}
					true
				} catch {
					//Erm imap connection is dead...
					//Re-enqueue the operation
					case e: java.io.EOFException =>
							println("IMAP connection closed, requeueing operation")
							ops.enqueue(op)
							false
				}
			}.takeWhile( (v: Boolean) => v ).toList
		}
	}

	class ImapConnectionThread(val ops: FIFOStream[MailOperation], val knownIds: Set[Int], val auth: Tuple2[String, String]) extends Thread {
		override def run() {
			while(true) {
				val conn = new Imaps()
				conn.connect("imap.gmail.com", 993)
				conn.login(auth._1, auth._2)
				conn.compress()

				val folder = conn.examine("[Gmail]/Tous les messages")
				ImapConnectionThread.downloadMails(ops, conn, knownIds, folder)
			}
		}
	}

	//Start listing files soon, this might take a while
	val futureKnownUids = scala.concurrent.Future {
		val folder = new java.io.File("maildir/cur/")
		import scala.util.matching.Regex._
		val regex = raw"[0-9]+\.([0-9]+)\.[0-9]+.*".r
		val r: Set[Int] = folder.list().map{ fileName =>
				regex.findFirstMatchIn(fileName) match {
					case Some(m) => m.group(1).toInt
					case None => -1
				}
			}.toSet
		r
	}

	def main(args: Array[String]) {
		val conn = new Imaps()
		conn.connect("imap.gmail.com", 993)
		conn.capabilities()
		println(conn.login(args(0), args(1)))
		conn.compress()

		val folder = conn.examine("[Gmail]/Tous les messages")

		import scala.concurrent._
		import scala.concurrent.duration._
		val knownUids = Await.result(futureKnownUids, Duration.Inf)

		var mails = List.empty[Int]
		for(line <- conn.fetch("1", folder._4.head.toString, "(UID)")) {
			try {
				val getUid = raw"([0-9]*) FETCH\s*\(UID ([0-9]*)\)".r
				val id =
					getUid.findFirstMatchIn(line)
						.map( _.group(1))
						.get
				val uid =
					getUid.findFirstMatchIn(line)
						.map( _.group(2))
						.get

				fifo.enqueue( (uid.toInt, 'InitSync) )

				idUidMap(id.toInt) = uid.toInt
			} catch {
				case e: Exception => println("ERR: " + line)
			}
		}

		println("Done enqueuing mails")

		println("Starting fetcher threads")
		val threads = (1 to 2).map( v => new ImapConnectionThread(fifo, knownUids, (args(0), args(1))))
		threads.foreach(_.start())

		fifo.enqueue( (-1, 'Idle) )
		ImapConnectionThread.downloadMails(fifo, conn, knownUids, folder)
	}
}
