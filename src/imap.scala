/* vim: noai:ts=3:sw=3
*/

package me.phh.imap

class Imaps {
	var conn: java.net.Socket = null

	var connLines: Iterator[String] = null
	var connSource: io.Source = null
	var connSink: java.io.PrintWriter = null
	var connIS: java.io.InputStream = null

	def sendCommand(tag: String, command: String) {
		println("Sending " + command)
		connSink.print(s"$tag $command\r\n")
		connSink.flush()
	}

	def waitForResult(tag: String) = {
		val tagSpace = s"$tag "
		//println("Waiting for result...")

		class resultIterator extends Iterator[String] {
			var res: String = null
			var last = false
			def hasNext() =
				if(last || !connLines.hasNext) {
					false
				} else {
					res = connLines.next
					if(res.startsWith(tagSpace))
						last = true
					true
				}

			def next() = res
		}

		new resultIterator()
	}

	def login(user: String, pass: String) = {
		sendCommand("A", s"LOGIN $user $pass")
		waitForResult("A").toList
	}

	def capabilities() {
		sendCommand("V", "CAPABILITY")
		println(waitForResult("V").toList)
	}

	def parseFlags(line: Option[String]) = {
		line.flatMap(_.split(raw"(\(|\))") match {
			case Array(_, flags, _*) =>
				Some(flags.split(" "))
			case _ =>
				None
		})
	}

	val imapDelimiter = raw"( |\[|\])"
	def examine(folder: String) = {
		sendCommand("F", "EXAMINE \""+folder+"\"")
		val res = waitForResult("F").toList

		val flags = parseFlags(res.find( _.startsWith("* FLAGS (")))
		val permanentFlags = parseFlags(res.find( _.startsWith("* OK [PERMANENTFLAGS] (")))

		val uidValidity =
			res.find( _.startsWith("* OK [UIDVALIDITY"))
				.flatMap( _.split(imapDelimiter) match {
					case Array(_, _, _, _, id, _*) => Some(id.toInt)
					case _ => None
				})

		val uidNext =
			res.find( _.startsWith("* OK [UIDNEXT"))
				.flatMap( _.split(imapDelimiter) match {
					case Array(_, _, _, _, id, _*) => Some(id.toInt)
					case _ => None
				})

		val exists =
			res.flatMap( _.split(imapDelimiter) match {
					case Array("*", n, "EXISTS", _*) => Some(n.toInt)
					case _ => None
				})

		val recent =
			res.flatMap( _.split(imapDelimiter) match {
					case Array("*", n, "RECENT", _*) => Some(n.toInt)
					case _ => None
				})

		for(line <- res)
			println(line)
		(flags, permanentFlags, uidValidity, exists, recent, uidNext)
	}

	def idle(timeout: Int, keep: Boolean = true): Iterator[(String, String)] = {
		def startIdle() = {
			println("Start idling...")
			sendCommand("A", "IDLE")
			val idleRet = connLines.next
			if(idleRet != "+ idling") throw new Exception("Expected idling, got " + idleRet)
		}

		def endIdle() = {
			connSink.print(s"DONE\r\n")
			connSink.flush
			waitForResult("A").toList
		}

		if(keep) {
			startIdle()
		}

		conn.setSoTimeout(timeout)
		object idleIterator extends Iterator[String] {
			var res: String = null
			var remaining= List.empty[String]
			def hasNext() = {
				if(!remaining.isEmpty) {
					println("Draining remaining...")
					true
				} else {
					try {
						if(!keep) {
							startIdle()
						}

						res = connLines.next

						if(!keep) {
							println("Pausing idling...")
							val remaining = endIdle()
							println("Remaining idle infos " + remaining)
							this.remaining = remaining.dropRight(1)
						}

						true
					} catch {
						case e: java.net.SocketTimeoutException =>
							println("Pausing idling...")
							connSink.print(s"DONE\r\n")
							connSink.flush
							waitForResult("A").toList
							conn.setSoTimeout(0)
							false
					}
				}
			}

			def next() = {
				if(!remaining.isEmpty) {
					println("Draining remaining...")
					var r = remaining.head
					remaining = remaining.tail
					r
				} else {
					val r = res
					if(r == null) {
						hasNext
						res
					} else {
						res = null
						r
					}
				}
			}
		}

		for(line <- idleIterator) yield {
			val s = line.split(imapDelimiter)
			println(line)
			s match {
				//This means a new message appeared
				case Array("*", id, "EXISTS") =>
					("EXISTS", id)
				//This means some flags changed
				case Array("*", id, "FETCH", _*) =>
					("FETCH", id)
				//This means a message has been deleted
				case Array("*", id, "EXPUNGE") =>
					("EXPUNGE", id)
				case _ =>
					println(line)
					throw new Exception()
					("", "")
			}
		}
	}

	sealed abstract class Tree {
		def ::(newTree: Tree): Tree
	}

	case class Leaf(val str: String) extends Tree {
		def ::(newTree: Tree) =
         new Node(List(newTree, this))
	}

	case class Node(val elems: List[Tree]) extends Tree {
		def ::(tree: Tree) =
         new Node(tree :: elems)
      override def toString() =
         "Node("+elems.mkString(", ")+")"
      def apply(n: Int) = elems(n)
	}

	case class FinishedNode(val elems: List[Tree]) extends Tree {
		def ::(tree: Tree) =
         new Node(tree :: List(this))

      override def toString() =
         "FinishedNode("+elems.mkString(", ")+")"

      def apply(n: Int) = elems(n)
	}

	//I think this function is much much too complicated for what it does...
	def splitAnswer(s: String) = {
		import scala.util.matching.Regex._
		
		//Just want to now the depth every character is at, with which start-of-sequence character
		val stacks = s.scanLeft(List.empty[Char]) { (stack, c) =>
			if(c == '(') {
				c :: stack
			} else if(c == ')') {
				assert(stack.head == '(')
				stack.tail
			} else if(c == '"') {
				val top = if(stack.isEmpty) 0 else stack.head
				if(top == '"')
					stack.tail
				else if(top == '\\')
					stack.tail
				else
					c :: stack
			} else if(!stack.isEmpty && stack.head == '\\') {
				stack.tail
			} else if(c == '\\') {
				c :: stack
			} else {
				stack
			}
		}

		def split(v: Seq[Tuple2[Char, Seq[Char]]], env: Char = '+'): Tree = {
			val (prefix, suffix) = v.span{ case (char, s) =>
            if(s.isEmpty && env != '"')
               char != ' '
            else
               true
         }

         val prefixTree =
            if(!prefix.find(_._2.filter( _ != '\\').length != 0).isEmpty) {
               val separator = prefix.head._1
               val prefixFiltered = prefix.filter(_._2.filter( _ != '\\').length >= 1).dropRight(1)
               val r = split(prefixFiltered.map{ case (char, stack) => (char, stack.dropRight(1)) }, separator)
               r match {
                  case r: Node => new FinishedNode(r.elems)
                  case r: FinishedNode => r // This helps have only one depth when doing (( ))
                  case r: Leaf => r
               }
				} else {
               val str = prefix.map(_._1).mkString
               new Leaf(str)
				}

			 if(suffix.isEmpty)
				 prefixTree
			 else {
				 prefixTree :: split(suffix.tail)
			 }

		}
		split(s zip stacks)
	}

	val fetchInfos = "(X-GM-MSGID X-GM-THRID X-GM-LABELS UID)"
	def fetch(from: String, to: String, infos: String) = {
		sendCommand("G", s"FETCH $from:$to $infos")
		waitForResult("G")
	}

	def fetch(id: String, infos: String = fetchInfos) = {
		sendCommand("G", s"FETCH $id $infos")
		waitForResult("G")
	}

	def fetchUid(uid: String, infos: String = fetchInfos) = {
		sendCommand("W", s"UID FETCH $uid $infos")
		waitForResult("W")
	}

	def connect(host: String, port: Int) = {
		val socketSsl = javax.net.ssl.SSLSocketFactory.getDefault
			.createSocket(host, port)

		val is = socketSsl.getInputStream
		val os = socketSsl.getOutputStream

		conn = socketSsl

		connSource = io.Source.fromInputStream(is)
      connLines = connSource.getLines
		connSink = new java.io.PrintWriter(os, false)
		connIS = is

		val greeting = connLines.next
		println("Greeted by " + greeting)
		//TODO: What to do with it? Save connection UID?
	}

	def compress() = {
		sendCommand("C", "COMPRESS DEFLATE")
		waitForResult("C").toList

		import java.util.zip._

		val is = new InflaterInputStream(conn.getInputStream, new Inflater(true /*RFC1591*/))
		val os = new DeflaterOutputStream(conn.getOutputStream, new Deflater(Deflater.BEST_COMPRESSION, true /*RFC1591*/), true /* also flush compressor */)

		connSource = io.Source.createBufferedSource(is, 2 /* Don't try to bufferize, Deflater is already bufferized*/)(io.Codec.UTF8)
      connLines = connSource.getLines
		connSink = new java.io.PrintWriter(os, false)
      connIS = is
	}

   def mailFilename(uid: String, gm_msgid: String, flags: List[String]) = {
      val timestamp: Long = System.currentTimeMillis / 1000
      val translatedFlags = flags.flatMap{
         case "\\Answered" => List('R')
         case "\\Flagged" => List('F')
         case "\\Draft" => List('D')
         case "\\Deleted" => List('T')
         case "\\Seen" => List('S')
         case _ => List()
      }.sorted.mkString
      s"$timestamp.$uid.$gm_msgid:2,$translatedFlags"
   }

	def fetchMail(requestedUid: String) = {
      import scala.util.matching.Regex._

      val mailInfos = fetchUid(requestedUid, "(X-GM-MSGID X-GM-LABELS UID FLAGS)").toList
		println("Got mailInfos = " + mailInfos)
		val answerLine = mailInfos.find( l=> l.contains("FETCH") && l.contains(s"UID $requestedUid")).get

      val elems =
			splitAnswer(mailInfos(0)).asInstanceOf[Node](3).asInstanceOf[FinishedNode].elems

      val msgid = (elems zip elems.tail).flatMap {
         case (Leaf("X-GM-MSGID"), Leaf(tagValue)) => List(tagValue)
         case _ => List()
      }.head

      val uid = (elems zip elems.tail).flatMap {
         case (Leaf("UID"), Leaf(tagValue)) => List(tagValue)
         case _ => List()
      }.head

      val flags = (elems zip elems.tail).flatMap {
         case (Leaf("FLAGS"), e: Leaf) => List(e)
         case (Leaf("FLAGS"), FinishedNode(e)) => e
         case _ => List()
      }.map{ case Leaf(tag) => tag; case _ => "" }

      val labels = (elems zip elems.tail).flatMap {
         case (Leaf("X-GM-LABELS"), e: Leaf) => List(e)
         case (Leaf("X-GM-LABELS"), FinishedNode(e)) => e
         case _ => List()
      }.map{case Leaf(label) => label; case _ => "" }
	   .map{ _.replace("\\\\", "\\")}

      sendCommand("M", s"UID FETCH $uid (RFC822)")

	  val iter = Iterator.continually(connIS.read())
	  val part1 = iter.takeWhile( _ != '\r').map(_.toChar).toList.mkString
	  iter.next // Consume \n

      val getSize = raw"RFC822 \{([0-9]*)\}".r
      val size = getSize.findFirstMatchIn(part1)
         .map( _.group(1))
         .get
         .toInt
	println(s"Retrieving message $requestedUid, size $size")

	val msg = iter.take(size).map(_.toChar).toList.mkString
      
      val msgStr = msg.mkString

      val filename = mailFilename(uid, msgid, flags)
	  new java.io.PrintWriter(s"maildir/cur/$filename") {
		  try {
			  write("X-Keywords: " + labels.mkString(",") + "\r\n")
			  write(msgStr)
		  } finally {
			  close()
		  }
	  }

      waitForResult("M").toList
	}
}
