case class BitPtr(buf:Array[Byte],byteCnt:Int,bitCnt:Int){
  def next() = {
    val lah =
      if (bitCnt % 8 == 7) {
        byteCnt+1
      } else {
        byteCnt
      }
    BitPtr(buf,lah,(bitCnt % 8) + 1)
  }
  def read() = {
    if ((buf(byteCnt) & (1 << (bitCnt % 8))) != 0) {
      1
    } else {
      0
    }
  }
  def take(n:Int)= {
    val (next,res) = (0 to (n-1)).foldLeft((this,0)) {
      case ((state,sum),i) =>
        (state.next(),sum + (state.read() << i))
    }
    (res,next)
  }
  def revTake(n:Int) = {
    val (next,res) = (0 to (n-1)).foldLeft((this,0)) {
      case ((state,sum),i) =>
        (state.next(),sum + (state.read() << ((n-1)-i)))
    }
    (res,next)
  }
}

case class WinPtr(buf:Array[Byte],cnt:Int) {
  def next() = {
    WinPtr(buf,cnt+1)
  }
  def read() = {
    buf(cnt)
  }
  def write(v:Byte) = {
    buf(cnt) = v
  }
  def expand(len:Int,dist:Int) = {
    (0 to (len - 1)).foreach {
      i =>
        buf(cnt+i) = buf(cnt-dist+i)
    }
    WinPtr(buf,cnt+len)
  }
}

package object Deflate {
  type Sym = Int
  type Len = Int
  type Code = Int

  sealed abstract class Mode
  final case object Error extends Mode
  final case object Custom extends Mode
  final case object Fixed extends Mode
  final case object NoComp extends Mode

  val bufMaxSize = 32000
  val fixedTree = Huffman.lenToTree(generateFixedHuffmanLen())
  val lenCode = Map(257 -> (0,3),258 -> (0,4),259 -> (0,5),260 -> (0,6),261 -> (0,7),262 -> (0,8),263 -> (0,9),
    264 -> (0,10),265 -> (1,11),266 -> (1,13),267 -> (1,15),268 -> (1,17),269 -> (2,19),270 -> (2,23),
    271 -> (2,27),271 -> (2,27),272 -> (2,31),273 -> (3,35),274 -> (3,43),275 -> (3,51),276 -> (3,59),
    277 -> (4,67),278 -> (4,83),279 -> (4,99),280 -> (4,115),281 -> (5,131),282 -> (5,163),283 -> (5,195),
    284 -> (5,227),285 -> (0,258))
  val distCode = Map(0 -> (0,1),1 -> (0,2),2 -> (0,3),3 -> (0,4),4 -> (1,5),5 -> (1,7),6 -> (2,9),7 -> (2,13),8 -> (3,17),
    9 -> (3,25),10 -> (4,33),11 -> (4,49),12 -> (5,65),13 -> (5,97),14 -> (6,129),15 -> (6,193),16 -> (7,257),17 -> (7,385),
    18 -> (8,513),19 -> (8,769),20 -> (9,1025),21 -> (9,1537),22 -> (10,2049),23 -> (10,3073),24 -> (11,6145),25 -> (11,6145),
    26 -> (12,8193),27 -> (12,12289),28 -> (13,16385),29 -> (13,24577))

  def generateFixedHuffmanLen(): List[(Sym,Len)] = {
    {
      (0 to 143).map {
        i => (i:Sym,8:Len)
      } ++
        (144 to 255).map {
          i => (i:Sym,9:Len)
        } ++
        (256 to 279).map {
          i => (i:Sym,7: Len)
        } ++
        (280 to 287).map {
          i => (i:Sym,8:Len)
        }
    }.toList
  }

  def readMode(src:BitPtr): (Mode,BitPtr) = {
    val low = src.read()
    val high = src.next().read()
    (low,high) match {
      case (0,0) => (NoComp,src.next().next())
      case (0,1) => (Custom,src.next().next())
      case (1,0) => (Fixed,src.next().next())
      case (1,1) => (Error,src.next().next())
      case _ => throw new Exception() // never
    }
  }

  def decode(tree:Huffman.Tree,state:BitPtr): (Sym,BitPtr) = {
    tree match {
      case Huffman.Node(left,right) =>
        state.read() match {
          case 0 => decode(left,state.next())
          case 1 => decode(right,state.next())
        }
      case Huffman.Leaf(sym) => (sym,state)
    }
  }

  def readContent(litTr:Huffman.Tree,distTr:Option[Huffman.Tree],src:BitPtr,dst:WinPtr): (BitPtr,WinPtr) = {
    def takeExtraLen(state:BitPtr,alphabet:Int) = {
      val (el,offset) = lenCode(alphabet)
      if (el > 0) {
        val (res, next) = state.take(el)
        (offset + res, next)
      } else {
        (offset,state)
      }
    }
    def takeDist(state:BitPtr) = {
      val (dist,next) = distTr match {
        case Some(tr) => decode(tr,state)
        case None => state.take(5)
      }
      val (el,offset) = distCode(dist)
      if (el > 0) {
        val (res, another) = next.take(el)
        (offset + res, another)
      } else {
        (offset,next)
      }
    }
    val (alphabet,next) = decode(litTr,src)
    if (alphabet < 256) {
      dst.write(alphabet.asInstanceOf[Byte])
      readContent(litTr,distTr,next,dst.next())
    } else if (alphabet == 256) {
      (next,dst)
    } else {
      val (len,second) = takeExtraLen(next,alphabet)
      val (dist,third) = takeDist(second)
      readContent(litTr,distTr,third,dst.expand(len,dist))
    }
  }

  def readHuffmanCode(src:BitPtr) = {
    def repeat(ls:List[Int],state:BitPtr,f:Function1[BitPtr,(Int,BitPtr)]) = {
      ls.foldLeft((Nil:List[(Int,Int)],state)) {
          case ((res,state),num) =>
            val (prob,next) = f(state)
            if (prob != 0) {
              ((num, prob) :: res, next)
            } else {
              (res, next)
            }
        }
    }
    val (hlit,lits) = src.revTake(5)
    val (hdist,dists) = lits.revTake(5)
    val (hclen,clens) = dists.revTake(4)
    val (ca,_) = List(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15).splitAt(hclen+4)
    val (lens,state) = repeat(ca,clens,st => st.revTake(3))

    val tree = Huffman.lenToTree(lens.sortBy{case (x,y) => x})
    def readCode(state:BitPtr) = {
      val (code,second) = decode(tree,state)
      code match {
        case 16 =>
          val (add,third) = second.revTake(2)
          (3+add,third)
        case 17 =>
          val (add,third) = second.revTake(3)
          (3+add,third)
        case 18 =>
          val (add,third) = second.revTake(7)
          (11+add,third)
        case _ =>
          (code,second)
      }
    }
    val (litCode,second) = repeat((0 to (hlit + 256)).toList,state,readCode)
    val (distCode,third) = repeat((0 to hdist).toList,second,readCode)
    (Huffman.lenToTree(litCode),Huffman.lenToTree(distCode),third)
  }

  def readBlock(src:BitPtr,dst:WinPtr): Boolean = {
    val isLast = src.read() != 0
    readMode(src.next()) match {
      case (Fixed,next) =>
        val (nsrc,ndst) = readContent(fixedTree,None,next,dst)
        if (isLast) {
          true
        } else {
          readBlock(nsrc,ndst)
        }
      case (Custom,next) =>
        val (litTr,distTr,second) = readHuffmanCode(next)
        val (nsrc,ndst) = readContent(litTr,Some(distTr),second,dst)
        if (isLast) {
          true
        } else {
          readBlock(nsrc,ndst)
        }
      case (NoComp,BitPtr(srcBuf,srcCnt,_)) =>
        val (len,_) = BitPtr(srcBuf,srcCnt+1,0).take(16)
        val WinPtr(dstBuf,dstCnt) = dst
        (0 to len).foreach{
          i =>
            dstBuf(dstCnt+i) = srcBuf(srcCnt+5+i)
        }
        if (isLast) {
          true
        } else {
          readBlock(BitPtr(srcBuf,srcCnt+6+len,0),WinPtr(dstBuf,dstCnt+len))
        }
      case (Error,_) => false
    }
  }

  def decompress(src:Array[Byte],bufSize:Int = bufMaxSize) = {
    val buf = new Array[Byte](bufSize)
    if (readBlock(BitPtr(src,0,0),WinPtr(buf,0))) {
      Some(buf)
    } else {
      None
    }
  }

  def javaImpl(src:Array[Byte]) = {
    def byteToBinaryStr(src:Byte) = {
      (0 to 7).foldLeft(List[Char]()){
        (res,i) =>
          if ((src & (1 << i)) > 0) {
            '1' :: res
          } else {
            '0' :: res
          }
      }.mkString
    }
    //val comp = new java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION,true)
    val comp = new java.util.zip.Deflater(java.util.zip.Deflater.NO_COMPRESSION,true)
    val dcom = new java.util.zip.Inflater()
    var outBuf = new Array[Byte](src.length+5)
    var outBuft = new Array[Byte](src.length)
    comp.setInput(src,0,src.length)
    comp.finish()
    comp.deflate(outBuf)
    //dcom.setInput(outBuf)
    //dcom.inflate(outBuft)
    println(outBuf.toList)
    println(src.map(_.toChar).mkString)
    //println(decompress(outBuf).map(_.toChar).mkString)
  }
}

