package example

object Huffman {
  type Sym = Int
  type Len = Int
  type Code = Int
  type Prob = Int
  case class Chain(weight: Int,symCnt: Int,child: Option[Chain])
  case class Stage(left: Chain,right: Chain,rest: List[(Sym,Prob)])

  abstract class Tree
  case class Node(left:Tree,right:Tree) extends Tree
  case class Leaf(value:Sym) extends Tree

  sealed abstract class Mode
  final case object Error extends Mode
  final case object Custom extends Mode
  final case object Fixed extends Mode
  final case object NoComp extends Mode

  val sizeOfLitLen = 286
  val maxCodeLen = 15
  val fixedTree = codeToTree(lenToCode(generateFixedHuffmanLen()))

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

  def codeToTree(cand:List[(Sym,Code,Len)],depth:Int = 0): Tree = {
    val left = cand.filter{ case (_:Sym,code:Code,len:Len) => ((code & (1 << (len - depth))) == 0) }
    val right = cand.filter{ case (_:Sym,code:Code,len:Len) => ((code & (1 << (len - depth))) != 0) }
    (left,right) match {
      case (Nil,Nil) => throw new Exception // todo
      case ((symLeft,_,_) :: Nil,(symRight,_,_) :: Nil) => Node(Leaf(symLeft),Leaf(symRight))
      case ((sym,_,_) :: Nil,_) => Node(Leaf(sym),codeToTree(right,depth+1))
      case (_,(sym,_,_) :: Nil) => Node(codeToTree(left,depth+1),Leaf(sym))
      case (Nil,_) => codeToTree(right,depth+1)
      case (_,Nil) => codeToTree(left,depth+1)
      case _ => Node(codeToTree(left,depth+1),codeToTree(right,depth+1))
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
    //val comp = new java.util.zip.Deflater()
    val comp = new java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION,true)
    val dcom = new java.util.zip.Inflater()
    var outBuf = new Array[Byte](src.length)
    var outBuft = new Array[Byte](src.length)
    comp.setInput(src,0,10)//src.length)
    comp.finish()
    comp.deflate(outBuf)
    //dcom.setInput(outBuf)
    //dcom.inflate(outBuft)
    println(src.map(_.toChar).mkString)
    src.foreach{
      c =>
        print(byteToBinaryStr(c))
        print(' ')
    }
    println()
    outBuf.foreach{
      c =>
        print(byteToBinaryStr(c))
        print(' ')
    }
    println()
    def getNextBit(state:(Byte,Int,Iterator[Byte])) = {
      val (value,count,it) = state
      val next =
        (if (count % 8 == 7) {
          it.next()
        } else {
          value
        },(count+1) % 8,it)
      if ((value & (1 << (count % 8))) > 0) {
        (1,next)
      } else {
        (0,next)
      }
    }
    def readMode(src:Byte): Mode = {
      val low = (src & 2) != 0
      val high = (src & 4) != 0
      (low,high) match {
        case (false,false) => NoComp
        case (false,true) => Custom
        case (true,false) => Fixed
        case (true,true) => Error
      }
    }
    def decode(tree:Tree,state:(Byte,Int,Iterator[Byte])): (Sym,(Byte,Int,Iterator[Byte])) = {
      tree match {
        case Node(left,right) =>
          getNextBit(state) match {
            case (0,next) => decode(left,next)
            case (1,next) => decode(right,next)
          }
        case Leaf(sym) => (sym,state)
      }
    }
    def readContent(tree:Tree,state:(Byte,Int,Iterator[Byte]),dst:Array[Byte],cnt:Int): (Byte,Int,Iterator[Byte]) = {
      def take(n:Int,state:(Byte,Int,Iterator[Byte]))= {
        val (next,res) = (0 to (n-1)).foldLeft((state,0)) {
          case ((state,sum),i) =>
            val (res,next) = getNextBit(state)
            (next,sum + (res << i))
        }
        (res,next)
      }
      def takeExtraLen(state:(Byte,Int,Iterator[Byte]),alphabet:Int) = {
        val (el,ll) = lenCode(alphabet)
        if (el > 0) {
          val (res, next) = take(el, state)
          (ll + res, next)
        } else {
          (ll,state)
        }
      }
      def takeDist(state:(Byte,Int,Iterator[Byte])) = {
        val (dist,next) = take(5,state)
        val (el,ll) = distCode(dist)
        if (el > 0) {
          val (res, another) = take(el, next)
          (ll + res, another)
        } else {
          (ll,state)
        }
      }
      val (alphabet,next) = decode(tree,state)
      if (alphabet < 256) {
        println(alphabet)
        dst(cnt) = alphabet.asInstanceOf[Byte]
        readContent(tree,next,dst,cnt+1)
      } else if (alphabet == 256) {
        next
      } else {
        val (len,another) = takeExtraLen(next,alphabet)
        val (dist,alt) = takeDist(another)
        println(len)
        println(dist)
        readContent(tree,alt,dst,cnt+1)
      }
    }
    def readBlock(it:Iterator[Byte],dst:Array[Byte],cnt:Int) = {
      val firstByte = it.next()
      val isLast = (firstByte & 1) != 0
      val init = (firstByte,3,it)
      readMode(firstByte) match {
        case Fixed =>
          readContent(fixedTree,init,dst,cnt)
      }
    }
    readBlock(outBuf.iterator,new Array[Byte](30),0)
    println(outBuft.map(_.toChar).mkString)
  }

  private def updateStage(stages: List[Stage],cnt: Int) : List[Stage] = {
    if (cnt >= 2) {
      stages
    } else {
      stages match {
        case Nil => Nil
        case Stage(_, Chain(weight, symCnt, child), Nil) :: Nil => stages // ??
        case Stage(_, Chain(weight, symCnt, child), (_, lookahead) :: next) :: Nil =>
          updateStage(Stage(Chain(weight, symCnt, child), Chain(lookahead, symCnt + 1, child), next) :: Nil, cnt + 1)
        case Stage(_, Chain(weight, symCnt, child), (sym, lookahead) :: next) :: pre :: rest =>
          pre match {
            case Stage(Chain(weightLeft, _, _), Chain(weightRight, preSymCnt, preChild), _) =>
              if (lookahead < weightRight + weightLeft) {
                updateStage(Stage(Chain(weight, symCnt, child), Chain(lookahead, symCnt + 1, child), next) :: pre :: rest, cnt + 1)
              } else {
                updateStage(Stage(Chain(weight, symCnt, child),
                  Chain(weightLeft + weightRight, symCnt, Some(Chain(weightRight, preSymCnt, preChild))) , (sym, lookahead) :: next)
                  :: updateStage(pre :: rest, 0), cnt + 1)
                // TODO: refactor
              }
          }
        case Stage(_, Chain(weight, symCnt, child), Nil) :: pre :: rest =>
          pre match {
            case Stage(Chain(weightLeft, _, _), Chain(weightRight, preSymCnt, preChild), _) =>
              updateStage(Stage(Chain(weight, symCnt, child),
                Chain(weightLeft + weightRight, symCnt, Some(Chain(weightRight, preSymCnt, preChild))), Nil) :: updateStage(pre :: rest, 0), cnt + 1)
            // TODO: refactor
          }
      }
    }
  }
  def histToLengths(hist: List[(Sym,Int)],upper: Int = maxCodeLen,arraySize: Int = sizeOfLitLen) = {
    def initStage(hist: List[(Sym,Int)]) = {
      hist match {
        case (_,lookahead1) :: (_,lookahead2) :: rest =>
          Stage(Chain(lookahead1,1,None),Chain(lookahead2,2,None),rest)
          // remark: checked by follow
      }
    }
    def traverseChain(ch: Chain) : List[Int] = {
      ch match {
        case Chain(_,symCnt,None) =>
          symCnt :: Nil
        case Chain(_,symCnt,Some(child)) =>
          symCnt :: traverseChain(child)
      }
    }
    //val stage0 = hist.sortBy{case (x,c) => c}
    val num = hist.length
    if (num < 2)
      throw new Exception
      // TODO: error handling
    val stages = for (_ <- (1 to upper).toList) yield { initStage(hist) }
    val lastChain = (1 to num - 2).foldLeft(stages){ (acc,_) => updateStage(acc,0) }.head match {
      case Stage(_,right,_) => right
    }
    var res = new Array[Int](arraySize)
    for (i <- traverseChain(lastChain)) {
      hist.foldLeft(0){
        case (cnt,(sym,_)) =>
          if (cnt < i) {
            res(sym) += 1
          }
          cnt + 1
      }
    }
    res
  }
  def lenToCode(src: List[(Sym,Len)]) = {
    def calcHist(src: List[(Sym,Len)]) = {
      val lengths = src.map{case (_,len) => len}.sorted
      lengths.foldLeft ((lengths.head,0)::Nil) {
        case ((prev, cnt) :: res, now) =>
          if (prev == now) {
            (prev, cnt+1) :: res
          } else {
            (now, 1) :: (prev, cnt) :: res
          }
        case (Nil, c) => throw new RuntimeException // never!
      }.sortBy{case (x,y) => x}
    }
    def calcBase(pLen: Int, pCnt: Int, pRes: Int, assocSrc: List[(Len,Int)], assocDst: Array[Len]): Array[Len] = {
      assocSrc match {
        case Nil => assocDst
        case (0, cnt) :: rest =>
          calcBase(0, 0, 0, rest, assocDst)
        case (len, cnt) :: rest =>
          val res = (pCnt + pRes) << (len - pLen)
          assocDst(len) = res
          calcBase(len,cnt,res,rest,assocDst)
      }
    }
    val hist = calcHist(src)
    var base = calcBase(0,0,0,hist,new Array[Len](maxCodeLen+1))
    src.sortBy{case (x,y) => x}.map {
      case (sym,len) =>
        val res = base(len)
        base(len) += 1
        (sym,res,len)
    }
  }

  def test () = {
    var buf = new Array[Int](286)
    buf('a') = 3
    buf('b') = 3
    buf('c') = 3
    buf('d') = 3
    buf('e') = 3
    buf('f') = 2
    buf('g') = 4
    buf('h') = 4
    //makeCanonHuffmanTable(buf)
    buf.zipWithIndex.foreach {
      case (cnt,ind) =>
        if (cnt != 0)
          println(ind.toChar + " " + cnt.toString)
    }
  }
  def test2 () = {
    val hist = List(('A':Sym,2),('E':Sym,4),('B':Sym,6),('D':Sym,8),('C':Sym,10))
    var buf = histToLengths(hist,3,255)
    //makeCanonHuffmanTable(buf)
    buf.zipWithIndex.foreach {
      case (cnt,ind) =>
        if (cnt != 0)
          println(ind.toChar + " " + cnt.toString)
    }
  }
}

object Hello extends Greeting with App {
  var bytes = new Array[Byte](20)
  bytes(0) = 'c'
  Huffman.javaImpl(bytes)
  //println(Huffman.codeToTree(0,Huffman.lenToCode(List(('a',2),('b',1),('c',3),('d',3)))))
  //println(Huffman.codeToTree(0,Huffman.lenToCode(List(('a',3),('b',3),('c',3),('d',3),('e',3),('f',2),('g',4),('h',4)))))
  //println(Huffman.lenToCode(List(('a',2),('b',1),('c',3),('d',3))))
  //println(Huffman.lenToCode(List(('a',2),('b',1),('c',3),('d',3))))
}

trait Greeting {
  lazy val greeting: String = "hello"
}
