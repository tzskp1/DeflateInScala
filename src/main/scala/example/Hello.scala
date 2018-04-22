package example

object Huffman {
  type Sym = Int
  case class Chain(weight: Int,symCnt: Int,child: Option[Chain])
  case class Stage(left: Chain,right: Chain,rest: List[(Sym,Int)])

  val sizeOfLitLen = 286
  val maxCodeLen = 3 //15

  def updateStage(stages: List[Stage],cnt: Int) : List[Stage] = {
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
  def initStage(hist: List[(Sym,Int)]) = {
    hist match {
      case (_,lookahead1) :: (_,lookahead2) :: rest =>
        Stage(Chain(lookahead1,1,None),Chain(lookahead2,2,None),rest)
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
  def histToLengths(hist: List[(Sym,Int)]) = {
    //val stage0 = hist.sortBy{case (x,c) => c}
    val num = hist.length
    if (num < 2)
      throw new Exception
      // TODO: error handling
    val stages = for (_ <- (1 to maxCodeLen).toList) yield { initStage(hist) }
    val lastChain = (1 to num - 2).foldLeft(stages){ (acc,_) => updateStage(acc,0) }.head match {
      case Stage(_,right,_) => right
    }
    var res = new Array[Int](sizeOfLitLen)
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
  // lengths of code of char -> codes of char
  def makeCanonHuffmanTable(src: Array[Int]) = {
    def arrayToHist(src: Array[Int]) = {
      // by assumption of order, same numbers are in same segment.
      src.foldLeft (List((src.head,0))) {
        case ((prev, num) :: res, now) =>
          if (prev == now) {
            (prev, num+1) :: res
          }else {
            (now, 1) :: (prev, num) :: res
          }
        case (Nil, c) => throw new RuntimeException // never!
      }.sortBy{case (x,y) => x}
    }
    def calcBase(pLen: Int, pCnt: Int, pRes: Int, assocSrc: List[(Int,Int)], assocDst: Array[Int]): Array[Int] = {
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
    val hist = arrayToHist(src)
    var base = calcBase(0,0,0,hist,new Array[Int](maxCodeLen+1))
    src.zipWithIndex.foreach {
      case (len,ind) =>
        if (len != 0) {
          src(ind) = base(len)
          base(len) += 1
        }
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
    makeCanonHuffmanTable(buf)
    buf.zipWithIndex.foreach {
      case (cnt,ind) =>
        if (cnt != 0)
          println(ind.toChar + " " + cnt.toString)
    }
  }
  def test2 () = {
    val hist = List(('A':Sym,2),('E':Sym,4),('B':Sym,6),('D':Sym,8),('C':Sym,10))
    val buf = histToLengths(hist)
    buf.zipWithIndex.foreach {
      case (cnt,ind) =>
        if (cnt != 0)
          println(ind.toChar + " " + cnt.toString)
    }
  }
}

object Hello extends Greeting with App {
  Huffman.test2()
}

trait Greeting {
  lazy val greeting: String = "hello"
}
