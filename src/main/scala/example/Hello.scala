package example

object Huffman {
  type Sym = Int
  case class Chain(weight: Int,totalCnt: Int,symCnt: Int,next: List[(Sym,Int)],child: Option[Chain])

  val sizeOfLitLen = 286
  val maxCodeLen = 15
  def histToLengths(hist: List[(Sym,Int)]) = {
    //val stage0 = hist.sortBy{case (x,c) => c}
    val num = hist.length
    def calcNextChain(stages: List[Chain]) : List[Chain] = {
      stages match {
        case Nil => Nil
        case Chain(_,totalCnt,symCnt,(_,lookahead1: Int)::((_,lookahead2: Int)::(base2: List[(Sym,Int)]) as base1) as orig,child) ::
          ((Chain(weight,_,_,_,_) as pre) :: _ as (rest : List[Chain])) =>
          if (lookahead1 < weight) {
            if (lookahead2 < weight) {
              calcNextChain(Chain(lookahead2, totalCnt + 2, symCnt + 2, base2, child) :: rest)
            } else {
              calcNextChain(Chain(lookahead1, totalCnt + 1, symCnt + 1, base1, child) :: rest)
            }
          } else {
            Chain(weight,totalCnt+1,symCnt,orig,Some(pre)) :: calcNextChain(rest)
          }
      }
    }
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
}

object Hello extends Greeting with App {
  Huffman.test()
}

trait Greeting {
  lazy val greeting: String = "hello"
}
