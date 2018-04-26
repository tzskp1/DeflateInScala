package Deflate

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

  val litLenSize= 286
  val maxCodeLen = 15

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

  def lenToTree(len:List[(Sym,Len)]) = codeToTree(lenToCode(len))

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
  def histToLengths(hist: List[(Sym,Int)],upper: Int = maxCodeLen,arraySize: Int = litLenSize) = {
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
}

