package infra

import scala.annotation.tailrec

sealed trait Bit
case class Zero() extends Bit
case class One() extends Bit

type Code = List[Bit]

sealed trait Tree
case class Leaf(freq: Int, letter: String) extends Tree
case class Node(freq: Int, left: Tree, right: Tree) extends Tree

def getFreq(tree: Tree): Int = tree match
  case Leaf(f, l) => f
  case Node(f, l, r) => f

def getCounts(s: String): Map[String, Int] =
  s
    .split("")
    .groupBy(identity)
    .map((l, a) => (l, a.length))

def sortTreeList(tlist: List[Tree]): List[Tree] =
  tlist.sortBy(x => getFreq(x))

def makeLeaves(mp: Map[String, Int]): List[Tree] =
  mp.map((l, f) => Leaf(f, l)).toList

def buildTree(s: String): Tree =
  val leaves = sortTreeList(makeLeaves(getCounts(s)))

  @tailrec
  def build(lst: List[Tree]): Tree = lst match
    case Nil => Leaf(0, "")
    case x :: Nil => x
    case x :: y :: rest => build(sortTreeList(Node(getFreq(x) + getFreq(y), x, y) :: rest))
  build(leaves)

def buildCodes(tree: Tree): Map[String, Code] =
  def build(code: Code, tree: Tree): List[(String, Code)] =
    tree match
      case Leaf(_, l) => List((l, code.reverse))
      case Node(_, l, r) => build(One() :: code, l) ::: build(Zero() :: code, r)

  build(List(), tree).toMap

def bitToString(bit: Bit): String =
  bit match
    case One() => "1"
    case Zero() => "0"

def codeToString(code: Code): String =
  code.map(x => bitToString(x)).mkString

def encode(s: String): String =
  val tree = buildTree(s)
  val codes = buildCodes(tree)
  s
    .map(x => codes(x.toString))
    .map(x => codeToString(x))
    .mkString
