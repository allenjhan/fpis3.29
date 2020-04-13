object TreeFoldTest extends App {

  val myTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(Tree.size(myTree))
  println(Tree.maximum(myTree))
  println(Tree.depth(myTree))
  println(Tree.map(myTree)(x => x+1))

}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def size[A](t: Tree[A]): Int = fold(t)(z => 1)((x, y) => x+y+1)

  def maximum(t: Tree[Int]): Int = fold(t)(identity)((x, y) => x max y)

  def depth[A](t: Tree[A]): Int = fold(t)(z => 1)((x,y) => (x max y) + 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    val aToB: A => Tree[B] = z => Leaf(f(z))
    fold(t)(aToB)((x,y) => Branch(x, y))
  }

}
