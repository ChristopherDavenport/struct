package io.chrisdavenport.struct

import cats._
import scala.annotation.tailrec

sealed trait LeafTree[A]

object LeafTree {
  final case class Branch[A](left: LeafTree[A], right: LeafTree[A]) extends LeafTree[A]
  final case class Leaf[A](value: A) extends LeafTree[A]

  def of[A](a: A, xs: A*): LeafTree[A] = {
    xs.foldLeft(leaf(a))((tree, a) => Branch(tree, leaf(a)))
  }
  def branch[A](left: LeafTree[A], right: LeafTree[A]): LeafTree[A] = Branch(left, right)
  def leaf[A](value: A): LeafTree[A] = Leaf(value)


  // This Acts as a Non-Empty Tree as All Tree contain a value -> 
  // Should be rewritten if possible to non-empty-traverse
  implicit val finalInstancesForTree : SemigroupK[LeafTree] 
    with Traverse[LeafTree]
    with CoflatMap[LeafTree]
    with Monad[LeafTree] = new SemigroupK[LeafTree] 
      with Traverse[LeafTree]
      with CoflatMap[LeafTree]
      with Monad[LeafTree] {
    override def pure[A](a: A) = leaf(a)
    override def flatMap[A, B](fa: LeafTree[A])(f: A => LeafTree[B]): LeafTree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => LeafTree[Either[A, B]]): LeafTree[B] = {
      @tailrec
      def loop(open: List[LeafTree[Either[A, B]]], closed: List[LeafTree[B]]): List[LeafTree[B]] = open match {
        // Head of List is a Branch
        // Important Job is decomposing the branches and add those to
        // the list of open trees
        case Branch(l, r) :: next => 
          l match {
            // Left is also a branch, add this explicitly to the list of open trees 
            case Branch(_, _) => 
              loop(l :: r :: next, closed)
            // Left is untransformed Leaf, transform and then add to stack
            case Leaf(Left(a)) => 
              loop(f(a) :: r :: next, closed)
            // Left Value Is Transformed B, Move onto Closed List
            case Leaf(Right(b)) =>
              loop(r :: next, pure(b) :: closed)
          }
        // Head of List is Untransformed Leaf
        case Leaf(Left(a)) :: next  =>
          loop(f(a):: next, closed)
        // Head of List is transformed
        // Must Rebuild Tree Out of Trees
        case Leaf(Right(b)) :: next => 
          closed match {
            // Upon Two Values in Stack, Combine them Into A Single Tree
            case head :: tail =>
              loop(next, Branch(head, pure(b)) :: tail)
            // Empty Closed So Just Add The New Tree
            case Nil => 
              loop(next, pure(b):: Nil)
          }
        // Empty List Return List of Closed
        case Nil => closed
      }
      // Initiate With The Single Value -> 
      // Empty is Closed
      // Head As We should End With a Single Value in the Closed List As We Combine on more than one into a Branch
      loop(f(a) :: Nil , List.empty[LeafTree[B]]).head
    }

    def coflatMap[A, B](fa: LeafTree[A])(f: LeafTree[A] => B): LeafTree[B] = leaf(f(fa))

    def foldLeft[A, B](fa: LeafTree[A], b: B)(f: (B, A) => B): B = fa match {
      case Leaf(a) => f(b, a)
      case Branch(l, r) => foldLeft(r, foldLeft(l, b)(f))(f)
    }
    def foldRight[A, B](fa: LeafTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa match {
        case Leaf(a) => f(a, lb)
        case Branch(l, r) => foldRight(r,foldRight(l, lb)(f))(f)
      }
    }
    
    def traverse[G[_]: Applicative, A, B](fa: LeafTree[A])(f: A => G[B]): G[LeafTree[B]] = fa match {
      case Leaf(a) => Functor[G].map(f(a))(b => leaf(b))
      case Branch(l, r) => Apply[G].map2(traverse(l)(f), traverse(r)(f))(Branch(_, _))
    }

    def combineK[A](x: LeafTree[A],y: LeafTree[A]): LeafTree[A] = branch(x, y)

}

}

