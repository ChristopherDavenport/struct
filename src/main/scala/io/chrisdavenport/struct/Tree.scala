package io.chrisdavenport.struct

import cats._
// import cats.implicits._

sealed trait Tree[+A]

object Tree {
  case class Present[A](a: LeafTree[A]) extends Tree[A]
  case object Empty extends Tree[Nothing]
  def apply[A](a: A*): Tree[A] = {
    val l = a.toList
    l.headOption.fold(empty[A])(head => Present(LeafTree.of(head, l.drop(1):_*)))
  }
  def empty[A] : Tree[A] = Empty
  def ofTree[A](a: LeafTree[A]): Tree[A] = Present(a)

  implicit val treeInstance : Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = ofTree(LeafTree.leaf(a))
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = ???
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = ???
  }
}

