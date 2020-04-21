package com.samples

import com.samples.LinkedListDemo.LinkedList

object LinkedListDemo extends App{

  trait MyPredicate[-T] {
    def test(predicate: T): Boolean
  }

  trait MyTransformer[-A, B] {
    def transform(from: A) : B
  }

  abstract class ILinkedList[+T] {
    def head: T
    def tail: ILinkedList[T]
    def isEmpty: Boolean
    def add[B >: T](element: B): ILinkedList[B]
    def map[B](t : MyTransformer[T,B]) : ILinkedList[B]
    def filter(predicate: MyPredicate[T]) : ILinkedList[T]
    def flatmap[B](t : MyTransformer[T,ILinkedList[B]]): ILinkedList[B]
    def ++[B >: T](list: ILinkedList[B]): ILinkedList[B]
  }

  case object EmptyLinkedList extends ILinkedList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: ILinkedList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](element: B): ILinkedList[B] = new LinkedList(element, EmptyLinkedList)
    def map[B](t : MyTransformer[Nothing, B]) : ILinkedList[B] = EmptyLinkedList
    def filter(predicate: MyPredicate[Nothing]) : ILinkedList[Nothing] = EmptyLinkedList
    def flatmap[B](t : MyTransformer[Nothing, ILinkedList[B]]): ILinkedList[B] = EmptyLinkedList
    override def toString(): String = "[]"
    def ++[B >: Nothing](list: ILinkedList[B]): ILinkedList[B] = list
  }

  case class LinkedList[+T](h: T,  t: ILinkedList[T]) extends ILinkedList[T] {
    def head: T = h
    def tail: ILinkedList[T] = t
    def isEmpty: Boolean = false
    def add[B >: T](element: B): ILinkedList[B] = new LinkedList(element, this)
    def filter(predicate: MyPredicate[T]) : ILinkedList[T] = {
        if(predicate.test(h)) new LinkedList(h, t.filter(predicate))
        else t.filter(predicate)
    }

    def map[B](trans : MyTransformer[T, B]) : ILinkedList[B] = {
       new LinkedList[B](trans.transform(h), t.map(trans))
    }

    def flatmap[B](trans : MyTransformer[T, ILinkedList[B]]): ILinkedList[B] = {
      trans.transform(h) ++ t.flatmap(trans)
    }

    def ++[B >: T](list: ILinkedList[B]): ILinkedList[B] = new LinkedList[B](h, t ++ list)

    override def toString(): String = h.toString

  }

  //create a LinkedList of Int
  val myListInt = new LinkedList[Int](1, new LinkedList(2, new LinkedList(3, EmptyLinkedList)))
  iterateList[Int](myListInt)
  println()
  //create a LinkedList of String
  val myListStr = new LinkedList[String]("first", new LinkedList("second", new LinkedList("third", EmptyLinkedList)))
  iterateList[String](myListStr)

  //generic method to iterate a list
  def iterateList[A](node: ILinkedList[A]): Unit = {
    def iterateUtil[A](next: ILinkedList[A]): Unit = {
      if (!next.isEmpty) {
        print(next + " ")
        iterateUtil(next.tail)
      }
    }
    print("[")
    iterateUtil(node)
    print("]")
  }

  val intList = EmptyLinkedList.add(3).add(2).add(1)
  val mapList = intList.map(new MyTransformer[Int, Int] {
    override def transform(elem : Int): Int = 2 * elem
  })

  iterateList((mapList))

  val filterList = intList.filter(new MyPredicate[Int] {
    override def test(x : Int) : Boolean = if(x % 2 == 0) true else false
  })

  iterateList(filterList)

  val flatList = intList.flatmap(new MyTransformer[Int, ILinkedList[Int]] {
    override def transform(elem : Int): ILinkedList[Int] = {
      new LinkedList(elem, new LinkedList(elem+1, EmptyLinkedList))
    }
  })

  iterateList(flatList)
}