package com.samples

import com.samples.LinkedListDemo.LinkedList

object LinkedListDemo extends App{

  abstract class ILinkedList {
    def head: Int
    def tail: ILinkedList
    def isEmpty: Boolean
    def addInt(element: Int): ILinkedList
  }

  object EmptyLinkedList extends ILinkedList {
    def head: Int = throw new NoSuchElementException
    def tail: ILinkedList = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def addInt(element: Int): ILinkedList = new LinkedList(element, EmptyLinkedList)
    override def toString(): String = "[]"
  }

  class LinkedList(h: Int,  t: ILinkedList) extends ILinkedList {
    def head: Int = h
    def tail: ILinkedList = t
    def isEmpty: Boolean = false
    def addInt(element: Int): ILinkedList = new LinkedList(element, this)
    override def toString(): String = h.toString
  }

  //test the LinkedList class
  println(EmptyLinkedList.toString())
  val myList = new LinkedList(1, new LinkedList(2, new LinkedList(3, EmptyLinkedList)))

  def iterateList(node: ILinkedList): Unit = {
    def iterateUtil(next: ILinkedList): Unit = {
      if (!next.isEmpty) {
        print(next + " ")
        iterateUtil(next.tail)
      }
    }
    print("[")
    iterateUtil(node)
    print("]")
  }

  iterateList(myList)
}