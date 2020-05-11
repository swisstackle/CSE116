package main

class Node[A](var value: A, var next: Node[A]) {
  override def toString: String = {
    if (this.next == null) {
      this.value.toString
    } else {
      this.value.toString + ", " + this.next.toString
    }
  }
}
