package main

class Phonebook {
  val phonebook:HashTable = new HashTable(50)

  def addContact(name:String, number: Long)={
    phonebook.insert(name, number)

  }
  def deleteContact(name:String)={
    phonebook.delete(name)
  }
  def getNumber(name:String):Long={
    phonebook.get(name)
  }

  override def toString(): String = {
    phonebook.toString()
  }
}
