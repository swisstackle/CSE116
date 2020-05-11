package main

/**
 * My own implementation of a main.HashTable with String to Longeger. Feedback always welcome
 *
 * @author
 *         Alain Schaerer, University at Buffalo
 *
 * @param initial_size
 *                     Initial cardinality of the hashtable/array
 */

class HashTable(initial_size:Int) {
  var cardinality:Int = initial_size
  private var array:Array[Node[(String, Long)]] = new Array[Node[(String, Long)]](initial_size)
  private var keyAmount = 0
  private var test = 0
  /**
   *
   * @param S
   *          The String to hash
   * @return
   *         Returns the final hashvalue with respect to the cardinality of the hashtable. Returns an Integer between 0 and the cardinality.
   *         Probability of collision is 1/m + 1/p where m is the cardinality of the hashtable and p is a big primenumber (in this case 100000019)
   *         So in other words, the probability of collision is 1/m if p is big enough
   */
  def hash(S:String):Int={
    rehash()
    val bignumber = hashString(S,100000019, 199)
    hashInteger(bignumber, 199,5,100000019)
  }

  /**
   * @param S
   *          The value of the String to be hashed
   * @param p
   *          A big primenumber
   * @param x
   *          An Integer between 0 and p-1
   * @return
   *         Returns the hash value. However, this is not the final hashvalue, since it has not been adapted to the cardinality of our hashtable
   */
  private def hashString(S:String, p:Int, x:Int):Int={
    hashStringHelper(S,p,x,0,0)
  }

  /**
   * Recursion for the hashString() method
   * Each character gets hashed the following way: (x * (hashNumber from before) * Character (mod p))
   * where p is a big prime number and x some nunber between 0 and p-1
   */
  private def hashStringHelper(S:String, p:Int, x:Int, newHash:Int, i:Int):Int={
    var nHash = newHash
    if (i < S.length){
      nHash =  hashStringHelper(S, p, x,  (x * nHash + S(i.toInt)) % p , (i+1))
    }
    nHash
  }

  /**
   *
   * @param x
   *          The Integer to hash
   * @param a
   * @param b
   * @param p
   * @return
   *         Hashes the Integer with respect to the cardinality of the hashtable. Returns a value between 0 and the cardinality
   */
  def hashInteger(x:Int, a:Int, b:Int, p:Int): Int ={
    Math.abs(((a*x+b) % p) % cardinality).toInt
  }

  /**
   * Inserts key value pair
   * @param key
   *            key that is going to be hashed
   * @param value
   *              value to insert
   */
  def insert(key:String, value:Long)={
    val id = this.hash(key)
    if(array(id) == null){
      keyAmount+=1
      array(id) = new Node[(String, Long)]((key, value),null)
    }else{
      keyAmount+=1
      insertHelper(key, value, array(id))
    }

  }

  /**
   * In case of when a linked list already existed, we have to traverse through until we find a null value
   */
  private def insertHelper(key:String, value:Long, next:Node[(String, Long)]): Unit ={
    if(next.next != null){
      insertHelper(key, value, next.next)
    }else{
      next.next = new Node[(String, Long)]((key, value),null)
    }
  }

  def get(key:String):Long={
    val id = hash(key)
    val list = array(id)

    if(list.value._1 == key){
      list.value._2
    }else{
      getHelper(key, list)
    }
  }

  private def getHelper(key:String, next:Node[(String, Long)]):Long={
    if(next.next.value._1 != key){
      getHelper(key, next.next)
    }else{
      next.next.value._2
    }
  }

  /**
   * Deletes key-value pair
   * @param key
   *            key of key-value pair to delete
   */
  def delete(key:String)={
    val id = this.hash(key)
    if(array(id).value._1 == key){
      array(id) = null
      // TODO: Rearange the array, so that we don't have null values
    }else{
      deleteHelper(key, array(id))
    }
  }

  /**
   * In case of when a linked list already existed, we have to traverse through until we find a null value
   */
  private def deleteHelper(key:String, next:Node[(String, Long)]): Unit ={
    if(next.next.value._1 == key){
      next.next = next.next.next
    }else{
      deleteHelper(key, next.next)
    }
  }



  /**
   *  Resizes the array if the loadfactor is above 0.9 to 2 times the size of the size before, which puts the loadfactor to 0.9*1/2
   *  Does so, so that we can assure a dynamic size of the array
   */
  def rehash()={
    if (getLoadFactor() > 0.9){
      val temp = this.array
      this.cardinality = 2 * this.cardinality

      this.array = new Array[Node[(String, Long)]](this.cardinality)
      for (list<- temp ) {
        var listt = list
        while(listt !=null){
          insert(listt.value._1, listt.value._2)
          keyAmount-=1
          listt = listt.next
        }
      }
    }
  }



  def getLoadFactor():Double={
    this.keyAmount.toDouble/this.cardinality.toDouble
  }


  def print()={
    for ( list<- this.array ) {
        if(list!=null){
          println(list.toString)
        }
    }
  }

  override def toString: String = {
    var s:String = ""
    for (list <- this.array) {
      if(list!=null){
        s += list.toString
      }
    }
    s
  }

}

