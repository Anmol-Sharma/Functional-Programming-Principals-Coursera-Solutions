package recfun

import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ( c==0 || c==r ){return 1}

      def loop(arr:ArrayBuffer[Int],tot:Int): ArrayBuffer[Int] = {
        if (tot==r){return arr}
        else
          {
            if (arr.size==1){arr+=1;loop(arr,tot+1)}
            else {
                var myarr = ArrayBuffer[Int]()
                myarr += 1
                for (i <- 0 to arr.size-2)
                  {
                    myarr += arr(i) + arr(i+1)
                  }
                myarr+=1
              loop(myarr,tot+1)
            }
          }
      }
      val pres_buff=ArrayBuffer[Int]()
      pres_buff+=1
      val res = loop(pres_buff,0)
      return res(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val stack = ArrayBuffer.empty[Char]
      def push(ch: Char) : Unit = { stack += ch }
      def pop () : Unit = {stack.remove(0)}
      var flag = false
      def loop(cr:List[Char]) : Unit = {
        if (cr.isEmpty) return
        else
          {
            if (cr.head == '(') {push('(');loop(cr.tail)}
            else if (cr.head == ')')
              {
                if(stack.isEmpty) {flag=true;return}
                else {pop();loop(cr.tail)}
              }
            else {loop(cr.tail)}
          }
      }
      loop(chars)
      if (flag == true) { return false}
      else { if (stack.isEmpty) {return true} else return false}
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def all_ways(money:Int, coins : List[Int], index : Int) : Int = {
        if (money < 0) return 0
        if (money == 0) return 1

        if (index == coins.length && money > 0) {return 0}
        all_ways(money - coins(index), coins, index) + all_ways(money , coins, index + 1)
      }
      all_ways(money,coins,0)
    }
}
