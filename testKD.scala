class Run{  
  def main() = {
    println
    println("Traversals :")
    println
    println("DFS:")
    tree.dfsTrav(print)
    println
    println
    println("BFS:")
    tree.bfsTrav(print)
    println
    println
    var numFormatVar = true
    var input_x = 0.0
    var input_y = 0.0
    println
    try{
      println("Give a Node point:")
      print("Enter X: ")
      input_x=scala.io.StdIn.readDouble()
      println(input_x)
      print("Enter Y: ")
      input_y=scala.io.StdIn.readDouble()
      println(input_y)
    }catch{
      case ex: NumberFormatException => numFormatVar=false;
    }
    if(numFormatVar){
      try{
        println("Parent: " + input_x + " , "+ input_y + "\nLeft child " + tree.search(input_x, input_y).left.datax + " , " + tree.search(input_x, input_y).left.datay)
      }catch {
        case ex: NullPointerException => println("Parent: " +  input_x+ " , "+ input_y  + "\nLeft child " + ex.getMessage)
      }
      try {
        println("Right child " + tree.search(input_x, input_y).right.datax + " , " + tree.search(input_x, input_y).right.datay)
      } catch {
        case ex: NullPointerException => println("Right child " + ex.getMessage)
      }
    }else{
      println("The x , y are numbers")
    }
  }
}

new Run().main
