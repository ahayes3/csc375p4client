object Main {
  def main(args: Array[String]): Unit = {
    val height = 5
    val width = height*2
    val s = 200
    val t = 200
    val c1 = .75
    val c2 = 1.0
    val c3 = 1.25
    val alloy = new Alloy(50,1/3,c1,1/3,c2,1/3,c3)
    val jacobi = new Jacobi(alloy.arr,t,s,alloy,100)


  }
}
