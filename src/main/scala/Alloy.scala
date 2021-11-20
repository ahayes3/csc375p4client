import scala.util.Random

class Alloy(val width:Int,height:Int,val c1:Double,val cm1:Double,val c2:Double,val cm2:Double, val c3:Double,val cm3:Double) {
  val arr = Array.fill[Cell](width,height)(randomCell(Alloy.roomTemp))
  def randomCell(startTemp:Double): Cell = {
    val maxVar = Random.between(0,25)
    val props = Array(c1,c2,c3)
    for(i <- 0 until maxVar) {
      val choice = Random.between(0,3)
      val neg = if(Random.nextBoolean()) 1 else -1

      props(choice) += (.01 * neg)

      for(j <- props.indices) {
        if(j != choice)
          props(j) -= (.005 * neg)
      }
    }
    Cell(props(0),cm1,props(1),cm2,props(2),cm3,startTemp)
  }
}

object Alloy {
  val roomTemp = 20
}


case class Cell(val c1:Double,val cm1:Double,val c2:Double,val cm2:Double, val c3:Double,val cm3:Double,var temp:Double) {
  def tempProps(): (Double,Double,Double) = {
    (c1 * temp, c2 * temp, c3* temp)
  }
}