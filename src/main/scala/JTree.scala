import java.util.concurrent.{RecursiveAction, RecursiveTask}

abstract class JTree extends RecursiveTask[Double] {
  def setArr(a:Array[Array[Cell]]):Unit
  def computeT(): Double = {
    compute()
  }
}

class Interior(q1:JTree,q2:JTree,q3:JTree,q4:JTree) extends JTree {
  def compute(): Double = {
    q1.invoke() +
    q2.invoke() +
    q3.invoke() +
    q4.computeT()
  }

  override def setArr(a:Array[Array[Cell]]): Unit = {
    q1.setArr(a)
    q2.setArr(a)
    q3.setArr(a)
    q4.setArr(a)
  }
}

class Leaf(var arr:Array[Array[Cell]],var out:Array[Array[Cell]],val tl:Coord,val br:Coord) extends JTree {
  override def compute(): Double = {
    var diff:Double = 0
    for(i <- tl.x until br.x) {
      for(j <- tl.y until br.y) {
        val oldCell = arr(i)(j)
        val neighbors = getNeighbors(i,j,arr)
        val partTemps = tuple3toSeq(neighbors.map(_.tempProps()).reduce((a,b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))).map(p => p/neighbors.length)
        val newTemp = (partTemps(0) * oldCell.cm1) + (partTemps(1) * oldCell.cm2) + (partTemps(2) * oldCell.cm3)
        diff += newTemp - oldCell.temp
        out(i)(j) = oldCell.copy(temp = newTemp)
        //code here returns total difference
      }
    }
    diff
  }

  override def setArr(a: Array[Array[Cell]]): Unit = (arr = a)

  private def tuple3toSeq[T](t: (T,T,T)): Seq[T] = Seq(t._1,t._2,t._3) //converts a tuple of 3 of the same type to a Seq

  def getNeighbors(x:Int,y:Int,arr:Array[Array[Cell]]): IndexedSeq[Cell] = {
    var neighbors = IndexedSeq[Cell]()
    if(x > 0)
      neighbors :+= arr(x-1)(y)

    if(x < arr.length)
      neighbors :+= arr(x+1)(y)

    if(y > 0)
      neighbors :+= arr(x)(y-1)

    if(y < arr(x).length)
      neighbors :+= arr(x)(y+1)

    neighbors
  }
}

class Jacobi(var old:Array[Array[Cell]],val t:Double,val s:Double,alloy:Alloy, val maxSteps:Int) {
  var heat1 = t
  var heat2 = s
  val maxDiff = 0.01
  val minSize = 30
  val roomTempt = 20
  //var old = Array.fill[Cell](arr.length,arr(0).length)(alloy.randomCell(20)) //room temp 20
  var out:Array[Array[Cell]] = Array.ofDim[Cell](old.length,old.head.length)
  val root:JTree = build(old,Coord(0,0),Coord(old.length,old(0).length))


  private def build(arr:Array[Array[Cell]],tl:Coord,br:Coord): JTree = {
    if((br.x - tl.x) * (br.y - tl.y) < minSize)
      new Leaf(arr,out,tl,br)
    else {
      val midX = ((br.x + tl.x)/2).toInt
      val midY = ((br.y + tl.y)/2).toInt
      new Interior(
        build(arr,Coord(tl.x,tl.y),Coord(midX,midY)),
        build(arr,Coord(midX,tl.y),Coord(br.x,midY)),
        build(arr,Coord(tl.x,midY),Coord(midX,br.y)),
        build(arr,Coord(midX,midY),Coord(br.x,br.y))
      )
    }
  }

  def compute(): Array[Array[Cell]] = {
    //loop until converged
    var steps = 0
    var difference:Double = 100
    while(difference > maxDiff && steps < maxSteps) {
      //graphics here
      difference = root.computeT()
      old = out.clone()
      root.setArr(old)
      //updateHeatingWaveSynced(20)  //will be tested when graphics

      steps += 1
      println(s"Step: $steps")
    }
    out
    //this is where the graphics stuff happens
  }

  def updateHeating(t:Double,s:Double): Unit = {
    heat1 = t
    heat2 = s
  }
  def updateHeatingWaveSynced(amplitude:Double): Unit = { //might look cool, we'll see
    val heat = math.sin(2 * math.Pi * 3 * (System.currentTimeMillis()/1000.0))
  }
  //i did something better, should be unused but i like to look at this monstrosity
  def getDifference(): Double = {
    old.indices.map(_.toDouble).reduce[Double]((a,b) => a + old(b.toInt).indices.map(_.toDouble).reduce[Double]((i,j) => i + math.abs(old(b.toInt)(j.toInt).temp - out(b.toInt)(j.toInt).temp))) //why do i do this to myself
  }
}


case class Coord(x:Int,y:Int)