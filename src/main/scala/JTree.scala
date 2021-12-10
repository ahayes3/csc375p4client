import org.lwjgl.glfw.GLFW.{glfwPollEvents, glfwSwapBuffers, glfwWindowShouldClose}
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.{GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT, GL_MODELVIEW, GL_PROJECTION, GL_QUADS, glBegin, glClear, glClearColor, glColor3f, glEnd, glLoadIdentity, glMatrixMode, glOrtho, glVertex3f, glVertex3i, glViewport}

import java.net.{Inet4Address, InetSocketAddress, SocketException}
import java.nio.{BufferOverflowException, ByteBuffer}
import java.nio.channels.SocketChannel
import java.util.concurrent.{RecursiveAction, RecursiveTask}

abstract class JTree extends RecursiveTask[Double] {
  def setArr(a: Array[Array[Cell]]): Unit

  def computeT(): Double = {
    compute()
  }

  def leaves(): Seq[Leaf]
}

class Interior(q1: JTree, q2: JTree, q3: JTree, q4: JTree) extends JTree {
  val quads = Seq(q1, q2, q3, q4)

  def compute(): Double = {
    q1.fork()
    q2.fork()
    q3.fork()

    val a = q4.computeT()
    val b = q1.join() + q2.join() + q3.join()
    q1.reinitialize()
    q2.reinitialize()
    q3.reinitialize()
    a + b
  }

  override def leaves(): Seq[Leaf] = {
    q1.leaves() :++ q2.leaves() :++ q3.leaves() :++ q4.leaves()
  }

  override def setArr(a: Array[Array[Cell]]): Unit = {
    q1.setArr(a)
    q2.setArr(a)
    q3.setArr(a)
    q4.setArr(a)
  }
}

class Leaf(var arr: Array[Array[Cell]], var out: Array[Array[Cell]], val tl: Coord, val br: Coord) extends JTree {
  override def compute(): Double = {
    var diff: Double = 0
    for (i <- tl.x until br.x) {
      for (j <- tl.y until br.y) {
        val oldCell = arr(i)(j)
        val neighbors = getNeighbors(i, j, arr)

        val thermConsts = (neighbors(0).cm1, neighbors(0).cm2, neighbors(0).cm3)
        val partTemps = neighbors.map(p => p.tempProps()).reduce((a, b) => ((a._1 + b._1), (a._2 + b._2), (a._3 + b._3)))
        val adjusted: (Double, Double, Double) = ((partTemps._1 * oldCell.cm1), ((partTemps._2 * oldCell.cm2)), ((partTemps._3 * oldCell.cm3)))
        val newTemp: Double = (adjusted._1 + adjusted._2 + adjusted._3) / neighbors.length


        diff += math.abs(newTemp - oldCell.temp)

        out(i)(j) = oldCell.copy(temp = newTemp)
      }
    }
    diff
  }

  override def leaves(): Seq[Leaf] = {
    Seq(this)
  }

  override def setArr(a: Array[Array[Cell]]): Unit = (arr = a)

  private def tuple3toSeq[T](t: (T, T, T)): Seq[T] = Seq(t._1, t._2, t._3) //converts a tuple of 3 of the same type to a Seq

  def getNeighbors(x: Int, y: Int, arr: Array[Array[Cell]]): IndexedSeq[Cell] = {
    var neighbors = IndexedSeq[Cell]()
    if (x > 0)
      neighbors :+= arr(x - 1)(y)

    if (x < arr.length - 1)
      neighbors :+= arr(x + 1)(y)

    if (y > 0)
      neighbors :+= arr(x)(y - 1)

    if (y < arr(x).length - 1)
      neighbors :+= arr(x)(y + 1)

    neighbors
  }

  def put(buff: ByteBuffer,size:ByteBuffer,index:Int): Unit = {
    //put a boolean in for if there is padding on top right bottom and left
    var padBits = 0
    var top =false
    var right = false
    var bottom = false
    var left = false

    if (tl.y > 0) {
      padBits += Padding.TOP
      top = true
    }
    if (br.x < arr.length) {
      padBits += Padding.RIGHT
      right =true
    }
    if (br.y < arr(0).length) {
      padBits += Padding.BOTTOM
      bottom=true
    }
    if (tl.x > 0) {
      padBits += Padding.LEFT
      left=true
    }

    val width = br.x - tl.x + (if(left) 1 else 0) + (if(right)1 else 0)
    val height = br.y - tl.y + (if(top) 1 else 0) + (if(bottom)1 else 0)
    val stx = if(left) tl.x - 1 else tl.x
    val ex = if(right) br.x + 1 else br.x
    val sty = if(top) tl.y -1 else tl.y
    val ey = if(bottom) br.y + 1 else br.y

    buff.putInt(index)
    buff.putInt(padBits) //padding
    buff.putInt(width)
    buff.putInt(height)

    size.putInt(width)
    size.putInt(height)

    for(i <- stx until ex) {
      for(j <-sty until ey) {
        try {
          arr(i)(j).put(buff)
        }catch {
          case e:BufferOverflowException => e.printStackTrace()
        }
      }
    }
  }

}
case class Server(path:String, port:Int)

class Jacobi(var old: Array[Array[Cell]], val t: Double, val s: Double, alloy: Alloy, val maxSteps: Int, val cellSize: Int) {
  var heat1 = t
  var heat2 = s
  val maxDiff = 50
  val minSize = 20
  val roomTemp = Alloy.roomTemp
  private val graphicMaxHeat = math.max(heat1, heat2)
  var servers: Seq[Server] = Seq(Server("wolf.cs.oswego.edu",8001), Server("rho.cs.oswego.edu",8001),Server("altair.cs.oswego.edu",8001))
  //var servers: Seq[Server] = Seq(Server("localhost",8001))

  var out: Array[Array[Cell]] = Array.ofDim[Cell](old.length, old.head.length)
  val root: JTree = build(old, Coord(0, 0), Coord(old.length, old(0).length))
  val buff = ByteBuffer.allocate(10000)


  private def build(arr: Array[Array[Cell]], tl: Coord, br: Coord): JTree = {
    if ((br.x - tl.x) * (br.y - tl.y) < minSize)
      new Leaf(arr, out, tl, br)
    else {
      val midX = ((br.x + tl.x) / 2).toInt
      val midY = ((br.y + tl.y) / 2).toInt
      new Interior(
        build(arr, Coord(tl.x, tl.y), Coord(midX, midY)),
        build(arr, Coord(midX, tl.y), Coord(br.x, midY)),
        build(arr, Coord(tl.x, midY), Coord(midX, br.y)),
        build(arr, Coord(midX, midY), Coord(br.x, br.y))
      )
    }
  }

  def compute(window: Option[Long]): Array[Array[Cell]] = {
    out = old.map(p => p.clone())
    GL.createCapabilities()
    glClearColor(1, 1, 1, 0)
    glViewport(0, 0, 1920, 1080)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, 1920, 1080, 0, 1, -1)
    glMatrixMode(GL_MODELVIEW)

    var steps = 0
    var difference: Double = 100
    var color: (Float, Float, Float) = null

    while (!glfwWindowShouldClose(window.get) && difference > maxDiff && steps < maxSteps) {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      applyHeat()

      //todo: changes here

      val leaves = root.leaves()
      val buffs = leaves.map(_ => ByteBuffer.allocate(10000))
      val sizes = leaves.map(_ => ByteBuffer.allocate(8))
      val recBuffs = buffs.indices.map(_ => ByteBuffer.allocate(10000))
      val positions = for(i <- leaves.indices) yield {
        buffs(i).clear()
        leaves(i).put(buffs(i),sizes(i),i)
        sizes(i).flip()
        buffs(i).flip()
        leaves(i).tl
      }
      //positions represents inclusively the top right of the area

//      val sockets = for(i <- servers.indices) yield {
//        val socket = SocketChannel.open()
//        val s = servers(i)
//        socket.connect(new InetSocketAddress(s._1,s._2))
//        socket
//      }
      val sockets = sendAll(sizes,buffs)
      val outputs = receiveAll(positions, sockets)

      for(o <- outputs) {
        val c =o._2
        val m = o._1
        for(i <- m.indices) {
          for (j <- m(i).indices) {
            out(c.x + i)(c.y+j).temp = m(i)(j)
          }
        }
      }
//      outputs.foreach(p => {
//        val c = p._2
//        val m = p._1
//        for(i <- c.x until c.x+m.length) {
//          for(j <- c.y until c.y+m(i).length) {
//            println(c)
//            old(i)(j).temp = m(i)(j)
//          }
//        }
//      })
      difference = 0
      for(i <- out.indices) {
        for(j <- out(i).indices) {
          difference += out(i)(j).temp - old(i)(j).temp
        }
      }
      //difference = root.computeT()

      //no longer needed, out unused, probably can delete it
      old = out.map(p => p.clone())


      //todo: don't need to change past here
      root.setArr(old)
      //updateHeatingWaveSynced(20)  //will be tested when graphics
      steps += 1
      if (steps % 100 == 0)
        println(s"Step: $steps")

      glBegin(GL_QUADS)

      for (i <- old.indices) {
        for (j <- old(i).indices) {
          if(old(i)(j)==null)
            println("NULL")
          color = interpolateHeatColor(old(i)(j).temp)
          glColor3f(color._1, color._2, color._3)
          rect(i * cellSize, j * cellSize, cellSize, cellSize)
        }
      }
      glEnd()


      glfwSwapBuffers(window.get)
      glfwPollEvents()
      println(s"Difference: $difference    Steps: $steps")
    }
    out
  }

  private def sendAll(sizes:Seq[ByteBuffer], buffs:Seq[ByteBuffer]): Seq[SocketChannel] = {
    for(i <- buffs.indices) yield {
      val server = servers(i%servers.length)
      val channel = SocketChannel.open(new InetSocketAddress(server.path,server.port))
      channel.finishConnect()

      //println(channel.isConnectionPending)

      channel.write(sizes(i))

      val written = channel.write(buffs(i))
      if(buffs(i).remaining() > 0)
        throw new Exception()
      channel
    }
  }

  private def receiveAll(positions:Seq[Coord], sockets:Seq[SocketChannel]): Seq[(Array[Array[Double]],Coord)] = {
    val buff = ByteBuffer.allocate(10000)
    for(i <- sockets) yield {
      buff.clear()
      i.configureBlocking(false)
//      while({
//        val read = i.read(buff)
//        read > 0
//      }){}
      var totalBytes = 0
      //println(s"Index: ${sockets.indexOf(i)}")
      while({
        val bytesRead = i.read(buff)
        if(bytesRead == -1)
          throw new SocketException()
        totalBytes += bytesRead
//        println(s"Bytes Read: $bytesRead    Total Bytes: $totalBytes")
//        println(s"Loop: ${bytesRead != 0 && totalBytes != 0}")
        (bytesRead != 0 || totalBytes == 0)
      }){}
      //println(s"TOTAL bytes: $totalBytes")
      val tmp = ByteBuffer.allocate(4)
      tmp.putInt(-2)
      tmp.flip()
      i.configureBlocking(true)
      i.write(tmp) //lets server know it can close connection

      buff.flip()
      if(!buff.hasRemaining)
        println("HERE")
      val idx = buff.getInt()
      //println(s"Index: $idx")
      val width = buff.getInt()
      val height = buff.getInt()
      i.close()
      val matrix = (for(i <- 0 until width) yield {
        (for(j <- 0 until height) yield {
          buff.getDouble()
        }).toArray
      }).toArray
      (matrix,positions(idx))
    }
  }

  private def resend(): Unit = {

  }

  private def rect(x: Int, y: Int, width: Int, height: Int): Unit = {
    glVertex3i(x, y, 0)
    glVertex3i(x + width, y, 0)
    glVertex3i(x + width, y + height, 0)
    glVertex3i(x, y + height, 0)
  }

  private def applyHeat(): Unit = {
    old(0)(0).temp = heat1
    old.last.last.temp = heat2
  }


  private def updateHeating(t: Double, s: Double): Unit = {
    heat1 = t
    heat2 = s
  }

  private def updateHeatingWaveSynced(amplitude: Double): Unit = { //might look cool, we'll see
    val heat = math.sin(2 * math.Pi * 3 * (System.currentTimeMillis() / 1000.0))
  }

  def interpolateHeatColor(heat: Double): (Float, Float, Float) = {
    val t = ((heat - roomTemp) / (graphicMaxHeat - roomTemp)).toFloat
    val r = t
    val g = (1 - (2 * math.abs(.5 - t))).toFloat
    val b = 1 - t
    (r, g, b)
  }
}


case class Coord(x: Int, y: Int)