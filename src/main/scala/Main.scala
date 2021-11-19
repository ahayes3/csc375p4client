import org.lwjgl.Version
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.{GLFW_FALSE, GLFW_KEY_DOWN, GLFW_KEY_ESCAPE, GLFW_KEY_LEFT, GLFW_KEY_RIGHT, GLFW_KEY_UP, GLFW_RELEASE, GLFW_RESIZABLE, GLFW_TRUE, GLFW_VISIBLE, glfwCreateWindow, glfwDefaultWindowHints, glfwDestroyWindow, glfwGetPrimaryMonitor, glfwGetVideoMode, glfwGetWindowSize, glfwInit, glfwMakeContextCurrent, glfwPollEvents, glfwSetErrorCallback, glfwSetKeyCallback, glfwSetScrollCallback, glfwSetWindowPos, glfwSetWindowShouldClose, glfwShowWindow, glfwSwapBuffers, glfwSwapInterval, glfwTerminate, glfwWindowHint, glfwWindowShouldClose}
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.{GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT, glClear, glClearColor, glScaled, glTranslated}
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL

import scala.util.Using

object Main {
  private var window:Option[Long] = Option.empty
  val height = 5
  val width = height*2
  val s = 200
  val t = 200
  val c1 = .75
  val c2 = 1.0
  val c3 = 1.25
  val alloy = new Alloy(10,1/3,c1,1/3,c2,1/3,c3)
  val jacobi = new Jacobi(alloy.arr,t,s,alloy,100)
  private val translation = 20

  def loop():Unit = {
    val a = jacobi.compute(window)
  }

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    println("Hello LWJGL " + Version.getVersion +"!")

    init()
    loop()

    glfwFreeCallbacks(window.get)
    glfwDestroyWindow(window.get)

    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }
  def init():Unit = {
    GLFWErrorCallback.createPrint(System.err).set()

    if(!glfwInit())
      throw new IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)

    window = Option(glfwCreateWindow(500,500,"HELLO WORLD ! ! !",NULL,NULL)) //use the java NULL
    if(window.isEmpty)
      throw new RuntimeException("Failed to create the GLFW window")

    glfwSetKeyCallback(window.get,(window,key,scancode,action,mods) => {
      (key,action) match {

        case (GLFW_KEY_ESCAPE,GLFW_RELEASE) => glfwSetWindowShouldClose(window, true)
        case (GLFW_KEY_RIGHT,GLFW_RELEASE) => glTranslated(translation,0,0)
        case (GLFW_KEY_LEFT,GLFW_RELEASE) => glTranslated(-translation,0,0)
        case (GLFW_KEY_UP,GLFW_RELEASE) => glTranslated(0,-translation,0)
        case (GLFW_KEY_DOWN,GLFW_RELEASE) => glTranslated(0,translation,0)
        case _ =>
      }

    })

    glfwSetScrollCallback(window.get, (win,dx,dy) => {
      val s = 1 + (.02 * dy)
      glScaled(s,s,0)
    })

    Using(stackPush()) { stack =>
      val pWidth = stack.mallocInt(1) // int*
      val pHeight = stack.mallocInt(1)
      // Get the window size passed to glfwCreateWindow
      glfwGetWindowSize(window.get, pWidth, pHeight)
      // Get the resolution of the primary monitor
      val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor)
      // Center the window
      glfwSetWindowPos(window.get, (vidmode.width - pWidth.get(0)) / 2, (vidmode.height - pHeight.get(0)) / 2)
    }

    glfwMakeContextCurrent(window.get)
    glfwSwapInterval(1)
    glfwShowWindow(window.get)

  }
}
