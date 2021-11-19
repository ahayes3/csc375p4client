name := "csc375p3"

version := "0.1"

scalaVersion := "3.0.2"

libraryDependencies ++= {
  val version = "3.3.0"
  val os = "linux"

  Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-opengl"
  ).flatMap {
    module => {
      Seq(
        "org.lwjgl" % module % version,
        "org.lwjgl" % module % version classifier s"natives-$os"
      )
    }
  }
}