import scala.collection.mutable.ListBuffer
import scala.util.Random

object robotNameSpace {
  var names = new ListBuffer[String]()
  /*
    generate a unique name. If caller passes a name then they want to reset the robot name so
    generate a new name and remove the existing name from the names List
  */
  def generateUnique(existingName: String = ""): String = {
      val newName = generate(letters = 2, digits = 3)
      if (robotNameSpace.names.contains(newName))
        generateUnique()
      else {
        robotNameSpace.names += newName
      }

    if(existingName.nonEmpty){
      robotNameSpace.names -= existingName
    }

    newName
  }
  private def generate(letters: Int, digits: Int): String = {
    Random.alphanumeric.filter(_.isUpper).take(letters).mkString +
      Random.alphanumeric.filter(_.isDigit).take(digits).mkString
  }
}

class Robot {
  var name: String = robotNameSpace.generateUnique()
  def reset(): Unit = {
     name = robotNameSpace.generateUnique(name)
  }
}