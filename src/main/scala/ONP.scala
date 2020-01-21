import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object ONP extends App {

  private val prior = Map("^" -> 4, "*" -> 3, "/" -> 3, "+" -> 2, "-" -> 2, "=" -> 1, "(" -> 0)
  private val operands = prior.keys.toSet + ")"

  private val input = StdIn.readLine()
  private val tokens = findTokens(input)
  private val output = convertToOnp(tokens)

  println(s"Output: ${output.mkString(" ")}")

  private def convertToOnp(tokens: List[String]): List[String] = {
    val stack = new mutable.Stack[String]()
    val onp = new ListBuffer[String]()

    tokens.foreach { token =>
      token match {
        case "^" | "*" | "/" | "+" | "-" =>
          while (stack.nonEmpty && prior(stack.top) >= prior(token)) {
            onp.addOne(stack.pop())
          }
          stack.push(token)
        case "(" => stack.push(token)
        case ")" =>
          var top: String = stack.pop()
          while (top != "(") {
            onp.addOne(top)
            top = stack.pop()
          }
        case _ => onp.addOne(token)
      }
    }

    while (stack.nonEmpty) {
      onp.addOne(stack.pop())
    }

    onp.toList
  }

  private def findTokens(input: String): List[String] = {
    val result = new ListBuffer[String]()
    val builder = new StringBuilder

    input.foreach { char =>
      if (operands.contains(char.toString)) {
        result.addOne(builder.toString())
        result.addOne(char.toString)
        builder.clear()
      } else {
        builder.addOne(char)
      }
    }

    result.addOne(builder.toString())

    result.toList.filter(_.nonEmpty)
  }
}