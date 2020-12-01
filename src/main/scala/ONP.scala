import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object ONP extends App {

  private val prior = Map("^" -> 4, "*" -> 3, "/" -> 3, "+" -> 2, "-" -> 2, "=" -> 1, "(" -> 0)
  private val operands = prior.keys.toSet + ")"

  private val input = StdIn.readLine()
  private val tokens = findTokens(input)
  private val onpTokens = convertToOnp(tokens)
  private val calculatedValue = calculate(onpTokens);

  println(s"ONP: ${onpTokens.mkString(" ")}")
  println(s"Evaluated value: $calculatedValue")

  def calculate(tokens: List[String]) = {
    val stack = new mutable.Stack[Double]()
    tokens.foreach {
      case "+" =>
        stack.push(stack.pop + stack.pop)
      case "-" =>
        val x = stack.pop
        stack.push(stack.pop - x)
      case "*" =>
        stack.push(stack.pop * stack.pop)
      case "/" =>
        val x = stack.pop
        stack.push(stack.pop / x)
      case "^" =>
        val x = stack.pop
        stack.push(math.pow(stack.pop, x))
      case token =>
        stack.push(token.toDouble);
    }
    stack.pop()
  }



  private def convertToOnp(tokens: List[String]): List[String] = {
    val stack = new mutable.Stack[String]()
    val onp = new ListBuffer[String]()

    tokens.foreach {
      case token@("^" | "*" | "/" | "+" | "-") =>
        while (stack.nonEmpty && prior(stack.top) >= prior(token)) {
          onp.addOne(stack.pop())
        }
        stack.push(token)
      case token@"(" => stack.push(token)
      case ")" =>
        var top: String = stack.pop()
        while (top != "(") {
          onp.addOne(top)
          top = stack.pop()
        }
      case token => onp.addOne(token)
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