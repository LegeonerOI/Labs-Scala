import scala.io.StdIn
object CoDecode extends App {
  val libcode = Map('a' -> "097", 'b' -> "098", 'c' -> "099", 'd' -> 100, 'e' -> 101, 'f' -> 102, 'g' -> 103, 'h' -> 104,
    'i' -> 105, 'j' -> 106, 'k' -> 107, 'l' -> 108, 'm' -> 109, 'n' -> 110, 'o' -> 111, 'p' -> 112, 'q' -> 113, 'r' -> 114,
    's' -> 115, 't' -> 116, 'u' -> 117, 'v' -> 118, 'w' -> 119, 'x' -> 120, 'y' -> 121, 'z' -> 122, ' ' -> " | ").withDefaultValue(" Not found ")

  def code(str: String): String = { //Функция кодирования
    var res = ""
    for (i <- 0 until (str.length)) {
      res = res.concat(libcode(str.charAt(i)).toString)
    }
    res
  }

  def decode(str: String): String = { //Функция декодирования
    val libdecode = for ((k, v) <- libcode) yield (v.toString, k) //Инвертированный ассоциативный массив
    var res = ""
    for (i <- str.indices) {
      if ((i > 0) & (i % 3 == 2)) {
        var group = str.charAt(i - 2).toString + str.charAt(i - 1).toString + str.charAt(i).toString //Группировка символов по 3
        res = res.concat(libdecode(group).toString)
      }
    }
    res
  }

  var res = ""
  println("Выбор действия (1 - шифровка, 2 - расшифровка): ")
  val choose = StdIn.readLine()
  if (choose == "1") {
    println("Введите слово для шифровки:")
    println (code(StdIn.readLine()))
  }
  if (choose == "2") {
    println("Введите слово для расшифровки:")
    println(decode(StdIn.readLine()))
  }
}
