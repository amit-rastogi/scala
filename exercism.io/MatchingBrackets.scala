object MatchingBrackets {
  def isPaired(brackets: String): Boolean = {
    def util(stack: List[Char], next: Int): Boolean = {
      val bracketsMap = Map[Char,Char]('('->')','{'->'}','['->']')
      if(next >= brackets.length && stack.isEmpty) true
      else if(next >= brackets.length && stack.nonEmpty) false
      else if(bracketsMap.contains(brackets(next)))
        util(brackets(next)::stack, next+1)
      else if(stack.nonEmpty && bracketsMap.find(_._2 == brackets(next)).map(_._1).contains(stack.head))
        util(stack.tail, next+1)
      else if(bracketsMap.find(_._2 == brackets(next)).map(_._1).isEmpty) util(stack, next+1)
      else false
    }
    if(brackets.isEmpty) true
    else util(List(), 0)
  }
}
