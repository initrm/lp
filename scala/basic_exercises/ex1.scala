/** checks if the string given as input is palindrome */
def is_palindrome(str: String):Boolean = {
  val filtered = str.toLowerCase().filter(char => char >= 'a' && char <= 'z')
  for (i <- 0 to filtered.length/2) if (filtered(i) != filtered(filtered.length-1-i)) return false
  return true
}

/** given a dictionary of strings, checks if the input string is an
anagram of one or more of the strings in the dictionary */  
def is_an_anagram(str: String, list: List[String]):Boolean = {
  val srcStrFreqMap = buildCharFrequencyMap(str)
  for(candidate <- list) {
    if(srcStrFreqMap == buildCharFrequencyMap(candidate)) return true
  }
  return false
}

/** builds and returns a map char -> int which represents the number of times
the char appears into the string */
def buildCharFrequencyMap(str: String):Map[Char, Integer] = {
  val strFreq = Map[Char, Integer]()
  for (c <- str) {
    if (strFreq contains c) strFreq(c) = strFreq(c) + 1
    else strFreq(c) = 1
  }
  return strFreq
}

/** given a number calculates all its prime factors */
def factors(n: Integer):List[Integer] = {
  var factors = List[Integer]()
  var number = n
  for(i <- 2 to number) {
    if(is_prime(i)) {
      while(number%i == 0) {
        factors = i::factors
        number = number/i
      }
    }
  }
  return factors
}

/** verifies if the given number is a prime one */
def is_prime(n: Integer):Boolean = {
  if(n == 1) return true
  for(i <- 2 to n-1) if(n%i == 0) return false
  return true
}

/** given a number calculates if it is a perfect number or not */
def is_proper(n: Integer):Boolean = {
  var total = 0
  for(i <- 1 to n/2) if(n%i == 0) total = total + i
  return total == n
}