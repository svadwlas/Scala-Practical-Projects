

def collatzAlgorithm(seednumber:BigInt):Stream[BigInt] = seednumber match {


  case n if n == 1 =>Stream(1)
  case n if n %2 == 0 => Stream.cons(seednumber,collatzAlgorithm(seednumber/2))
  case _ => Stream.cons(seednumber, collatzAlgorithm(seednumber*3 +1))



}


