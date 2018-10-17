object Main extends App {


  def mergeSortInScala(xs: List[Int]): List[Int] =  {

    val n = xs.length/2
    if (n==0) xs
    else {
      def mergeInScala(xs:List[Int], ys:List[Int]):List[Int] =

        (xs,ys) match {

          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1)  =>
            if (x<y) x::mergeInScala(xs1, ys)
            else y::mergeInScala(xs, ys1)





      }

      val(left,right) = xs splitAt(n)

      mergeInScala(mergeSortInScala(left), mergeSortInScala(right))


    }





  }






}
