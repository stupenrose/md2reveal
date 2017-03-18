package us.penrose.md2revealjs

object Util {
   def groupSequential[T, K](items:List[T])(fn:(T)=>K):List[(K, List[T])] = {
    items.foldLeft(List[(K, List[T])]()){(accum, next)=>
      val key = fn(next)
      val maybePrev = accum.lastOption
      val newAccum = maybePrev match {
        case None => {
          val items = accum :+ (key, List(next))
          items
        }
        case Some((prevKey, group)) => {
          if(prevKey == key){
            accum.dropRight(1) :+ (key, group :+ next)
          }else{
            accum :+ (key, List(next))
          }
        }
      }
      newAccum
    }
  }
   
   
}