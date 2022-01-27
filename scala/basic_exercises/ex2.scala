/** given two generic lists, returns a new list that
is the intersection of the two lists */
def intersect[A](x: List[A], y: List[A]):List[A] = {
  for { L <- x.distinct if(y.contains(L)) } yield L
}

/** given two lists, returns a new list that is
the symmetric difference of the two lists. */
def symmetric_difference[A](x: List[A], y: List[A]):List[A] = {
  for { L <- x:::y.distinct if(!(y.contains(L) && x.contains(L))) } yield L
}