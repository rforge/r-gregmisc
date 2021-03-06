# $Id$

# Reorder the levels of a factor.

reorder.factor <- function(x,
                           X,
                           FUN,
                           ...,
                           order=is.ordered(x),
                           new.order,
                           sort=mixedsort)
{
    constructor <- if (order) ordered else factor

    if (!missing(new.order))
      {
        if (is.numeric(new.order))
          new.order <- levels(x)[new.order]
        else
          new.order <- new.order
      }
    else if (!missing(FUN))
      new.order <- names(sort(tapply(X, x, FUN, ...)))
    else
      new.order <- sort(levels(x))

    constructor(x, levels=new.order)
}
