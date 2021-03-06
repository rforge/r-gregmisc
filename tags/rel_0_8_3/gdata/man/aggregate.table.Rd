% $Id$
%
% $Log$
% Revision 1.3  2002/09/23 13:59:30  warnes
% - Modified all files to include CVS Id and Log tags.
%
%

\name{aggregate.table}
\alias{aggregate.table}
\title{Create 2-Way Table of Summary Statistics}
\description{
  Splits the data into subsets based on two factors, computes a summary
  statistic on each subset, and arranges the results in a 2-way table.
}
\usage{
aggregate.table(x, by1, by2, FUN=mean, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ data to be summarized }
  \item{by1}{ first grouping factor.  }
  \item{by2}{ second grouping factor. }
  \item{FUN}{ a scalar function to compute the summary statistics which can
          be applied to all data subsets. Defaults to \code{mean}.}
  \item{\dots}{ Optional arguments for \code{FUN}. }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  Returns a matrix with one element for each combination of \code{by1}
  and \code{by2}.
}
\author{ Gregory R. Warnes \email{gregory\_r\_warnes\@groton.pfizer.com}}

\seealso{ \code{\link{aggregate}}, \code{\link{tapply}},
          \code{\link{interleave}} }

\examples{
# Useful example:
#
# Create a 2-way table of means, standard errors, and # obs

g1 <- sample(letters[1:5], 1000, replace=TRUE)
g2 <- sample(LETTERS[1:3], 1000, replace=TRUE )
dat <- rnorm(1000)

stderr <- function(x) sqrt( var(x,na.rm=TRUE) / (nobs(x) - 1 ) )

means   <- aggregate.table( dat, g1, g2, mean )
stderrs <- aggregate.table( dat, g1, g2, stderr )
ns      <- aggregate.table( dat, g1, g2, nobs )
blanks <- matrix( " ", nrow=5, ncol=3)

tab <- interleave( "Mean"=round(means,2),
                   "Std Err"=round(stderrs,2),
                   "N"=ns, " " = blanks, sep=" " )

print(tab, quote=FALSE)
}
\keyword{iteration}
\keyword{category}

