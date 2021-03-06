 % $Id$
%
% $Log$
% Revision 1.11  2002/09/26 12:11:50  warnes
% - Added note and example code to illustrate how to properly compute
%   contrasts for the first factor in the model.
%
% Revision 1.10  2002/09/23 14:27:17  warnes
%
% - Fixed syntax errors in barplot2.Rd and CrossTable.Rd
% - Fixed incorrect translation of 'F' (distribution) to 'FALSE' in glh.test.Rd
%
% Revision 1.9  2002/09/23 13:59:30  warnes
% - Modified all files to include CVS Id and Log tags.
%
%

\name{glh.test}
\alias{glh.test}
\alias{print.glh.test}
\alias{summary.glh.test}

\title{ Test a General Linear Hypothesis for a Regression Model }
\description{
Test, print, or summarize a general linear hypothesis for a regression model }
}

\usage{
glh.test(reg, cm, d=rep(0, nrow(cm)) )
print.glh.test(x, digits=4,...)
summary.glh.test(object, digits=4,...)
}

\arguments{
  \item{reg}{ Regression model }
  \item{cm}{ matrix .  Each row specifies a linear combination of the
    coefficients }
  \item{d}{ vector specifying the null hypothis values for each linear
    combination}
  \item{x, object}{glh.test object}
  \item{digits}{number of digits}
  \item{...}{ optional parameters (ignored)}
}
\details{
  Test the general lineary hypothesis
  \eqn{ C \hat{beta} = d }{C \%*\% \hat{beta} == d }
  for the regression model \code{reg}.

  The test statistic is obtained from the formula: 
  \deqn{f = \frac{(C \hat{\beta} - d)' ( C (X'X)^{-1} C' ) (C \hat{\beta}
      - d) / r }{ SSE / (n-p) } }{ 
    F = (C Beta-hat - d)' ( C (X'X)^-1   C' ) (C Beta-hat - d) / r /
        ( SSE / (n-p) )
  }
  
  Under the null hypothesis, f will follow a F-distribution with r and
  n-p degrees of freedom.
}
\note{
    When using treatment contrasts (the default) the first level of the
    factors are subsumed into the intercept term.  The estimated model
    coefficients factors are contrasts verses the first level. This
    should be taken into account when forming contrast matrixes,
    particularly when computing contrasts that include 'baseline'
    level of factors.

    See the comparison with \code{contrast.lm} in the examples below.
}
\value{
  Object of class \code{c("glh.test","htest")} with elements:
  \item{call }{Function call that created the object}
  \item{statistic }{F statistic}
  \item{parameter}{ vector containing the numerator (r) and denominator
    (n-p) degrees of freedom }
  \item{p.value}{ p-value}
  \item{estimate}{ computed estimate for each row of \code{cm} }
  \item{null.value}{ d }
  \item{method}{ description of the method }
  \item{data.name}{ name of the model given for \code{reg} }
  \item{matrix}{ matrix specifying the general linear hypotheis
    (\code{cm}) }
}
\references{ R.H. Myers, Classical and Modern Regression with
  Applications,  2nd Ed, 1990, p. 105}
\author{Gregory R. Warnes \email{gregory\_r\_warnes\@groton.pfizer.com} }
\seealso{\code{\link{contrast.lm}}, \code{\link{estimable}},
  \code{\link{contrasts}} }
\examples{

# fit a simple model
y <- rnorm(100)
x <-  cut(rnorm(100, mean=y, sd=0.25),c(-4,-1.5,0,1.5,4))
reg <- lm(y ~ x)
summary(reg)

# test both group 1 = group 2  and group 3 = group 4
# *Note the 0 in the column for the intercept term*

C <- rbind( c(0,-1,0,0), c(0,0,-1,1) )
ret <- glh.test(reg, C)
ret  # same as 'print(ret) '
summary(ret)

# To compute a contrast between the first and second level of the factor
# 'x' using 'contrast.lm' gives:

contrast.lm( reg, x,c(1,-1,0,0) )
	
# To test this sampe contrast using 'glh.test', use a contrast matrix
# with a zero coefficient for the intercept term.  See the Note section,
# above, for an explanation.

C <- rbind( c(0,-1,0,0) )
glh.test( reg, C )


}
\keyword{ models }
\keyword{ regression }
