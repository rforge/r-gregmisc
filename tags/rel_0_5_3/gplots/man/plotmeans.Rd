% $Id$
%
% $Log$
% Revision 1.8  2002/04/09 00:51:32  warneg
% Checkin for version 0.5.3
%
% Revision 1.7  2001/12/12 01:37:33  warneg
%
% Added documentation for xaxt parameter.
%
% Revision 1.6  2001/12/05 19:41:43  warneg
% - Added documentation of use.t parameter.
%
% Revision 1.5  2001/10/16 23:04:45  warneg
%
% Forgot to add minbar and maxbar to usage section.
%
% Revision 1.4  2001/10/16 23:03:27  warneg
%
% Added description of minbar and maxbar.
%
% Revision 1.3  2001/10/16 22:35:36  warneg
%
% Added description of minsd parameter.
%
% Revision 1.2  2001/08/25 05:46:51  warneg
% Added CVS header.
%
% Revision 1.1  2001/08/25 05:45:10  warneg
% Initial Checkin
%
\name{plotmeans}
\alias{plotmeans}
\title{Plot Group Means and Confidence Intervals}
\description{Plot group means and confidence intervals.}
%\synopsis{
\usage{
plotmeans(formula, data=NULL, subset, na.action,
          bars=TRUE, p=0.95, minsd=0, minbar, maxbar,
          xlab=names(mf)[2], ylab=names(mf)[1], mean.labels=FALSE,
          ci.label=FALSE, n.label=TRUE, digits=getOption("digits"),
          col="black", barwidth=1, barcol="blue",
          connect=TRUE, ccol=col, legends=names(means), xaxt, use.t=TRUE, ...)
}
%
%\usage{
%  plotmeans( outcome ~ treatment)
%}

\arguments{
  \item{formula}{symbolic expression specifying the outcome and grouping
    variable.  See lm() for details.}

  \item{data}{optional data frame containing the variables in the model.}

  \item{subset}{an optional vector specifying a subset of observations to be
    used in the fitting process.}

  \item{na.action}{a function which indicates what should happen when the data
    contain `NA's.  See lm() for details. }

  \item{bars}{a logical value indicating whether confidence interval
    bars should be plotted. Defaults to TRUE.}

  \item{p}{confidence level for error bars.  Defaults to 0.95.}

  \item{minsd}{minumum permitted value for the standard deviation within
    each factor level.  Any standard deviation estimates smaller than
    \code{minsd} will be replaced with \code{minsd}. Defaults to 0.}

  \item{minbar}{minumum allowed value for bar ends.  If specified,
    values smaller than \code{minbar} will be replaced with
    \code{minbar}. } 
  
  \item{maxbar}{maximum allowed value for bar ends.  If specified,
    values larger than \code{maxbar} will be replaced with
    \code{maxbar}. }
  
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}

  \item{mean.labels}{ either a logical value indicating whether the circles
    representing the group means should be replaced with text giving the
    actual mean values or a vector containing labels to use
    instead. Defaults to FALSE.}

  \item{ci.label}{ a logical value indicating whether text giving the
    actual interval end values should be placed at the end of each
    confidence interval bar. Defaults to FALSE.}

  \item{n.label}{ a logical value indicating whether text giving the number of
    observations in each group should should be added to the plot. }

  \item{digits}{ number of significant digits to use when displaying
    mean or confidince limit values.}

  \item{col}{ color of cicles marking group means.  Default is "black".}

  \item{barwidth}{ linewidth of interval bars and end marks. Default is
    1.}

  \item{barcol}{ color of interval bars and end marks.  Default is
    "blue".}

  \item{connect}{ either a logical value indicating whether the means of
    each group should be connected by a line, or a list of vectors giving the
    index of bars that should be connected by a line. Defaults to TRUE.}

  \item{ccol}{ color of lines used to connect means. Defaults to the
    same color as "col".}

  \item{legends}{ vector containing strings used to label groups along
    the x axis. Defaults to group names.}

  \item{xaxt}{A character which specifies the axis type. Specifying `"n"'
          causes an axis to be set up, but not plotted.}
  
  \item{use.t}{ a logical value indicating whether the t distribution
    should be used to compute confidence intervals.  If \code{TRUE}, the
    default, a t distribution will the correct number of degrees of
    freedom for each group be used.  If \code{FALSE}, the a normal
    distribution will be used.}

  \item{\dots}{ optional plotting parameters. }

  }

%\details{ 
%  
%  
%}


\examples{

  # show comparison with boxplot
  data(state)
  plotmeans(state.area ~ state.region)   

  # show some color and mean labels
  plotmeans(state.area ~ state.region,
             mean.labels=TRUE, digits=-3,
             col="red", connect=FALSE)

  # show how to specify which means should be connected
  plotmeans(state.area ~ state.region, connect=list(1:2, 3:4),
            ccol="red", pch=7 )
}

\author{Gregory R. Warnes \email{Gregory\_R\_Warnes@groton.pfizer.com}}

\seealso{\code{\link{plotCI}}, \code{\link{boxplot}}}

\keyword{hplot}
