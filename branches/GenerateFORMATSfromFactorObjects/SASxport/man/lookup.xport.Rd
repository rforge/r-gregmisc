\name{lookup.xport}
\alias{lookup.xport}
\alias{print.lookup.xport}
\alias{summary.lookup.xport}
\alias{print.summary.lookup.xport}
\title{Describe the Contents of an SAS XPORT File}
\description{
  Describe the contents of an SAS XPORT file.
}
\usage{
lookup.xport(file)
\method{print}{lookup.xport}(x, ...)
\method{summary}{lookup.xport}(object, ...)
\method{print}{summary.lookup.xport}(x, ...)
}
\arguments{
  \item{file}{Character string specifying the name or URL of a SAS XPORT file.}
  \item{x, object}{Object to be printed or summarized}
  \item{...}{Optional arguments}
}
\value{
  \code{lookup.xport} returns a list with one component for each dataset
  in the XPORT format library.

  \code{summary.lookup.xport} returns a single data frame containing:
  \item{dataset}{ Dataset name,}
  \item{name}{ Variable name,}
  \item{type}{ Type of variable (one of 'character' or 'numeric'),}
  \item{format}{ SAS format, }
  \item{width}{ SAS format width, }
  \item{label}{ Variable label, }
  \item{nobs}{ Number of observations. }
  
}
\details{
  The   \code{lookup.xport} function is a simple wrapper for the
  \code{\link[foreign]{lookup.xport}} function  provided by the
  \code{foreign} library.  The wrapper adds the ability to handle URL's,
  and returns an object of class  \code{lookup.xport} for which
  appropriate \code{print}, and \code{summary} functions are provided. 
} 
\seealso{
  For complete documentation of \code{lookup.xport} see the manual page
  for \code{\link[foreign]{lookup.xport}}.
}
\examples{
\dontrun{
## Get information on a local file
lookup.xport("xxx.xpt")
}

## Or read a copy of test2.xpt available on the web:
\dontrun{
url <- 'http://biostat.mc.vanderbilt.edu/cgi-bin/viewvc.cgi/*checkout*/Hmisc/trunk/tests/test2.xpt'
w <- lookup.xport(url)

# display the information (calls 'print.lookup.xport')
w

# names of data sets
names(w)

# names of variables within data sets
w$Z$name

# use summary
wS <- summary(w)
wS # same display

# variable names within all data sets
wS$name 
}

}
\keyword{file}
\keyword{manip}
