\name{read.xport}
\alias{read.xport}
\title{Import a SAS XPORT File}
\description{
  Read a SAS XPORT format file and return the contained dataset(s).
}
\usage{
read.xport(file,
           force.integer=TRUE,
           formats=NULL,
           name.chars=NULL,
           names.tolower=FALSE,
           keep=NULL,
           drop=NULL,
           as.is=0.95,
           verbose=FALSE,
           as.list=FALSE
           )
}
\arguments{
  \item{file}{Character string specifying the name or URL of a SAS XPORT
    file.
  }
  \item{force.integer}{Logical flag indicating whether integer-valued
    variables should be returned as integers (\code{TRUE}) or doubles
    (\code{FALSE}). Variables outside the supported integer range
    (\code{.Machine$integer.max}) will always be converted to
    doubles. 
  }
  \item{formats}{a data frame or list (like that created by
    \code{foreign:::read.xport}) containing \code{PROC FORMAT}
    output, if such output is not stored in the main transport
    file.
  }
  \item{name.chars}{Vector of additional characters permissible in
    variable names. By default, only the alpha and numeric
    characters ([A-Za-z0-9]) and periods ('.') are permitted. All
    other characters are converted into periods ('.').   
  }
  \item{names.tolower}{Logical indicating whether variable and dataset
    names should be converted to lowercase (\code{TRUE}) or left
    uppercase (\code{FALSE})
  }
  \item{keep}{a vector of names of SAS datasets to process.  This list
    must include \code{PROC FORMAT} dataset if it is present for
    datasets to use use any of its value label formats.
  } 
  \item{drop}{a vector of names of SAS datasets to ignore (original SAS
    upper case names)
  }
  \item{as.is}{
    Either a logical flag indicating whether SAS character variables should
    be preserved as character objects (\code{TRUE}) or factor
    objects (\code{FALSE}), or a fractional cutoff between 0 and 1.
    
    When a fractional cutoff is provided, character variables
    containing a more than this fraction of unique values will be
    stored as a character variables. This is done in order to
    preserve space, since factors must store both the integer factor
    codes and the character factor labels.
  }
  \item{verbose}{Logical indicating whether progress should be printed
    during the data loading and conversion process.}
  \item{as.list}{Logical indicating whether to return a list even if
    the SAS xport file contains only only one dataset.}
}
\value{
  If there the trasport file only contains one dataset (not counting any
  \code{PROC FORMAT} datasets) and \code{as.list=FALSE}, the result is a
  single data set.  Otherwise the result is a list 
  containing all the non-\code{PROC FORMAT} datasets.
}

\details{

  \itemize{
    \item SAS date, time, and date/time variables are converted respectively to \code{Date}, 
    POSIX, or \code{chron} objects
    
    \item SAS labels are stored in "label" attributes on each variable
    
    \item SAS formats are stored in "format" attributes on each variable
    
    \item SAS integer variables are stored as integers unless
      \code{force.integer} is \code{FALSE}
  }

  If the file includes the output of \code{PROC FORMAT CNTLOUT=},
  variables having customized label formats will be converted to \code{factor}
  objects with appropriate labels.
}
\author{ Gregory R. Warnes \email{greg@random-technologies-llc.com}
  based on \code{Hmisc:::sasxport.get} by Frank E. Harrell, Jr.}  
\section{Note}{
  This code provides a subset of the functionality of the
  \code{sasxport.get} function in the Hmisc library.
}
\seealso{
  \code{\link[foreign]{read.xport}},
  \code{\link{label}},
  \code{\link[Hmisc]{sas.get}},
  \code{\link[Hmisc]{sasxport.get}},  
  \code{\link{Dates}},
  \code{\link{DateTimeClasses}},
  \code{\link[chron]{chron}},
  \code{\link[foreign]{lookup.xport}},
  \code{\link[Hmisc]{contents}},
  \code{\link[Hmisc]{describe}}
}
\examples{

# -------
# SAS code to generate test dataset:
# -------
# libname y SASV5XPT "test2.xpt";
#
# PROC FORMAT; VALUE race 1=green 2=blue 3=purple; RUN;
# PROC FORMAT CNTLOUT=format;RUN;  * Name, e.g. 'format', unimportant;
# data test;
# LENGTH race 3 age 4;
# age=30; label age="Age at Beginning of Study";
# race=2;
# d1='3mar2002'd ;
# dt1='3mar2002 9:31:02'dt;
# t1='11:13:45't;
# output;
#
# age=31;
# race=4;
# d1='3jun2002'd ;
# dt1='3jun2002 9:42:07'dt;
# t1='11:14:13't;
# output;
# format d1 mmddyy10. dt1 datetime. t1 time. race race.;
# run;
# data z; LENGTH x3 3 x4 4 x5 5 x6 6 x7 7 x8 8;
#    DO i=1 TO 100;
#        x3=ranuni(3);
#        x4=ranuni(5);
#        x5=ranuni(7);
#        x6=ranuni(9);
#        x7=ranuni(11);
#        x8=ranuni(13);
#        output;
#        END;
#    DROP i;
#    RUN;
# PROC MEANS; RUN;
# PROC COPY IN=work OUT=y;SELECT test format z;RUN; *Creates test2.xpt;
# ------

# Read this dataset from a local file:
\dontrun{
w <- read.xport('test2.xpt')
}


# Or read a copy of test2.xpt available on the web:
url <- 'http://biostat.mc.vanderbilt.edu/cgi-bin/viewvc.cgi/*checkout*/Hmisc/trunk/tests/test2.xpt'
w <- read.xport(url)


# We can also get the dataset wrapped in a list
w <- read.xport(url, as.list=TRUE)

\dontshow{
SASxport:::assert( is.data.frame(w)==FALSE && is.list(w)==TRUE )
}

\dontrun{
## The Hmisc library provides many useful functions for interacting with
## data imported from SAS via read.xport()
library(Hmisc)

describe(w$test)   # see labels, format names for dataset test
lapply(w, describe)# see descriptive stats in more detaiil for each variable

contents(w$test)   # another way to see variable attributes
lapply(w, contents)# show contents of individual items in more detail

options(digits=7)  # compare the following matrix with PROC MEANS output
t(sapply(w$z, function(x)
 c(Mean=mean(x),SD=sqrt(var(x)),Min=min(x),Max=max(x))))
}

}
\keyword{interface}
\keyword{manip}
