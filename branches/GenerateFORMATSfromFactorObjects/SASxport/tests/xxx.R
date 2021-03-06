library(SASxport)

## manually create a data set
abc <- data.frame( x=c(1, 2, NA, NA ), y=c('a', 'B', NA, '*' ) )

## add a format specifier (not used by R)
attr(abc$x, 'format') <- 'date7.'

## add a variable label (not used by R)
attr(abc$y, 'label')  <- 'character variable'

# create a SAS XPORT file from our local data fram
write.xport(abc,
            file="xxx2.xpt",
            cDate=strptime("28JUL07:21:08:06 ", format="%d%b%y:%H:%M:%S"),
            osType="SunOS",
            sasVer="9.1"
            )

# read the original SAS data file
abc.SAS <- read.xport("xxx.xpt", names.tolower=FALSE)

## read.xport currently doesn't store the format attribute...
attr(abc.SAS$X, 'format') <- 'date7.'

# create a SAS XPORT file from the SAS data
write.xport(abc=abc.SAS,
            file="xxx3.xpt",
            cDate=strptime("28JUL07:21:08:06 ", format="%d%b%y:%H:%M:%S"),
            osType="SunOS",
            sasVer="9.1"
            )




## Load both files back in as raw data
a.1 <- readBin( con="xxx.xpt",  what=raw(), n=1e5 )
a.2 <- readBin( con="xxx2.xpt", what=raw(), n=1e5 )
a.3 <- readBin( con="xxx3.xpt", what=raw(), n=1e5 )

## R doesn't have multiple NA types, while SAS does.  The original
## file contains a SAS '.A' missing value, while what we've created
## contains an ordinary '.' missing value, so mash this one byte to
## avoid a comparison error for this know limitation.  

a.1[1089] <- as.raw("0x2e")

## Test that the files are otherwise identical
SASxport:::assert( all(a.1 == a.2) )
SASxport:::assert( all(a.1 == a.3) )


