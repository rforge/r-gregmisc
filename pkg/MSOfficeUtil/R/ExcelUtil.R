##
## $Id$
##

## TODO: 
##   row.names for importing/exporting data.frames
##   dates/currency, general converters
##   error checking

getExcelSheetNames <- function(file.name)
{
  fn <- gsub("/", "\\\\", file.name)
  if (!file.exists(fn))
    stop("File does not exist.")
  
  .COMInit()
  xls <- COMCreate("Excel.Application")
  wbs <- xls[["workbooks"]] 
  book <- wbs$Open(fn) ## Open a particular workbook
  d <- list()    
  sheets <- book[["sheets"]]
  n.sheets <- sheets[["Count"]]    
  sheet.names <- data.frame(Sheet=1:n.sheets,
                            Name=sapply(1:n.sheets,
                              function(i){ sheets[[i]][["Name"]] }))
  wbs$Close()
  xls[["Visible"]] <- FALSE
  xls$Quit()
  .COMInit(FALSE)
  sheets <- book <- wbs <- xls <- NULL    
  rm(sheets, book, wbs, xls)
  gc()

  return(sheet.names)
}

getExcelWorksheet <- function(file.name, sheet=1, rows=NULL, cols=NULL)
{
  fn <- gsub("/", "\\\\", file.name)
  if (!file.exists(fn))
    stop("File does not exist.")

  
  .COMInit()
  xls <- COMCreate("Excel.Application")
  wbs <- xls[["workbooks"]] 
  book <- wbs$Open(fn) ## Open a particular workbook
  d <- list()    
  sheets <- book[["sheets"]]
  n.sheets <- sheets[["Count"]]    
  sheet.names <- sapply(1:n.sheets, function(i){ sheets[[i]][["Name"]] })   
  k <- 1
  okay <- TRUE
  for (s in sheet)
    {
      read.sheet <- ifelse(is.numeric(s), s %in% 1:n.sheets, s %in% sheet.names)
      if (read.sheet)
        {
          ws <- sheets$Item(s)    ## Open a particular sheet
          d[[k]] <- try(importDataFrame(wks=ws, rows=rows, cols=cols))  ## Import the data

          if (class(d[[k]])=="try-error")
            {
              okay <- FALSE
              rm(ws)
              break
            } 
          attr(d[[k]], "sheet.name") <- ifelse(is.numeric(s), sheet.names[s],
                                               s)
          rm(ws)
          k <- k + 1
        }
      else
        print.noquote(paste("Sheet : ", s, " does not exist", sep=""))
    }
  if (!okay)
    print.noquote(paste("Problem reading Excel file sheet", s))
  wbs$Close()
  xls[["Visible"]] <- FALSE
  xls$Quit()
  .COMInit(FALSE)
  rm(sheets, book, wbs, xls)
  sheets <- book <- wbs <- xls <- NULL    
  gc()

  if (length(d))
    names(d) <- sapply(d, attr, "sheet.name")
  return(d)
}

exportTableToExcel <- function (df, file.name, sheet=1, range=NULL,
                                overwrite=TRUE) 
{

  fn <- gsub("/", "\\\\", file.name)
  if (!file.exists(dirname(fn)))
    stop("No such directory.")
  
  .COMInit()
  xls <- COMCreate("Excel.Application")
  xls[["Visible"]] <- FALSE
  wbs <- xls[["workbooks"]]

  if (file.exists(fn) & !overwrite)
    {
      book <- wbs$Open(fn)
      sheets <- book[["sheets"]]
      n.sheets <- sheets[["Count"]]
      sheet.names <- sapply(1:n.sheets, function(i) {
        sheets[[i]][["Name"]]
      })
      sheet.exists <- ifelse(is.numeric(sheet), sheet %in% 1:n.sheets, 
                             sheet %in% sheet.names)
      if (sheet.exists) {
        ws <- sheets$Item(sheet)
        if (is.null(range)) {
          rng <- ws[["UsedRange"]]
          c1 <- rng[["Column"]]
          rws <- rng[["Rows"]]
          r1 <- rng[["Row"]]
          r2 <- r1 + rws[["Count"]] + 1
          r.start <- ifelse(r1 == 1 & rws[["Count"]] == 1, 2, r2)
          range <- ws$Cells(r.start, c1)
          rng <- rws <- NULL
          rm(rng, rws)
        }
      }
    else
      {
        sheets$Add()
        sheet <- 1
        range <- sheets$Item(1)$Range("A1")
      }
    }
  else
    {
      if (file.exists(fn))
        file.remove(fn)
      wbs$Add()
      wbs$Item(1)$SaveAs(fn)
      book <- wbs$Open(fn)
      sheets <- book[["sheets"]]
      ws <- sheets$Item(1)
      range <- ws$Range("A1")
    }
  exportDataFrame(df, at=range)
  wbs$Item(1)$Save()
  wbs$Close()
  xls[["Visible"]] <- FALSE
  xls$Quit()
  .COMInit(FALSE)
  range <- sheets <- book <- ws <- wbs <- xls <- NULL
  rm(range, sheets, book, wbs, ws, xls)
  gc()
  invisible()
}

exportDataFrame <- function(df, at, ...)
  ## export the data.frame "df" into the location "at" (top,left cell)
  ## output the occupying range.
  ## TODO: row.names, more error checking
{
  d <- dim(df)
  if(d[2]<1) stop("data.frame must have at least one column")
  r1 <- at[["Row"]]                ## 1st row in range
  c1 <- at[["Column"]]             ## 1st col in range
  c2 <- c1 + d[2] - 1              ## last col (*not* num of col)
  ws <- at[["Worksheet"]]

  ## headers

  h1 <- ws$Cells(r1, c1)
  h2 <- ws$Cells(r1, c2)
  hdrRng <- ws$Range(h1, h2)
  hdrRng[["Value"]] <- names(df)

  ## data

  rng <- ws$Cells(r1+1, c1)        ## top cell to put 1st column 
  for(j in seq(from=1, to=d[2])){
    exportVector(df[,j], at=rng, ...)
    rng <- rng[["Next"]]         ## next cell to the right
  }
  ## return the actual used range (including headings)
  d2 <- ws$Cells(r1+d[1], c2)
  out <- ws$Range(h1, d2) 
  invisible(out)
}

exportVector <- function(obj, at=NULL, byrow=FALSE, ...)
  ## coerce obj to a simple (no attributes) vector and export to
  ## the range specified at "at" (can refer to a single starting cell);
  ## byrow=TRUE puts obj in one row, otherwise in one column.
  ## How should we deal with unequal of ranges and vectors?  Currently
  ## we stop, modulo the special case when at refers to the starting cell.
  ## TODO: converters (currency, dates, etc.)
{
  n <- length(obj)
  if(n<1) return(at)
  rws <- at[["Rows"]]
  cls <- at[["Columns"]]
  d <- c(rws[["Count"]], cls[["Count"]])
  N <- prod(d)
  if(N==1 && n>1){     ## at refers to the starting cell
    r1c1 <- c(at[["Row"]], at[["Column"]])
    r2c2 <- r1c1 + if(byrow) c(0,n-1) else c(n-1, 0)
    ws <- at[["Worksheet"]]
    r1 <- ws$Cells(r1c1[1], r1c1[2])
    r2 <- ws$Cells(r2c2[1], r2c2[2])
    at <- ws$Range(r1, r2)
  } 
  else if(n != N)
    stop("range and length(obj) differ")

  ## currently we can only export primitives...

  if(class(obj) %in% c("logical", "integer", "numeric", "character"))
    obj <- as.vector(obj)     ## clobber attributes
  else
    obj <- as.character(obj)  ## give up -- coerce to chars

  ## here we create a C-level COM safearray
  d <- if(byrow) c(1, n) else c(n,1)
  at[["Value"]] <- asCOMArray(matrix(obj, nrow=d[1], ncol=d[2]))
  invisible(at)
}

importDataFrame <- function(rng=NULL, wks=NULL, n.guess=5,
                         dateFun=as.chron.excelDate,
                         rows=NULL, cols=NULL)
  ## Create a data.frame from the range rng or from the "Used range" in
  ## the worksheet wks.  The excel data is assumed to be a "database" (sic) 
  ## excel of primitive type (and possibly time/dates).
  ## We guess at the type of each "column" by looking at the first
  ## n.guess entries ... but it is only a very rough guess.
{
  on.exit(rm(rng, wks, areas, cols, rows))
  if(is.null(rng) && is.null(wks))
    stop("need to specify either a range or a worksheet")
  if(is.null(rng))
    rng <- wks[["UsedRange"]]       ## actual region
  else
    wks <- rng[["Worksheet"]]       ## need to query rng for its class

  areas <- rng[["Areas"]]
  n.areas <- areas[["Count"]]        ## must have only one region
  if(n.areas!=1)
    stop("data must be in a contigious block of cells")

  if (!is.null(cols))
    {
      cols <- range(cols)
      c1 <- cols[1]
      c2 <- cols[2]
    }
  else
    {
      cls <- rng[["Columns"]]            ## all cols
      c1 <- rng[["Column"]]              ## first col
      c2 <- cls[["Count"]] - c1 + 1      ## last columns
    }
  rws <- rng[["Rows"]]  ## all rows
  if (!is.null(rows))
    {
      rows <- range(rows)
      r1 <- rows[1]
      r2 <- rows[2]
    }
  else
    {
      r1 <- rng[["Row"]]                 ## first row
      r2 <- rws[["Count"]] - r1 + 1      ## last row
    }
  ## headers
  n.hdrs <- rng[["ListHeaderRows"]]
  if(n.hdrs==0)
    hdr <- paste("V", seq(from=c1, to=c2), sep="")
  else if(n.hdrs==1) {
    h1 <- rws$Item(r1)
    hdr <- unlist(h1[["Value2"]]) 
  }
  else {    ## collapse multi-row headers
    h <- vector("list", c2)         ## list by column
    h1 <- wks$Cells(r1, c1)
    h2 <- wks$Cells(r1+n.hdrs-1, c2)
    r <- rng$Range(h1, h2)
    jj <- 1
    hdr.cols <- r[["Columns"]]
    for(j in seq(from=c1, to=c2)){
      this.col <- hdr.cols$Item(j)
      val <- this.col[["Value2"]]
      h[[jj]] <- unlist(val[[1]])
      jj <- jj+1
    }
    hdr <- sapply(h, paste, collapse=".")
  }
  r1 <- r1 + n.hdrs

  ## Data region 

  d1 <- wks$Cells(r1, c1)
  d2 <- wks$Cells(r2, c2)
  dataRng <- wks$Range(d1, d2)
  dataCols <- dataRng[["Columns"]]
  out <- vector("list", length(hdr))
  for(j in seq(along=out)){
    f1 <- dataCols$Item(j)
    val <- f1[["Value2"]]
    f <- unlist(lapply(val[[1]], function(x) if(is.null(x)) NA else x))
    cls <- guessExcelColType(f1)
    out[[j]] <- if(cls=="logical") as.logical(f) else f
  }
  if (length(out) > 0)
    names(out) <- make.names(hdr)
  as.data.frame(out)
}

guessExcelColType <- function(colRng, n.guess=5, hint=NULL)
  ## colRng points to an range object corresponding to one excel column
  ## e.g., colRng=rng$Columns()$Item("H")
  ## TODO: currently we return one of "logical", "numeric", "character"
  ## need to add "SCOMIDispatch"
{
  app <- colRng[["Application"]]
  wf <- app[["WorksheetFunction"]]
  S.avail <- c("logical", "numeric", "integer", "character")
  ## we should get the following from the Excel type library
  fmt <- colRng[["NumberFormat"]]
  num.fmt <- c("general", "number", "currency", "accounting", 
    "percentage", "fraction", "scientific")

  fld <- colRng[["Rows"]]
  n <- fld[["Count"]]
  k <- min(n.guess, n)
  cls <- character(k)
  c1 <- colRng$Cells(1,1)
  c2 <- colRng$Cells(k,1)
  for(i in 1:k){
    x <- fld$Item(i)
    if(wf$IsText(x)) 
      cls[i] <- "character"
    else if(wf$IsNumber(x)) {
      if(!is.null(fmt) && tolower(fmt) %in% num.fmt)
        cls[i] <- "numeric"
      else 
        cls[i] <- "character"
    }
    else if(wf$IsLogical(x)) 
      cls[i] <- "logical"
    else if(wf$IsNA(x)) 
      cls[i] <- "NA"
    else 
      cls[i] <- "character"
  }
  ## if not all rows agree, use character type
  cls <- cls[cls %in% S.avail]
  if(length(cls)==0 || length(unique(cls))>1)
    return("character")
  else
    return(cls[1])
}

as.chron.excelDate <- function(xlsDate, date1904=FALSE)
{
  if(date1904){
    orig <- c(month=12, day=31, year=1903)
    off <- 0
  } 
  else {
    orig <- c(month=12, day=31, year=1899)
    off <- 1
  }
  chron(xlsDate - off, origin <- c(month=12, day=31, year=1899))
}

as.excelData.chron <- function(chronDate, date1904=FALSE)
{
  if(date1904){
    orig <- c(month=12, day=31, year=1903)
    off <- 0
  } 
  else {
    orig <- c(month=12, day=31, year=1899)
    off <- 1
  }
  if(any(origin(chronDate)!=orig))
    origin(chronDate) <- orig
  as.numeric(chronDate) + off
}
