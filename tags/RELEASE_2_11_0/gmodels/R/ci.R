# $Id$

ci  <-  function(x, confidence=0.95,alpha=1-confidence,...)
  UseMethod("ci")

ci.default <- function(x, confidence=0.95,alpha=1-confidence,na.rm=FALSE,...) {
  est <- mean(x, na.rm=na.rm)
  stderr <-  sd(x, na.rm=na.rm)/sqrt(nobs(x));
  ci.low  <- est + qt(alpha/2, nobs(x)-1) * stderr
  ci.high <- est - qt(alpha/2, nobs(x)-1) * stderr
  retval  <- c(
               Estimate=est,
               "CI lower"=ci.low,
               "CI upper"=ci.high,
               "Std. Error"=stderr,
               )

  retval
}

ci.binom <- function(x, confidence=0.95,alpha=1-confidence,...)
  {
    if( !(all(x) %in% c(0,1)) ) stop("Binomial values must be either 0 or 1.")

    est  <-  mean(x, na.rm=TRUE)
    n <- nobs(x)
    stderr <- sqrt(est*(1-est)/n)
    ci.low  <- qbinom(p=alpha/2, prob=est, size=n)/n
    ci.high <- qbinom(p=1-alpha/2, prob=est, size=n)/n

    retval  <- cbind(Estimate=est,
                     "CI lower"=ci.low,
                     "CI upper"=ci.high,
                     "Std. Error"= stderr
                     )
    retval
  }

ci.lm  <-  function(x,confidence=0.95,alpha=1-confidence,...)
{
  x  <-  summary(x)
  est  <-  coef(x)[,1] ;
  ci.low  <- est + qt(alpha/2, x$df[2]) * coef(x)[,2] ;
  ci.high <- est - qt(alpha/2, x$df[2]) * coef(x)[,2] ;
  retval  <- cbind(Estimate=est,
                   "CI lower"=ci.low,
                   "CI upper"=ci.high,
                   "Std. Error"= coef(x)[,2],
                   "p-value" = coef(x)[,4])

  retval
}

ci.lme <- function(x,confidence=0.95,alpha=1-confidence,...)
  {
  x  <-  summary(x)
  est  <-  x$tTable[,"Value"] ;
  ci.low  <- est + qt(alpha/2, x$tTable[,"DF"]) * x$tTable[,"Std.Error"] ;
  ci.high <- est - qt(alpha/2, x$tTable[,"DF"]) * x$tTable[,"Std.Error"] ;
  retval  <- cbind(Estimate=est,
                   "CI lower"=ci.low,
                   "CI upper"=ci.high,
                   "Std. Error"= x$tTable[,"Std.Error"],
                   "DF" = x$tTable[,"DF"],
                   "p-value" = x$tTable[,"p-value"])
  rownames(retval)  <-  rownames(x$tTable)
  retval
}

ci.lmer <- function(x,confidence=0.95,alpha=1-confidence,...)
  {
  est  <-  fixef(x)
  se <- sqrt(diag(vcov(x)))
  df <- getFixDF(x)
  ci.low  <- est + qt(alpha/2, df) * se
  ci.high <- est - qt(alpha/2, df) * se
  retval  <- cbind(Estimate=est,
                   "CI lower"=ci.low,
                   "CI upper"=ci.high,
                   "Std. Error"= se,
                   "DF" = df,
                   "p-value" = 2*(1-pt(abs(est/se), df)))
  rownames(retval)  <-  names(est)
  retval
}
