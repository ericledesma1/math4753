#' Title
#'
#' @param n sample size
#' @param alpha confidence %
#'
#' @return confidence interval
#' @export
#'
#' @examples myci(x)
myci= function(x,n=30,alpha=.05/2){
  t=qt(1-alpha,df=n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
