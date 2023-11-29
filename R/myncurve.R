#' myncurve
#'
#' @param a Probability taht x = a, where P(X<=a)
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#'
#' @return plot showing the normal curve w/prob of x <= a
#' @export
#'
#' @examples myncurve(mu=10,sigma=2, a=6)
#'
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma),xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(mu,0, paste("Area = ", prob, sep=""))
}
