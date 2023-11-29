#' ntickets
#'
#' @param N number of seats available
#' @param gamma probability of overbooking
#' @param p probability that a passenger will show
#'
#' @return A list of nd, nc, N, p, and gamma; and two graphs, a discrete and continuous curve of optimal tickets to sell
#' @export
#'
#' @examples ntickets(400,.02, .95)
#'
ntickets = function(N,gamma, p) {
  ndisc = c(N:(N+20))
  discrete = (1-gamma)-pbinom(N,ndisc,p)
  nd = which.min(abs(discrete))
  nd = ndisc[nd]

  ncont = seq(N,N+20,length = 100000)
  continous = (1-gamma)-pnorm(N+.5,mean=ncont*p,sd=sqrt(ncont*p*(1-p)))
  nc = which.min(abs(continous))
  nc = ncont[nc]

  layout(matrix(1:2, nrow=2,ncol=2))

  plot(ndisc,discrete, type="b",pch = 21, bg = "blue",main = paste("Objective vs n to find optimal tickets sold/n", "(",nd, ")","gamma=",gamma, ",N=",N,"Discrete"),ylab = "Objective",xlab="n")

  abline(h = 0, v = nd, lwd = 2, col="red")

  plot(ncont, continous, type ="l", main = paste("Objective vs n to find optimal tickets sold\n","(", nc,")", ",gamma=", gamma, ", N=", N,",Continous"),ylab ="Objective", xlab = "n", col = "black")

  abline(h=0, v = nd, lwd = 2, col = "blue")

  print(list(nd=nd, nc=nc, N=N, p=p, gamma = gamma))

}
