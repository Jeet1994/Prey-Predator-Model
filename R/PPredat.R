#' A function to take the parameters from the user and plot the Predator Abundance Vs Prey Abundance Plot. 
#' 
#' This function allows you to see different plots with different values of the set paremeters.
#' @param love Do you love plots? Defaults to TRUE.
#' @keywords Prey vs Predator model
#' @export
#' @examples
#' PrPred()


PrPred <- function(a,b,g,d){
  library(deSolve)
  
  Pars <- c(a, b, g, d)
  State <- c(x = 10, y = 10)
  
  
  LotVmod <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dx = x*(a - b*y)
      dy = -y*(g - d*x)
      return(list(c(dx, dy)))
    })
  }
  
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
  
  matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
  legend("topright", c("Prey", "Predator"), lty = c(1,2), col = c(1,2), box.lwd = 0)
  
}

