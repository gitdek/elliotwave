###############################################################################
# package: package source
#   By puglisij Copyright (C) 2015, All rights reserved.
#
###############################################################################


#' linked_list
#'
#' @return function list
linked_list <- function() {
  head <- list(0)
  length <- 0
  
  methods <- list()
  
  methods$add <- function(val) {
    length <<- length + 1
    head <<- list(head, val)
  }
  
  methods$as.list <- function() {
    b <- vector('list', length)
    h <- head
    for (i in length:1) {
      b[[i]] <- head[[2]]
      head <- head[[1]]
    }
    return(b)
  }
  methods
}

#' @title Golden Ratio
#' @description Determine a local minimum of a continuous real function f
#'
#' @param f function
#' @param a numeric
#' @param b numeric > a
#' @param eps numeric (default 1e-16)
#' @param maxiter  maximum number of iterations
#' @return  list containing approximation of local minimum,
#'   value at local minimum and some control data
#' @export
golden_ratio <- function(f,
                         a,
                         b,
                         eps = 1e-16,
                         maxiter = 100) {
  stopifnot(
    is.numeric(a) & is.finite(a),
    is.numeric(b) & is.finite(b),
    b > a,
    is.numeric(eps) & is.finite(eps) & eps > 0,
    is.numeric(maxiter) & is.finite(maxiter) & maxiter > 0
  )
  
  phi <- (sqrt(5) - 1) / 2
  convergence <- 1
  
  for (i in 1:maxiter) {
    # Update xl, xp
    xl <- b - phi * (b - a)
    xp <- a + phi * (b - a)
    if (f(xl) > f(xp)) {
      a <- xl
    } else {
      b <- xp
    }
    
    # Check if method converged
    if (abs(b - a) < eps) {
      convergence <- 0
      break
    }
  }
  
  if (convergence == 1)
    warning("Method didn't converge")
  
  list(
    par = (a + b) / 2,
    value = f((a + b) / 2),
    counts = i,
    convergence = convergence,
    message = NULL
  )
}


#' @title  Elliot wave - puglisij
#' @description  Criteria must match as follows: Climb in instrument begins as wave 1, followed by a retracement
#' which is wave 2, followed by another climb wave 3, followed by a retracement wave 4, and
#' then followed by the final climb, wave 5.
#'
#' @param v  vector of instrument price data
#' @param eps  numeric
#' @param maxiter  maximum number of iterations
#' @param fib  vector of fibonacci ratios
#' @param time_restrictive boolean each wave is restricted to its individual time interval
#' @return list containing entries and golden ratio computed position scaled
#' @author puglisij
#' @export
#'
elliot_wave <- function(v,
                        eps = 1e-16,
                        maxiter = 10000,
                        fib = c(0.236, 0.382, 0.5, 0.618),
                        time_restrictive = TRUE) {
  stopifnot(is.vector(v) && is.vector(fiblevel))
  
  x <- vector(numeric, length = length(v))
  j <- as.vector(v)
  z <- vector(mode = "complex", length = 0)
  
  peaks_valleys <- cbind(x, z)
  entrys <- linked_list()
  
  for (i in 2:length(v) - 1) {
    if (v(i) <= v(i + 1) &&
        v(i - 1) >= v(i) || v(i) >= v(i + 1) && v(i - 1) <= v(i)) {
      x = c(x, v(i))
      z = c(z, j(i))
      
      x = x[1, diff(x) != 0]
      z = z[1, diff(x) != 0]
      peaks_valleys = c(x, z)
    }
  }
  
  entry <-
    function(fib_ratio,
             instrument_vector,
             index)
      ((0.999 * instrument_vector(index + 3)) - ((
        instrument_vector(index - 1) - ((
          instrument_vector(index - 1) - instrument_vector(index - 2)
        ) * fib_ratio)
      ) * 1.001)) / ((instrument_vector(index - 1) - ((instrument_vector(index -
                                                                           1) - instrument_vector(index - 2)) * fib_ratio
      )) * 1.001)
  
  for (n in 3:length(x) - 1) {
    if (x(n) < x(n - 1) &&
        x(n) >= x(n - 2) &&
        x(n + 1) > x(n - 1) && x(n + 2) > x(n - 1)) {
      if (x(n + 1) - x(n) >= x(n - 1) - x(n - 2) || x(n + 1) - x(n) >= x
          (n + 3) - x(n + 2)) {
        if (time_restrictive ==  TRUE) {
          if (z(n) - z(n - 1) <= (0.382 * (z(n - 1) - z(n - 2))) &&
              z(n + 1) - z(n) <= (1.618 * (z(n - 1) - z(n - 2))) &&
              z(n + 2) - z(n + 1) <= (0.382 * (z(n + 1) - z(n))) &&
              z(n + 3) - z(n + 2) <= (1.618 * (z(n + 1) - z(n)))) {
            
          } else {
            next
          }
        }
        for (fr in fib) {
          if (x(n) <= (x(n - 1) - ((x(n - 1) - x(n - 2)) * fr)) * 1.001 &&
              x(n) >= (x(n - 1) - ((x(n - 1) - x(n - 2)) * fr)) * 0.999) {
            e <- entry(
              fib_ratio = fr,
              instrument_vector = x,
              index = n
            )
            
            gr <-
              golden_ratio(
                function(x)
                  x * cos(0.1 * exp(x)) * sin(0.1 * exp(x)),
                a = e,
                b = x(n - 1),
                maxiter = maxiter
              )
            
            entrys$add(e, gr, n, x(n))
          }
        }
        
      }
    }
  }
  
  entrys
}
