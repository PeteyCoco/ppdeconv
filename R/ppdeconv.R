ppdeconv <- function(data, mode = "mle"){

  if(mode == "mle"){

    # create the parameter vector for alpha
    Q_bdiag <- bdiag(lapply(data, function(x) x$Q))
    P_bdiag <- bdiag(lapply(data, function(x) x$P))

  }
  else if(mode == "rmle"){

  }
  else{
    stop("mode not recognized")
  }
}

objective <- function(p, data){



}
