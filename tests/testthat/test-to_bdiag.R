test_that("results are correct dimension and values", {

    data <- list(list(y = c(1,2),
                      Q = matrix(1, nrow = 5, ncol = 4),
                      P = matrix(1, nrow = 2, ncol = 5),
                      S = matrix(1, nrow = 4, ncol = 4),
                      c0 = 1),
                 list(y = c(1,2,3),
                      Q = matrix(1, nrow = 3, ncol = 3),
                      P = matrix(1, nrow = 3, ncol = 3),
                      S = matrix(1, nrow = 3, ncol = 3),
                      c0 = 2)
                 )
    # Put data into block diagonal format
    y_bdiag <- unlist(lapply(data, function(x) x$y))
    Q_bdiag <- bdiag(lapply(data, function(x) x$Q))
    P_bdiag <- bdiag(lapply(data, function(x) x$P))
    S_bdiag <- bdiag(lapply(data, function(x) x$S))
    c0_bdiag <- rep(unlist(lapply(data, function(x) x$c0)),
                   times = unlist(lapply(data, function(x) nrow(x$S))))

    data_bdiag <- list(y = y_bdiag, Q = Q_bdiag, P = P_bdiag, S = S_bdiag, c0 = c0_bdiag)

    # Check dimensions
    expect_length(y_bdiag, 5)
    expect_equal(dim(Q_bdiag), c(8, 7))
    expect_equal(dim(P_bdiag), c(5, 8))
    expect_equal(dim(S_bdiag), c(7, 7))
    expect_length(c0_bdiag, 7)
})
