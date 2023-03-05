find_mle_linear_bfgs = function(design, outcome) {

    n_pred = ncol(design)
    coeff_init = rep(1, n_pred)

    op_result = optim(coeff_init,
                      log_likelihood_linear,
                      log_likelihood_linear_gradient,
                      design = design,
                      outcome = outcome,
                      method = "BFGS",
                      control = list(fnscale = -1))

    return(op_result$par)
}

find_mle_linear_pseudo_inv = function(design, outcome) {

    A = crossprod(design)
    b = crossprod(design, outcome)
    upper = chol(A)

    as.vector(backsolve(upper, backsolve(upper, b, transpose = TRUE)))
}

find_mle_logit_bfgs = function(design, outcome) {

    n_pred = ncol(design)
    coeff_init = rep(1, n_pred)

    op_result = optim(coeff_init,
                      log_likelihood_logit,
                      log_likelihood_logit_gradient,
                      design = design,
                      outcome = outcome,
                      method = "BFGS",
                      control = list(fnscale = -1))

    return(op_result$par)
}
