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

find_mle_logit_newton = function(design, outcome, max_iter = 10000, init = rep(0, ncol(design))) {

    coeff = init
    iter = 0

    while (iter < max_iter) {
        hessian = log_likelihood_logit_hessian(design, outcome, coeff)
        gradient = log_likelihood_logit_gradient(design, outcome, coeff)
        coeff = coeff - solve(hessian) %*% gradient

        iter = iter + 1
    }

    return(as.vector(coeff))
}
