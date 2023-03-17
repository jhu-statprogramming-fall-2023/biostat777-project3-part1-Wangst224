find_mle_bfgs = function(design, outcome, func_likelihood, func_gradient) {

    n_pred = ncol(design)
    coeff_init = rep(1, n_pred)

    op_result = optim(coeff_init,
                      func_likelihood,
                      func_gradient,
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

find_mle_logit_newton = function(design, outcome, max_iter = 10000, init = rep(0, ncol(design))) {

    coeff = init
    iter = 0
    abs_tol = 1e-6
    rel_tol = 1e-6

    while (iter < max_iter) {
        pre_loglikelihood = log_likelihood_logit(design, outcome, coeff)

        hessian = log_likelihood_logit_hessian(design, outcome, coeff)
        gradient = log_likelihood_logit_gradient(design, outcome, coeff)
        coeff = coeff - solve(hessian) %*% gradient

        post_loglikelihood = log_likelihood_logit(design, outcome, coeff)
        abs_change = abs(post_loglikelihood - pre_loglikelihood)
        rel_change = abs_change / abs(pre_loglikelihood)

        if ((abs_change < abs_tol) | (rel_change < rel_tol)) {
            break
        }

        iter = iter + 1

    }

    if (iter == max_iter) {
        warning("Max iteration reached without convergence.")
    }

    return(as.vector(coeff))
}
