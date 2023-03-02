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

    chol2inv(chol(t(design) %*% design)) %*% t(design) %*% outcome

}
