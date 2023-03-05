log_likelihood_linear = function(design, outcome, coeff, noise_var = 1){

    n = length(outcome)

    - (n/2) * log(2 * pi) - n * log(noise_var^0.5) - (1/(2 * noise_var)) * sum((outcome - design %*% coeff)^2)
}

log_likelihood_linear_gradient = function(design, outcome, coeff, noise_var = 1){

    1/noise_var * t(design) %*% (outcome - design %*% coeff)
}

log_likelihood_logit = function(design, outcome, coeff) {

    prob = exp(design %*% coeff) / (1 + exp(design %*% coeff))
    sum(outcome * log(prob) + (1 - outcome) * log(1 - prob))
}

log_likelihood_logit_gradient = function(design, outcome, coeff) {

    prob = exp(design %*% coeff) / (1 + exp(design %*% coeff))
    t(design) %*% (outcome - prob)
}

log_likelihood_logit_hessian = function(design, outcome, coeff) {

    prob = as.vector(exp(design %*% coeff) / (1 + exp(design %*% coeff)))
    - t(design) %*% diag(prob * (1 - prob)) %*% design
}
