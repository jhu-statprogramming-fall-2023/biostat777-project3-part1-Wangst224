log_likelihood_linear = function(design, outcome, coeff, noise_var = 1){

    n = length(outcome)

    - (n/2) * log(2 * pi) - n * log(noise_var^0.5) - (1/(2 * noise_var)) * sum((outcome - design %*% coeff)^2)
}

log_likelihood_linear_gradient = function(design, outcome, coeff, noise_var = 1){

    1/noise_var * t(design) %*% (outcome - design %*% coeff)

}
