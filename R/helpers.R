expit = function(design, coeff) {
    1 / (1 + exp(-design %*% coeff))
}
