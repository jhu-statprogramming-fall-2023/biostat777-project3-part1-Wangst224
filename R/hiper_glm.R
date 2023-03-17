#' @export
hiper_glm = function(design, outcome, model = "linear", option = list()){

    supported_model = c("linear", "logit")

    if (!(model %in% supported_model)) {
        stop(sprintf("The model %s is not supported.", model))
    }

    hglm_out = list()
    class(hglm_out) = "hglm"

    if (model == "linear") {

        if (is.null(option$mle_finder)) {
            stop("Please specify MLE finder: 'pseudo_inv' or 'bfgs'.")
        }

        else if (option$mle_finder == "pseudo_inv"){
            hglm_out$coefficients = find_mle_linear_pseudo_inv(design, outcome)
        }

        else if (option$mle_finder == "bfgs") {
            hglm_out$coefficients = find_mle_bfgs(design, outcome, log_likelihood_linear, log_likelihood_linear_gradient)
        }

        else {
            stop("MLE finder not supported. Currently available: 'pseudo_inv' or 'bfgs'.")
        }
    }

    else if (model == "logit") {

        if (is.null(option$mle_finder)) {
            stop("Please specify MLE finder: 'newton' or 'bfgs'.")
        }

        else if (option$mle_finder == "newton") {
            hglm_out$coefficients = find_mle_logit_newton(design, outcome)
        }

        else if (option$mle_finder == "bfgs") {
            hglm_out$coefficients = find_mle_bfgs(design, outcome, log_likelihood_logit, log_likelihood_logit_gradient)
        }

        else {
            stop("MLE finder not supported. Currently available: 'newton' or 'bfgs'.")
        }
    }

    return(hglm_out)
}
