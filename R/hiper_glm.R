#' @export
hiper_glm = function(design, outcome, model = "linear", option = list()){

    supported_model = c("linear", "logit")

    if (!(model %in% supported_model)){
        stop(sprintf("The model %s is not supported.", model))
    }

    hglm_out = list()
    class(hglm_out) = "hglm"

    if (model == "linear"){

        if (is.null(option$mle_finder == "pseudo_inv")){
            hglm_out$coefficients = find_mle_linear_pseudo_inv(design, outcome)
        }

        else {
            hglm_out$coefficients = find_mle_linear_bfgs(design, outcome)
        }
    }

    return(hglm_out)
}
