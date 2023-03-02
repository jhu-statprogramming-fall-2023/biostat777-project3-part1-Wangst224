#' @export
coef.hglm = function(hglm_out){
    hglm_out$coefficients
}


#' @export
vcov.hglm = function(hglm_out){
    warning("To be implemented")
}


#' @export
print.hglm = function(hglm_out){
    cat("Output for hiperGLM")
}
