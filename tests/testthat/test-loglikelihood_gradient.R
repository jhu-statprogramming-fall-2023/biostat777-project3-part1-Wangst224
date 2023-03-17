test_that("Linear regression log likelihood gradient", {

    n_obs = 32
    n_pred = 4

    data_list = simulate_data(n_obs, n_pred, seed = 224)

    LL_gradient = log_likelihood_linear_gradient(data_list$design, data_list$outcome, data_list$coef_true)
    LL_gradient_numerical = numerical_gradient(function(x) {
        log_likelihood_linear(data_list$design, data_list$outcome, x)
    }, data_list$coef_true)

    expect_true(are_all_close(
        LL_gradient, LL_gradient_numerical, abs_tol = 1e-6, rel_tol = 1e-6
    ))

})

test_that("Logistic regression log likelihood gradient", {

    n_obs = 32
    n_pred = 4

    data_list = simulate_data(n_obs, n_pred, model = "logit", seed = 335)

    LG_gradient = log_likelihood_logit_gradient(data_list$design, data_list$outcome, data_list$coef_true)
    LG_gradient_numerical = numerical_gradient(function(x) {
        log_likelihood_logit(data_list$design, data_list$outcome, x)
    }, data_list$coef_true)

    expect_true(are_all_close(
        LG_gradient, LG_gradient_numerical, abs_tol = 1e-6, rel_tol = 1e-6
    ))

})
