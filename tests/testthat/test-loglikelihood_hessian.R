test_that("Logistic regression log likelihood Hessian matrix", {

    n_obs = 32
    n_pred = 4
    x = rep(1, n_pred)
    dx = 1e-6

    data_list = simulate_data(n_obs, n_pred, model = "logit", seed = 196)
    design = data_list$design
    outcome = data_list$outcome
    coeff = data_list$coef_true

    prod_H_x = log_likelihood_logit_hessian(design, outcome, coeff) %*% x
    prod_H_x_numerical = (
        log_likelihood_logit_gradient(design, outcome, coeff + (0 + dx) * x) -
        log_likelihood_logit_gradient(design, outcome, coeff + (0 - dx) * x)
    ) / (2 * dx)

    expect_true(are_all_close(
        prod_H_x, prod_H_x_numerical, abs_tol = 1e-6, rel_tol = 1e-6
    ))
})
