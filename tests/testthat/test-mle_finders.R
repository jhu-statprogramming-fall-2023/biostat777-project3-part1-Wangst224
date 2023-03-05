test_that("Linear MLE: BFGS and pseudo inverse", {

    n_obs = 32
    n_pred = 4

    data_simulated = simulate_data(n_obs, n_pred, seed = 233)

    hglm_out_bfgs = hiper_glm(data_simulated$design, data_simulated$outcome, option = list(mle_finder = "bfgs"))
    hglm_out_pseudo_inv = hiper_glm(data_simulated$design, data_simulated$outcome, option = list(mle_finder = "pseudo_inv"))

    expect_true(are_all_close(
        coef(hglm_out_bfgs),
        coef(hglm_out_pseudo_inv),
        abs_tol = 1e-6, rel_tol = 1e-6
    ))

})

test_that("Logit MLE: BFGS and Newton", {

    n_obs = 32
    n_pred = 4

    data_simulated = simulate_data(n_obs, n_pred, model = "logit", seed = 185)

    hglm_out_bfgs = hiper_glm(
        data_simulated$design,
        data_simulated$outcome,
        model = "logit",
        option = list(mle_finder = "bfgs")
    )

    hglm_out_newton = hiper_glm(
        data_simulated$design,
        data_simulated$outcome,
        model = "logit",
        option = list(mle_finder = "newton")
    )

    expect_true(are_all_close(
        coef(hglm_out_bfgs),
        coef(hglm_out_newton),
        abs_tol = 1e-6, rel_tol = 1e-6
    ))

})
