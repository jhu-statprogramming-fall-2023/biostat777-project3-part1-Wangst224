test_that("MLE from BFGS and pseudo inverse", {

    set.seed(233)
    n_obs = 32
    n_pred = 4

    data_simulated = simulate_data(n_obs, n_pred)

    hglm_out_bfgs = hiper_glm(data_simulated$design, data_simulated$outcome, option = list(mle_finder = "bfgs"))
    hglm_out_pseudo_inv = hiper_glm(data_simulated$design, data_simulated$outcome, option = list(mle_finder = "pseudo_inv"))

    expect_true(are_all_close(
        coef(hglm_out_bfgs),
        coef(hglm_out_pseudo_inv),
        abs_tol = 1e-6, rel_tol = 1e-6
    ))

})
