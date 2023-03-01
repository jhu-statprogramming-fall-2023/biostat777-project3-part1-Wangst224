test_that("MLE from BFGS and pseudo inverse", {

    n_obs = 32
    n_pred = 4

    data_simulated = simulate_data(n_obs, n_pred)

    hglm_out_bfgs = hiper_glm(data_simulated$design, data_simulated$outcome)
    hglm_out_pseudo_inv = hiper_glm(data_simulated$design, data_simulated$outcome, option = list(mle_finder = "pseudo_inv"))

    abs_tol = 1e-6
    rel_tol = 1e-6

    expect_true(are_all_close(
        hglm_out_bfgs$coefficients,
        hglm_out_pseudo_inv$coefficients,
        abs_tol = abs_tol, rel_tol = rel_tol
    ))

})
