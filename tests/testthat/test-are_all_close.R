test_that("all are close", {
    # Case 1: correctly return TRUE

    abs_tol = 1e-6
    rel_tol = 1e-6

    v = rexp(10)
    w = v
    w[1] = min(c(w[1] + abs_tol/10, w[1] * (1 + rel_tol/10)))

    expect_true(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})

test_that("all are close: relative error fail", {
    # Case 2: Relative error above tolerance

    abs_tol = 1e-6
    rel_tol = 1e-6

    v = rexp(10)
    w = v
    w[1] = w[1] * (1 + rel_tol * 10)

    expect_true(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})

test_that("all are close: absolute error fail", {
    # Case 2: Absolute error above tolerance

    abs_tol = 1e-6
    rel_tol = 1e-6

    v = rexp(10)
    w = v
    w[1] = w[1] + abs_tol * 10

    expect_true(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})
