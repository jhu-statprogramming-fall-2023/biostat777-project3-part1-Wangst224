abs_tol = 1e-6
rel_tol = 1e-6

test_that("Correctly return TRUE", {

    v = c(1, 1)
    w = v

    expect_true(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})

test_that("Relative error fail", {

    v = c(1, 1)
    w = c(1, 1 * (1+ rel_tol * 10))

    expect_false(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})

test_that("Absolute error fail", {

    v = c(1, 1)
    w = c(1, 1 + abs_tol * 10)

    expect_false(are_all_close(
        v, w, abs_tol = abs_tol, rel_tol = rel_tol
    ))

})
