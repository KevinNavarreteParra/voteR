test_that("Correctly identifies duplicates", {

  data1 <- data.frame(a = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                      b = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))

  data2 <- data.frame(a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                      b = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                      c = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))

  expect_equal(check_identical(data1, 2, "greater"), 5)

  expect_equal(check_identical(data2, 2, "greater"), 10)
})
