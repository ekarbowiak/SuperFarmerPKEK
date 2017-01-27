test_that("Spelniony warunek wygranej",{expect_true(czy_wygrana(c(1,1,1,1,1,1,1)))})

test_that("Spelniony warunek wygranej",{expect_true(czy_wygrana(c(1,1,1,1,1,0,0)))})

test_that("Niespelniony warunek wygranej",{expect_false(czy_wygrana(c(1,1,1,0,1,1,1)))})

test_that("Niespelniony warunek wygranej",{expect_false(czy_wygrana(c(0,0,0,0,1,0,0)))})
