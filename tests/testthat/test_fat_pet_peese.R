# Tests for fat_pet_peese (conditional FAT-PET-PEESE)

test_that("fat_pet_peese returns NA when k < 3", {
  df <- data.frame(effect = c(0.2, 0.3), se_s1 = c(0.1, 0.2))
  res <- fat_pet_peese(df)
  expect_true(is.na(res$est))
  expect_true(is.na(res$se))
  expect_true(is.na(res$fat))
  expect_true(is.na(res$fat_se))
  expect_true(is.na(res$type))
})

test_that("fat_pet_peese reports the FAT (Egger) slope from the WLS PET fit", {
  se <- c(0.05, 0.08, 0.10, 0.12, 0.20, 0.25)
  effect <- c(0.10, 0.12, 0.14, 0.15, 0.22, 0.30) # grows with se -> asymmetry
  df <- data.frame(effect = effect, se_s1 = se)

  res <- fat_pet_peese(df, alpha = 0.1)

  w <- 1 / se^2
  pet <- stats::lm(effect ~ se, weights = w)
  co <- summary(pet)$coefficients
  expect_equal(res$fat, unname(co[2, "Estimate"]), tolerance = 1e-10)
  expect_equal(res$fat_se, unname(co[2, "Std. Error"]), tolerance = 1e-10)
})

test_that("fat_pet_peese picks PEESE iff the one-sided PET test is significant", {
  df <- data.frame(
    effect = c(0.30, 0.31, 0.33, 0.34, 0.36, 0.38),
    se_s1  = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
  )
  res <- fat_pet_peese(df, alpha = 0.1)

  w <- 1 / df$se_s1^2
  pet <- stats::lm(effect ~ se_s1, weights = w, data = df)
  co <- summary(pet)$coefficients
  k <- nrow(df)
  p_one <- stats::pt(co[1, "Estimate"] / co[1, "Std. Error"], df = k - 2, lower.tail = FALSE)
  expected_type <- if (p_one < 0.1) "PEESE" else "PET"

  expect_equal(res$type, expected_type)

  if (expected_type == "PEESE") {
    peese <- stats::lm(effect ~ I(se_s1^2), weights = w, data = df)
    expect_equal(res$est, unname(summary(peese)$coefficients[1, "Estimate"]), tolerance = 1e-9)
  } else {
    expect_equal(res$est, unname(co[1, "Estimate"]), tolerance = 1e-9)
  }
})

test_that("fat_pet_peese reports PET (not PEESE) when the corrected estimate is non-positive", {
  # Pure small-study effect with a clearly negative intercept: one-sided test for
  # effect > 0 cannot be significant, so the PET estimate is reported.
  se <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
  effect <- -0.20 + 2 * se + c(0.002, -0.003, 0.001, -0.002, 0.003, -0.001)
  df <- data.frame(effect = effect, se_s1 = se)

  res <- fat_pet_peese(df, alpha = 0.1)
  expect_equal(res$type, "PET")
})
