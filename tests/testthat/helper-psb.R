# Shared test helper for PSB fixtures.

# Add the derived columns that the PSB methods require, computed the same way
# compute_derived_quantities() does: se_s1 (used by get_re1_tau2) and pcc3
# (used by uwls3). Fixtures that supply t_value and dof can be passed directly.
add_pcc_derived <- function(df) {
  r_p <- df$t_value / sqrt(df$t_value^2 + df$dof)
  df$se_s1 <- sqrt((1 - r_p^2) / df$dof)
  df$pcc3 <- df$t_value / sqrt(df$t_value^2 + df$dof + 3)
  df
}
