### Imputing degrees of freedom

- **Question**: When imputing degrees of freedom from sample size - how do I handle small/zero/negative resulting degrees of freedom?
- **Details**: In the degrees of freedom calculation, I use `sample_size - 7` when DoFs are missing. This, however, sometimes leads to negative imputed degrees of freedom. How should I handle these values? If left unchanged, they will cause issues down the line by having a negative sign, leading to problems such as taking a square root of a negative, resulting in errors.

- **Follow-up question**: When using `sample_size - 7` to impute the missing degrees of freedom, what happens during the PCC variance calculation?
- **Details**: In your setup for this analysis, there should be two different denominator offsets when calculating the initial PCC variance - `n - 1`, and `n - 2`. You mentioned degrees of freedom should be used here for n when present, but I'm a bit confused by your instructions on how to handle the case when DoF is missing. You suggest using `sample_size - 7`, which makes sense. However, I understand correctly that in order for the two different ways of computing PCC variance to make sense in this setup, the denominator should still use the offset (-1, -2) and be set to `sample_size - 8`, and `sample_size - 9` in case of missing DoFs?

### Calculating the UWLS+3

- **Question**: When calculating UWLS+3, what are the correct offsets?
- **Details**:

  ```R
  uwls3 <- function(df) {
    t_ <- df$effect / df$se
    dof_ <- get_dof_or_sample_size(df, target = "dof")

    pcc_ <- t_ / sqrt(t_^2 + dof_ + 3) # dof_ + 3 ~~ sample_size - 7 + 3
    pcc_var_ <- (1 - pcc_^2) / (dof_ + 3) # dof_ + 3 ~~ sample_size - 7 + 3
    ...
  }
  ```

  In the snippet above, are the offsets used in the denominators correct? We assume the DoF is set to either the DoF when present, or sample size - 7 when missing. The offsets from the two PCC variances in case of RE1/RE2/UWLS1/UWLS2 (-1 and -2) are discarded, do I understand that correctly?

### REML fails to converge

- **Question**: The rma method when calculating Random-Effects with `method = "ML"` sometimes fails to converge for seemingly valid data with the following errors message:

  ```R
  Fisher scoring algorithm did not converge. See 'help(rma)' for possible remedies.
  ```

  What could be the cause? Reading through the help manual, I failed to identify the potential problem.
