extract_ordinal_regression_results <- function(model) {
  # Extract coefficients and standard errors for predictors only
  coef_model <- coef(model)
  predictors <- names(coef_model)[!grepl("^alpha", names(coef_model))]  # Exclude thresholds
  coefficients <- coef_model[predictors]
  se <- sqrt(diag(vcov(model)))[predictors]
  
  # Calculate t-values and p-values
  t_values <- coefficients / se
  df_residual <- df.residual(model)
  p_values <- 2 * pt(abs(t_values), df = df_residual, lower.tail = FALSE)
  
  # Calculate 95% confidence intervals
  lower_95 <- coefficients - 1.96 * se
  upper_95 <- coefficients + 1.96 * se
  
  # Create a data.table to store the results for predictors
  result_table <- data.table(
    Variable = predictors,
    Estimate = round(coefficients, 3),
    Std.Error = round(se, 3),
    TValue = round(t_values, 3),
    PValue = round(p_values, 3),
    Coef.Lower95 = round(lower_95, 3),
    Coef.Upper95 = round(upper_95, 3)
  )
  
  return(result_table)
}

summary_model = function(model,type, digits = 3, alpha = 0.05){
  if (type == 'glm') {
    library(data.table)
    glm.coefs <- as.data.table(summary(model)$coefficients, keep.rownames = TRUE)
    setnames(x = glm.coefs, old = "rn", new = "Variable")
    z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
    glm.coefs[, Odds.Ratio := exp(Estimate)]
    glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
    glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
    
    return(glm.coefs[])
  }
  if (type == 'lm') {
    library(data.table)
    lm.coefs <- as.data.table(summary(model)$coefficients, keep.rownames = TRUE)
    setnames(x = lm.coefs, old = "rn", new = "Variable")
    
    z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
    lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
    lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
    return(lm.coefs)
  }
}