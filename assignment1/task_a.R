

# Header ------------------------------------------------------------------

pacman::p_load(MASS)

colnames(Boston)


# Creating the Function ---------------------------------------------------

boston_quick_ols <- function(dependent, ...) {
  
  # Create a formula string from the inputs
  independents <- paste(c(...), collapse = " + ")
  formula_string <- paste(dependent, "~", independents)
  
  # Fit the model 
  fitted_model <- lm(as.formula(formula_string), data = Boston)
  
  # Get the summary
  fitted_model_summary <- summary(fitted_model)
  
  # Get point estimates and confidence intervals
  list_coef <- fitted_model_summary$coefficients
  list_conf <- confint(fitted_model, level = 0.95)
  list_ervr <- fitted_model_summary$sigma^2
  
  # Output a list
  return(list(
    coefficients = list_coef[,1],
    error_variance = list_ervr,
    test_statistic_t = list_coef[,3],
    test_statistic_p = list_coef[,4],
    confidence_intervals = list_conf
  ))
  
}


# Running it --------------------------------------------------------------

boston_quick_ols("medv", "rm", "age", "dis", "nox")

