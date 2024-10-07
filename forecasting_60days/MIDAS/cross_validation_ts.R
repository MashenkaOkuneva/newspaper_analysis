cross_validation_ts <- function(estimation_data, y, n_folds = 10, grid = NULL, alpha = 1) {
  
  # Number of Observations
  n_obs <- length(y)
  
  # If no grid supplied, generate one
  if (is.null(grid)) {
    grid <- 10^seq(10, -2, length.out = 100)
  }
  
  # Initial training size
  initial_size <- n_obs - n_folds
  
  # Placeholder for errors
  lambda_errors <- matrix(0, nrow = length(grid), ncol = n_folds)
  
  # Loop for walk-forward validation
  for (i in 1:n_folds) {
    
    # Training and testing data split
    train_data <- estimation_data[1:(initial_size + i - 1), ]
    train_y <- y[1:(initial_size + i - 1)]
    validate_data <- estimation_data[(initial_size + i), , drop = FALSE]
    validate_y <- y[(initial_size + i)]
    
    # Fit Model
    fit <- glmnet(train_data, train_y, alpha = alpha, lambda = grid)
    
    # Validate Model
    for (lambda_index in seq_along(grid)) {
      predictions <- predict(fit, newx = validate_data, s = grid[lambda_index])
      # Mean squared prediction error
      lambda_errors[lambda_index, i] <- mean((predictions - validate_y)^2)
    }
  }
  
  # Average the error for each lambda across all folds
  avg_lambda_error <- rowMeans(lambda_errors, na.rm = TRUE)
  
  # Find the optimal lambda
  best_lambda <- grid[which.min(avg_lambda_error)]
  
  return(list(best_lambda = best_lambda, avg_error = avg_lambda_error))
}