# This script implements a forecasting experiment using a MIDAS model with hard data only, 
# where LASSO is applied for dimensionality reduction to select the most relevant predictors,
# followed by OLS for final prediction. Forecasts are generated across different 
# forecasting horizons (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts) and 
# for varying numbers of lags (p).
#
# For each number of lags, forecast errors are calculated, and forecasts are plotted against 
# the actual annualized GDP growth. The Diebold-Mariano test (both one-sided and two-sided) 
# is performed to compare the MIDAS-LASSO-OLS forecasts against AR(1) and Reuters poll forecasts.
# RMSE ratios and statistical significance results based on Diebold-Mariano p-values 
# are saved to LaTeX tables.

rm(list = ls())
# Required libraries
library(glmnet)
library(midasr)
# Source the function
source("cross_validation_ts.R")

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

perform_forecast <- function(data, nlag, p, mon, forecast_indicator) {
  
  # Function Parameters:
  # data: The dataframe coresponding to a particular vintage, containing GDP growth and hard economic data.
  # nlag: Specifies the lag structure:
  #       nlag = 0 for backcast (looking backward),
  #       nlag = 1 for nowcast (current period),
  #       nlag = 2 for one-step-ahead forecast, and so on.
  # p: Specifies the number of lags used in the MIDAS regression.
  # mon: Specifies the number of months of available data.
  # forecast_indicator: A string that indicates the column name in 'data' that identifies the forecast date.
  
  # Get column names starting with "financial" or "economic"
  selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
  
  # Filter out rows with all NA values in the financial/economic columns
  data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
  
  # Remove the first (1+mon) observations and the last (mon + 1) observations.
  #   - The first (1+mon) observations are removed to ensure the time series is compatible
  #     with a MIDAS approach, as the number of observations
  #     must be divisible by 3 to represent a quarterly series effectively.
  #   - The last (mon + 1) observations are removed to guarantee that the
  #     subsequent analysis only uses data points where all economic and financial 
  #     series are available.
  data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]
  
  # Extract the GDP growth data series and remove NA values
  y <- data$d_gdp
  y <- y[!is.na(y)]
  y <- y[(2 + nlag):length(y)]
  
  # Create an empty dataframe for the lagged values of each series
  lags_df <- data.frame()
  
  # Create lagged features for each economic/financial series
  for (series_name in selected_columns) {
    # Get the series
    X <- standardize_data(data_rm_na[[series_name]])
    # Create the lags for this series
    mlags_series <- fmls(X, k = p, m = 3)
    # Rename the columns based on the series name
    colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
    # Bind the lags to the lags_df data frame
    lags_df <- if (nrow(lags_df) == 0) mlags_series else cbind(lags_df, mlags_series)
  }
  
  # Filter out rows with NA values
  num_na_rows <- sum(apply(lags_df[1:length(y), 1:ncol(lags_df)], 1, function(row) all(is.na(row))))
  y <- y[(1 + num_na_rows):length(y)]
  
  # Split the data into estimation and prediction datasets
  estimation_data <- lags_df[(1 + num_na_rows):(length(y) + num_na_rows), ]
  prediction_data <- lags_df[(length(y) + 1 + num_na_rows + nlag):(length(y) + 1 + num_na_rows + nlag), ]
  
  # Create a sequence of lambda values for cross-validation
  grid = 10^seq(10,-2, length=100)
  
  # Set a random seed for reproducibility
  #set.seed(1)
  
  # Perform k-fold cross-validation (default k=10) to determine the best lambda
  #cv.out = cv.glmnet(estimation_data, y,
  #                   alpha = 1, lambda=grid)
  
  # Extract the lambda value that gave the smallest mean cross-validated error
  #bestlam=cv.out$lambda.min
  
  # Cross-validaiton for time-series data
  cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)
  
  # Retrieve and use the optimal lambda
  bestlam <- cv_results$best_lambda
  
  # Fit the LASSO model on the estimation data
  out = glmnet(estimation_data, y, alpha = 1, lambda=grid)
  
  # Identify the predictors that have non-zero coefficients
  selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]
  selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]
  
  # Filter the estimation dataset to only use selected predictors
  ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]
  
  # OLS regression using the selected predictors
  ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))
  
  # Prepare the data for prediction
  prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)
  ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]
  ols_prediction_data$`(Intercept)` <- 1
  ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]
  
  # Generate forecast using the OLS model
  ols_pred <- predict(ols_model, newdata = ols_prediction_data)
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = ols_pred, date = forecast_date))
}

run_midas_lasso_ols <- function(p) {
  
  # Get the list of all .csv files in the 'vintages_hard_MIDAS' directory
  files <- list.files(path="../hard_data/vintages_hard_MIDAS/", pattern="*.csv")
  
  # Vectors to store results for all vintages
  backcasts <- c()
  nowcasts <- c()
  forecasts_1step <- c()
  forecasts_2step <- c()
  
  backcast_dates <- c()
  nowcast_dates <- c()
  forecast1_dates <- c()
  forecast2_dates <- c()
  
  # Iterate over each file and perform forecasting
  for (file in files) {
    
    # Read in the data
    data <- read.csv(paste0("../hard_data/vintages_hard_MIDAS/", file), header = TRUE, stringsAsFactors = FALSE)
    
    backcast_result <- perform_forecast(data, nlag = 0, p = p, mon = 1, forecast_indicator = 'ind_backcast')
    nowcast_result <- perform_forecast(data, nlag = 1, p = p, mon = 1, forecast_indicator = 'ind_nowcast')
    one_step_result <- perform_forecast(data, nlag = 2, p = p, mon = 1, forecast_indicator = 'ind_forecast1Q')
    two_step_result <- perform_forecast(data, nlag = 3, p = p, mon = 1, forecast_indicator = 'ind_forecast2Q')
    
    # Append results to the result vectors
    backcasts <- c(backcasts, backcast_result$forecast)
    nowcasts <- c(nowcasts, nowcast_result$forecast)
    forecasts_1step <- c(forecasts_1step, one_step_result$forecast)
    forecasts_2step <- c(forecasts_2step, two_step_result$forecast)
    
    backcast_dates <- c(backcast_dates, backcast_result$date)
    nowcast_dates <- c(nowcast_dates, nowcast_result$date)
    forecast1_dates <- c(forecast1_dates, one_step_result$date)
    forecast2_dates <- c(forecast2_dates, two_step_result$date)
    
  }
  
  # Convert lists to data frames and order them by date
  backcasts_df <- data.frame(date = unlist(backcast_dates), forecast = unlist(backcasts))
  nowcasts_df <- data.frame(date = unlist(nowcast_dates), forecast = unlist(nowcasts))
  forecasts_1step_df <- data.frame(date = unlist(forecast1_dates), forecast = unlist(forecasts_1step))
  forecasts_2step_df <- data.frame(date = unlist(forecast2_dates), forecast = unlist(forecasts_2step))
  
  # Sort the data frames by date
  backcasts_df <- backcasts_df[order(backcasts_df$date), ]
  nowcasts_df <- nowcasts_df[order(nowcasts_df$date), ]
  forecasts_1step_df <- forecasts_1step_df[order(forecasts_1step_df$date), ]
  forecasts_2step_df <- forecasts_2step_df[order(forecasts_2step_df$date), ]
  
  # Save the results to CSV files without column names
  write.table(backcasts_df, sprintf("backcasts_midas_lasso_ols_%dlags_hard.csv", p), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(nowcasts_df, sprintf("nowcasts_midas_lasso_ols_%dlags_hard.csv", p), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(forecasts_1step_df, sprintf("forecasts_1step_midas_lasso_ols_%dlags_hard.csv", p), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  write.table(forecasts_2step_df, sprintf("forecasts_2step_midas_lasso_ols_%dlags_hard.csv", p), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
  
  
  return(list(backcasts = backcasts_df, nowcasts = nowcasts_df, forecasts_1step = forecasts_1step_df, forecasts_2step = forecasts_2step_df))
}


calculate_forecast_errors <- function(results, p) {
  
  # Load gdp_growth.csv
  gdp_growth <- read.csv(file.path("../DFM/data", "gdp_growth.csv"), stringsAsFactors = FALSE)
  
  # Extracting the dataframes with the forecasts
  backcasts <- results$backcasts
  nowcasts <- results$nowcasts
  forecasts_1step <- results$forecasts_1step
  forecasts_2step <- results$forecasts_2step
  
  # Merging forecasts with gdp_growth based on date
  backcasts <- merge(backcasts, gdp_growth, by = "date", all.x = TRUE)
  nowcasts <- merge(nowcasts, gdp_growth, by = "date", all.x = TRUE)
  forecasts_1step <- merge(forecasts_1step, gdp_growth, by = "date", all.x = TRUE)
  forecasts_2step <- merge(forecasts_2step, gdp_growth, by = "date", all.x = TRUE)
  
  # Calculate forecast errors
  backcasts$fe <- backcasts$forecast - backcasts$d_gdp
  nowcasts$fe <- nowcasts$forecast - nowcasts$d_gdp
  forecasts_1step$fe <- forecasts_1step$forecast - forecasts_1step$d_gdp
  forecasts_2step$fe <- forecasts_2step$forecast - forecasts_2step$d_gdp
  
  # Combine all forecast errors into a single dataframe
  combined_errors <- data.frame(
    backcasts_fe = backcasts$fe,
    nowcasts_fe = nowcasts$fe,
    forecasts_1step_fe = forecasts_1step$fe,
    forecasts_2step_fe = forecasts_2step$fe
  )
  
  # Save the combined errors to a CSV
  file_name <- sprintf("forecast_errors_midas_lasso_ols_%dlags_hard.csv", p)
  current_dir <- getwd() # Get the current directory
  write.csv(combined_errors, file.path(current_dir, file_name), row.names = FALSE)
  
  return(combined_errors)
}

forecasts_midas_plot <- function(results, p) {
  
  # Load gdp_growth.csv
  gdp_growth <- read.csv(file.path("../DFM/data", "gdp_growth.csv"), stringsAsFactors = FALSE)
  
  # Extracting the dataframes with the forecasts
  backcasts <- results$backcasts
  nowcasts <- results$nowcasts
  forecasts_1step <- results$forecasts_1step
  forecasts_2step <- results$forecasts_2step
  
  # Merging forecasts with gdp_growth based on date
  backcasts <- merge(backcasts, gdp_growth, by = "date", all.x = TRUE)
  nowcasts <- merge(nowcasts, gdp_growth, by = "date", all.x = TRUE)
  forecasts_1step <- merge(forecasts_1step, gdp_growth, by = "date", all.x = TRUE)
  forecasts_2step <- merge(forecasts_2step, gdp_growth, by = "date", all.x = TRUE)
  
  # Convert the date columns to Date class
  backcasts$date <- as.Date(backcasts$date, format = "%Y-%m-%d")
  nowcasts$date <- as.Date(nowcasts$date, format = "%Y-%m-%d")
  forecasts_1step$date <- as.Date(forecasts_1step$date, format = "%Y-%m-%d")
  forecasts_2step$date <- as.Date(forecasts_2step$date, format = "%Y-%m-%d")
  
  # Define a function to generate a plot
  generate_plot <- function(data, title_str, file_name) {
    pdf(file_name, width = 10, height = 7)
    
    plot(data$date, data$d_gdp, type = "l", col = "blue", lwd = 2, 
         xlab = "Date", ylab = "", ylim = range(c(data$d_gdp, data$forecast)),
         main = title_str)
    
    lines(data$date, data$forecast, col = "red", lwd = 2)
    legend("topleft", legend = c("d_gdp", "forecast"),
           col = c("blue", "red"), lty = c(1,1), lwd = 2)
    
    dev.off()
  }
  
  # Generate plots for each dataframe
  generate_plot(backcasts, "Backcasts vs. GDP Growth", sprintf("backcasts_plot_midas_lasso_ols_%dlags_hard.pdf", p))
  generate_plot(nowcasts, "Nowcasts vs. GDP Growth", sprintf("nowcasts_plot_midas_lasso_ols_%dlags_hard.pdf", p))
  generate_plot(forecasts_1step, "One-step-ahead Forecasts vs. GDP Growth", sprintf("forecasts_1step_plot_midas_lasso_ols_%dlags_hard.pdf", p))
  generate_plot(forecasts_2step, "Two-step-ahead Forecasts vs. GDP Growth", sprintf("forecasts_2step_plot_midas_lasso_ols_%dlags_hard.pdf", p))
  
}

neweywest <- function(x, q) {
  n <- length(x)
  
  w2 <- var(x)
  
  if (q > 0) {
    for (k in 1:q) {
      errorcov <- cov(x[(1+k):n], x[1:(n-k)])
      w2 = w2 + (1-k/(q+1))*2*errorcov
    }
  }
  return(w2)
}

compare_forecast_errors <- function(fe_ms_model1, comparison_type = "AR1", test_type = "two-sided") {
  
  # Choose the second model for comparison based on the input argument
  if (comparison_type == "AR1") {
    fe_ms_model2 <- read.csv('..\\AR1\\forecast_errors_ar1.csv', header = TRUE)
  } else if (comparison_type == "professional") {
    fe_ms_model2 <- read.csv('..\\reuters-poll-eval\\forecast_errors_professional.csv', header = TRUE)
  } else {
    stop("Invalid comparison type. Please choose either 'AR1' or 'professional'.")
  }
  
  # Initialization
  hmax <- 4
  DM <- numeric(hmax)
  MDM <- numeric(hmax)
  
  for (h in 1:hmax) {
    e1 <- fe_ms_model1[, h]
    e2 <- fe_ms_model2[, h]
    tau <- length(e1)
    
    # Define the loss differential and calculate its mean
    d <- e1^2 - e2^2
    dMean <- mean(d)
    
    # Calculate the variance of the loss differential, taking into account autocorrelation
    Sigma_Ir <- neweywest(d, h-1)
    
    # Calculate the Diebold-Mariano statistic DM ~ N(0,1) and the modified DM statistic
    DM[h] <- dMean / sqrt((1/tau) * Sigma_Ir)
    MDM[h] <- tau^(-0.5) * sqrt(tau + 1 - 2*h + tau^(-1)*h*(h - 1)) * DM[h]
  }
  
  # Test decisions using DM
  if (test_type == "two-sided") {
    pval_equal_DM <- 2 * (1 - pnorm(abs(DM)))
  } else if (test_type == "one-sided") {
    pval_model1better_DM <- pnorm(DM)
  } else {
    stop("Invalid test type. Please choose either 'two-sided' or 'one-sided'.")
  }
  
  # Mean squared errors and RMSE
  MSE_model1 <- colMeans(fe_ms_model1^2)
  RMSE_model1 <- sqrt(MSE_model1)
  MSE_model2 <- colMeans(fe_ms_model2^2)
  RMSE_model2 <- sqrt(MSE_model2)
  RMSE_ratio <- RMSE_model1 / RMSE_model2
  
  # Output results
  list(
    RMSE_ratio = RMSE_ratio,
    pval_equal_DM = if (exists("pval_equal_DM")) pval_equal_DM else NULL,
    pval_model1better_DM = if (exists("pval_model1better_DM")) pval_model1better_DM else NULL
  )
}

format_rmse <- function(rmse, p_value) {
  formatted <- sprintf("%.2f", rmse)
  
  # Apply asterisks based on p-value
  if (p_value <= 0.01) {
    formatted <- paste0(formatted, "***")
  } else if (p_value <= 0.05) {
    formatted <- paste0(formatted, "**")
  } else if (p_value <= 0.1) {
    formatted <- paste0(formatted, "*")
  }
  
  # Apply bold based on RMSE value
  if (rmse <= 0.95) {
    formatted <- paste0("\\textbf{", formatted, "}")
  }
  
  return(formatted)
}

generate_latex_table <- function(comparison_results, comparison_type, test_type, file_name) {
  latex_table <- data.frame()
  
  for (p in 0:p_max) {
    # Extract RMSEs and p-values
    rmse_ratios <- comparison_results[[p+1]][[comparison_type]][[test_type]]$RMSE_ratio
    if (test_type == "two_sided"){
      p_values <- comparison_results[[p+1]][[comparison_type]][[test_type]]$pval_equal_DM
    }else{
      p_values <- comparison_results[[p+1]][[comparison_type]][[test_type]]$pval_model1better_DM
    }
    
    
    # Format RMSEs
    formatted_rmse <- mapply(format_rmse, rmse_ratios, p_values)
    
    # Create a row of the LaTeX table
    row <- c(paste("hard, sign1, LASSO $+$ OLS ($p = ", p, "$)", sep = ""), formatted_rmse)
    latex_table <- rbind(latex_table, row)
  }
  
  # Convert the data frame to a character vector of LaTeX table rows
  latex_rows <- apply(latex_table, 1, function(row) {
    paste(row, collapse = " & ")
  })
  
  # Collapse the character vector to a single string, with rows separated by "\\" (LaTeX new line in tables)
  latex_table_string <- paste(latex_rows, collapse = " \\\\\n")
  
  # Save to .tex file
  writeLines(latex_table_string, con = file_name)
}

# Initialize parameters
p_max <- 5
comparison_results <- list()

# Loop through each value of p
for (p in 0:p_max) {
  # Run the MIDAS regression with LASSO+OLS
  results <- run_midas_lasso_ols(p = p)
  
  # Calculate forecast errors
  fe_midas_lasso_ols = calculate_forecast_errors(results = results, p = p)
  
  # Generate plots
  forecasts_midas_plot(results = results, p = p)
  
  # Run the Diebold-Mariano test
  ar1_two_sided <- compare_forecast_errors(fe_midas_lasso_ols, comparison_type = "AR1", test_type = "two-sided")
  ar1_one_sided <- compare_forecast_errors(fe_midas_lasso_ols, comparison_type = "AR1", test_type = "one-sided")
  professional_two_sided <- compare_forecast_errors(fe_midas_lasso_ols, comparison_type = "professional", test_type = "two-sided")
  professional_one_sided <- compare_forecast_errors(fe_midas_lasso_ols, comparison_type = "professional", test_type = "one-sided")
  
  # Save results
  comparison_results[[p+1]] <- list(AR1 = list(two_sided = ar1_two_sided, one_sided = ar1_one_sided),
                                    Professional = list(two_sided = professional_two_sided, one_sided = professional_one_sided))
  
}

# Create LaTeX tables
comparison_types <- c("AR1", "Professional")
test_types <- c("two_sided", "one_sided")

for (comparison_type in comparison_types) {
  for (test_type in test_types) {
    file_name <- paste0("lasso_ols_hard_", tolower(comparison_type), "_", test_type, ".tex")
    generate_latex_table(comparison_results, comparison_type, test_type, file_name)
  }
}
