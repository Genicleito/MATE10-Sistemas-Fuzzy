# Input parameters for all the error functions
#   - y_true: the ground truth numeric vector.
#   - y_pred: the predicted numeric vector.

# Function to calculate RMSE (Root Mean Squared Error)
rmse <- function(y_true, y_pred) {
    error <- y_true - y_pred
    return(sqrt(mean(error^2)))
}

# Function to calculate MAE (Mean Absolute Error)
mae <- function(y_true, y_pred) {
    error <- y_true - y_pred
    return(mean(abs(error)))
}

# Function to calculate Pearson R (pearson correlation)
pearson_r <- function(y_true, y_pred) {
    return(cor(y_pred, y_true, use = "complete.obs", method = "pearson"))
}

# Function to calculate Coefficient of Determination (R-Squared)
r_squared <- function(y_true, y_pred) {
    library(MLmetrics)
    return(R2_Score(y_pred, y_true))
}

# Function to calculate Factor of 2 measure
factor_of_2 <- function(y_pred, y_true) {
    min_ = 0.5
    max_ = 2.0
    division <- y_pred / y_true
    greater_min <- division >= min_
    less_max <- division <= max_
    res <- greater_min == less_max
    f2 <- sum(res == TRUE) / length(res)
    return(f2)
}