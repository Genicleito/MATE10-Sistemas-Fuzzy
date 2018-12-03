# Function to calculate RMSE (Root Mean Squared Error)
# Input parameters:
#   - y_true: the ground truth numeric vector.
#   - y_pred: the predicted numeric vector.
rmse <- function(y_true, y_pred) {
    error <- y_true - y_pred
    return(sqrt(mean(error^2)))
}

# Function to calculate MAE (Mean Absolute Error)
# Input parameters:
#   - y_true: the ground truth numeric vector.
#   - y_pred: the predicted numeric vector.
mae <- function(y_true, y_pred) {
    error <- y_true - y_pred
    return(mean(abs(error)))
}