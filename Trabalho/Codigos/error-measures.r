# Function to calculate RMSE (Root Mean Squared Error)
# Input parameters:
#   - y_pred: the predicted numeric vector.
#   - y_true: the ground truth numeric vector.
rmse <- function(y_pred, y_true) {
    return(sqrt(mean((y_pred - y_true)^2)))
}