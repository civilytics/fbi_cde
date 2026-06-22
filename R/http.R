#' Flatten CDE JSON response
#'
#' Turns an object-of-{label -> {period -> value}} into a long/tidy data.frame
#'
#' @param obj A nested list/object from the CDE API with structure {label -> {period -> value}}
#' @return A data.frame with columns: label, period, value
#' @examples
#' obj <- list(
#'   "United States Offenses" = list(
#'     "01-2015" = 27.6,
#'     "01-2016" = 29.62
#'   )
#' )
#' flatten_cde_json(obj)
flatten_cde_json <- function(obj) {
  if (is.null(obj) || length(obj) == 0) {
    return(data.frame(label = character(), period = character(), value = numeric(), stringsAsFactors = FALSE))
  }
  
  # Initialize empty vectors to collect data
  labels <- c()
  periods <- c()
  values <- c()
  
  # Iterate through each label in the object
  for (label in names(obj)) {
    label_data <- obj[[label]]
    
    # Handle case where label_data might be NULL
    if (is.null(label_data)) {
      next
    }
    
    # Iterate through each period for this label
    for (period in names(label_data)) {
      value <- label_data[[period]]
      
      # Skip NULL values
      if (is.null(value)) {
        next
      }
      
      labels <- c(labels, label)
      periods <- c(periods, period)
      values <- c(values, value)
    }
  }
  
  # Return as data.frame
  data.frame(
    label = labels,
    period = periods,
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
}
