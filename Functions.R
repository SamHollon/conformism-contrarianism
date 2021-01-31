#
#
# FUNCTIONS
# Define functions for use in other scripts.
#
#



# =============================================================================
# --- remove.outliers ---

# Removes data outside more than 1.5 quantiles out.
# Function to remove outliers.
remove.outliers <- function(x, dst = 1.5, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- dst * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}