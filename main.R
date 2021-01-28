#
#
# MAIN
# The main project file. Run this before running other scripts in the project.
#
#



# =============================================================================
# --- version ---

R.version.string  # "R version 4.0.3 (2020-10-10)"



# =============================================================================
# --- global variables ---

wk.dir <- getwd()



# =============================================================================
# ---- libraries ----

# Install libraries needed for the project.
install.packages("ggplot2")
###
###

# Load the libraries needed for the project to run.
###
###
###



# =============================================================================
# --- folder management ---

# Store names of the project folders.
folder.names <- c("1.Data", "2.Results","3.Figures")

# Create each folder if it doesn't already exist.
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# Store the file path to each folder.
PathData <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
PathResults <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
PathFigures <- paste(wk.dir, "/", folder.names[3], "/", sep = "")



# =============================================================================
# --- run scripts ---

###
###
###



# ==== end =================================================================