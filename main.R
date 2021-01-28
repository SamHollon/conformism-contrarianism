# =============================================================================
# --- version ---

R.version.string  # "R version 4.0.3 (2020-10-10)"



# =============================================================================
# --- notes ---

###
###
###



# =============================================================================
# --- global variables ---

wk.dir <- getwd()



# =============================================================================
# ---- libraries ----

# Install libraries needed for the project.
###
###
###

# Load the libraries needed for the project to run.
###
###
###



# =============================================================================
# --- folder management ---

# Store names of the project folders.
folder.names <- c("1.RawData","2.CleanData", "3.Results","4.Figures")

# Create each folder if it doesn't already exist.
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# Store the file path to each folder.
PathDataRaw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
PathDataClean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
PathResults <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
PathFigures <- paste(wk.dir, "/", folder.names[4], "/", sep = "")



# =============================================================================
# --- run scripts ---

###
###
###



# ==== end =================================================================