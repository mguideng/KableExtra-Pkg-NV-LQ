#-----------------------------------------------------------------#
#                      kableExtra-Pkg-NV-LQ                       #
#-----------------------------------------------------------------#
# Script purpose:                                                 #
#   Merge all files into a single csv                             #
#-----------------------------------------------------------------#

# CSV input files stored in this repository: "/KableExtra_Pkg_NV_LQ/data/allfiles.zip" 
  # Download and unzip the files. Set working directory to where they are saved.
getwd()
#setwd("C:/.../allfiles")

# Get file list
listfiles <- list.files(pattern="csv")

# Read all CSV files and create a list of dataframes
allfiles <-  lapply(listfiles, read.csv, stringsAsFactors = F)

# Combine each dataframe in the list into a single dataframe
dfraw <- do.call(rbind , allfiles)

# Remove row names
rownames(dfraw) <- c()

# Output
write.csv(dfraw, "dfraw.csv", row.names = F)

