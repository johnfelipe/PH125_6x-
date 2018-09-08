# loading dslabs and tidyverse packages
library(dslabs)
library(tidyverse)

# locating the external data file directory "extdata" in dslabs and assigning the path to a variable
path <- system.file("extdata", package = "dslabs")
path

# listing the files in "extdata"
list.files(path)

# copying the a raw data file "murders.cvs" into the working directory. 
# In this case I created a raw data folder in the project and set the working 
# directory to that folder.
# I did all that inside RStudio

# creating some variables to use in the function calls
filename <- "murders.csv"

# the file.path function gets the full path in the correct form
fullpath <- file.path(path, filename)
fullpath

# file.copy copies the file. In this case into the raw data folder. 
# IMPORTANT the working directory must be pointed at where you want the file to be 
# to be copied
file.copy(fullpath, getwd())

# file.exists checks if the file is in the working directors
file.exists(filename)

# the "readr" package has functions to read various text files. 
# the # "readxl" package has funtions to read excel files, 
# including a specific spreadshhet tab w/in the file

# R-base functions such as "read.cvs" return a data frame.
# tidyverse functions such as "read_cvs" return a tibble.
# Basically a data frame w/o converting character stings to factors,
# Generally prefer tidyverse read functions
