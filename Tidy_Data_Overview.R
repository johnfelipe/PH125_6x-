# load tidyverse library

library(tidyverse)

# downloading a file to practice on. See my notes in Data_Import.R
# locating the external data file directory "extdata" in dslabs and assigning the path to a variable
path <- system.file("extdata", package = "dslabs")
path

# listing the files in "extdata"
list.files(path)

# copying the practicefile into the working directory. 
# In this case I created a raw data folder in the project and set the working 
# directory to that folder. I did all that inside RStudio

# creating some variables to use in the function calls
filename <- "fertility-two-countries-example.csv"

# the file.path function gets the full path in the correct form
fullpath <- file.path(path, filename)
fullpath

# file.copy copies the file. In this case into the raw data folder. 
# IMPORTANT the working directory must be pointed at where you want the file to be 
# to be copied
file.copy(fullpath, getwd())

# read and assign the file to a new object so I can practice tidying it.
# note read_csv produces a tibble

wide_data <- read_csv("fertility-two-countries-example.csv")

# inspecting wide_data

select(wide_data, '1960':'1967')

# each year is a variable. Instead, we want create a new varible "year" that gathers the years.
# Also convert the years to numeric. Use the gather() function.
# the -country excludes it from the gather() function

new_tidy_data <- wide_data %>% 
                  gather(year, fertility, -country, convert = TRUE)

# just running a plot and saving it to the plot folder
# I set to working directory to the plot folder. Need to make sure to set the
# working directory back to the project folder

new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point() +
  ggsave("fertility.png")




