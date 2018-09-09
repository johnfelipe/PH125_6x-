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

head(new_tidy_data)

# just running a plot and saving it to the plot folder
# I set to working directory to the plot folder. Need to make sure to set the
# working directory back to the project folder

new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point() +
  ggsave("fertility.png")

# now I'm going to use the spread() function to reverse the gather()
#

new_wide_data <- new_tidy_data %>% 
                    spread(year, fertility)

head(new_wide_data)

# downloading and reading another data file for processing 
# to the projects raw_data" folder
# remember to set the working directory to the raw data folder

path <- system.file("extdata", package = "dslabs")
list.files(path)
fullpath <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
file.copy(fullpath, getwd())

# reading the file to a new tibble

wide_data_2 <- read_csv("life-expectancy-and-fertility-two-countries-example.csv") 

# inspecting the tibble

select(wide_data_2, 1:5)

# columns are variables that combine year and fertility AND year and life expectancy
# starting to wrangle. 
# step 1 First, place the column headers into a single column as variables
# excluding the country

dat <- wide_data_2 %>% 
        gather(key,value, - country)

head(dat)

# step 2  using the separate() to separte the key into 2 parts
# note since key is of the form year_life_expentance
# use extra = "merge" 

dat_2 <- dat %>% 
  separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

head(dat_2)

# now use the spread() function to make columns for fertility and life_expectancey

new_tidy_data_2 <- dat_2 %>% 
                    spread(variable_name, value)
head(new_tidy_data_2)