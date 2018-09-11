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

# using bind_cols() and

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

# practicing joins with subsets/slices of two tables from the dslabs package
#where the table do NOT have matching state columns
library(dslabs)
data(murders)
data(polls_us_election_2016)

tab1 <- as.tibble(slice(murders, 1:6) %>%
  select(state, population))
tab1

tab2 <- as.tibble(slice(results_us_election_2016, c(1:3,5,7:8)) %>% 
  select(state, electoral_votes))
tab2

# left_join. In this example tab1 is kept and matches in tab2 adds electoral_votes #or NAs where there is no matching state in tbb2
# both table have a "state" column. I'm forcing these as tibbles
left_join(tab1,tab2)

# left_join using %>% 
tab1 %>% 
  left_join(tab2)

# right_join example, keeps tab2 and adds population or NAs
tab1 %>% 
  right_join(tab2)

# could have used a left_join, column order is switched
tab2 %>%
  left_join(tab1)

# inner_join keeps the intersection of the 2 tabs. Could have used %>% 
inner_join(tab1,tab2)
tab1 %>% 
  inner_join(tab2)

# full_join is a union of the 2 tabs, keeps all the rows in both tabs and
# fills in the missing values w/ NA
full_join(tab1,tab2)

tab1 %>% 
  full_join(tab2)

# semi_join subsets tab1 based on matches in tab2. 
# The tab2 columns are NOT added

semi_join(tab1,tab2)

# anti_join subsets tab1 where there are NO matches in tab2

anti_join(tab1,tab2)

# using bind_cols() and bind_rows() to create tibbles
# making some subsets to practice with
# note that I could have used select() to choose the columns
tab <- as.tibble(results_us_election_2016)
head(tab)
tab1 <- tab[,1:2]
tab1
tab2 <- tab[,3:4]
tab2
tab3 <- tab[,5]
tab3

# using bind_cols() to reassemble tab
new_tab <- bind_cols(tab1,tab2,tab3)
head(new_tab)

# now using bind_rows to create a tibble
# creating some subsets to practice with using the slice() function

tab1 <- tab %>%
  slice(1:2)
tab1
tab2 <- tab %>%
  slice(3:6)
tab2

# using bind_rows() to reassemble tab
new_tab <- bind_rows(tab1,tab2)
new_tab

# set operations practice on data frames - tidyverse must be loaded to use
# on tables

#intersect() on vectors example

intersect(1:10, 6:16)

# intersect() on 2 tables, must have the same column names, returns the rows
# in common
# making 2 pactice tabls w/ overlapping rows

tab1 <- tab %>%
  slice(1:4)
tab1
tab2 <- tab %>% 
  slice(3:6)
tab2

# intersect() of tab1 and tab2
intersect(tab1, tab2)

# union() on vectors
union(1:10, 6:16)

# union() of tab1 and tab2. Note there are no duplicates
# also row ordering is NOT preserved
union(tab1,tab2)

# setdiff() on vectors. Note that the order of arguements is important
# the function is NOT symetric

setdiff(1:10, 6:16)
setdiff(6:16, 1:10)

# setdiff() on tab1 and tab2 returns rows that are in tab1 BUT NOT in tab2
setdiff(tab1,tab2)

# setdiff() on tab2 and tab1 returns row that are in tab2 BUT NOT tab1
setdiff(tab2,tab1)

# setequal() checks if the sets are equal reguardless of order

# setequal() on vectors, the first return FALSE, the second returns TRUE
setequal(1:5,1:6)
setequal(1:5,5:1)

# setequal() on tab1 and tab2 returns rows that are missing from the other table
# usefull for checking for missing rows across tables
tab1
tab2
setequal(tab1,tab2)

# web scaping example and practice
# load the library rvest
library(rvest)

# copy/pasted the url from the eb site

url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"

# read the url using read_html and assign to an object 
#and check the object's class

h <- read_html(url)
class(h)

# extract the "table" nodes using html_nodes() and assign to an object tab
tab <- h %>% 
  html_nodes("table")

# inspect the nodes in tab, based on the inspection we want the 2nd table node 
tab

# assign just the 2nd table node to tab and inspect again

tab <- tab[[2]]
tab
# convert the HTML table to a data frame (I made it a tibble) using the rvest function html_table
tab <- as.tibble(tab %>%
  html_table)

# check the tibble, needs soma data wrangling!
head(tab)

# change the column names to make them shorter
tab <- tab %>%
  setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# another test of web scaping

url2 <- "https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate"
h2 <- read_html(url2)
tab2 <- h2 %>% 
  html_nodes("table")
# inspect tab2 to pick the right data table node
tab2
# it's node [5] 
tab2 <- tab2[[5]]
tab2
# convert the HTML table to a data frame (I made it a tibble) using the rvest function html_table
tab2 <- as.tibble(tab2 %>%
                   html_table)
# check the results. Got the data - needs wrangling!
head(tab2)

s <- '5\'10"'
s
cat(s)

# Part 1 string processing  using parse_number() to remove extra characters and convert columns to numeric
head(tab)

tab_new <- tab %>%
  mutate_at(c(2:9), parse_number)

tab_new

# Part 2 string processing, looking for patterns using regular expressions (regex)

# simple example of searchin for a "," in the murders tipple "tab" total column
pattern <- ","
str_detect(tab$total, pattern)

# another example for the pattern "[a]"
str_subset(tab$murders, "[a]")

# example using \\d to seach for a digit and the or "|" operator
# Do NOT include spaces before after the "|" unless you intend to check
# for a space in the pattern. Better to use "\\s" to check for spaces
# a test vector for the example

s <- c("70", "5 ft", "4' 11\""," ", "5'10\"",".", "Six feet")

# a pattern searching for a digit or "feet.
# note the output goes to the Viewer not the Console
# NOTE 2 - need to load the "htmlwidgets" package
pattern <- "\\d|feet"
str_view_all(s, pattern)



# more pattern searching using character classes
# the characters are enclosed in square brackets, e.g. [ab] or [4-6]

str_detect(s,"[f]")
str_view_all(s,"[4-6]")

# "\\s" will find a space, using "\\s*" will find 0 or more spaces. 
# The "*" allows repeat matching 0 or more or the character that preceeds it.
# In this case a space \\s

pattern <- "^[4-5]'\\s*\\d{1,2}\"$"
str_view_all(s, pattern)

# find and replace practice

schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools

test2 <- schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
  
test2

# more pattern matching and replacing
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
problems

pattern_w_groups <- "^([4-7])[,\\.](\\d*)$"
str_match(problems, pattern_w_groups)

pattern_w_groups2 <- "^([4-7])[,\\.\\s](\\d*)$"
str_match(problems, pattern_w_groups2)
