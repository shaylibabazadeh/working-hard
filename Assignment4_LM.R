#Hi Leen, everything was very well done! I really liked how you implemented the pipes to better deal with your data 
#all throughout the assignment. It is a very good way to look at different parts of the data and you did a great job 
#narrowing the data both for your analysis purposes and for a reader. 
#I also really liked how you used R functions to look at your data before taking any steps.
#When I was doing my own assignment, I was just manually looking at it but yours is a way more efficient way to do so. 
#Overall, amazing job!


### Leen Madani | Assignment 4
### The version of R used: 4.0.3

#load the dplyr and tidyr package as it will be used moving forward
library("dplyr")
library("tidyr")

# set working directory where the dataset is found to access it
setwd("~/1855-Coding-in-R")


### Q read the dataset into a dataframe ensuring no spaces in column names
# use read.csv to read ufo dataset into a dataframe
# the default in read.csv is check.names=T, where R converts column names that are not
# ..valid variable names (e.g. contain spaces or special characters or start with numbers)
#...into valid variable names, e.g. by replacing spaces with dots like 6th and 7th column
# however, to ensure the col name are valid we use check.names = T to check that column names are valid and modify them if not.
ufo <- read.csv("ufo_subset.csv", check.names = T)

dim(ufo) #check the dimensions of the dataset to learn more about it (26,008 rows and 11 columns)

glimpse(ufo) # will look at the structure of ufo dataset where 8 columns contain characters and 3 contain db1


### Q find rows where Shape information is missing and impute with "unknown"
# first i will want to view the rows with missing shape information to make sure my impute is correct
filter_shape <- filter(ufo, shape == "") # we have 317 out of 26008 observations that are missing shape information

#impute with "unknown" using mutate and case when and assign it to new dataframe ufo1
ufo1 <- ufo %>%
  mutate(shape = case_when(
    shape == "" ~ "unknown",
    .default = shape))

### Q remove the rows that do not have country information using filter()
# first, let me view those rows with no country information to make sure i'm removing them correctly
print(ufo1 %>% filter(country == ""))
# or another way would be same as above if you want to see all the info as dataframe
filter_country <- filter(ufo, country == "")  # 2542 out of 26008 observations do not have country information
# proceed to remove them and assign it to new dataframe ufo2
ufo2 <- ufo1 %>% filter(!(country == "")) # 26008 (total) - 2542 (no country info) = 23466 observations


### Q Convert Datatime and Date_posted columns into appropriate formats
ufo2$datetime <- lubridate::ymd_hm(ufo2$datetime) # ymd_hm will parse a character string that represents datetime with the format "year-month-day hour:minute".
# ymd_hms will parse it into hour minute second but because our dataset doesn't have second, its not used.
ufo2$date_posted <- lubridate::dmy(ufo2$date_posted)  # dmy will parse the character string into date-month-year. ymd is not used becasue data_posted column has ymd format

### Q NUFORC officials comment on sighting that may be hoax. Figoure out a way (go through comments and decide how a proper filter should look like) to identify posisble
### hoax reports. Create a new boolean column "is_haox" and populate this column with TRUE if the sighitng is a possible hoax, FALSE otherwise.

# identify the rows the word hoax in them using regex

# first i want to see what do the rows that contain the word hoax look like
# i decided to put them in a data.frame to go through the observations easier
hoax_rows <- data.frame(ufo2$comments[grepl("hoax", ufo2$comments, ignore.case = TRUE)]) # this will give me any row that has the letters hoax while ignoring the case of the letter.
# row 77 from the 209 observations with the word hoax has the comment: "this is not a hoax". I should deal with this.
# i could use value = T instead the [] to return the actual matching strings

# this will not count any observation that has the phrase "not a hoax".
hoax_rows_proper <- data.frame(ufo2$comments[grepl("hoax", ufo2$comments, ignore.case = TRUE) & !grepl("not a hoax", ufo2$comments, ignore.case = TRUE)])
# i get 208 observations now, which means that the "not a hoax" observation was removed.


# I created a new column and use ifelse to assign TRUE for observations that are possible hoax, otherwise FALSE.
ufo2$is_hoax <- ifelse(
  grepl("hoax", ufo2$comments, ignore.case = TRUE) & !grepl("not a hoax", ufo2$comments, ignore.case = TRUE),
  TRUE,
  FALSE
)


### Q Create a table reporting the percentage of a hoax sightings per country
hoax_row_table <- ufo2 %>%
  filter(is_hoax) %>%                                 # filter is_hoax that are equal to T
  group_by(country) %>%                               # group by country to
  summarise(hoax_percentage = (n()/count(hoax_rows_proper))*100) # divide by the count of hoax_rows_proper, which is equivalent to total of hoax


### Q Add another column to the dataset (report_delay) and populate with the time difference in days,
###   between the date of the sighting and the date it was reported.
ufo3 <- ufo2 %>%
  mutate(report_delay = date_posted - as.Date(datetime)) #mutate() will add another column called time_difference

### Q Remove the rows where the sighting was reported before it happened.
ufo4 <- ufo3 %>%
  filter(!(date_posted <= as.Date(datetime))) # now we have 23275 from 23466 observations
# this mean that 23466 - 23725 = 191 observations have sighting reported before it happened
# because the date of sighting is before the day it was reported as indicated by "<="
# i considered the time difference of zero days to be indicative that sighting was reported
#... in same day because exact time is not given for day reported and its not considered delay
# to double check that the 191 rows removed are indeed the observations where sighting was reported before it happened,
#... i use basic R functions to view rows that have time difference less than or equal than 0 and then count them
ufo3[ufo3$report_delay <= 0,,]
count(ufo3[ufo3$report_delay <= 0,,])

### Q Create a table reporting the average report_delay per country.
average_report_delay_table <- ufo4 %>%
  group_by(country) %>%
  summarise(average_report_day = mean(report_delay))

### Q Check the data quality (missingness, format, range etc) of the "duration seconds" column. Explain what kinds
### ..of problems you have identified and how you chose to deal with them, in your comments.

# checking missingness
class(ufo4$duration.seconds) # numeric class which is what is preferred for a seconds column
sum(is.na(ufo$duration.seconds)) # no missing values
filter_seconds <- filter(ufo4, duration.seconds == "") # double checks there is no missing value or empty cells

# checking range and arranging the column
summary(ufo4$duration.seconds)
ufo5 <- ufo4 %>%
  arrange(duration.seconds) # let's start by arranging this column in ascending order
head(ufo5$duration.seconds) # gives an idea about the minimum values in duration.seconds column
tail(ufo5$duration.seconds) # gives an idea about the max values in duration.seconds column


# checking format

unique_patterns <- ufo5[unique(ufo5$duration.seconds) == T ,,] # will give an idea about some unique patterns observed and here we can see decimal points
decimal_points <- ufo5[grep("[.]", ufo5$duration.seconds),,] # get an idea of how many observations have decimals for the duration.seconds using regex
# i've noticed that decimal_points variable only has observation with decimal places
# for example, it doesn't count 1.0 as a number with decimal point
# R considers 1.0 and 1 the same, so no need to adjust anything
### Q Create a histogram using the "duration seconds" column.
hist(log(ufo4$duration.seconds),             # to improve the visuals of the histogram and see data more clearly, do a log transformation.
     main = "Duration Seconds Histogram",
     xlab = "Duration(s)",
     ylab = "Frequency",
     breaks = 20)
