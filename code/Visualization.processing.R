# ===============================================================
# Data Cleaning and Preprocessing (For Visualization)
# Description: 
# Name: Caylie Marie Connelly and Yiyang Shen
# ===============================================================

library(stringr)

# Cleaning Data
EP.data <- read.csv('./rawdata/Basin.EP.ibtracs_wmo.v03r06.csv', header = FALSE,
                    stringsAsFactors = FALSE)
NA.data <- read.csv('./rawdata/Basin.NA.ibtracs_wmo.v03r06.csv', header = FALSE,
                    stringsAsFactors = FALSE)


# Extracting Years 1980-2010
  # Finding Start/End Points of Data
head(EP.data[EP.data$V2 == 1980,]) # First storm in 1980 is in row 7844
tail(EP.data[EP.data$V2 == 2010,]) # Last storm in 2010 is in row 24012

head(NA.data[NA.data$V2 == 1980,]) # First storm in 1980 is in row 32887
tail(NA.data[NA.data$V2 == 2010,]) # Last storm in 2010 is in row 46230

  # Extracting Rows
EP.data <- EP.data[c(7844:24012),]
NA.data <- NA.data[c(32887:46230),]

  # Creating clean data frame
EP.df <- data.frame(
  id = EP.data$V1,
  day.time = EP.data$V7,
  lat = EP.data$V9,
  long = EP.data$V10,
  wind = EP.data$V11,
  press = EP.data$V12,
  stringsAsFactors = FALSE
)

NA.df <- data.frame(
  id = NA.data$V1,
  day.time = NA.data$V7,
  lat = NA.data$V9,
  long = NA.data$V10,
  wind = NA.data$V11,
  press = NA.data$V12,
  stringsAsFactors = FALSE
)


  # Separating Date and Time
dates.EP <- str_sub(EP.df$day.time, start = 1, end = 10)
dates.NA <- str_sub(NA.df$day.time, start = 1, end = 10)
EP.df$day <- dates.EP
NA.df$day <- dates.NA


  # Creating Year, Month Columns
years.EP <- str_sub(EP.df$day, start = 1, end = 4)
years.NA <- str_sub(NA.df$day, start = 1, end = 4)
EP.df$year <- years.EP
NA.df$year <- years.NA

with.month.EP <- str_sub(EP.df$day, start = 1, end = 7)
with.month.NA <- str_sub(NA.df$day, start = 1, end = 7) # This will make it easy to aggregate by month

all.storms <- rbind(EP.df, NA.df)

# Function to Create Graphs
library(maps)
library(ggplot2)
all.storms$long <- as.numeric(all.storms$long)
all.storms$lat <- as.numeric(all.storms$lat)
all.storms$month <- as.numeric(str_sub(all.storms$day, start = 6, end = 7))
all.storms$year <- as.numeric(all.storms$year)
lats <- c(4,73)
longs <- c(-180,0)  # Not the same as the maxes and mins in order to get smaller map
map('world', xlim = longs, ylim = lats, fill = TRUE, col = "#4db8d6", bg = "#0000ffaa")
title(main = list("Storms of All Years", col = "#ffff00"))
with(all.storms,
     points(x = as.numeric(long), y = as.numeric(lat), pch = '.', col = "#2288bb"))

ggplot(data = all.storms, aes(x = long, y = lat)) + 
  geom_point(col = "#ffff0080") +
  facet_wrap(~ month) +
  ggtitle("Storms By Month")

storms80s <- all.storms[all.storms$year >= 1980 & all.storms$year <= 1989, ]
storms90s <- all.storms[all.storms$year >= 1990 & all.storms$year <= 1999, ]
storms20s <- all.storms[all.storms$year >= 1999 & all.storms$year <= 2010, ]

ggplot(data = storms80s, aes(x = long, y = lat)) + 
  geom_point(col = "#ffff0080") +
  facet_wrap(~ year) +
  ggtitle("Storms in 1980s")

ggplot(data = storms90s, aes(x = long, y = lat)) + 
  geom_point(col = "#ffff0080") +
  facet_wrap(~ year) +
  ggtitle("Storms in 1990s")

ggplot(data = storms20s, aes(x = long, y = lat)) + 
  geom_point(col = "#ffff0080") +
  facet_wrap(~ year) +
  ggtitle("Storms in 2000s")
