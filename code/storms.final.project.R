# ===============================================================
# Data Cleaning and Preprocessing
# Description: Clean the raw data from source files and also
#              export these data to csv files
# Name: Caylie Marie Connelly and Yiyang Shen
# ===============================================================

library(MASS)
library(stringr)

# ==============================================
# Creating File storms.csv
# ==============================================
NA.Basin <- readLines('./rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat')

# Extracting the Headers from the Data
NA.headers <- grep("[0-9]{2}/[0-9]{2}/[0-9]{4}", NA.Basin, value = TRUE)

# Extracting Length of Storm
NA.ID <- str_extract(NA.headers, "[0-9]{5}")

count.days.NA <- c(rep("", length(NA.ID)))
count.days <- function(x, y) {
  z <- as.numeric(y)
  for (i in 1:length(y)-1) {
    days <- (z[i + 1] - z[i] - 2)
    x[i] <- days
  }
  x
}

NA.days <- cbind(NA.headers, count.days(count.days.NA, NA.ID))
NA.days[length(NA.days)] <- 5
# The function I made only calculates the first n-1 lengths,
# so I am entering the final length manually.

# Extracting the Date Information
NA.date <- str_sub(NA.headers, start = 7, end = 16)

# Re-formatting Date information
NA.Date <- as.Date(NA.date, format = "%m/%d/%Y")
NA.Date <- strftime(NA.Date, format = "%m/%d/%Y")

# Attaching Dates to Headers
storms.df <- data.frame(Header = NA.days[,1], Date = NA.Date, Days = NA.days[,2],
                        stringsAsFactors = FALSE)

# Creating Storm IDs
ID <- c(1:nrow(storms.df))
storms.df$ID <- ID

# Extracting the Storm Names
storm.names <- str_sub(storms.df$Header,
                           start = 36, end = 47)
storms.df$Names <- str_trim(storm.names)

# Writing the CSV File
storms <- data.frame(id = storms.df$ID, date = storms.df$Date,
                       days = storms.df$Days, name = storms.df$Names)
write.csv(storms, "./data/storms.csv")





# =================================
# Creating the File tracks.csv
# =================================
# Extracting Daily Data
NA.daily.data <- grep("[0-9]/[0-9][0-9][*SEWL]", NA.Basin, value = TRUE)


# Creating ID's's
length.ids <- 4*as.numeric(storms.df$Days)
tracks.ids <- rep(1:1777, times = length.ids)


# Extracting Dates
empty.date.list.NA <- list()
duplicate.date <- function(dates, days, empty.list) {
  dates.vec <- as.Date(dates, format = "%m/%d/%Y")
  num.days <- as.numeric(days)
  for(i in 1:length(dates)) {
    empty.list[[i]] <- seq(dates.vec[i], by = 1, length.out = num.days[i])
  }
  empty.list
}


# Duplicating Days (4 Daily Measurements)
# NA Basin
list.days.NA <- duplicate.date(NA.date, NA.days[,2], empty.date.list.NA)
vector.days.NA <- as.Date(unlist(list.days.NA), origin = "1970-01-01")
days.NA <- strftime(vector.days.NA, "%m/%d/%Y")
days.rep.NA <- rep(days.NA, each = 4)


# Creating Period Vector
length(days.rep.NA)/4
periods <- c("00h", "06h", "12h", "18h")
period.NA <- rep(periods, length.out = length(days.rep.NA))


# Extracting Observation Data by Hour
# 00h
NA.00h.obs <- str_sub(NA.daily.data, start = 12, end = 28)
# 06h
NA.06h.obs <- str_sub(NA.daily.data, start = 29, end = 45)
# 12h
NA.12h.obs <- str_sub(NA.daily.data, start = 46, end = 62)
# 18h
NA.18h.obs <- str_sub(NA.daily.data, start = 63, end = 79)


# Ordering Hourly Observations
obs.by.time.NA <- c(rep("", length(period.NA)))
obs.placement <- function(p, q, r, s, t) {
  times <- c(0:(length(p)-1))
  for(i in 1:length(p)) {
    t[1 + 4*times[i]] <- p[i]
    t[2 + 4*times[i]] <- q[i]
    t[3 + 4*times[i]] <- r[i]
    t[4*(times[i] + 1)] <- s[i]
  }
  t
}

NA.obs <- obs.placement(NA.00h.obs, NA.06h.obs,
                        NA.12h.obs, NA.18h.obs,
                        obs.by.time.NA)

# Appending Observations, Sorted by Date
NA.obs.df <- data.frame(Date = days.rep.NA, Obs = NA.obs, Period = period.NA)
tracks.df <- NA.obs.df[order(as.Date(NA.obs.df$Date, "%m/%d/%Y")),]
tracks.df$ID <- tracks.ids


# Converting Symbol Notation to Storm Stage
stages <- str_sub(tracks.df$Obs, start = 1, end = 1)
stage.fac <- as.factor(stages)
levels(stage.fac) # The existing levels (storm stage symbols) are *, E, and S.
stage.names <- c("cyclone", "extratropical", "subtropical")
levels(stage.fac) <- stage.names

tracks.df$Stage <- stage.fac


# Extracting Latitude and Longitude Data
lat <- str_sub(tracks.df$Obs, start = 2, end = 4)
long <- str_sub(tracks.df$Obs, start = 5, end = 8)
num.lat <- as.numeric(lat)/10
num.long.pos <- as.numeric(long)/10
num.long.neg <- num.long.pos - 360

tracks.df$Lat <- num.lat
tracks.df$Long <- num.long.neg


# Extracting Wind Speed
speed <- str_sub(tracks.df$Obs, start = 9, end = 12)
trim.speed <- as.numeric(str_trim(speed))

tracks.df$Wind <- trim.speed


# Extracting Pressure
press <- str_sub(tracks.df$Obs, start = 14, end = 18)
trim.press <- as.numeric(str_trim(press))

tracks.df$Pressure <- trim.press


# Removing Rows with All NA values
tracks.no.na.df <- tracks.df[tracks.df$Obs != "*0000000   0    0", ]

clean.tracks <- data.frame(id = tracks.no.na.df$ID,
                           date = tracks.no.na.df$Date,
                           period = tracks.no.na.df$Period,
                           stage = tracks.no.na.df$Stage,
                           lat = tracks.no.na.df$Lat,
                           long = tracks.no.na.df$Long,
                           wind = tracks.no.na.df$Wind,
                           press = tracks.no.na.df$Pressure
                           )

write.csv(clean.tracks, "./data/tracks.csv")
