# ===============================================================
# Final Project
# Name: Caylie Marie Connelly and Yiyang Shen
# ===============================================================

library(MASS)
library(stringr)

# ==============================================
# Creating File storms.csv
# ==============================================

EP.Basin <- readLines('./rawdata/Basin.EP.ibtracs_hurdat.v03r06.hdat')
NA.Basin <- readLines('./rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat')

# Extracting the Headers from the Data
EP.headers <- grep("[0-9]{2}/[0-9]{2}/[0-9]", EP.Basin, value = TRUE)
NA.headers <- grep("[0-9]{2}/[0-9]{2}/[0-9]", NA.Basin, value = TRUE)

# Extracting Length of Storm
EP.ID <- str_extract(EP.headers, "[0-9]{5}")
NA.ID <- str_extract(NA.headers, "[0-9]{5}")

count.days.EP <- c(rep("", length(EP.ID)))
count.days.NA <- c(rep("", length(NA.ID)))
count.days <- function(x, y) {
  z <- as.numeric(y)
  for (i in 1:length(y)-1) {
    days <- (z[i + 1] - z[i] - 2)
    x[i] <- days
  }
  x
}

EP.days <- cbind(EP.headers, count.days(count.days.EP, EP.ID))
NA.days <- cbind(NA.headers, count.days(count.days.NA, NA.ID))
EP.days[length(EP.days)] <- 4 
NA.days[length(NA.days)] <- 5
# The function I made only calculates the first n-1 lengths,
# so I am entering the final length manually.

# Extracting the Date Information
EP.date <- str_sub(EP.headers, start = 7, end = 16)
NA.date <- str_sub(NA.headers, start = 7, end = 16)

# Re-formatting Date information
EP.Date <- as.Date(EP.date, format = "%m/%d/%Y")
NA.Date <- as.Date(NA.date, format = "%m/%d/%Y")
EP.Date <- strftime(EP.Date, "%m/%d/%Y")
NA.Date <- strftime(NA.Date, "%m/%d/%Y")

# Attaching Dates to Headers
EP.by.Date <- data.frame(Header = EP.days[,1], Date = EP.Date, Days = EP.days[,2])
NA.by.Date <- data.frame(Header = NA.days[,1], Date = NA.Date, Days = NA.days[,2])

# Ordering all Storm Data by Date
all.storms <- rbind(EP.by.Date, NA.by.Date)
storms.df <- all.storms[order(as.Date(all.storms$Date, "%m/%d/%Y")),]

# Creating Storm IDs
ID <- c(1:nrow(storms.df))
storms.df$ID <- ID

# Extracting the Storm Names
storm.names <- str_sub(storms.df$Header,
                           start = 36, end = 47)
storms.df$Names <- str_trim(storm.names)

# Writing the CSV File
clean.df <- data.frame(id = storms.df$ID, date = storms.df$Date,
                       days = storms.df$Days, name = storms.df$Names)
write.csv(clean.df, "./storms.csv")






# =================================
# Creating the File tracks.csv
# =================================
# Extracting Daily Data
EP.daily.data <- grep("[0-9]/[0-9][0-9][*SEWL]", EP.Basin, value = TRUE)
NA.daily.data <- grep("[0-9]/[0-9][0-9][*SEWL]", NA.Basin, value = TRUE)

# Extracting Dates
empty.date.list.EP <- list()
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
# EP Basin
list.days.EP <- duplicate.date(EP.date, EP.days[,2], empty.date.list.EP)
vector.days.EP <- as.Date(unlist(list.days.EP), origin = "1970-01-01")
days.EP <- strftime(vector.days.EP, "%m/%d/%Y")
days.rep.EP <- rep(days.EP, each = 4)

# NA Basin
list.days.NA <- duplicate.date(NA.date, NA.days[,2], empty.date.list.NA)
vector.days.NA <- as.Date(unlist(list.days.NA), origin = "1970-01-01")
days.NA <- strftime(vector.days.NA, "%m/%d/%Y")
days.rep.NA <- rep(days.NA, each = 4)


# Creating Period Vector
length(days.rep)/4
periods <- c("00h", "06h", "12h", "18h")
period.EP <- rep(periods, length.out = length(days.rep.EP))
period.NA <- rep(periods, length.out = length(days.rep.NA))

# Extracting Observation Data by Hour
# 00h
EP.00h.obs <- str_sub(EP.daily.data, start = 12, end = 28)
NA.00h.obs <- str_sub(NA.daily.data, start = 12, end = 28)
# 06h
EP.06h.obs <- str_sub(EP.daily.data, start = 29, end = 45)
NA.06h.obs <- str_sub(NA.daily.data, start = 29, end = 45)
# 12h
EP.12h.obs <- str_sub(EP.daily.data, start = 46, end = 62)
NA.12h.obs <- str_sub(NA.daily.data, start = 46, end = 62)
# 18h
EP.18h.obs <- str_sub(EP.daily.data, start = 63, end = 79)
NA.18h.obs <- str_sub(NA.daily.data, start = 63, end = 79)


# Ordering Hourly Observations
obs.by.time <- c(rep("", length(period.NA))) # Used length(period.EP) for EP Basin
obs.placement <- function(p, q, r, s) {
  times <- c(0:(length(p)-1))
  for(i in 1:length(p)) {
    obs.by.time[1 + 4*times[i]] <- p[i]
    obs.by.time[2 + 4*times[i]] <- q[i]
    obs.by.time[3 + 4*times[i]] <- r[i]
    obs.by.time[4*(times[i] + 1)] <- s[i]
  }
  obs.by.time
}



EP.obs <- obs.placement(EP.00h.obs, EP.06h.obs,
                        EP.12h.obs, EP.18h.obs)
NA.obs <- obs.placement(NA.00h.obs, NA.06h.obs,
                        NA.12h.obs, NA.18h.obs)

# Appending Observations, Sorted by Date
EP.obs.df <- data.frame(Date = days.rep.EP, Obs = EP.obs, Period = period.EP)
NA.obs.df <- data.frame(Date = days.rep.NA, Obs = NA.obs, Period = period.NA)

all.tracks <- rbind(EP.obs.df, NA.obs.df)
tracks.df <- all.tracks[order(as.Date(all.tracks$Date, "%m/%d/%Y")),]


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
press <- str_sub(tracks.df$Obs, start = 13, end = 18)
trim.press <- as.numeric(str_trim(press))

tracks.df$Pressure <- trim.press


# Changing 0 Values to NA
tracks.df$Lat[tracks.df$Lat == 0] <- NA
tracks.df$Long[tracks.df$Long == 0] <- NA
tracks.df$Wind[tracks.df$Wind == 0] <- NA
tracks.df$Pressure[tracks.df$Pressure == 0] <- NA

