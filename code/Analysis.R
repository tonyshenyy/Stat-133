# ===============================================================
# Data Analysis
# Description: Analyze the number of storms for different years
#              and also find out the relation between pressure
#              and wind speed
# Name: Caylie Marie Connelly and Yiyang Shen
# ===============================================================
library(stringr)
library(dplyr)
library(ggplot2)

# Aggregating Data by Year
storms.csv <- read.csv("./data/storms.csv", stringsAsFactors = FALSE)
tracks.csv <- read.csv("./data/tracks.csv", stringsAsFactors = FALSE)

dates <- as.Date(storms.csv$date, format = "%m/%d/%Y")


# Num. of Storms per Year
table(format(dates, "%Y"))
barplot(table(format(dates, "%Y")), main = "Number of Storms per Year")

# Num. of storms per year with winds >= 35 knots
rplot()
id35 <- unique(tracks.csv$id[tracks.csv$wind >= 35])
table(format(dates[storms.csv$id %in% id35], "%Y"))
barplot(table(format(dates[storms.csv$id %in% id35], "%Y")),
        main = "Number of Storms Per Year With Winds >= 35 Knots")

# Num. of storms per year with winds >= 64 knots
id64 <- unique(tracks.csv$id[tracks.csv$wind >= 64])
table(format(dates[storms.csv$id %in% id64], "%Y"))
barplot(table(format(dates[storms.csv$id %in% id64], "%Y")),
        main = "Number of Storms Per Year With Winds >= 64 Knots")

# Num. of storms per year with winds >= 96 knots
id96 <- unique(tracks.csv$id[tracks.csv$wind >= 96])
table(format(dates[storms.csv$id %in% id96], "%Y"))
barplot(table(format(dates[storms.csv$id %in% id96], "%Y")),
        main = "Number of Storms Per Year With Winds >= 96 Knots")

# Num. of storms per month
table(format(dates, "%B"))


# Num. of storms per month with winds >= 35 knots
table(format(dates[storms.csv$id %in% id35], "%B"))
barplot(table(format(dates[storms.csv$id %in% id35], "%B")),
        main = "Number of Storms per Month with Winds >= 35 Knots")

# Num. of storms per month with winds >= 64 knots
table(format(dates[storms.csv$id %in% id64], "%B"))
barplot(table(format(dates[storms.csv$id %in% id64], "%B")),
        main = "Number of Storms per Month With Winds >= 64 Knots")

# Num. of storms per month with winds >= 96 knots
table(format(dates[storms.csv$id %in% id96], "%B"))
barplot(table(format(dates[storms.csv$id %in% id96], "%B")),
        main = "Number of Storms per Month With Winds >= 96 Knots")

# Avg Number of storms >= 35 knots
storms35 <- as.numeric(table(format(dates[storms.csv$id %in% id35], "%Y")))
summary(storms35)
# There were an average of 9 storms >= 35 knots per year.

# Avg Number of storms >= 64 knots
storms64 <- as.numeric(table(format(dates[storms.csv$id %in% id64], "%Y")))
summary(storms64)
# There were an average of 6 storms >= 64 knots per year.

# Avg Number of storms >= 96 knots
storms96 <- as.numeric(table(format(dates[storms.csv$id %in% id96], "%Y")))
summary(storms96)
# There were an average of 2.76 storms >= 96 knots per year.

# Mean pressure and mean wind for each storm
tracksCopy = tracks.csv
tracksCopy = tracksCopy[tracksCopy$press != 0,]
head(tracksCopy)

meanPress = numeric(length(unique(tracksCopy$id)))
meanWind = numeric(length(unique(tracksCopy$id)))

j <- 1
for (i in unique(tracksCopy$id)) {
  meanPress[j] = mean(tracksCopy$press[tracksCopy$id == i])
  meanWind[j] = mean(tracksCopy$wind[tracksCopy$id == i])
  j <- j + 1
}

lm.mean <- lm(meanWind ~ meanPress)
plot(meanPress, meanWind)
abline(lm.mean)

# median pressure and median wind for each storm
medianPress = numeric(length(unique(tracksCopy$id)))
medianWind = numeric(length(unique(tracksCopy$id)))

j <- 1
for (i in unique(tracksCopy$id)) {
  medianPress[j] = median(tracksCopy$press[tracksCopy$id == i])
  medianWind[j] = median(tracksCopy$wind[tracksCopy$id == i])
  j <- j + 1
}

lm.median <- lm(medianWind ~ medianPress)
plot(medianPress, medianWind)
abline(lm.median)

