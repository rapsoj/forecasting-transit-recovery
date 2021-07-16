#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop/Forecasting Transit Recovery")

# Load in data manipulation libraries
library(readxl)
library(data.table)
library(plyr)
library(dplyr)
library(stringr)

# Load in data visualization libraries
library(ggplot2)
library(corrplot)
library(extrafont)
loadfonts()

# Load in linear model libraries
library(stats)
library(skedastic)
library(olsrr)
library(car)
library(nlme)
library(lmtest)
library(strucchange)

# Load in time series libraries
library(forecast)
library(tseries)
library(aTSA)
library(DescTools)

# Read in most recent data
df_agencies <- read_excel("Covid Public.xlsx", sheet = "Agencies Daily")
df_metro <- read_excel("Covid Public.xlsx", sheet = "Metro Areas Daily")

# Set colours
orange <- "#F58024"


#### CLEAN OUTPUT DATA ####

# Select metro areas of interest in agencies data
df_toronto <- df_agencies[df_agencies$Municipality == "Toronto" & df_agencies$Country == "Canada", ]
df_montreal <- df_agencies[df_agencies$Municipality == "Montréal" & df_agencies$Country == "Canada", ]
df_vancouver <- df_agencies[df_agencies$Municipality == "Vancouver" & df_agencies$Country == "Canada", ]

# Remove excess columns in agencies data
df_toronto <- df_toronto[ , !names(df_toronto) %in% c("Municipality","Country", "State")]
df_montreal <- df_montreal[ , !names(df_montreal) %in% c("Municipality","Country", "State")]
df_vancouver <- df_vancouver[ , !names(df_vancouver) %in% c("Municipality","Country", "State")]

# Rename metro area column in metro areas data
names(df_metro)[names(df_metro) == "Metro Area"] <- "Name"

# Combine transit data for each city
df_toronto <- rbind(df_toronto, df_metro[df_metro$Name == "Toronto" & !is.na(df_metro$Name), ])
df_montreal <- rbind(df_montreal, df_metro[df_metro$Name == "Montréal" & !is.na(df_metro$Name), ])
df_vancouver <- rbind(df_vancouver, df_metro[df_metro$Name == "Vancouver" & !is.na(df_metro$Name), ])

# Transpose rows and columns
df_toronto <- t(df_toronto)
df_montreal <- t(df_montreal)
df_vancouver <- t(df_vancouver)

# Fix column names
colnames(df_toronto) <- df_toronto[1,]
colnames(df_montreal) <- df_montreal[1,]
colnames(df_vancouver) <- df_vancouver[1,]

# Delete excess first row
df_toronto <- as.data.frame(df_toronto[-1,])
df_montreal <- as.data.frame(df_montreal[-1,])
df_vancouver <- as.data.frame(df_vancouver[-1,])

# Reset row names
df_toronto <- setDT(df_toronto, keep.rownames = TRUE)[]
df_montreal <- setDT(df_montreal, keep.rownames = TRUE)[]
df_vancouver <- setDT(df_vancouver, keep.rownames = TRUE)[]

# Rename timestamps
names(df_toronto)[names(df_toronto) == "rn"] <- "Date"
names(df_montreal)[names(df_montreal) == "rn"] <- "Date"
names(df_vancouver)[names(df_vancouver) == "rn"] <- "Date"

# Format timestamps as date
df_toronto$Date <- as.Date(substr(df_toronto$Date, start = 1, stop = 10))
df_montreal$Date <- as.Date(substr(df_montreal$Date, start = 1, stop = 10))
df_vancouver$Date <- as.Date(substr(df_vancouver$Date, start = 1, stop = 10))

# Format data as a dataframe
setDF(df_toronto)

# Format values as numeric
df_toronto[, -1] <- lapply(df_toronto[, -1], as.character)
df_toronto[, -1] <- lapply(df_toronto[, -1], as.numeric)


#### PERFORM FEATURE ENGINEERING ####

# Read in COVID-19 measures data and format timestamps as date
df_measures <- read.csv("measures.csv")
df_measures$Start.Date <- as.Date(df_measures$Start.Date)
df_measures$End.Date <- as.Date(df_measures$End.Date)

# Create new dataframe with dates
df_dates <- as.data.frame(df_toronto$Date)
names(df_dates)[names(df_dates) == "df_toronto$Date"] <- "Date"

# Replace nominal end date with current date to avoid errors
df_measures$End.Date[is.na(df_measures$End.Date)] <- Sys.Date()

# Match state of emergencies to dates
category <- "State of Emergency"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"State of Emergency" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"State of Emergency", value), "NA "),
           as.character(df_dates$"State of Emergency"))
}

# Match school closures to dates
category <- "School Closures"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"School Closures" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"School Closures", value), "NA "),
           as.character(df_dates$"School Closures"))
}

# Match gathering restrictions to dates
category <- "Gathering Restrictions"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Gathering Restrictions" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Gathering Restrictions", value), "NA "),
           as.character(df_dates$"Gathering Restrictions"))
}

# Match travel restrictions to dates
category <- "Travel Restrictions"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Travel Restrictions" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Travel Restrictions", value), "NA "),
           as.character(df_dates$"Travel Restrictions"))
}

# Match business closures to dates
category <- "Business Closures"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Business Closures" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Business Closures", value), "NA "),
           as.character(df_dates$"Business Closures"))
}

# Match lockdowns to dates
category <- "Lockdowns"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Lockdowns" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Lockdowns", value), "NA "),
           as.character(df_dates$"Lockdowns"))
}

# Match park closures to dates
category <- "Park Closures"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Park Closures" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Park Closures", value), "NA "),
           as.character(df_dates$"Park Closures"))
}

# Match mandatory masks to dates
category <- "Mandatory Masks"
df_dates[, category] <- vector()
df_category <- df_measures[df_measures$Category == category, ]

for(i in  1:nrow(df_category)) {
  start <- df_category[i, "Start.Date"]
  end <- df_category[i, "End.Date"]
  value <- as.character(df_category[i, "Measure"])
  df_dates$"Mandatory Masks" <-
    ifelse(df_dates$Date >= start & df_dates$Date < end,
           str_remove(paste(df_dates$"Mandatory Masks", value), "NA "),
           as.character(df_dates$"Mandatory Masks"))
}

# Code state of emergencies
col = df_dates$"State of Emergency"
df_dates$"State of Emergency" <-
  ifelse(is.na(col), 0, col)
df_dates$"State of Emergency" <-
  ifelse(grepl("Province", col), 1, 0)

# Code school closures
col = df_dates$"School Closures"
df_dates$"School Closures" <-
  ifelse(is.na(col), 0, 1)

# Code gathering restrictions
col = df_dates$"Gathering Restrictions"
df_dates$"Gathering Restrictions" <-
  ifelse(is.na(col), 0, col)
df_dates$"Gathering Restrictions" <-
  ifelse(grepl(" 250 | 50 ", col), 1, df_dates$"Gathering Restrictions")
df_dates$"Gathering Restrictions" <-
  ifelse(grepl(" 10 ", col), 2, df_dates$"Gathering Restrictions")
df_dates$"Gathering Restrictions" <-
  ifelse(grepl(" 5 |all", col), 3, df_dates$"Gathering Restrictions")

# Code travel restrictions
col = df_dates$"Travel Restrictions"
df_dates$"Travel Restrictions" <-
  ifelse(is.na(col), 0, 1)

# Code business closures
col = df_dates$"Business Closures"
df_dates$"Business Closures" <-
  ifelse(is.na(col), 0, col)
df_dates$"Business Closures" <-
  ifelse(grepl("Stage 3", col) & !grepl("prohibited", col), 1, df_dates$"Business Closures")
df_dates$"Business Closures" <-
  ifelse(grepl("Stage 2|prohibited|red", col), 2, df_dates$"Business Closures")
df_dates$"Business Closures" <-
  ifelse(grepl("closure|gray", col), 3, df_dates$"Business Closures")

# Code lockdowns
col = df_dates$"Lockdowns"
df_dates$"Lockdowns" <-
  ifelse(is.na(col), 0, col)
df_dates$"Lockdowns" <-
  ifelse(grepl("Province|City", col), 1, df_dates$"Lockdowns")
df_dates$"Lockdowns" <-
  ifelse(grepl("Province", col) & grepl("City", col), 2, df_dates$"Lockdowns")
df_dates$"Lockdowns" <-
  ifelse(grepl("Stay-at-home", col), 3, df_dates$"Lockdowns")

# Code park closures
col = df_dates$"Park Closures"
df_dates$"Park Closures" <-
  ifelse(is.na(col), 0, 1)

# Code mandatory masks
col = df_dates$"Mandatory Masks"
df_dates$"Mandatory Masks" <-
  ifelse(is.na(col), 0, col)
df_dates$"Mandatory Masks" <-
  ifelse(grepl("Province|City", col), 1, df_dates$"Mandatory Masks")
df_dates$"Mandatory Masks" <-
  ifelse(grepl("Province", col) & grepl("City", col), 2, df_dates$"Mandatory Masks")

# Merge coded measures with output data
df_toronto <- merge(df_toronto, df_dates, all.x = TRUE)

# Read in unemployment rates
df_unemploy <- read.csv("unemployment.csv")

# Average unemployment rates for 2017-2019
df_2017 <- df_unemploy[1:12, -1]
df_2018 <- df_unemploy[13:24, -1]
df_2019 <- df_unemploy[25:36, -1]
avg <- (df_2017 + df_2018 + df_2019)/3

# Add month column to average
avg$Date <- format(as.Date(df_unemploy[1:12, 1]), "%m")

# Calculate difference from 2017-2019 average
for(i in  2:ncol(df_unemploy)) {
  df_unemploy[, i] <-
  avg[, i - 1][match(as.matrix(format(as.Date(df_unemploy$Date), "%m")),
                     avg$Date)] - df_unemploy[, i]
}

# Merge unemployment rates with output data
dates <- df_toronto$Date
df_unemploy$Date <- format(as.Date(df_unemploy$Date), "%Y-%m")
df_toronto <- 
  join(transform(df_toronto, Date = format(as.Date(Date), "%Y-%m")), df_unemploy, by="Date")
df_toronto$Date <- dates

# Read in COVID-19 case rates
df_episode <- read.csv("cases_episode.csv")
df_reported <- read.csv("cases_reported.csv")

# Replace missing values with zeroes
df_episode[is.na(df_episode)] <- 0
df_reported[is.na(df_reported)] <- 0

# Rename date column
colnames(df_episode)[1] <- "Date"
colnames(df_reported)[1] <- "Date"

# Format date column as date
df_episode$Date <- as.Date(df_episode$Date)
df_reported$Date <- as.Date(df_reported$Date)

# Count total cases
df_episode$"Total Episode" <-
  df_episode$Recovered + df_episode$Active + df_episode$Deceased
df_reported$"Total Reported" <-
  df_reported$Recovered + df_reported$Active + df_reported$Deceased

# Merge COVID-19 case rates with output data
df_toronto <-
  merge(df_toronto, df_episode[, c("Date", "Total Episode")], all.x = TRUE)
df_toronto <-
  merge(df_toronto, df_reported[, c("Date", "Total Reported")], all.x = TRUE)
df_toronto$"Total Episode"[is.na(df_toronto$"Total Episode")] <- 0
df_toronto$"Total Reported"[is.na(df_toronto$"Total Reported")] <- 0

# Add a column for weekends
df_toronto$Weekend <-
  ifelse(weekdays(df_toronto$Date) == "Sunday" |
           weekdays(df_toronto$Date) == "Saturday", 1, 0)
  

#### CONDUCT EXPLORATORY ANALYSIS ####

# Visualize trends for Toronto
ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021") +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of state of emergencies
v <- ifelse(df_toronto$State.of.Emergency == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (State of Emergencies Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of school closures
v <- ifelse(df_toronto$School.Closures == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (School Closures Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of gathering restrictions
v <- ifelse(df_toronto$Gathering.Restrictions == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

v2 <- ifelse(df_toronto$Gathering.Restrictions == 2, 1, 0)
inds2 <- diff(c(0, v2))

start2 <- df_toronto$Date[inds2 == 1]
end2 <- df_toronto$Date[inds2 == -1]
if (length(start2) > length(end2)) end2 <- c(end2, tail(df_toronto$Date, 1))
rects2 <- data.frame(start=start2, end=end2, group=seq_along(start2))

v3 <- ifelse(df_toronto$Gathering.Restrictions == 3, 1, 0)
inds3 <- diff(c(0, v3))

start3 <- df_toronto$Date[inds3 == 1]
end3 <- df_toronto$Date[inds3 == -1]
if (length(start3) > length(end3)) end3 <- c(end3, tail(df_toronto$Date, 1))
rects3 <- data.frame(start=start3, end=end3, group=seq_along(start3))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Gathering Restrictions Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.1) +
  geom_rect(data=rects2,
            inherit.aes=FALSE,
            aes(xmin=start2, xmax=end2, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_rect(data=rects3,
            inherit.aes=FALSE,
            aes(xmin=start3, xmax=end3, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of travel restricitons
v <- ifelse(df_toronto$Travel.Restrictions == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Travel Restrictions Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of business closures
v <- ifelse(df_toronto$Business.Closures == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

v2 <- ifelse(df_toronto$Business.Closures == 2, 1, 0)
inds2 <- diff(c(0, v2))

start2 <- df_toronto$Date[inds2 == 1]
end2 <- df_toronto$Date[inds2 == -1]
if (length(start2) > length(end2)) end2 <- c(end2, tail(df_toronto$Date, 1))
rects2 <- data.frame(start=start2, end=end2, group=seq_along(start2))

v3 <- ifelse(df_toronto$Business.Closures == 3, 1, 0)
inds3 <- diff(c(0, v3))

start3 <- df_toronto$Date[inds3 == 1]
end3 <- df_toronto$Date[inds3 == -1]
if (length(start3) > length(end3)) end3 <- c(end3, tail(df_toronto$Date, 1))
rects3 <- data.frame(start=start3, end=end3, group=seq_along(start3))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Business Closures Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.1) +
  geom_rect(data=rects2,
            inherit.aes=FALSE,
            aes(xmin=start2, xmax=end2, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_rect(data=rects3,
            inherit.aes=FALSE,
            aes(xmin=start3, xmax=end3, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of lockdowns
v <- ifelse(df_toronto$Lockdowns == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

v2 <- ifelse(df_toronto$Lockdowns == 2, 1, 0)
inds2 <- diff(c(0, v2))

start2 <- df_toronto$Date[inds2 == 1]
end2 <- df_toronto$Date[inds2 == -1]
if (length(start2) > length(end2)) end2 <- c(end2, tail(df_toronto$Date, 1))
rects2 <- data.frame(start=start2, end=end2, group=seq_along(start2))

v3 <- ifelse(df_toronto$Lockdowns == 3, 1, 0)
inds3 <- diff(c(0, v3))

start3 <- df_toronto$Date[inds3 == 1]
end3 <- df_toronto$Date[inds3 == -1]
if (length(start3) > length(end3)) end3 <- c(end3, tail(df_toronto$Date, 1))
rects3 <- data.frame(start=start3, end=end3, group=seq_along(start3))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Lockdowns Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.1) +
  geom_rect(data=rects2,
            inherit.aes=FALSE,
            aes(xmin=start2, xmax=end2, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_rect(data=rects3,
            inherit.aes=FALSE,
            aes(xmin=start3, xmax=end3, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.5) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of park closures
v <- ifelse(df_toronto$Park.Closures == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Park Closures Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize effect of mandatory masks
v <- ifelse(df_toronto$Mandatory.Masks == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

v2 <- ifelse(df_toronto$Mandatory.Masks == 2, 1, 0)
inds2 <- diff(c(0, v2))

start2 <- df_toronto$Date[inds2 == 1]
end2 <- df_toronto$Date[inds2 == -1]
if (length(start2) > length(end2)) end2 <- c(end2, tail(df_toronto$Date, 1))
rects2 <- data.frame(start=start2, end=end2, group=seq_along(start2))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Mandatory Masks Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.1) +
  geom_rect(data=rects2,
            inherit.aes=FALSE,
            aes(xmin=start2, xmax=end2, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))

# Visualize continuous variable correlations
cols <- append(c("Toronto"), colnames(df_toronto[, 22:30]))
data <- na.omit(df_toronto[cols])

par(family="CMU Serif")
col <- colorRampPalette(c("#476A6F",  "#FFFFFF", orange))
corrplot(cor(data), method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", tl.cex = 0.7,
         tl.col = "black", tl.srt = 90,
         sig.level = 0.01, insig = "blank", 
         diag = FALSE)

# Visualize effect of weekends
v <- ifelse(df_toronto$Weekend == 1, 1, 0)
inds <- diff(c(0, v))

start <- df_toronto$Date[inds == 1]
end <- df_toronto$Date[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(df_toronto$Date, 1))
rects <- data.frame(start=start, end=end, group=seq_along(start))

ggplot(data=df_toronto, aes(Date, Toronto)) +
  theme_minimal() +
  geom_line(color=orange, lwd=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Weekends Highlighted)") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  geom_rect(data=rects,
            inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(df_toronto$Toronto),
                ymax=max(df_toronto$Toronto), group=group),
            color="transparent",
            fill=orange,
            alpha=0.3) +
  geom_hline(yintercept=0) +
  theme(text=element_text(family="CMU Serif"))


#### BUILD SINGLE-EQUATION REGRESSION MODEL ####

# Make column names valid
colnames(df_toronto) <- make.names(colnames(df_toronto), unique = TRUE)

# Create training model and hold-out set with most recent 100 days
data_train <- head(na.omit(df_toronto), -100)
data_test <- tail(na.omit(df_toronto), 100)

# Select best model for entire training set
best_fit <- lm(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
                 State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
                 Park.Closures + Park.Closures*Weekend, data_train)
summary(best_fit)


# Select best model for truncated training set
data_train2 <- tail(na.omit(data_train), -31)
best_fit2 <- lm(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
                  State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
                  Park.Closures + Park.Closures*Weekend, data_train2)
summary(best_fit2)

# Select best model for entire dataset
full_fit <- lm(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
                 State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
                 Park.Closures + Park.Closures*Weekend, df_toronto)
summary(full_fit)


#### VALIDATE REGRESSION MODEL ASSUMPTIONS ####

# Run White test for heteroscedascticity
white_lm(best_fit)
white_lm(best_fit2)
white_lm(full_fit)

# Visualize residuals versus predicted values
plot(fitted(best_fit), res, main = "Residual vs. Fitted Plot, Full Data",
     xlab = "Fitted Values", ylab = "Residuals", family = "CMU Serif")
abline(0,0)
plot(fitted(best_fit2), res2, main = "Residual vs. Fitted Plot, Truncated Data", 
     xlab = "Fitted Values", ylab = "Residuals", family = "CMU Serif")
abline(0,0)
plot(fitted(full_fit), res_full, main = "Residual vs. Fitted Plot, Truncated Data", 
     xlab = "Fitted Values", ylab = "Residuals", family = "CMU Serif")
abline(0,0)

# Run Shapiro-Wilk W test for normality of residuals
ols_test_normality(best_fit)
ols_test_normality(best_fit2)
ols_test_normality(full_fit)

# Visualize residual kernal density estimate
res <- resid(best_fit)
res2 <- resid(best_fit2)
res_full <- resid(full_fit)
plot(density(res),
     main = "Residual Density Plot, Full Training Data", family = "CMU Serif")
plot(density(res2), 
     main = "Residual Density Plot, Truncated Training Data", family = "CMU Serif")
plot(density(res_full),
     main = "Residual Density Plot, Full Data", family = "CMU Serif")

# Visualize residual quantiles and normal quantiles (Q-Q)
qqnorm(res, main = "Normal Q-Q Plot, Full Training Data", family = "CMU Serif")
qqline(res) 
qqnorm(res2, main = "Normal Q-Q Plot, Truncated Training Data", family = "CMU Serif")
qqline(res2)
qqnorm(res_full, main = "Normal Q-Q Plot, Full Data", family = "CMU Serif")
qqline(res_full) 

# Visualize residual interquartile range
par(family = "CMU Serif")
boxplot(res, res2, res_full, 
        names = c("Full Training", "Truncated Training", "Full Data"))

# Calculate Durbin-Watson statistic to test for serial correlation
durbinWatsonTest(best_fit)
durbinWatsonTest(best_fit2)
durbinWatsonTest(full_fit)

# Conduct Ramsey RESET test for specification errors
resettest(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
            State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
            Park.Closures + Park.Closures*Weekend, power = 2:3,
          type = c("fitted", "regressor", "princomp"), data = data_train)

resettest(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
            State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
            Park.Closures + Park.Closures*Weekend, power = 2:3,
          type = c("fitted", "regressor", "princomp"), data = data_train2)

resettest(Toronto ~ Plus.Discouraged + Total.Reported + Weekend +
            State.of.Emergency + Gathering.Restrictions + Travel.Restrictions +
            Park.Closures + Park.Closures*Weekend, power = 2:3,
          type = c("fitted", "regressor", "princomp"), data = df_toronto)


# Conduct Chow tests for structural breaks
sctest(best_fit, type = "Chow", point = as.Date("2020-03-17"))
sctest(best_fit2, type = "Chow", point = as.Date("2020-0-17"))
sctest(full_fit, type = "Chow", point = as.Date("2020-03-17"))


#### ASSESS REGRESSION FORECAST ACCURACY ####

# Generate ex-ante forecasts
predict <- predict(best_fit, data_test, interval = "confidence")
predict2 <- predict(best_fit2, data_test, interval = "confidence")
predict_full <- predict(full_fit, data_test, interval = "confidence")

# Generate metrics for forecasts
accuracy(predict[,1], data_test$Toronto)
accuracy(predict2[,1], data_test$Toronto)
accuracy(predict_full[,1], data_test$Toronto)

# Visualize full training data predictions with errors and actual data
data_train <- head(na.omit(df_toronto), -100)
data_train$fit <- NA
data_train$lwr <- NA
data_train$upr <- NA
data <- cbind(data_test, as.data.frame(predict))
data <- rbind(data_train, data)

ggplot(data=data, aes(x=Date)) +
  theme_minimal() +
  geom_line(aes(y=Toronto), color="#F58024", lwd=0.5) +
  geom_line(aes(y=fit), color="darkgrey", lwd=0.5) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill="darkgrey", alpha=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Full Data Prediction)") +
  geom_vline(xintercept=as.Date("2020-12-22"), linetype=2, alpha=0.5) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))


# Visualize truncated training data predictions with errors and actual data
data_train2$fit <- NA
data_train2$lwr <- NA
data_train2$upr <- NA
data <- cbind(data_test, as.data.frame(predict2))
data <- rbind(data_train2, data)

ggplot(data=data, aes(x=Date)) +
  theme_minimal() +
  geom_line(aes(y=Toronto), color="#F58024", lwd=0.5) +
  geom_line(aes(y=fit), color="darkgrey", lwd=0.5) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill="darkgrey", alpha=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Truncated Data Prediction)") +
  geom_vline(xintercept=as.Date("2020-12-22"), linetype=2, alpha=0.5) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))

# Visualize full data predictions with errors and actual data
data_train <- head(na.omit(df_toronto), -100)
data_train$fit <- NA
data_train$lwr <- NA
data_train$upr <- NA
data <- cbind(data_test, as.data.frame(predict_full))
data <- rbind(data_train, data)

ggplot(data=data, aes(x=Date)) +
  theme_minimal() +
  geom_line(aes(y=Toronto), color="#F58024", lwd=0.5) +
  geom_line(aes(y=fit), color="darkgrey", lwd=0.5) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill="darkgrey", alpha=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (Full Data Prediction)") + 
  geom_vline(xintercept=as.Date("2020-12-22"), linetype=2, alpha=0.5) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))


#### BUILD ARIMA MODEL ####

# Build correlogram to check for white noise
acf(data_train$Toronto, lag.max = 20, main = "Transit Demand Change Correlogram")

diff <- diff(data_train$Toronto, differences = 1)
acf(diff, lag.max = 20, main = "Transit Demand Change Correlogram, First Difference")

diff2 <- diff(diff, differences = 1)
acf(diff2, lag.max = 20, main = "Transit Demand Change Correlogram, Second Difference")

# Check Q-statistic to test for white noise
for(i in 1:20) {
  print(Box.test(diff, lag = i))
}

# Perform Agumented Dickey-Fuller to confirm stationarity
adf.test(data_train$Toronto)
adf.test(diff)

# Create ARIMA model as baseline
Acf(diff, main='ACF for Series')
Pacf(diff, main='PACF for Series') 
auto.arima(data_train$Toronto)
fit_arima <- arima(data_train$Toronto, order=c(5,1,0))
fit_arima2 <- arima(data_train$Toronto, order=c(5,1,1))


#### VALIDATE ARIMA MODEL ASSUMPTIONS ####

# Confirm that sum of lagged series coefficients is less than one
-0.3042251 + -0.2221442 + -0.2404937 + -0.1554867 + -0.3844173 < 1

# Confirm that residuals are white noise with Q-statistic
checkresiduals(fit_arima)
checkresiduals(fit_arima2)


#### ASSESS ARIMA FORECAST ACCURACY ####

# Generate metrics for basic model forecast
fcast <- forecast(fit_arima, lead=100)
fcast2 <- forecast(fit_arima2, lead=100)
accuracy(as.data.frame(fcast)[,1], data_test$Toronto)
accuracy(as.data.frame(fcast2)[,1], data_test$Toronto)

# Calculate Theil's U-statistic
TheilU(data_test$Toronto, as.data.frame(fcast)[,1])
TheilU(data_test$Toronto, as.data.frame(fcast2)[,1])

# Visualize auto-ARIMA model predictions with errors and actual data
data_train <- head(na.omit(df_toronto), -100)
data_train$"Lead" <- NA
data_train$"Forecast" <- NA
data_train$"S.E" <- NA
data_train$"Lower" <- NA
data_train$"Upper" <- NA
data <- cbind(data_test, as.data.frame(fcast))
data <- rbind(data_train, data)
colnames(data) <- make.names(colnames(data), unique = TRUE)

ggplot(data=data, aes(x=Date)) +
  theme_minimal() +
  geom_line(aes(y=Toronto), color="#F58024", lwd=0.5) +
  geom_line(aes(y=Forecast), color="darkgrey", lwd=0.5) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper), fill="darkgrey", alpha=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (ARIMA Prediction)") +
  geom_vline(xintercept=as.Date("2020-12-22"), linetype=2, alpha=0.5) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))

# Visualize manual ARIMA model predictions with errors and actual data
data_train <- head(na.omit(df_toronto), -100)
data_train$"Lead" <- NA
data_train$"Forecast" <- NA
data_train$"S.E" <- NA
data_train$"Lower" <- NA
data_train$"Upper" <- NA
data <- cbind(data_test, as.data.frame(fcast2))
data <- rbind(data_train, data)
colnames(data) <- make.names(colnames(data), unique = TRUE)

ggplot(data=data, aes(x=Date)) +
  theme_minimal() +
  geom_line(aes(y=Toronto), color="#F58024", lwd=0.5) +
  geom_line(aes(y=Forecast), color="darkgrey", lwd=0.5) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper), fill="darkgrey", alpha=0.5) +
  ylab("Change in Public Transit Demand") +
  xlab("") +
  ggtitle("Change in Public Transit Demand From Pre-Pandemic Baseline") +
  labs(subtitle = "Toronto, February 2020 - April 2021 (ARIMA Prediction)") +
  geom_vline(xintercept=as.Date("2020-12-22"), linetype=2, alpha=0.5) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
  theme(text=element_text(family="CMU Serif"))