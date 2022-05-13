#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop/Projects/Data/2-In Progress/Forecasting Transit Recovery/Analysis")

# Load in data manipulation libraries
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(hutilscpp)

# Load in statistical libraries
library(DescTools)

# Load in data visualization libraries
library(ggplot2)
library(extrafont)
loadfonts()

# Load in time series libraries
library(zoo)
library(forecast)

# Set colours
orange <- "#F58024"

# Set date when data was last refreshed
date_refreshed = as.Date("2022-05-08")


#### CONDUCT DIAGNOSTIC ANALYSIS ####

# Read in source file for Google mobility data
link_mobi <- read_excel("data/predictor_sources.xlsx", sheet = "mobility")

# Read in Google mobility data
df_mobility <- rbind(
  # Bind 2020 mobility data for USA
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Chicago",]$filename1, sep = "")),
  # Bind 2021 mobility data for USA
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Chicago",]$filename2, sep = "")),
  # Bind 2022 mobility data for USA
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Chicago",]$filename3, sep = "")),
  # Bind 2020 mobility data for Canada
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Toronto",]$filename1, sep = "")),
  # Bind 2021 mobility data for Canada
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Toronto",]$filename2, sep = "")),
  # Bind 2022 mobility data for Canada
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Toronto",]$filename3, sep = "")),
  # Bind 2020 mobility data for Australia
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Brisbane",]$filename1, sep = "")),
  # Bind 2021 mobility data for Australia
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Brisbane",]$filename2, sep = "")),
  # Bind 2022 mobility data for Australia
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Brisbane",]$filename3, sep = "")),
  # Bind 2020 mobility data for UK
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "London",]$filename1, sep = "")),
  # Bind 2021 mobility data for UK
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "London",]$filename2, sep = "")),
  # Bind 2022 mobility data for UK
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "London",]$filename3, sep = "")),
  # Bind 2020 mobility data for France
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Paris",]$filename1, sep = "")),
  # Bind 2021 mobility data for France
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Paris",]$filename2, sep = "")),
  # Bind 2022 mobility data for France
  read.csv(paste("data/mobility/",
                 link_mobi[link_mobi$metro_area == "Paris",]$filename3, sep = ""))) %>%
  # Rename columns
  rename(
    work_from_home = workplaces_percent_change_from_baseline,
    transit_demand = transit_stations_percent_change_from_baseline) %>%
  # Create new columns
  mutate(
    # Convert mobility values to percent
    work_from_home = work_from_home / 100,
    transit_demand = transit_demand / 100,
    # Create region id
    region_id = paste(sub_region_1, sub_region_2, sep = " - "),
    # Identify metro area names
    metro_area = mapvalues(
      region_id,
      from = c("New York - Bronx County", "Ontario - Toronto", "Texas - Bexar County",
               "California - San Diego County", "New York - Richmond County",
               "New York - Queens County", "Pennsylvania - Philadelphia County",
               "Île-de-France - Paris", "Greater London - City of London",
               "Quebec - Montreal", "Arizona - Maricopa County", "Texas - Harris County",
               "New York - Kings County", "Alberta - Division No. 6 - Calgary",
               "Texas - Dallas County", "Illinois - Cook County", "New York - New York County",
               "Queensland - Brisbane City", "California - San Francisco County"),
      to = c("New York", "Toronto", "San Antonio", "San Diego", "New York",
             "New York", "Philadelphia", "Paris", "London", "Montréal", "Phoenix",
             "Houston", "New York", "Calgary", "Dallas", "Chicago", "New York",
             "Brisbane", "San Francisco")),
    # Identify date values
    date = as.Date(date),
    # Calculate population weight of New York counties
    weight = as.numeric(
      ifelse(region_id %in%
               c("New York - Bronx County", "New York - Kings County",
                 "New York - New York County", "New York - Queens County",
                 "New York - Richmond County"),
             mapvalues(region_id,
                       from = c("New York - Bronx County", "New York - Kings County",
                                "New York - New York County", "New York - Queens County",
                                "New York - Richmond County"),
                       to = c(1418207, 2559903, 1628706, 2253858, 476143)), NA)),
    # Calculate weighted mobility for New York
    work_from_home_weighted = work_from_home * weight / 8336817,
    transit_demand_weighted = transit_demand * weight / 8336817,
    # Create column for general id
    id = paste(metro_area, date, sep = ": ")) %>%
  # Select regions of interest
  filter(metro_area %in% c("New York", "Toronto", "San Antonio", "San Diego", 
                           "Philadelphia", "Paris", "London", "Montréal",
                           "Phoenix", "Houston", "Calgary", "Dallas", "Chicago",
                           "Brisbane", "San Francisco")) %>%
  # Group by date and metro-area
  group_by(id) %>%
  # Aggregate New York counties as a weighted mean
  mutate(
    work_from_home = ifelse(
      metro_area == "New York", sum(work_from_home_weighted), work_from_home),
    transit_demand = ifelse(
      metro_area == "New York", sum(transit_demand_weighted), transit_demand)) %>%
  # Ungroup data
  ungroup() %>%
  # Select single New York county with full weighted data for New York
  filter(!sub_region_2 %in% c("Bronx County", "Kings County",
                              "Queens County", "Richmond County")) %>%
  # Group by metro-area
  group_by(metro_area) %>%
  # Fix issue with missing mobility data
  mutate(
    # Replace missing mobility data with data from the previous day
    work_from_home = ifelse(
      is.na(work_from_home), lag(work_from_home), work_from_home),
    transit_demand = ifelse(
      is.na(transit_demand), lag(transit_demand), transit_demand),
    # Repeat in case of two missing mobility data reports in a row
    work_from_home = ifelse(
      is.na(work_from_home), lag(work_from_home), work_from_home),
    transit_demand = ifelse(
      is.na(transit_demand), lag(transit_demand), transit_demand),
    # Identify year-month pair for each date
    year_mon = as.yearmon(date)) %>%
  # Ungroup data
  ungroup() %>%
  # Group by metro area and year-month
  group_by(metro_area, year_mon) %>%
  # Set all remaining missing values to the average for the month
  mutate(
    work_from_home = ifelse(
      is.na(work_from_home), mean(work_from_home, na.rm = T), work_from_home),
    transit_demand = ifelse(
      is.na(transit_demand), mean(transit_demand, na.rm = T), transit_demand)) %>%
  # Ungroup data
  ungroup() %>%
  # Group by metro area
  group_by(metro_area) %>%
  # Calculate metrics of interest
  mutate(
    # Adjust demand for weekday variation using moving average
    transit_demand = rollmean(transit_demand, 7, na.pad = T),
    work_from_home = rollmean(work_from_home, 7, na.pad = T),
    # Create metric for demand in past month
    metric_past_month = mean(
      transit_demand[date >= max(date) - 31], na.rm = T),
    # Create metric for whether or not demand has recovered
    metric_recovered = ifelse(
      # Check if past month demand drop was greater than 5% below pre-pandemic levels
      metric_past_month >= -0.05, "Yes", "No"),
    # Create metric for average demand drop over entire pandemic
    metric_pandemic = mean(
      transit_demand[date >= "2020-03-15"], na.rm = T),
    # Create metric for initial demand drop in first month
    metric_initial_drop = mean(
      transit_demand[date >= "2020-03-15" & date <= "2020-04-15"], na.rm = T),
    # Create metric for minimum demand date
    metric_minimum_date = min(date[transit_demand <= metric_initial_drop], na.rm = T),
    # Create metric for demand recovered since drop
    metric_demand_recovered = metric_past_month - metric_initial_drop) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(metro_area, date, year_mon, transit_demand, work_from_home,
         metric_past_month, metric_recovered, metric_pandemic,
         metric_initial_drop, metric_minimum_date, metric_demand_recovered)

# Graph transit demand using Google mobility data
graph_ridership <- df_mobility %>%
  ggplot(aes(x = date)) +
  # Set graph theme
  theme_minimal() +
  # Plot demand as a line
  geom_line(aes(y = transit_demand), lwd = 0.6, color = orange) +
  # Set axis labels and plot titles
  ylab("Change in Public Transit Ridership") +
  xlab("") +
  labs(title = "How has the COVID-19 pandemic affected public transit ridership?",
       subtitle = paste("Change in transit station vistors",
                        "compared to pre-pandemic baseline",
                        "(Select cities, Feb 2020 - Apr 2022)"),
       caption = "Google. (2022). COVID-19 Community Mobility Reports.") +
  # Create line for normal pre-pandemic public transit demand
  geom_hline(yintercept = 0) +
  # Format axis ticks
  scale_y_continuous(
    labels = function(x) ifelse(x > 0, paste0("+", x*100, "%"), paste0(x*100, "%")),
    breaks = seq(-1, 1, .5), limits = c(-1, 1),
    minor_breaks = NULL) +
  scale_x_date(
    date_breaks = "3 months" , date_labels = "%b-%y",
    limits = c(floor_date(min(df_mobility$date), "month"),
               ceiling_date(max(df_mobility$date), "month"))) +
  # Customize graph appearance
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.caption = element_text(size = 10, hjust = 0, vjust = 4),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, vjust = 4),
        plot.margin = unit(c(1, 1, 1, 1.2), "cm"),
        panel.grid.minor =  element_line(color = "#CCCCCC"),
        panel.grid.major = element_line(color = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA),
        panel.background = element_rect(fill = "#E6E6E6", color = NA),
        legend.position = "none") +
  # Create panels for each metro area
  facet_wrap(~metro_area, ncol = 5, scales = "free_x")

# Save graph
png("./images/ridership.png", units = "in",
    width = 12, height = 8, res = 1000)
graph_ridership
dev.off()


#### CLEAN PREDICTOR DATA ####

# Read in most recent policy data for selected metro areas
policies <- read.csv("data/predictors_policies.csv") %>%
  mutate(
    # Change character types of columns
    start_date = as.Date(start_date),
    # Adjust policy end date to exclude the day the policy was lifted
    end_date = as.Date(end_date) - 1) %>%
  # Replace missing values on continued policies with current date
  replace(is.na(.), Sys.Date())

# Read in source file for linking predictor data
link_case <- read_excel("data/predictor_sources.xlsx", sheet = "cases")
link_vaxx <- read_excel("data/predictor_sources.xlsx", sheet = "vaccines")

# Read in most recent daily case data for Brisbane
case_brisbane <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "Brisbane",]$filename, sep = "")) %>%
  # Filter to Queensland area only
  filter(state == "Queensland") %>%
  mutate(
    # Create column for metro area name
    metro_area = "Brisbane",
    # Format date column
    date = as.Date(date),
    # Set negative case numbers to zero
    cases = ifelse(confirmed < 0, 0, confirmed),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == "Brisbane",]$population * 100000) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for Calgary
case_calgary <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "Calgary",]$filename, sep = "")) %>%
  # Filter to Calgary Zone only 
  filter(Alberta.Health.Services.Zone == "Calgary Zone") %>%
  # Count individual rows as daily case totals
  count(Date.reported, Alberta.Health.Services.Zone) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Calgary",
    # Format date column
    date = as.Date(Date.reported),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      n / link_case[link_case$metro_area == "Calgary",]$population * 100000) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for London
case_london <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "London",]$filename, sep = "")) %>%
  # Rename columns
  rename(cases = newCasesBySpecimenDate) %>%
  rename(metro_area = areaName) %>%
  mutate(
    # Format date column
    date = as.Date(date),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == "London",]$population * 100000) %>%
  # Sort by date
  arrange(date) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for Montréal
case_montreal <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "Montréal",]$filename, sep = "")) %>%
  # Rename columns
  rename(cases = Moyenne.mobile..7.jours.) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Montréal",
    # Format date column
    date = as.Date(Date.de.déclaration.du.cas),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == "Montréal",]$population * 100000) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for New York
case_nyc <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "New York",]$filename, sep = "")) %>%
  # Rename columns
  rename(cases = CASE_COUNT) %>%
  mutate(
    # Create column for metro area name
    metro_area = "New York",
    # Format date column
    year = substr(date_of_interest, 7, 11),
    month = substr(date_of_interest, 1, 2),
    day = substr(date_of_interest, 4, 5),
    date = as.Date(paste(year, month, day, sep = "-")),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == "New York",]$population * 100000) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for Paris
case_paris <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "Paris",]$filename, sep = ""), sep = ";") %>%
  # Filter to Île de France region only 
  filter(reg == 11) %>%
  # Filter to all age groups only
  filter(cl_age90 == 0) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Paris",
    # Format date column
    date = as.Date(jour),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      P / link_case[link_case$metro_area == "Paris",]$population * 100000) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily case data for Toronto
case_toronto <- read_excel(
  paste("data/cases/",
        link_case[link_case$metro_area == "Toronto",]$filename, sep = ""),
  sheet = "Cases by Reported Date") %>%
  mutate(
    # Create column for metro area name
    metro_area = "Toronto",
    # Format date column
    date = as.Date(`Reported Date`),
    # Calculate daily cases
    cases = Recovered + Active,
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == "Toronto",]$population * 100000) %>%
  # Sort by date
  arrange(date) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent case data for United States metro areas, excluding New York
case_usa <- read.csv(
  paste("data/cases/",
        link_case[link_case$metro_area == "Dallas",]$filename, sep = "")) %>%
  # Transform data from wide to long format
  pivot_longer(!c("countyFIPS", "County.Name", "State", "StateFIPS"),
               names_to = "date", values_to = "cases") %>%
  # Create unique id for county-state pairs
  mutate(
    County.Name = trimws(County.Name),
    id = paste(County.Name, "-", State)) %>%
  # Filter to selected USA counties only
  filter(id %in% c("Cook County - IL", "Dallas County - TX", "Harris County - TX",
                   "Philadelphia County - PA", "Maricopa County - AZ",
                   "Bexar County - TX", "San Diego County - CA",
                   "City and County of San Francisco - CA")) %>%
  group_by(County.Name) %>%
  mutate(
    # Create column for metro area name
    metro_area = case_when(
      County.Name == "Cook County" ~ "Chicago",
      County.Name == "Dallas County" ~ "Dallas",
      County.Name == "Harris County" ~ "Houston",
      County.Name == "Philadelphia County" ~ "Philadelphia",
      County.Name == "Maricopa County" ~ "Phoenix",
      County.Name == "Bexar County" ~ "San Antonio",
      County.Name == "San Diego County" ~ "San Diego",
      County.Name == "City and County of San Francisco" ~ "San Francisco"),
    # Format date column
    date = as.Date(gsub("\\.", "-", substring(date, 2))),
    # Get cases as daily total instead of cumulative count
    cases = cases - lag(cases),
    # Set negative cumulative cases equal to the previous day's cases
    cases = ifelse(cases < 0, lag(cases), cases),
    # Repeat in case of two negative counts in a row
    cases = ifelse(cases < 0, lag(cases), cases),
    # Calculate cases as rate per 100,000 residents
    case_rate =
      cases / link_case[link_case$metro_area == metro_area[[1]],]$population * 100000) %>%
  ungroup() %>%
  # Filter out columns with missing data
  filter(!is.na(case_rate)) %>%
  # Select columns for final data frame
  select(metro_area, date, case_rate)

# Read in most recent daily vaccination data for Brisbane
vaxx_brisbane <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Brisbane",]$filename, sep = "")) %>%
  # Filter to Queensland only 
  filter(administrative_area_level_2 == "Queensland") %>%
  # Rename columns
  rename(vaxx = vaccines_1) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Brisbane",
    # Format date column
    date = as.Date(date),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Brisbane",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent daily vaccination data for Calgary
vaxx_calgary <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Calgary",]$filename, sep = "")) %>%
  # Filter to Alberta only 
  filter(prename == "Alberta") %>%
  # Rename columns
  rename(vaxx = numtotal_atleast1dose) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Calgary",
    # Format date column
    date = as.Date(week_end),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Calgary",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)
# Add data for dates between weekend reporting dates
vaxx_calgary <- vaxx_calgary %>%
  # Attach new rows representing missing dates
  rbind(cbind.data.frame(
    date =
      seq(min(vaxx_calgary$date), max(vaxx_calgary$date), by = "days"),
    metro_area = 
      rep("Calgary", as.numeric(max(vaxx_calgary$date) - min(vaxx_calgary$date) + 1)),
    vaxx_rate =
      rep(NA, as.numeric(max(vaxx_calgary$date) - min(vaxx_calgary$date) + 1)))) %>%
  # Sort by date
  arrange(date) %>%
  # Set in between dates equal to weekend reported data
  na.locf() %>%
  # Remove duplicated rows
  distinct(date, .keep_all = TRUE)

# Read in most recent daily vaccination data for London
vaxx_london <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "London",]$filename, sep = "")) %>%
  # Rename columns
  rename(vaxx = cumPeopleVaccinatedFirstDoseByVaccinationDate) %>%
  mutate(
    # Create column for metro area name
    metro_area = "London",
    # Format date column
    date = as.Date(date),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "London",]$population * 100) %>%
  # Sort by date
  arrange(date) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent daily vaccination data for Montréal
vaxx_montreal <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Montréal",]$filename, sep = "")) %>%
  # Rename columns
  rename(vaxx = Dose.1) %>%
  rename(date = Date.de.vaccination) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Montréal",
    # Format date column
    date = as.Date(date),
    # Calculate cumulative sum of vaccinations
    vaxx = cumsum(vaxx),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Montréal",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent daily vaccination data for New York
vaxx_nyc <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "New York",]$filename, sep = "")) %>%
  # Rename columns
  rename(vaxx = ADMIN_DOSE1_CUMULATIVE) %>%
  rename(date = DATE) %>%
  mutate(
    # Create column for metro area name
    metro_area = "New York",
    # Format date column
    date = as.Date(date),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "New York",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent daily vaccination data for Paris
vaxx_paris <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Paris",]$filename, sep = ""), sep = ";") %>%
  # Filter to Île de France region only 
  filter(reg == 11) %>%
  # Rename columns
  rename(vaxx = n_cum_dose1) %>%
  rename(date = jour) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Paris",
    # Format date column
    date = as.Date(date),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Paris",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent daily vaccination data for Toronto
vaxx_toronto <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Toronto",]$filename, sep = "")) %>%
  # Filter to dose one only
  filter(Dose.Num..Covax.Ll.Public.Sas7Bdat. == "Dose 1") %>%
  # Filter to rows with date information
  filter(Imm.date.level != "") %>%
  # Rename columns
  rename(vaxx = Count.of.Client.ID..covax.ll.public.sas7bdat.) %>%
  rename(date = Imm.date.level) %>%
  separate(date, into=c("month", "day", "year"), sep = "/") %>%
  mutate(
    # Create column for metro area name
    metro_area = "Toronto",
    # Format date column
    date = as.Date(paste(year, month, day, sep = "-")),
    # Calculate cumulative sum of vaccinations
    vaxx = cumsum(as.numeric(as.numeric(gsub(",", "", vaxx)))),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Toronto",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Read in most recent vaccination data for US metro areas, excluding New York and Texas
vaxx_usa <- read.csv(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Chicago",]$filename, sep = "")) %>%
  # Create unique id for county-state pairs
  mutate(id = paste(Recip_County, "-", Recip_State)) %>%
  # Filter to selected USA counties only
  filter(id %in% c("Cook County - IL", "Philadelphia County - PA",
                   "Maricopa County - AZ", "San Francisco County - CA")) %>%
  # Rename columns
  rename(vaxx = Administered_Dose1_Recip) %>%
  group_by(Recip_County) %>%
  mutate(
    # Create column for metro area name
    metro_area = case_when(
      Recip_County == "Cook County" ~ "Chicago",
      Recip_County == "Philadelphia County" ~ "Philadelphia",
      Recip_County == "Maricopa County" ~ "Phoenix",
      Recip_County == "San Francisco County" ~ "San Francisco"),
    # Format date column
    year = substr(Date, 7, 11),
    month = substr(Date, 1, 2),
    day = substr(Date, 4, 5),
    date = as.Date(paste(year, month, day, sep = "-")),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == metro_area[[1]],]$population * 100) %>%
  # Sort by date
  arrange(date) %>%
  ungroup() %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Duplicate San Francisco vaccination data for San Diego
vaxx_san_diego <- vaxx_usa[vaxx_usa$metro_area == "San Francisco",] %>%
  mutate(metro_area = "San Diego")

# Read in most recent daily vaccination data for Dallas
vaxx_dallas <- read_excel(
  paste("data/vaccines/",
        link_vaxx[link_vaxx$metro_area == "Dallas",]$filename, sep = ""),
  sheet = "By Vaccination Date",
  col_types = c("date", "numeric", "numeric", "numeric", "numeric")) %>%
  # Remove grand total row
  filter(!is.na(`Vaccination Date`)) %>%
  # Rename columns
  rename(vaxx = `People Vaccinated with at least One Dose`) %>%
  rename(date = `Vaccination Date`) %>%
  mutate(
    # Create column for metro area name
    metro_area = "Dallas",
    # Format date column
    date = as.Date(date),
    # Calculate cumulative sum of vaccinations
    vaxx = cumsum(vaxx),
    # Calculate first dose vaccination as percentage
    vaxx_rate =
      vaxx / link_vaxx[link_vaxx$metro_area == "Dallas",]$population * 100) %>%
  # Select columns for final data frame
  select(metro_area, date, vaxx_rate)

# Duplicate Texas vaccination data for Houston
vaxx_houston <- vaxx_dallas %>%
  mutate(metro_area = "Houston")

# Duplicate Texas vaccination data  for San Antonio
vaxx_san_antonio <- vaxx_dallas %>%
  mutate(metro_area = "San Antonio")

# Combine case and vaccination data for all metro areas
case_all <- rbind(case_brisbane, case_calgary, case_london, case_montreal,
                  case_nyc, case_paris, case_toronto, case_usa)
vaxx_all <- rbind(vaxx_brisbane, vaxx_calgary, vaxx_london, vaxx_montreal,
                  vaxx_nyc, vaxx_paris, vaxx_toronto, vaxx_usa, vaxx_san_diego,
                  vaxx_dallas, vaxx_houston, vaxx_san_antonio)

# Create table for lagged monthly demand averages
lagged_demand <- df_mobility %>%
  # Filter out rows with no adjusted demand data
  filter(!is.na(transit_demand)) %>%
  # Group by metro area and year month
  group_by(metro_area, year_mon) %>%
  # Get monthly average for each metro area
  summarise(lagged_month_average = mean(transit_demand, rm.na = TRUE)) %>%
  # Shift values forward one month to match with rest of data as past values
  mutate(year_mon = year_mon + 1/12)


#### PERFORM FEATURE ENGINEERING ####

# Create new data frame with selected metro areas
df <- df_mobility %>%
  # Join daily COVID-19 case rates to data
  merge(case_all, by = c("metro_area", "date"), all.x = TRUE) %>%
  # Join daily COVID-19 vaccination rates to data
  merge(vaxx_all, by = c("metro_area", "date"), all.x = TRUE) %>%
  # Add new columns
  mutate(
    # Replace missing values with zeroes
    case_rate = ifelse(is.na(case_rate), 0, case_rate),
    vaxx_rate = ifelse(is.na(vaxx_rate), na.locf(vaxx_rate), vaxx_rate)) %>%
  # Add local policies to each date in data frame as dummy variables
  group_by(metro_area) %>%
  mutate(
    # Add major border closure dates as dummy variables
    policy_border = ifelse(date %in% unlist(
      Map(`:`,
          policies[policies$metro_area == metro_area[[1]] &
                     policies$category == "Major Border Closure",]$start_date,
          policies[policies$metro_area == metro_area[[1]] &
                     policies$category == "Major Border Closure",]$end_date)), 1, 0),
    # Add park closure dates as dummy variables
    policy_parks = ifelse(date %in% unlist(
      Map(`:`, policies[policies$metro_area == metro_area[[1]] &
                          policies$category == "Park Closures",]$start_date,
          policies[policies$metro_area == metro_area[[1]] &
                     policies$category == "Park Closures",]$end_date)), 1, 0),
    # Add state of emergency dates as dummy variables
    policy_emergency = ifelse(date %in% unlist(
      Map(`:`, policies[policies$metro_area == metro_area[[1]] &
                          policies$category == "State of Emergency",]$start_date,
          policies[policies$metro_area == metro_area[[1]] &
                     policies$category == "State of Emergency",]$end_date)), 1, 0),
    # Add gathering restriction dates as dummy variables for each level
    policy_gathering = case_when(
      # Add gathering restriction dates for least severe restrictions
      date %in% unlist(
        Map(`:`, policies[policies$metro_area == metro_area[[1]] &
                            policies$category == "Gathering Restrictions" &
                            policies$encoding == 1,]$start_date,
            policies[policies$metro_area == metro_area[[1]] &
                       policies$category == "Gathering Restrictions" &
                       policies$encoding == 1,]$end_date)) ~ 1,
      # Add gathering restriction dates for moderately severe restrictions
      date %in% unlist(
        Map(`:`, policies[policies$metro_area == metro_area[[1]] &
                            policies$category == "Gathering Restrictions" &
                            policies$encoding == 2,]$start_date,
            policies[policies$metro_area == metro_area[[1]] &
                       policies$category == "Gathering Restrictions" &
                       policies$encoding == 2,]$end_date)) ~ 2,
      # Add gathering restriction dates for most severe restrictions
      date %in% unlist(
        Map(`:`, policies[policies$metro_area == metro_area[[1]] &
                            policies$category == "Gathering Restrictions" &
                            policies$encoding == 3,]$start_date,
            policies[policies$metro_area == metro_area[[1]] &
                       policies$category == "Gathering Restrictions" &
                       policies$encoding == 3,]$end_date)) ~ 3,
      TRUE ~ 0),
    policy_gathering = as.factor(policy_gathering),
    # Add gathering restrictions as column dummy variables
    policy_gathering_low = ifelse(policy_gathering %in% c(1,2,3), 1, 0),
    policy_gathering_med = ifelse(policy_gathering %in% c(2,3), 1, 0),
    policy_gathering_hig = ifelse(policy_gathering == 3, 1, 0),
    # Create column with average of all past pandemic demand
    past_average = lag(cummean(transit_demand %>% replace_na(0))),
    # Create column for days since pandemic started
    days_since_pandemic = as.numeric(date - metric_minimum_date),
    # Square term to get quadratic effects
    days_since_pandemic_sqr = days_since_pandemic^2,
    # Adjust case rate for weekday variation using moving average
    case_rate = rollmean(case_rate, 7, na.pad = T),
    # Create column for cumulative case rates
    cases_total = cumsum(case_rate %>% replace_na(0)),
    # Create column for cumulative lockdown days
    lockdown_total = ifelse(policy_gathering == "3", 1, 0) %>% cumsum(),
    # Create column for state of emergency days
    emergency_total = cumsum(policy_emergency),
    # Create column for gathering restriction days
    gathering_total = cumsum(policy_gathering_low),
    # Create columns for policy lengths
    border_length = cumsum_reset(policy_border == 1, policy_border),
    parks_length = cumsum_reset(policy_parks == 1, policy_parks),
    emergency_length = cumsum_reset(policy_emergency == 1, policy_emergency),
    border_length_sqr = border_length^2,
    parks_length_sqr = parks_length^2,
    emergency_length_sqr = emergency_length^2,
    # Create columns for gathering restriction lengths
    gathering_low_length = ifelse(policy_gathering_low == "1", 1, 0) %>%
      cumsum_reset(. == 1, .),
    gathering_med_length = ifelse(policy_gathering_med == "1", 1, 0) %>%
      cumsum_reset(. == 1, .),
    gathering_hig_length = ifelse(policy_gathering_hig == "1", 1, 0) %>%
      cumsum_reset(. == 1, .),
    gathering_non_length = ifelse(policy_gathering == "0", 1, 0) %>%
      cumsum_reset(. == 1, .),
    gathering_low_length_sqr = gathering_low_length^2,
    gathering_med_length_sqr = gathering_med_length^2,
    gathering_hig_length_sqr = gathering_hig_length^2) %>%
  # Ungroup data
  ungroup() %>%
  # Join lagged month demand average to data
  merge(lagged_demand, by = c("metro_area", "year_mon"), all.x = TRUE) %>%
  # Set lagged demand to zero for first month
  mutate(
    lagged_month_average = ifelse(
      is.na(lagged_month_average), 0, lagged_month_average)) %>%
  # Remove rows after the date the data was last refreshed
  filter(date <= date_refreshed) %>%
  # Remove data where demand data is missing
  filter(!is.na(transit_demand)) %>%
  # Group by metro area and month
  group_by(metro_area, year_mon) %>%
  # Create variable for total average demand before current month
  mutate(
    past_average_month = ifelse(
      !is.na(past_average[date == min(date)]),
      past_average[date == min(date)], 0)) %>%
  # Ungroup data
  ungroup() %>%
  # Sort by date
  arrange(metro_area, date)

# Remove excess variables
remove(case_all, case_brisbane, case_calgary, case_london, case_montreal,
       case_nyc, case_paris, case_toronto, case_usa, lagged_demand, link_case,
       link_mobi, link_vaxx, policies, vaxx_all, vaxx_brisbane, vaxx_calgary,
       vaxx_dallas, vaxx_houston, vaxx_london, vaxx_montreal, vaxx_nyc,
       vaxx_paris, vaxx_san_antonio, vaxx_san_diego, vaxx_toronto, vaxx_usa,
       date_refreshed, df_mobility)