#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop/Projects/Data/2-In Progress/Forecasting Transit Recovery/Analysis")

# Load data model
source("model.R")


#### GENERATE FULL SET PREDICTIONS MONTH BY MONTH ####

# Create new dataframe for future predictions
df_future <- df %>%
  # Select prediction variables
  select(metro_area, year_mon, date, transit_demand, work_from_home,
         metric_initial_drop, policy_border, policy_parks, policy_emergency,
         emergency_length, policy_gathering_low, gathering_low_length,
         policy_gathering_med, gathering_med_length, 
         policy_gathering_hig, gathering_hig_length,
         days_since_pandemic, cases_total, lockdown_total,
         lagged_month_average, past_average, past_average_month)

# Set number of months to forecast
months <- 24
# Generate predictions for each month
for(month in 0:months){
  # Create modified to activate when dealing with future months
  modifier <- ifelse(month == 0, 0, 1)
  # Create new rows for each month, including remainder of current month
  metro_area_new <- c()
  date_new <- c()
  # Add new dates for each metro area
  for(i in unique(df$metro_area)){
    # Add new dates for each day between the last day and the end of the month
    for(j in (max(df_future$date) + 1):
        (as.Date(as.yearmon(max(df_future$date) + modifier) + 1/12) - 1)){
      # Store new dates in vectors
      metro_area_new <- append(metro_area_new, i)
      date_new <- append(date_new, j)
    }
  }
  # Bind new prediction data to dataframe
  df_future <- rbind.fill(
    df_future, data.frame(
      metro_area = metro_area_new, date = date_new, predicted = "Yes")) %>%
    # Sort by metro area and date
    arrange(metro_area, date) %>%
    # Group by metro area
    group_by(metro_area) %>%
    # Replace missing data with most recently available data
    fill(work_from_home, metric_initial_drop, policy_border, policy_parks,
         policy_emergency, policy_gathering_low, policy_gathering_med,
         policy_gathering_hig, cases_total, lockdown_total) %>%
    # Calculate remainder of missing columns
    mutate(
      # Calculate year month
      year_mon = as.yearmon(date),
      # Create columns for gathering restriction lengths
      gathering_low_length = ifelse(policy_gathering_low == "1", 1, 0) %>%
        cumsum_reset(. == 1, .),
      gathering_med_length = ifelse(policy_gathering_med == "1", 1, 0) %>%
        cumsum_reset(. == 1, .),
      gathering_hig_length = ifelse(policy_gathering_hig == "1", 1, 0) %>%
        cumsum_reset(. == 1, .),
      # Create column for state of emergency lengths
      emergency_length = cumsum_reset(policy_emergency == 1, policy_emergency),
      # Create column for days since pandemic start
      days_since_pandemic = as.numeric(date - min(date)),
      days_since_pandemic_sqr = days_since_pandemic^2,
      # Create column for past average using value at first day of month
      past_average_month = ifelse(
        is.na(past_average_month),
        mean(past_average[date == (as.Date(as.yearmon(max(date))) - 1)]),
        past_average_month),
      # Create column for lagged month average using average of past month
      lagged_month_average = ifelse(
        is.na(lagged_month_average),
        mean(transit_demand[year_mon == (as.yearmon(max(date)) - 1/12)]),
        lagged_month_average)) %>%
    # Ungroup data
    ungroup()
  # Predict future demand
  predict_future <- predict(fit_full, df_future, interval = "prediction", level = 0.9)
  # Add predictions to dataframe
  df_future$demand_predict <- predict_future
  # Replace transit demand values with predictions
  df_future <- df_future %>%
    mutate(
      # Replace missing transit demand values with predictions
      transit_demand = ifelse(
        is.na(transit_demand), demand_predict, transit_demand),
      # Create column with average of all past pandemic demand
      past_average = lag(cummean(transit_demand %>% replace_na(0))))
}

# Remove excess variables
remove(i, j, metro_area_new, date_new, month, months, modifier)

# Graph predictions
graph_future <- df_future %>% 
  # Edit columns
  mutate(
    # Set demand to true values only, ignoring predictions
    transit_demand = ifelse(is.na(predicted), transit_demand, NA),
    # Set predicted demand to predictions only, ignoring extant values
    demand_predict = ifelse(is.na(predicted), NA, demand_predict),) %>%
  # Plot data
  ggplot(aes(x = date)) +
  # Set graph theme
  theme_minimal() +
  # Plot demand as a line
  geom_line(aes(y = transit_demand), lwd = 0.6, color = orange) +
  # Plot predicted demand as a line
  geom_line(aes(y = demand_predict), lwd = 0.6, color = "#737373") +
  # Plot mean absolute error range
  #geom_ribbon(aes(
  #  ymin = demand_predict - accuracy(predict_train, df_test$transit_demand)[3],
  #  ymax = demand_predict + accuracy(predict_train, df_test$transit_demand)[3]),
  #  fill = "darkgrey", alpha = 0.5) +
  # Set axis labels and plot titles
  ylab("Change in Public Transit Ridership") +
  xlab("") +
  labs(title = "How will public transit ridership recover over the next two years?",
       subtitle = paste("Forecasted change in transit station vistors",
                        "compared to pre-pandemic baseline",
                        "(Select cities, May 2022 - May 2024)"),
       caption = "Google. (2022). COVID-19 Community Mobility Reports.") +
  # Create line for normal pre-pandemic public transit demand
  geom_hline(yintercept = 0) +
  # Format axis ticks
  scale_y_continuous(
    labels = function(x) ifelse(x > 0, paste0("+", x*100, "%"), paste0(x*100, "%")),
    breaks = seq(-1, 1, .5), limits = c(-1, 1),
    minor_breaks = NULL) +
  scale_x_date(
    date_breaks = "6 months" , date_labels = "%b-%y",
    limits = c(floor_date(min(df_future$date), "month"),
               ceiling_date(max(df_future$date), "month"))) +
  # Customize graph appearance
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain", size = 14),
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
png("./images/forecast.png", units = "in",
    width = 12, height = 8, res = 1000)
graph_future
dev.off()

# Get predictions by quarter
View(df_future %>%
  # Create variable to indicate year-quarter
  mutate(quarter = as.yearqtr(date)) %>%
  # Filter data to predicted values only
  filter(predicted == "Yes") %>%
  # Group by metro area and year-quarter
  group_by(metro_area, quarter) %>%
  # Get average prediction for each subsequent quarter
  summarize(average = round(mean(demand_predict, na.rm = T) * 100, 1)))