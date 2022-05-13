#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop/Projects/Data/2-In Progress/Forecasting Transit Recovery/Analysis")

# Clean data
source("data_eng.R")

# Load in statistical libraries
library(forecast)
library(randomForest)

# Load in data manipulation libraries
library(tibble)

# Set random seed
set.seed(300)


#### BUILD FORECASTING TRAINING MODEL ####

# Create training data
df_train <- df %>%
  group_by(metro_area) %>%
  slice(1:(n()-100)) %>%
  ungroup()

# Create test data
df_test <- df %>%
  group_by(metro_area) %>%
  slice(tail(row_number(), 100)) %>%
  ungroup()

# Select best model for training dataset
fit_train <- randomForest(
  transit_demand ~
    policy_parks + policy_emergency + policy_gathering_low +
    policy_gathering_med + policy_gathering_hig + policy_border +
    cases_total + metric_initial_drop + days_since_pandemic +
    lagged_month_average + lockdown_total + work_from_home +
    past_average_month + emergency_length + gathering_low_length +
    gathering_hig_length + gathering_med_length, df_train)
summary(fit_train)

predict_train <- predict(fit_train, df_test, interval = "prediction", level = 0.9)
accuracy(predict_train, df_test$transit_demand)

df_test$demand_predict <- predict_train

# Graph test data predictions compared to actual results
df_train %>% mutate(
  # Attach predictions to test dataset
  demand_predict = NA) %>%
  rbind(df_test) %>%
  # Create graph
  ggplot(aes(x = date)) +
  # Set graph theme
  theme_minimal() +
  # Plot demand as a line
  geom_line(aes(y = transit_demand), lwd = 0.6, color = orange) +
  # Plot predicted demand as a line
  geom_line(aes(y = demand_predict), lwd = 0.6, color = "darkgrey") +
  # Set axis labels and plot titles
  ylab("Change in Public Transit Ridership") +
  xlab("") +
  ggtitle("Change in Public Transit Ridership From Pre-Pandemic Baseline") +
  labs(subtitle = "Select Metro Areas, February 2020 - April 2022") +
  # Create line for normal pre-pandemic public transit demand
  geom_hline(yintercept = 0) +
  # Format axis ticks
  scale_y_continuous(
    labels = scales::percent, breaks = seq(-1, 1, .5), limits = c(-1, 1),
    minor_breaks = NULL) +
  scale_x_date(
    date_breaks = "2 months" , date_labels = "%b-%y",
    limits = c(floor_date(min(df$date), "month"),
               ceiling_date(max(df$date), "month"))) +
  # Customize graph appearance
  theme(
    text = element_text(family  ="CMU Typewriter Text Bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none") +
  # Create panels for each metro area
  facet_wrap(~metro_area, ncol = 5, scales = "free_x")


#### BUILD FORECASTING FULL MODEL ####

# Select best model for entire dataset
fit_full <- randomForest(
  transit_demand ~
    policy_parks + policy_emergency + policy_gathering_low +
    policy_gathering_med + policy_gathering_hig + policy_border +
    cases_total + metric_initial_drop + days_since_pandemic +
    lagged_month_average + lockdown_total + work_from_home +
    past_average_month + emergency_length + gathering_low_length +
    gathering_hig_length + gathering_med_length, df)
summary(fit_full)

predict_full <- predict(fit_full, df, interval = "prediction", level = 0.9)
accuracy(predict_full, df$transit_demand)

df$demand_predict <- predict_full

# Graph changes in demand by select metro areas
df %>% 
  ggplot(aes(x = date)) +
  # Set graph theme
  theme_minimal() +
  # Plot demand as a line
  geom_line(aes(y = transit_demand), lwd = 0.6, color = orange) +
  # Plot predicted demand as a line
  geom_line(aes(y = demand_predict), lwd = 0.6, color = "darkgrey") +
  # Set axis labels and plot titles
  ylab("Change in Public Transit Ridership") +
  xlab("") +
  ggtitle("Change in Public Transit Ridership From Pre-Pandemic Baseline") +
  labs(subtitle = "Select Metro Areas, February 2020 - April 2022") +
  # Create line for normal pre-pandemic public transit demand
  geom_hline(yintercept = 0) +
  # Format axis ticks
  scale_y_continuous(
    labels = scales::percent, breaks = seq(-1, 1, .5), limits = c(-1, 1),
    minor_breaks = NULL) +
  scale_x_date(
    date_breaks = "2 months" , date_labels = "%b-%y",
    limits = c(floor_date(min(df$date), "month"),
               ceiling_date(max(df$date), "month"))) +
  # Customize graph appearance
  theme(
    text = element_text(family = "CMU Typewriter Text Bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "none") +
  # Create panels for each metro area
  facet_wrap(~metro_area, ncol = 5, scales = "free_x")

# Plot variable importance in model
graph_importance <- as.data.frame(varImpPlot(fit_full)) %>%
  # Add row names as a column
  rownames_to_column() %>%
  # Plot data
  ggplot(aes(x=reorder(rowname, IncNodePurity)), weight = IncNodePurity) +
  # Set graph theme
  theme_minimal() +
  # Add variable importance as bars
  geom_bar(aes(y = IncNodePurity), stat = "identity") +
  # Set axis labels and plot titles
  ylab("Variable Importance (Increase in Node Purity)") +
  xlab("") +
  labs(title = "How much did pandemic-related factors affect public transit ridership?",
       subtitle = paste("Relative importance of variables in ridership model",
                        "(Select cities, Feb 2020 - Apr 2022)"),
       caption = "Google. (2022). COVID-19 Community Mobility Reports.") +
  # Create line for zero
  geom_hline(yintercept = 0) +
  # Flip graph
  coord_flip() + 
  # Customize graph appearance
  theme(text = element_text(family = "CMU Bright", size = 16),
        plot.title = element_text(family = "CMU Bright SemiBold"),
        plot.subtitle = element_text(face = "plain"),
        plot.caption = element_text(size = 10, hjust = 0, vjust = -4),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, vjust = 4),
        plot.margin = unit(c(1, 1, 1, 1.2), "cm"),
        panel.grid.minor =  element_line(color = "#CCCCCC"),
        panel.grid.major = element_line(color = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA),
        panel.background = element_rect(fill = "#E6E6E6", color = NA),
        legend.position = "none")

# Save graph
png("./images/variable_importance.png", units = "in",
    width = 12, height = 8, res = 1000)
graph_importance
dev.off()