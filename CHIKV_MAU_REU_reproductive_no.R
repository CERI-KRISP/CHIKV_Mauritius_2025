## Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(EpiEstim)
library(ggplot2)
library(MMWRweek)

## Read in the Excel file
cases <- read_excel("cases.xlsx")
View(cases)

# Ensure date column is correctly formatted
cases$dates <- as.Date(cases$dates)

summary(cases$dates) #13 March - 31 May 2025

# Aggregate cases by collection date
daily_cases <- cases %>%
  group_by(dates) %>%
  count(dates, name = "cases") 

# Rename 'cases' column to 'I' as required by EpiEstim
incidence_data <- daily_cases %>%
  rename(I = cases)

# Create a full set of dates from min to max (required by EpiEstim)
daily_cases_alldates <- data.frame(
  dates = seq(min(daily_cases$dates), max(daily_cases$dates), by = "day")
)

# Join with your existing data, fill missing days with 0
incidence_data <- daily_cases_alldates %>%
  left_join(daily_cases, by = "dates") %>%
  mutate(I = ifelse(is.na(cases), 0, cases)) %>%
  select(dates, I)


## Estimate R(t) using EpiEstim
res_13.3days <- estimate_R(
  incid = incidence_data,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 13.3, 
    std_si = 5.4
  ))
)

# Plot the estimated R over time
plot(res_13.3days)
plot(res_13.3days, 'R')

# Extract the summary results from EpiEstim

res_13.3days$R$t_end_dates <- incidence_data$dates[res_13.3days$R$t_end]
df_13.3 <- res_13.3days$R


# Create a custom plot
p2 <-ggplot(df_13.3, aes(x = t_end_dates, y = `Mean(R)`)) +
  geom_line(color = "firebrick4") +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.3, fill = "firebrick4") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    x = " ", 
    y = "R"
  ) +
  scale_x_date(
    date_labels = "%d %b %y",
    date_breaks = "2 week"
  ) +
  coord_cartesian(ylim = c(0, 5)) +  # Set y-axis limits
  theme_classic()+
  theme(panel.grid.major = element_line(color = "grey",
                                        size = 0.1)) +
  theme(
    plot.margin = margin(10, 20, 10, 10)  # top, right, bottom, left
  )

p2


#Manually imported Reunion_weekly_cases xlsx
#Convert column I to vector
REU_vec_Aug24 <- Reunion_weekly_cases_Aug24$I

## Estimate R(t) using EpiEstim
res_REU_weekly_Aug24 <- estimate_R_agg(incid = REU_vec_Aug24,
                                       dt = 7L, # aggregation window of the data
                                       dt_out = 14L, # desired sliding window length
                                       iter = 10L,
                                       method = "parametric_si",
                                       config = make_config(list(
                                         mean_si = 13.3,
                                         std_si = 5.4))
)


# Plot the estimated R over time
plot(res_REU_weekly_Aug24, 'R')

#Plot cases to check output
REU_cases <-ggplot(Reunion_weekly_cases, aes(x = Week, y = I)) +
  geom_col() +
  labs(
    title = "Cases",
    x = " ", 
    y = "Incidence"
  ) +
  theme_classic()

REU_cases

#Format dates for plotting from August 2024

start_date2 <- as.Date("2024-08-12")
# Dates corresponding to R estimates (midpoint of the time windows)
window_length2 <- res_REU_weekly_Aug24$R$t_end[1] - res_REU_weekly_Aug24$R$t_start[1] + 1

r_dates2 <- start_date2 + res_REU_weekly_Aug24$R$t_start + floor(window_length / 2) - 1

res_REU_weekly_Aug24$R$date <- r_dates2  # Add the date column to the R dataframe

REU_plot2 <- ggplot(res_REU_weekly_Aug24$R, aes(x = date, y = `Mean(R)`)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),
              alpha = 0.3, fill = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x = "",
       y = "R") +
  scale_x_date(
    #limits = as.Date(c("2025-01-09", "2025-06-02")),
    date_labels = "%d %b\n%y",
    date_breaks = "2 week"
  ) +
  theme_minimal()+ylim(0,10)+
  ggtitle("Reunion")+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

REU_plot2

# Extract the summary results 

res_REU_weekly_Aug24$R$t_end_dates <- incidence_data$dates[res_REU_weekly_Aug24$R$t_end]
REU <- res_REU_weekly_Aug24$R
write.csv(REU, "REU_R_estimates_Aug24.csv")

##combined plot

combined <- ggplot() +
  # Reunion (REU)
  geom_line(
    data = res_REU_weekly_Aug24$R,
    aes(x = as.Date(date), y = `Mean(R)`),
    color = "blue", size = 0.7
  ) +
  geom_ribbon(
    data = res_REU_weekly_Aug24$R,
    aes(x = as.Date(date), ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),
    fill = "blue", alpha = 0.3
  ) +
  
  # Mauritius (MAU)
  geom_line(
    data = df_13.3,
    aes(x = as.Date(t_end_dates), y = `Mean(R)`),
    color = "firebrick4", size = 0.7
  ) +
  geom_ribbon(
    data = df_13.3,
    aes(x = as.Date(t_end_dates), ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`),
    fill = "firebrick4", alpha = 0.3
  ) +
  
  # Reference line at R = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  
  # Axes and labels
  scale_x_date(
    #limits = as.Date(c("2025-01-09", "2025-06-02")),
    date_labels = "%d %b\n%y",
    date_breaks = "3 weeks"
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    x = "",
    y = "Reproduction Number (R)"
  ) +
  theme_minimal() 
combined
