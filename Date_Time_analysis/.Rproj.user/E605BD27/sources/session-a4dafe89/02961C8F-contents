
# Clear environment and console
rm(list = ls()) 
cat("\014")

# Load required packages
#gc() #garbage collector to clean up the workspace.

options(repos = c(CRAN = "https://cran.rstudio.com"))


# List of required packages for Date and Time Data Analysis
packages <- c("lubridate", "dplyr", "stringr", "zoo", "tsibble", "ggplot2", "plotly", "readr", "forecast", "plotly", "prettydoc", "janitor", "flexdashboard", "knitr", "rmarkdown", "yaml", "flextable", "gt", "reactable", "tidyverse")

# Install and load packages with descriptions
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

# Install quarto if not already installed
if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")


# **Load required libraries**

library(lubridate) # Simplifies the manipulation of dates and times in R (e.g., formatting, extracting components)
library(dplyr) # Provides tools for data manipulation, helpful for filtering and summarizing date-based data
library(stringr) # Simplifies string manipulation

library(zoo) # For working with time series data, including rolling calculations and handling missing data in time series
library(tsibble) # Provides tools for handling time series data, including date-time indexes and features for forecasting

library(readr) # For reading date-time data from CSV
library(haven) #  data from other statistical software formats (SPSS, SAS, Stata)
library(forecast) # Useful for time series forecasting, especially when working with seasonal or trend-based data
library(ggplot2) # For visualizing date-time trends in data (e.g., time series plots)
library(janitor) # Tabulation, cleaning column names, adding totals and proportions
library (plotly)     # Interactive visualizations

# Documentation/reporting
library(prettydoc)  # Pretty document templates
library(flexdashboard)# Interactive dashboards
library(knitr)      # Dynamic report generation
library(rmarkdown)  # R Markdown document processing
library(quarto)  # For rendering and publishing documents with the Quarto framework
library(yaml)       # YAML document processing

# Tabulation
library(flextable)  # Flexible table formatting
library(gt)         # Grammar of tables
library(reactable)  # Interactive tables

setwd("C:/Users/User/Desktop/Materials_ Course and proposals/Course Related/DataCamp/Data/Synthetic_data")

patients <- read.csv("patients.csv")
vaccination <- read.csv("immunizations.csv")



# Convert column names to lowercase
colnames(patients) <- tolower(colnames(patients))
colnames(vaccination) <- tolower(colnames(vaccination))

# View data
# head(patients)
# head(vaccination)

str(patients)
str(vaccination)

# Check for missing IDs
patients$id[patients$id == ""] <- NA
print(sum(is.na(patients$id)))

# Check duplicates
patients <- patients %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(dup = row_number()) %>%
  ungroup()

table(patients$dup)  # Check if any duplicates exist



# Merge with vaccination data
# vaccination <- vaccination %>%
#   mutate(id = patient)
# 
# vacc_pt_merged <- left_join(patients, vaccination, by = "id" )
# 
vacc_pt_merged <- left_join(patients, vaccination, by = c("id" = "patient"))

str (vacc_pt_merged)


# Converting Character to Date Format


# Convert to date format
str (vacc_pt_merged$birthdate)
str (vacc_pt_merged$date)
date is YYYY-MM-DD HH:MM:SS format.

vacc_pt_merged <- vacc_pt_merged %>%
  mutate(
    birthdate = parse_date(birthdate, format = "%Y-%m-%d"),
    vacc_date = as.Date(strptime(date, format = "%Y-%m-%dT%H:%M:%SZ"))
  )

# Check structure
str (vacc_pt_merged$birthdate)
str (vacc_pt_merged$vacc_date)

# Check converted formats
glimpse(vacc_pt_merged)

# no missing values on dates
sum(is.na(vacc_pt_merged$birthdate)) 
sum(is.na(vacc_pt_merged$vacc_date))

# Check date ranges
summary(vacc_pt_merged$birthdate)
summary(vacc_pt_merged$vacc_date)



  # **Age and Service year Calculation**
  
vacc_pt_merged <- vacc_pt_merged %>%
  mutate(
    age_years = floor (as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25),
    
    # Calculate vaccination service year by extracting the year from vacc_date
    vacc_service_year = year(vacc_date)
  )

# tabulation
table(vacc_pt_merged$age_years)
table(vacc_pt_merged$vacc_service_year)


# **Calculating Time Differences**


vacc_pt_merged <- vacc_pt_merged %>%
  mutate(
    days_to_vax = as.numeric(vacc_date - birthdate),  # Difference in days
    weeks_to_vax = as.numeric(difftime(vacc_date,  birthdate,  units = "weeks")),  # Difference in weeks
    months_to_vax = as.numeric(difftime(vacc_date,  birthdate,  units = "days")) / 30.44,  # Approximate months (average days per month)
    years_to_vax = floor(as.numeric(difftime(vacc_date,  birthdate,  units = "days")) / 365.25)  # Approximate years (including leap years)
  )


# View summary statistics 
summary(vacc_pt_merged$days_to_vax)
summary(vacc_pt_merged$weeks_to_vax)
summary(vacc_pt_merged$months_to_vax)
summary(vacc_pt_merged$years_to_vax)




# **Extracting Date Components**


vacc_pt_merged <- vacc_pt_merged %>%
  mutate(
    # Basic date components
    vacc_day = day(vacc_date),
    vacc_month = month(vacc_date),
    vacc_month_name = month(vacc_date, label = TRUE, abbr = FALSE),  # Full month name
    vacc_year = year(vacc_date),
    vacc_weekday = wday(vacc_date, label = TRUE, abbr = FALSE),     # Full weekday name
    vacc_week = week(vacc_date),                                    # Week of year
    vacc_quarter = quarter(vacc_date),
    
    # Fiscal year (assuming June start)
    vacc_fy = ifelse(vacc_month >= 6, vacc_year, vacc_year - 1),
    
    # Season (Northern Hemisphere)
    vacc_season = case_when(
      vacc_month %in% 3:5 ~ "Spring",
      vacc_month %in% 6:8 ~ "Summer",
      vacc_month %in% 9:11 ~ "Autumn",
      TRUE ~ "Winter"  # December-February
    ),
    
    # Weekend flag
    is_weekend = wday(vacc_date) %in% c(1, 7),  # 1=Sunday, 7=Saturday
    
    # Days since specific reference date (e.g., pandemic start)
    days_since_ref = floor(as.numeric(vacc_date - as.Date("2020-01-01")))
  )



# Filtering Between Date Ranges

  
# Filter only vaccinations in 2022
vacc_2022 <- vacc_pt_merged %>%
  filter(vacc_date >= ymd("2022-01-01") & vacc_date <= ymd("2022-12-31"))

vacc_1524 <- vacc_pt_merged %>%
  filter(vacc_service_year >= 2015 & vacc_service_year <= 2024)


table(vacc_1524$vacc_year)

table(vacc_1524$race)

table(vacc_1524$ethnicity)


vacc_1524 <- vacc_1524 %>% 
  select(id, gender, race, birthdate, date, vacc_date, age_years, vacc_service_year, days_to_vax, weeks_to_vax, 
         months_to_vax, years_to_vax, vacc_day, vacc_month, vacc_month_name, vacc_year, vacc_weekday, vacc_week, 
         vacc_quarter, vacc_fy, vacc_season, is_weekend, days_since_ref, everything())

# ----------------------------Tabulations ------------------------------------



library(janitor)
library(dplyr)
library(flextable)

# Recode gender
vacc_1524 <- vacc_1524 %>%
  mutate(gender = case_when(
    gender == "F" ~ "Female",
    gender == "M" ~ "Male",
    TRUE ~ gender
  ))

# Convert time variables to character for consistency
vacc_1524 <- vacc_1524 %>%
  mutate(across(c(vacc_service_year, vacc_quarter, vacc_weekday), as.character))

# Create and label each table
tab_year <- vacc_1524 %>%
  tabyl(vacc_service_year, gender) %>%
  adorn_totals("row") %>%
  mutate(Category = "Year", .before = 1) %>%
  rename(Label = vacc_service_year)

tab_quarter <- vacc_1524 %>%
  tabyl(vacc_quarter, gender) %>%
  adorn_totals("row") %>%
  mutate(Category = "Quarter", .before = 1) %>%
  rename(Label = vacc_quarter)

tab_weekday <- vacc_1524 %>%
  tabyl(vacc_weekday, gender) %>%
  adorn_totals("row") %>%
  mutate(Category = "Weekday", .before = 1) %>%
  rename(Label = vacc_weekday)

# Combine all
tab_combined <- bind_rows(tab_year, tab_quarter, tab_weekday)

# Calculate column proportions within each Category group (excluding "Total" row)
tab_combined <- tab_combined %>%
  group_by(Category) %>%
  mutate(
    Female_Prop = round(100 * Female / sum(Female[Label != "Total"]), 1),
    Male_Prop = round(100 * Male / sum(Male[Label != "Total"]), 1)
  ) %>%
  ungroup()

# Format with flextable
ft <- flextable(tab_combined) %>%
  set_header_labels(
    Category = "Time Unit",
    Label = "Category",
    Female = "Female Count",
    Male = "Male Count",
    Female_Prop = "Female %",
    Male_Prop = "Male %"
  ) %>%
  add_header_lines(values = "Table: Vaccination counts and gender proportions (%) across Year, Quarter, and Weekday") %>%
  theme_box() %>%
  autofit()

ft



# Other options of creating table 

# Load library
library(gmodels)

# Make sure gender is clean
vacc_1524 <- vacc_1524 %>%
  mutate(gender = case_when(
    gender == "F" ~ "Female",
    gender == "M" ~ "Male",
    TRUE ~ gender
  ))

# CrossTable: Year vs Gender
cat("\n===== Vaccination Year vs Gender =====\n")
CrossTable(vacc_1524$vacc_service_year, vacc_1524$gender,
           prop.r = FALSE, prop.c = TRUE, prop.t = FALSE,
           prop.chisq = FALSE, format = "SPSS", digits = 1)

# CrossTable: Quarter vs Gender
cat("\n===== Vaccination Quarter vs Gender =====\n")
CrossTable(vacc_1524$vacc_quarter, vacc_1524$gender,
           prop.r = FALSE, prop.c = TRUE, prop.t = FALSE,
           prop.chisq = FALSE, format = "SPSS", digits = 1)

# CrossTable: Weekday vs Gender
cat("\n===== Vaccination Weekday vs Gender =====\n")
CrossTable(as.character(vacc_1524$vacc_weekday), vacc_1524$gender,
           prop.r = FALSE, prop.c = TRUE, prop.t = FALSE,
           prop.chisq = FALSE, format = "SPSS", digits = 1)



# ------------------------------📊 Visualizing Date-Based Trends---------------------

# Monthly Vaccination Trend in each Year

#| fig-alt: "Monthly vaccination trends over years"
vacc_1524 %>%
  count(vacc_year, vacc_month) %>%
  ggplot(aes(x = as.Date(paste(vacc_year, vacc_month, "01", sep = "-")), 
             y = n)) +
  geom_line(color = "#1f78b4",http://127.0.0.1:9651/graphics/plot_zoom_png?width=2380&height=1241 linewidth = 1) +
  geom_point(color = "#33a02c") +
  labs(title = "Monthly Vaccination Trends",
       x = "Timeline", y = "Vaccinations") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


vacc_1524 %>%
  mutate(month = month(vacc_date, label = TRUE, abbr = TRUE)) %>%  # Extract month from date
  group_by(month) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = count, group = 1)) +
  
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Total Vaccinations by Month",
    x = "Month",
    y = "Vaccination Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

## **Weekday Heatmap**

#| fig-alt: "Heatmap of vaccinations by weekday and month"
vacc_1524 %>%
  count(vacc_month_name, vacc_weekday) %>%
  ggplot(aes(x = vacc_weekday, y = vacc_month_name, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(title = "Vaccination Intensity by Weekday and Month",
       x = "Weekday", y = "Month") +
  theme_minimal() +
  theme(legend.position = "bottom")

## **Column Chart: Seasonal Vaccination Distribution**

vacc_1524 %>%
  mutate(month = as.numeric(format(vacc_date, "%m")),
         season = case_when(
           month %in% c(12, 1, 2) ~ "Summer",
           month %in% c(3, 4, 5) ~ "Autumn",
           month %in% c(6, 7, 8) ~ "Winter",
           month %in% c(9, 10, 11) ~ "Spring"
         )) %>%
  count(season) %>%
  ggplot(aes(x = season, y = n, fill = season)) +
  geom_col() +
  labs(
    title = "Seasonal Vaccination Distribution",
    x = "Season",
    y = "Vaccination Count"
  ) +
  theme_minimal()


## Vaccination proportion by year and race


# Proportion by vacc_year and race with cleaned race labels
df_plot_race <- vacc_1524 %>%
  mutate(
    race = ifelse(race %in% c("native", "other"), "Other", race),
    race = str_to_title(race),
    vacc_year = as.integer(floor(ashttp://127.0.0.1:9651/graphics/plot_zoom_png?width=2380&height=1241.numeric(vacc_year)))  # Ensure whole number
  ) %>%
  group_by(race, vacc_year) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(race) %>%
  mutate(
    total = sum(count),
    proportion = (count / total) * 100
  ) %>%
  ungroup()

# Line plot
ggplot(df_plot_race, aes(x = vacc_year, y = proportion, color = race)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(df_plot_race$vacc_year), max(df_plot_race$vacc_year), by = 2)) +
  labs(
    title = "Vaccination Proportion by Race and Year",
    x = "Vaccination Year",
    y = "Proportion (%)",
    color = "Race"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Bar plot
ggplot(df_plot_race, aes(x = vacc_year, y = proportion, fill = race)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = seq(min(df_plot_race$vacc_year), max(df_plot_race$vacc_year), by = 2)) +
  labs(
    title = "Vaccination Proportion by Race and Year",
    x = "Vaccination Year",
    y = "Proportion (%)",
    fill = "Race"
  ) +
  theme_minimal()

# Proportion by vacc_year and gender

vacc_1524 %>%
  mutate(
    gender = case_when(
      gender == "M" ~ "Male",
      gender == "F" ~ "Female",
      TRUE ~ gender
    ),
    vacc_year = as.integer(vacc_year)  # Ensure year is whole number
  ) %>%
  group_by(gender, vacc_year) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(
    total = sum(count),
    proportion = (count / total) * 100
  ) %>%
  ungroup() %>% 
  
  ggplot(aes(x = vacc_year, y = proportion, color = gender)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +  # Ensures whole number years
  labs(
    title = "Vaccination Proportion by Gender and Year",
    x = "Vaccination Year",
    y = "Proportion (%)",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )



