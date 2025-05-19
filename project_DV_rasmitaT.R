# R-CODE Project #
# Name: Rasmita Timalsina #
# Date: 30 April 2024 #
# Project: Data visualization of NIST time scale data and leap seconds #

# Load necessary libraries
library(tidyverse)  # Data manipulation and visualization tools
library(ggplot2)    # Plotting library
library(scales)     # Adjust scales in ggplot2
library(zoo)        # Rolling averages for time series
library(Amelia)     # Missing data analysis
library(viridis)    # Color palettes (colorblind-friendly)
library(ggthemes)   # Enhanced ggplot2 themes
library(patchwork)  # Combine multiple plots
dev.new()           # Create a new graphics window
library(lubridate)

# Set working directory
setwd("/Users/rasmitatimalsina/Documents/data_visualization/project")

### Data Wrangling and Basic EDA : Visualization and Ansatz ###

# ................................................................ #
# Load and analyze Table 1: UT1-UTC and USNO-NIST Time Offset Data #
# ................................................................ #

# Load and combine all Table 1 CSV files matching the pattern
table1_files <- list.files(pattern = "^table_1_.*\\.csv$")
if(length(table1_files) == 0) stop("No CSV files found matching the pattern.")
table1_data <- map_df(table1_files, read_csv)

# Clean data: Rename columns, remove units, convert to numeric, and parse dates
table1_data <- table1_data %>%
  rename(
    ut1_utc = `UT1-UTC(±1 ms)`,  # Rename to shorter, clean column name
    usno_nist_offset = `UTC(USNO, MC) - UTC(NIST) (±5 ns)`  # Rename for clarity
  ) %>%
  mutate(
    ut1_utc = as.numeric(str_remove(ut1_utc, " ms")),  # Strip "ms" and convert to numeric
    usno_nist_offset = as.numeric(str_remove(usno_nist_offset, " ns")),  # Strip "ns" and convert
    Date = as.Date(Date)  # Convert 'Date' to proper Date format
  )

# Plot UT1-UTC over time with LOESS smoothing to highlight temporal trend
p1 <- ggplot(table1_data, aes(x = Date, y = ut1_utc)) +
  geom_line(color = "#1f78b4", size = 1) +  # Raw UT1-UTC line
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +  # LOESS trend
  labs(
    title = "Trend of UT1-UTC (Universal Time Offset)",
    subtitle = "Includes smoothed LOESS fit",
    y = "UT1-UTC (ms)", x = "Date"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Plot USNO-NIST time offset with reference line at zero
p2 <- ggplot(table1_data, aes(x = Date, y = usno_nist_offset)) +
  geom_line(color = "#e31a1c", size = 1) +  # Raw offset data
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Zero reference line
  labs(
    title = "USNO-NIST Time Transfer Offset",
    subtitle = "High-precision nanosecond deviations",
    y = "Offset (ns)", x = "Date"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Combine both plots into a single layout for side-by-side interpretation
combined_plot <- p1 + p2 +
  plot_layout(ncol = 1, heights = c(1, 1)) +
  plot_annotation(
    title = "Time Scale Variations in 2024",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )
print(combined_plot)

# Add a 'Year' column and compute summary statistics for UT1-UTC by year
table1_data <- table1_data %>% mutate(Year = year(Date))
ut1_summary <- table1_data %>%
  group_by(Year) %>%
  summarize(
    mean_UT1_UTC = mean(ut1_utc, na.rm = TRUE),  # Mean offset
    sd_UT1_UTC = sd(ut1_utc, na.rm = TRUE),      # Standard deviation
    min_UT1_UTC = min(ut1_utc, na.rm = TRUE),    # Minimum value
    max_UT1_UTC = max(ut1_utc, na.rm = TRUE),    # Maximum value
    .groups = "drop"
  )
print(ut1_summary)

# Visualize distribution of UT1-UTC values over the full period
ggplot(table1_data, aes(x = ut1_utc)) +
  geom_histogram(binwidth = 0.2, fill = "#2C7BB6", color = "black") +
  labs(
    title = "Distribution of UT1-UTC Values (2020–2024)",
    x = "UT1-UTC (milliseconds)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot UT1-UTC as a time series
ggplot(table1_data, aes(x = Date, y = ut1_utc)) +
  geom_line(color = "#D7191C") +
  labs(
    title = "UT1-UTC Over Time (2020–2024)",
    x = "Date",
    y = "UT1-UTC (milliseconds)"
  ) +
  theme_minimal()

# Compute 12-month rolling average of UT1-UTC values
table1_data <- table1_data %>%
  arrange(Date) %>%
  mutate(rolling_mean_UT1 = rollmean(ut1_utc, k = 12, fill = NA))  # Apply rolling mean

# Check for any missing values in Date or rolling_mean_UT1
table1_data %>%
  filter(is.na(Date) | is.na(rolling_mean_UT1))

# Filter out rows with missing values for clean visualization
table1_data_clean <- table1_data %>%
  filter(!is.na(Date) & !is.na(rolling_mean_UT1))

# Plot the smoothed 12-month rolling mean of UT1-UTC
ggplot(table1_data_clean, aes(x = Date, y = rolling_mean_UT1)) +
  geom_line(color = "#FDAE61") +
  labs(
    title = "12-Month Rolling Mean of UT1-UTC",
    x = "Date",
    y = "Rolling Mean (ms)"
  ) +
  theme_minimal()

# Optional: Plot UT1-UTC variation by year using faceted layout
ggplot(table1_data, aes(x = Date, y = ut1_utc)) +
  geom_point(color = "blue", alpha = 0.6) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(
    title = "UT1-UTC Variation by Year",
    x = "Date",
    y = "UT1-UTC (milliseconds)"
  ) +
  theme_minimal()



# -------------------------------------------------------------------
# Load and Analyze DUT1 Corrections Table 2: : NIST Time Data (2020–2024)
# -------------------------------------------------------------------



# -------------------------------------------------------------------
# 1. Load and combine all Table 2 files
# -------------------------------------------------------------------
table2_files <- list.files(pattern = "^table_2_.*\\.csv$")
table2_data <- map_df(table2_files, read_csv)

# -------------------------------------------------------------------
# 2. Clean and format data
# -------------------------------------------------------------------
table2_data <- table2_data %>%
  rename(date = `Start Date`, dut1_correction = `DUT1 Correction`) %>%
  mutate(
    dut1_correction = as.numeric(str_remove(trimws(dut1_correction), "\\s*s$")),
    date = as.Date(date)
  ) %>%
  arrange(date)

# -------------------------------------------------------------------
# 3. Add Year Column
# -------------------------------------------------------------------
table2_data <- table2_data %>%
  mutate(Year = lubridate::year(date))

# -------------------------------------------------------------------
# 4. Summary Statistics
# -------------------------------------------------------------------
# 4a. Summary by Year
dut1_summary <- table2_data %>%
  group_by(Year) %>%
  summarize(
    mean_DUT1 = mean(dut1_correction, na.rm = TRUE),
    sd_DUT1 = sd(dut1_correction, na.rm = TRUE),
    min_DUT1 = min(dut1_correction, na.rm = TRUE),
    max_DUT1 = max(dut1_correction, na.rm = TRUE),
    .groups = "drop"
  )

print(dut1_summary)

# 4b. Overall Summary
summary(table2_data$dut1_correction)

# -------------------------------------------------------------------
# 5. Visualizations
# -------------------------------------------------------------------

# 5a. Line plot of DUT1 over time
ggplot(table2_data, aes(x = date, y = dut1_correction)) +
  geom_line(color = "#1B9E77") +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    title = "DUT1 Correction Over Time",
    x = "Date",
    y = "DUT1 Correction (seconds)"
  ) +
  scale_x_date(labels = date_format("%b %Y"), 
               breaks = seq(min(table2_data$date), max(table2_data$date), by = "6 months")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10)
  )

# 5b. Histogram of all DUT1 corrections
ggplot(table2_data, aes(x = dut1_correction)) +
  geom_histogram(binwidth = 0.1, fill = "#7570B3", color = "black") +
  labs(
    title = "Histogram of DUT1 Corrections (2020–2024)",
    x = "DUT1 Correction (seconds)",
    y = "Frequency"
  ) +
  theme_minimal()

# 5c. Histogram of DUT1 corrections faceted by Year
ggplot(table2_data, aes(x = dut1_correction)) +
  geom_histogram(binwidth = 0.1, fill = "#984EA3", color = "white") +
  facet_wrap(~ Year, scales = "free_y") +
  labs(
    title = "Distribution of DUT1 Correction Values by Year",
    x = "DUT1 Correction (seconds)",
    y = "Frequency"
  ) +
  theme_minimal()

# 5d. Time Series Plot by Year (points only)
ggplot(table2_data, aes(x = date, y = dut1_correction)) +
  geom_point(color = "#4DAF4A", alpha = 0.6) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(
    title = "DUT1 Corrections by Year",
    x = "Date",
    y = "DUT1 Correction (seconds)"
  ) +
  theme_minimal()

# -------------------------------------------------------------------
# 6. Rolling Mean Calculation and Plot
# -------------------------------------------------------------------

# 6a. Compute 12-month rolling average
table2_data <- table2_data %>%
  arrange(date) %>%
  mutate(rolling_mean_DUT1 = rollmean(dut1_correction, k = 12, fill = NA, align = "right"))

# 6b. Filter valid rows
table2_data_clean <- table2_data %>%
  filter(!is.na(date) & !is.na(rolling_mean_DUT1))

# 6c. Plot rolling mean
ggplot(table2_data_clean, aes(x = date, y = rolling_mean_DUT1)) +
  geom_line(color = "#E41A1C") +
  labs(
    title = "12-Month Rolling Mean of DUT1 Corrections",
    x = "Date",
    y = "Rolling Mean (seconds)"
  ) +
  theme_minimal()


# -------------------------------------------------------------------
# Load and Analyze Table 3: UT1-UTC Variation Visualization
# -------------------------------------------------------------------


# Load and combine all Table 3 files
table3_files <- list.files(pattern = "^table_3_.*\\.csv$")
table3_data <- purrr::map_df(table3_files, read.csv)

# Check the column names to ensure correct reference for renaming
colnames(table3_data)

# Clean and format data
table3_data <- table3_data %>%
  rename(date = `Date`, ut1_utc_ns = `UTC.UTC.NIST...ns`) %>%  # Corrected column name
  mutate(
    ut1_utc_ns = as.numeric(ut1_utc_ns),  # Convert to numeric if needed
    ut1_utc_ms = ut1_utc_ns / 1e6,  # Convert nanoseconds to milliseconds
    date = as.Date(date)
  ) %>%
  arrange(date)

# Summary statistics for UT1-UTC
summary_stats <- table3_data %>%
  summarise(
    mean_UT1_UTC = mean(ut1_utc_ms, na.rm = TRUE),
    sd_UT1_UTC = sd(ut1_utc_ms, na.rm = TRUE),
    min_UT1_UTC = min(ut1_utc_ms, na.rm = TRUE),
    max_UT1_UTC = max(ut1_utc_ms, na.rm = TRUE)
  )

print(summary_stats)

# Plot UT1-UTC variation
ggplot(table3_data, aes(x = date, y = ut1_utc_ms)) +
  geom_line(color = "purple") +
  labs(title = "UT1-UTC Variation", y = "UT1-UTC (ms)", x = "Date") +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = scales::date_breaks("1 month")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        axis.text.y = element_text(size = 10))

# -------------------------------------------------------------------
#  Visualizations
# -------------------------------------------------------------------

# Line plot with smoothed LOESS curve for UT1-UTC variation
ggplot(table3_data, aes(x = date, y = ut1_utc_ms)) +
  geom_line(color = "purple") +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    title = "UT1-UTC Variation Over Time",
    x = "Date",
    y = "UT1-UTC (milliseconds)"
  ) +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = scales::date_breaks("2 months")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10)
  )

# Histogram: Distribution of UT1-UTC values (milliseconds)
ggplot(table3_data, aes(x = ut1_utc_ms)) +
  geom_histogram(bins = 20, fill = "darkorchid", color = "white") +
  labs(
    title = "Distribution of UT1-UTC Values",
    x = "UT1-UTC (milliseconds)",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot: UT1-UTC values by year
ggplot(table3_data, aes(x = factor(format(date, "%Y")), y = ut1_utc_ms)) +
  geom_boxplot(fill = "darkorchid", color = "white") +
  labs(
    title = "UT1-UTC Variation by Year",
    x = "Year",
    y = "UT1-UTC (milliseconds)"
  ) +
  theme_minimal()

# Scatter plot to check for trends over time
ggplot(table3_data, aes(x = date, y = ut1_utc_ms)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "UT1-UTC Variation Scatter Plot", x = "Date", y = "UT1-UTC (milliseconds)") +
  theme_minimal()

# Calculate rolling average (12 months)
table3_data <- table3_data %>%
  arrange(date) %>%
  mutate(rolling_mean_UT1_UTC = zoo::rollmean(ut1_utc_ms, k = 12, fill = NA))

# Check for NA or problematic values
table3_data %>%
  filter(is.na(rolling_mean_UT1_UTC) | rolling_mean_UT1_UTC == Inf | rolling_mean_UT1_UTC == -Inf)
table3_data_clean <- table3_data %>%
  filter(!is.na(rolling_mean_UT1_UTC) & rolling_mean_UT1_UTC != Inf & rolling_mean_UT1_UTC != -Inf)


# Plot rolling mean of UT1-UTC variation
ggplot(table3_data_clean, aes(x = date, y = rolling_mean_UT1_UTC)) +
  geom_line(color = "blue") +
  labs(
    title = "12-Month Rolling Mean of UT1-UTC Variation",
    x = "Date",
    y = "Rolling Mean (ms)"
  ) +
  theme_minimal()


# -------------------------------------------------------------------
# Leap Seconds and DUT1 Corrections, UT1_UTC Analysis
# -------------------------------------------------------------------

# Load datasets
leap_seconds <- read.csv("leap_seconds.csv", stringsAsFactors = FALSE)
dut1_corrections <- read.csv("dut1_corrections.csv", stringsAsFactors = FALSE)
ut1_utc <- read.csv("ut1_utc.csv", stringsAsFactors = FALSE)

# Clean leap_seconds data
leap_seconds$Date_1 <- as.Date(leap_seconds$Date_1, format = "%Y-%m-%d")
leap_seconds$MJD_1 <- as.numeric(leap_seconds$MJD_1)

# Clean dut1_corrections data
dut1_corrections$Start.Date <- as.Date(dut1_corrections$Start.Date, format = "%Y-%m-%d")
dut1_corrections$DUT1_Correction <- as.numeric(dut1_corrections$DUT1_Correction)

# Clean ut1_utc data
ut1_utc$Date <- as.Date(ut1_utc$Date, format = "%Y-%m-%d")
ut1_utc$UT1_UTC <- as.numeric(gsub(" ms", "", ut1_utc$UT1.UTC..1.ms.))
ut1_utc$UTC_USNO_NIST <- as.numeric(gsub(" ns", "", ut1_utc$UTC.USNO.MC....UTC.NIST....5.ns.))

# Check for missing values
cat("Missing values in leap_seconds:", sum(is.na(leap_seconds)), "\n")
cat("Missing values in dut1_corrections:", sum(is.na(dut1_corrections)), "\n")
cat("Missing values in ut1_utc:", sum(is.na(ut1_utc)), "\n")

# Visualize missingness for better understanding
missmap(leap_seconds, main = "Missing Values in Leap Seconds Data", col = c("yellow", "black"))
missmap(dut1_corrections, main = "Missing Values in DUT1 Corrections Data", col = c("yellow", "black"))
missmap(ut1_utc, main = "Missing Values in UT1-UTC Data", col = c("yellow", "black"))

# Summary statistics with median and IQR
cat("\n--- Summary of Leap Seconds ---\n")
print(summary(leap_seconds))

cat("\n--- Summary of DUT1 Corrections ---\n")
print(summary(dut1_corrections))

cat("\n--- Summary of UT1-UTC Measurements ---\n")
print(summary(ut1_utc))

# Structures
cat("\n--- Structure of leap_seconds ---\n")
str(leap_seconds)

cat("\n--- Structure of dut1_corrections ---\n")
str(dut1_corrections)

cat("\n--- Structure of ut1_utc ---\n")
str(ut1_utc)

# Visualization 1: UT1-UTC over time (Improved)
ggplot(ut1_utc, aes(x = Date, y = UT1_UTC)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1) +  # Highlight points for anomalies
  labs(title = "UT1-UTC Over Time", x = "Date", y = "UT1-UTC (ms)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.5), panel.grid.minor = element_blank())

# Visualization 2: DUT1 Correction Over Time (Improved)
ggplot(dut1_corrections, aes(x = Start.Date, y = DUT1_Correction)) +
  geom_step(color = "darkgreen", size = 1) +
  labs(title = "DUT1 Correction Over Time", x = "Start Date", y = "DUT1 Correction (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.5), panel.grid.minor = element_blank())

# Check for missing or invalid date values
leap_seconds %>%
  filter(is.na(Date_1) | !is.Date(Date_1))

# Clean the data by removing rows with missing or invalid Date_1 values
leap_seconds_clean <- leap_seconds %>%
  filter(!is.na(Date_1))

# Visualization 3: Leap Seconds Timeline (Date_1 only)
# Plot Leap Seconds Timeline
ggplot(leap_seconds_clean, aes(x = Date_1)) +
  geom_point(aes(y = 1), color = "red", size = 4) +  # Larger points for visibility
  scale_y_continuous(breaks = NULL) +
  labs(title = "Leap Seconds Introduced (2005–2024)", x = "Date", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 years") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Convert all Date_* columns to Date type
leap_seconds$Date_2 <- as.Date(leap_seconds$Date_2, format = "%Y-%m-%d")
leap_seconds$Date_3 <- as.Date(leap_seconds$Date_3, format = "%Y-%m-%d")
leap_seconds$Date_4 <- as.Date(leap_seconds$Date_4, format = "%Y-%m-%d")

# Pivot longer
leap_long <- leap_seconds %>%
  pivot_longer(cols = starts_with("Date_"), 
               names_to = "Leap_Type", 
               values_to = "Leap_Date") %>%
  filter(!is.na(Leap_Date)) %>%
  mutate(Leap_Date = as.Date(Leap_Date))

# Check the result
head(leap_long)

# Visualization 4: All leap second dates in long format with faceting
ggplot(leap_long, aes(x = Leap_Date)) +
  geom_point(aes(y = 1), color = "darkred", size = 2) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Leap Seconds Over Time", x = "Date", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 years") +
  facet_wrap(~Leap_Type, scales = "free_y", ncol = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Additional checks for outliers 

# Set up a 2-row layout to display both boxplots
par(mfrow = c(2, 1))  # 2 rows, 1 column layout

# Adjust margins for better spacing
par(mar = c(5, 6, 6, 3))  # Top, Left, Bottom, Right margins (adjusted for clarity)

# UT1-UTC Distribution Boxplot 
boxplot(ut1_utc$UT1_UTC,
        main = "UT1-UTC Distribution",
        ylab = "UT1-UTC (ms)",  # Adding unit (milliseconds)
        col = "lightblue",
        border = "blue",
        notch = FALSE,  # Removed notches
        horizontal = TRUE,
        cex.axis = 1.2,
        cex.lab = 1.3,
        cex.main = 1.5,
        boxwex = 0.4,
        ylim = c(min(ut1_utc$UT1_UTC) - 2, max(ut1_utc$UT1_UTC) + 2),
        grid = TRUE)

# DUT1 Correction Distribution Boxplot 
boxplot(dut1_corrections$DUT1_Correction,
        main = "DUT1 Correction Distribution",
        ylab = "DUT1 Correction (s)",  # Adding unit (seconds)
        col = "lightgreen",
        border = "darkgreen",
        notch = FALSE,  # Removed notches
        horizontal = TRUE,
        cex.axis = 1.2,
        cex.lab = 1.3,
        cex.main = 1.5,
        boxwex = 0.4,
        ylim = c(min(dut1_corrections$DUT1_Correction) - 0.1, max(dut1_corrections$DUT1_Correction) + 0.1),
        grid = TRUE)
