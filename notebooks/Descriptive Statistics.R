# 1 Set file path
data_file <- "C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv"

# Load dataset
airbnb <- read.csv(data_file)

# Disable scientific notation
options(scipen = 999)


# Drop unwanted columns
airbnb_clean <- subset(airbnb, select = -c(id, name, host_id, host_name, baths,number_of_reviews_ltm, bedrooms, license))
# Descriptive summary
summary(airbnb_clean)


#2 measure of location & Central tendency
# Disable scientific notation
options(scipen = 999)

# --- Mean ---
mean_price <- mean(airbnb$price, na.rm = TRUE)
mean_min_nights <- mean(airbnb$minimum_nights, na.rm = TRUE)

# --- Median ---
median_price <- median(airbnb$price, na.rm = TRUE)
median_min_nights <- median(airbnb$minimum_nights, na.rm = TRUE)

# --- Mode function ---
get_mode <- function(x) {
  x <- na.omit(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

mode_price <- get_mode(airbnb$price)
mode_min_nights <- get_mode(airbnb$minimum_nights)

# --- Results table ---
results <- data.frame(
  Statistic = c("Mean", "Median", "Mode"),
  Price = c(mean_price, median_price, mode_price),
  Minimum_Nights = c(mean_min_nights, median_min_nights, mode_min_nights)
)

print(results)



#3. Measure of Dispersion
# Disable scientific notation
options(scipen = 999)

# --- Standard Deviation & Variance ---
sd_price <- sd(airbnb$price, na.rm = TRUE)
var_price <- var(airbnb$price, na.rm = TRUE)

sd_min_nights <- sd(airbnb$minimum_nights, na.rm = TRUE)
var_min_nights <- var(airbnb$minimum_nights, na.rm = TRUE)

# --- Results table ---
results_dispersion <- data.frame(
  Statistic = c("Standard Deviation", "Variance"),
  Price = c(sd_price, var_price),
  Minimum_Nights = c(sd_min_nights, var_min_nights)
)

print(results_dispersion)



#4. Histogram

options(scipen = 999)

par(mfrow = c(1, 2))  # 1 row, 2 plots

# Price (all values)
hist(airbnb$price, breaks = 100,
     main = "Price Histogram (All Listings)",
     xlab = "Price ($)",
     col = "skyblue",
     border = "black")

# Price (zoomed ≤1000)
hist(airbnb$price[airbnb$price <= 1000], breaks = 50,
     main = "Price Histogram (≤ $1000)",
     xlab = "Price ($)",
     col = "yellow",
     border = "black")

par(mfrow = c(1, 1))  # reset layout


#Histogram minimum nights
par(mfrow = c(1, 2))  # 1 row, 2 plots

# Minimum nights (all values)
hist(airbnb$minimum_nights, breaks = 100,
     main = "Minimum Nights Histogram (All)",
     xlab = "Minimum nights",
     col = "lightgreen",
     border = "black")

# Minimum nights (zoomed ≤40)
hist(airbnb$minimum_nights[airbnb$minimum_nights <= 40], breaks = 40,
     main = "Minimum Nights Histogram (≤ 40)",
     xlab = "Minimum nights",
     col = "orange",
     border = "black")

par(mfrow = c(1, 1))  # reset layout


#5 CummUlative freq

options(scipen = 999)

# Define breaks up to 1000 in steps of 100, then one big bin above
breaks_price <- c(seq(0, 1000, by = 100), max(airbnb$price, na.rm = TRUE))

h_price <- hist(airbnb$price, breaks = breaks_price, plot = FALSE)

cum_freq_price <- data.frame(
  Range = paste(head(breaks_price, -1), "-", tail(breaks_price, -1)),
  Frequency = h_price$counts,
  Cumulative_Frequency = cumsum(h_price$counts),
  Cumulative_Percent = round(cumsum(h_price$counts) / sum(h_price$counts) * 100, 2)
)
print(cum_freq_price)



# Define breaks up to 100 in steps of 10, then one big bin above
breaks_nights <- c(seq(0, 100, by = 10), max(airbnb$minimum_nights, na.rm = TRUE))

h_nights <- hist(airbnb$minimum_nights, breaks = breaks_nights, plot = FALSE)

cum_freq_nights <- data.frame(
  Range = paste(head(breaks_nights, -1), "-", tail(breaks_nights, -1)),
  Frequency = h_nights$counts,
  Cumulative_Frequency = cumsum(h_nights$counts),
  Cumulative_Percent = round(cumsum(h_nights$counts) / sum(h_nights$counts) * 100, 2)
)

print(cum_freq_nights)



#6. box plot
options(scipen = 999)

# Boxplot of Price by Room Type
boxplot(price ~ room_type,
        data = airbnb,
        main = "Boxplots of Price by Room Type",
        ylab = "Price ($)",
        col = c("red", "green", "blue", "purple"),  # different colors per category
        outline = TRUE,   # show outliers
        log = "y")        # log scale to handle extreme skew

boxplot(minimum_nights ~ room_type,
        data = airbnb,
        main = "Boxplots of Minimum Nights by Room Type",
        ylab = "Minimum Nights",
        col = c("orange", "lightblue", "lightgreen", "pink"),
        outline = TRUE,
        log = "y")   # log scale to handle extreme long stays



#7 Correlation analysis
# Select numeric columns
# Keep only numeric columns
num_vars <- airbnb_clean[, c("latitude", "longitude", "price", "minimum_nights",
                             "number_of_reviews", "reviews_per_month",
                             "calculated_host_listings_count", "availability_365")]

#Compute correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

print(cor_matrix)


# Install package if needed
install.packages("corrplot")
library(corrplot)

# Select numeric columns from your cleaned dataset
num_vars <- airbnb_clean[, c("latitude", "longitude", "price", "minimum_nights",
                             "number_of_reviews", "reviews_per_month",
                             "calculated_host_listings_count", "availability_365")]

# Correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

# Clear, high-quality correlation plot
corrplot(cor_matrix,
         method = "color",       # colored tiles
         type = "upper",         # only upper triangle
         tl.col = "black",       # label color
         tl.cex = 1.0,           # label size
         addCoef.col = "black",  # show correlation values
         number.cex = 0.8,       # coefficient text size
         col = colorRampPalette(c("red", "white", "blue"))(200), # red→blue scale
         mar = c(0,0,2,0),
         title = "Correlation Plot of Airbnb Numeric Variables")



#8. Scatter plot Diagrams
options(scipen = 999)

plot(airbnb_clean$number_of_reviews, airbnb_clean$price,
     main = "Scatterplot: Price vs Number of Reviews (Raw Data)",
     xlab = "Number of Reviews",
     ylab = "Price ($)",
     pch = 19, col = rgb(0.2,0.4,0.6,0.5))


normal_data <- subset(airbnb_clean, price <= 1000 & minimum_nights <= 40)

plot(normal_data$number_of_reviews, normal_data$price,
     main = "Scatterplot: Price vs Number of Reviews (≤ $1000, ≤ 40 nights)",
     xlab = "Number of Reviews",
     ylab = "Price ($)",
     pch = 19, col = rgb(0.8,0.2,0.2,0.5))


plot(airbnb_clean$availability_365, airbnb_clean$price,
     main = "Scatterplot: Price vs Availability (Raw Data)",
     xlab = "Availability (days)",
     ylab = "Price ($)",
     pch = 19, col = rgb(0.2,0.6,0.3,0.5))


plot(normal_data$availability_365, normal_data$price,
     main = "Scatterplot: Price vs Availability (≤ $1000, ≤ 40 nights)",
     xlab = "Availability (days)",
     ylab = "Price ($)",
     pch = 19, col = rgb(0.6,0.1,0.7,0.5))




#9 Outliers
# Price Z-score outliers
z_price <- scale(airbnb_clean$price)
outliers_price_z <- which(abs(z_price) > 3)

print(outliers_price_z)

# Minimum Nights Z-score outliers
z_nights <- scale(airbnb_clean$minimum_nights)
outliers_nights_z <- which(abs(z_nights) > 3)

print(outliers_nights_z)



# Price IQR outliers
Q1_p <- quantile(airbnb_clean$price, 0.25, na.rm = TRUE)
Q3_p <- quantile(airbnb_clean$price, 0.75, na.rm = TRUE)
IQR_p <- Q3_p - Q1_p
outliers_price_iqr <- which(airbnb_clean$price < (Q1_p - 1.5*IQR_p) |
                              airbnb_clean$price > (Q3_p + 1.5*IQR_p))

print(outliers_price_iqr)

# Minimum Nights IQR outliers
Q1_n <- quantile(airbnb_clean$minimum_nights, 0.25, na.rm = TRUE)
Q3_n <- quantile(airbnb_clean$minimum_nights, 0.75, na.rm = TRUE)
IQR_n <- Q3_n - Q1_n
outliers_nights_iqr <- which(airbnb_clean$minimum_nights < (Q1_n - 1.5*IQR_n) |
                               airbnb_clean$minimum_nights > (Q3_n + 1.5*IQR_n))

print(outliers_nights_iqr) 



# =========================
# 10) Missing Data (table + plot)
# ========================#
options(scipen = 999)
library(VIM)

df <- airbnb_clean

aggr(df,
     numbers   = TRUE,      # show counts
     prop      = FALSE,     # use counts instead of proportions
     sortVars  = TRUE,
     labels    = names(df),
     cex.axis  = 0.7,       # axis label size
     gap       = 3,
     ylab      = c("Missing Data", "Pattern"))





#Histogram
# install.packages("scales")  # <- if you don't have it
library(ggplot2)
library(scales)

# ---------- 0) Reusable theme ----------
theme_report <- theme_minimal(base_size = 15) +
  theme(
    plot.title   = element_text(face = "bold", size = 16),
    axis.title   = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    plot.caption = element_text(color = "grey35", size = 10)
  )

# Helper to add a vertical line & label (e.g., median)
vline_labeled <- function(xpos, label, color = "firebrick") {
  list(
    geom_vline(xintercept = xpos, color = color, linewidth = 0.7, linetype = 2),
    annotate("text", x = xpos, y = Inf, label = label, vjust = -0.6, color = color, size = 4)
  )
}

# ---------- 1) PRICE ----------
# Data
price_all <- airbnb$price
price_1k  <- airbnb$price[airbnb$price <= 1000]

# Key stats (match your descriptive tables)
p_med <- median(price_all, na.rm = TRUE)

p_all <- ggplot(data.frame(price = price_all), aes(price)) +
  geom_histogram(binwidth = 50, fill = "#00C4CC", color = "black", linewidth = 0.3) +
  coord_cartesian(xlim = c(0, quantile(price_all, 0.999, na.rm = TRUE))) +  # avoid a single massive bar
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Price Histogram (All Listings)",
       x = "Price ($)", y = "Frequency",
       caption = "Note: axis limited to 99.9th percentile to keep extreme outliers visible but not dominant.") +
  theme_report +
  vline_labeled(p_med, paste0("Median = $", comma(round(p_med, 2))))

p_1k <- ggplot(data.frame(price = price_1k), aes(price)) +
  geom_histogram(binwidth = 20, fill = "#00C4CC", color = "black", linewidth = 0.3) +
  scale_x_continuous(labels = label_dollar(accuracy = 1)) +
  labs(title = "Price Histogram (≤ $1,000)",
       x = "Price ($)", y = "Frequency") +
  theme_report +
  vline_labeled(median(price_1k, na.rm = TRUE),
                paste0("Median (≤$1K) = $", round(median(price_1k, na.rm = TRUE), 2)))

# Show side-by-side
gridExtra::grid.arrange(p_all, p_1k, ncol = 2)

# High-res export
ggsave("price_histograms_report.png",
       gridExtra::arrangeGrob(p_all, p_1k, ncol = 2),
       width = 14, height = 7, dpi = 320)

# ---------- 2) MINIMUM NIGHTS ----------
mn_all <- airbnb$minimum_nights
mn_40  <- airbnb$minimum_nights[airbnb$minimum_nights <= 40]

mn_med <- median(mn_all, na.rm = TRUE)

m_all <- ggplot(data.frame(minimum_nights = mn_all), aes(minimum_nights)) +
  geom_histogram(binwidth = 10, fill = "#FFA500", color = "black", linewidth = 0.3) +
  coord_cartesian(xlim = c(0, quantile(mn_all, 0.999, na.rm = TRUE))) +
  labs(title = "Minimum Nights Histogram (All Listings)",
       x = "Minimum nights", y = "Frequency",
       caption = "Note: axis limited to 99.9th percentile so extreme long-stay requirements remain visible.") +
  theme_report +
  vline_labeled(mn_med, paste0("Median = ", round(mn_med, 2), " nights"))

m_40 <- ggplot(data.frame(minimum_nights = mn_40), aes(minimum_nights)) +
  geom_histogram(binwidth = 1, fill = "#FFA500", color = "black", linewidth = 0.3) +
  labs(title = "Minimum Nights Histogram (≤ 40)",
       x = "Minimum nights", y = "Frequency") +
  theme_report +
  vline_labeled(1,  "1-night cluster",  "#2C7FB8") +
  vline_labeled(30, "30-night cluster", "#2C7FB8")

gridExtra::grid.arrange(m_all, m_40, ncol = 2)

ggsave("minimum_nights_histograms_report.png",
       gridExtra::arrangeGrob(m_all, m_40, ncol = 2),
       width = 14, height = 7, dpi = 320)


