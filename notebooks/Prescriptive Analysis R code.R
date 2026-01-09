# =========================
# Prescriptive: Weighted Mean Predicted Price (Recommended)
# =========================

req <- c("readxl","dplyr","ggplot2","geosphere","scales","tidyr","purrr")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(req, library, character.only = TRUE))

# 1) Load data
data_path <- "C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.xlsx"
df <- readxl::read_excel(data_path)

# 2) Clean & filter (same as predictive)
df <- df %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(price)) %>%
  filter(price <= 1000)

# 3) If you have predicted prices, use them
if (!"pred_price" %in% names(df)) df$pred_price <- df$price

# 4) Distance (miles)
dist_miles <- function(lat1, lon1, lat2, lon2) {
  meters <- geosphere::distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))
  meters / 1609.344
}




# ---- 1) Setup & load data ----
library(dplyr)

# Path to your original Airbnb file
airbnb <- read.csv("C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv",
                   stringsAsFactors = FALSE)

# Use the same price cutoff you used in modeling
PRICE_CUTOFF <- 1000

# Keep only rows within the cap and with needed columns
needed_cols <- c("latitude", "longitude", "neighbourhood_group",
                 "minimum_nights","number_of_reviews","reviews_per_month",
                 "calculated_host_listings_count","availability_365",
                 "room_type")

presc_df <- airbnb[airbnb$price <= PRICE_CUTOFF, ]
presc_df <- presc_df[complete.cases(presc_df[ , needed_cols]), ]


# ---- 2) Predict RF price for these rows ----
# Assumes rf_fit already exists from your predictive script
presc_df$rf_price <- predict(rf_fit, newdata = presc_df)


# ---- 3) Haversine distance function (miles) ----
deg2rad <- function(x) x * pi / 180

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  R <- 3959  # Earth radius in miles
  phi1 <- deg2rad(lat1)
  phi2 <- deg2rad(lat2)
  dphi <- deg2rad(lat2 - lat1)
  dlambda <- deg2rad(lon2 - lon1)
  
  a <- sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlambda/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# ---- 4) Set your anchor location & radius ----
# Use the optimal anchor from your prescriptive write-up
anchor_lat <- 40.73   # Midtown Manhattan approx
anchor_lon <- -73.97
RADIUS_MILES <- 3     # or 1.75 if you want the optimal radius only

# Compute distance of each listing to the anchor
presc_df$distance_mi <- haversine_miles(
  lat1 = presc_df$latitude,
  lon1 = presc_df$longitude,
  lat2 = anchor_lat,
  lon2 = anchor_lon
)

# Filter to listings within the chosen radius
inside <- presc_df[presc_df$distance_mi <= RADIUS_MILES, ]


# ---- 5) Compute neighborhood-level table ----
results_tbl <- inside %>%
  group_by(neighbourhood_group) %>%
  summarise(
    mean_rf_price  = mean(rf_price, na.rm = TRUE),
    mean_distance  = mean(distance_mi, na.rm = TRUE),
    weighted_score = mean(rf_price / (1 + distance_mi), na.rm = TRUE),
    n_listings     = n()
  ) %>%
  arrange(desc(weighted_score))

print(results_tbl)








