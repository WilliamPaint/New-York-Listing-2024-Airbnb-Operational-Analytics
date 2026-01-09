############################################################
# DSCI 726 – Predictive Analysis (Airbnb NYC 2024)
# Target: price
# Models: Linear Regression (p-values + VIF), Decision Tree, Random Forest
# Author: William Kwame Paintsil (team)
############################################################

## ===================== 0) Setup ===========================
SEED <- 726
PRICE_CUTOFF <- 1000   # outlier cut for price
TEST_PROP <- 0.30      # 70/30 split
P_THRESH <- 0.05       # LM p-value threshold

set.seed(SEED)

needed_pkgs <- c("ggplot2","rpart","rpart.plot","randomForest","car","gridExtra")
to_install <- setdiff(needed_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(needed_pkgs, library, character.only = TRUE))

## ===================== 1) Utilities =======================
# get_metrics(): return RMSE, MAE, R2
get_metrics <- function(actual, predicted) {
  # Returns a named list with RMSE, MAE, R2 for numeric vectors of equal length
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  list(RMSE = rmse, MAE = mae, R2 = r2)
}

# plot_actual_vs_pred(): scatter with y=x reference
plot_actual_vs_pred <- function(actual, predicted, title_text) {
  # Creates a scatter plot of Actual vs Predicted with y = x reference line
  df <- data.frame(actual = actual, predicted = predicted)
  ggplot(df, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "red") +
    labs(title = title_text, x = "Actual Price", y = "Predicted Price") +
    theme_minimal(base_size = 14)
}

# clean_data(): keep selected vars, coerce types, remove outliers, drop NAs
clean_data <- function(df) {
  # Returns a cleaned data frame ready for modeling (price outliers removed; listwise deletion)
  keep <- c("price","minimum_nights","number_of_reviews","reviews_per_month",
            "calculated_host_listings_count","availability_365","room_type","neighbourhood_group")
  dat <- df[ , intersect(keep, names(df))]
  
  numeric_vars <- c("price","minimum_nights","number_of_reviews","reviews_per_month",
                    "calculated_host_listings_count","availability_365")
  for (v in intersect(numeric_vars, names(dat))) dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
  if ("room_type" %in% names(dat)) dat$room_type <- factor(dat$room_type)
  if ("neighbourhood_group" %in% names(dat)) dat$neighbourhood_group <- factor(dat$neighbourhood_group)
  
  dat <- subset(dat, is.na(price) | price <= PRICE_CUTOFF)
  dat <- dat[complete.cases(dat), ]
  dat
}

# split_data(): 70/30 split (drops unused factor levels)
split_data <- function(dat, test_prop = TEST_PROP, seed = SEED) {
  # Returns a list(train=..., test=...) with reproducible split
  set.seed(seed)
  n_train <- floor((1 - test_prop) * nrow(dat))
  idx <- sample.int(nrow(dat), size = n_train)
  train <- droplevels(dat[idx, ])
  test  <- droplevels(dat[-idx, ])
  list(train = train, test = test)
}

# fit_lm_step(): linear model with iterative p-value pruning + VIF report
fit_lm_step <- function(formula_obj, train_df, p_thresh = P_THRESH) {
  # Returns the final linear model after removing predictors with p > p_thresh; prints VIF
  m <- lm(formula_obj, data = train_df)
  repeat {
    sm <- summary(m)
    pvals <- coef(sm)[ , "Pr(>|t|)"]
    pvals <- pvals[names(pvals) != "(Intercept)"]
    if (length(pvals) == 0 || all(is.na(pvals)) || max(pvals, na.rm = TRUE) <= p_thresh) break
    worst <- names(which.max(pvals))
    terms_keep <- setdiff(attr(terms(m), "term.labels"), worst)
    new_form <- as.formula(paste("price ~", ifelse(length(terms_keep) > 0,
                                                   paste(terms_keep, collapse = " + "), "1")))
    m_new <- lm(new_form, data = train_df)
    if (identical(formula(m), formula(m_new))) break
    m <- m_new
  }
  if (requireNamespace("car", quietly = TRUE)) {
    cat("\nVIF (final LM):\n")
    print(tryCatch(car::vif(m), error = function(e) "VIF not available"))
  }
  m
}

# fit_tree(): rpart tree; choose cp by minimum xerror
fit_tree <- function(formula_obj, train_df) {
  # Returns a pruned regression tree (cp chosen by minimal cross-validated error)
  fit <- rpart(formula_obj, data = train_df, method = "anova",
               control = rpart.control(cp = 0.001, minsplit = 20))
  cp_tbl <- printcp(fit)
  best_cp <- cp_tbl[which.min(cp_tbl[ , "xerror"]), "CP"]
  prune(fit, cp = best_cp)
}

# fit_rf(): random forest with quick mtry tuning via OOB error
fit_rf <- function(formula_obj, train_df) {
  # Returns a tuned random forest (mtry tuned by OOB error; ntree moderately large)
  tuned <- tuneRF(x = subset(train_df, select = -price),
                  y = train_df$price,
                  stepFactor = 1.5,
                  improve = 0.01,
                  ntreeTry = 400,
                  trace = FALSE)
  best_mtry <- tuned[which.min(tuned[ , "OOBError"]), "mtry"]
  randomForest(formula_obj, data = train_df, ntree = 600, mtry = best_mtry, importance = TRUE)
}

## ===================== 2) Load & Clean =====================
airbnb <- read.csv("C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv",
                   stringsAsFactors = FALSE)

model_df <- clean_data(airbnb)
cat("Rows after cleaning:", nrow(model_df), "\n")

## ===================== 3) Split Data =======================
splits <- split_data(model_df)
train_df <- splits$train
test_df  <- splits$test

## ===================== 4) Common Formula ===================
model_formula <- price ~ minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 +
  room_type + neighbourhood_group

## ===================== 5) Linear Regression =================
lm_final <- fit_lm_step(model_formula, train_df)

# Diagnostics on training fit
par(mfrow = c(1, 2))
plot(lm_final$fitted.values, resid(lm_final),
     xlab = "Fitted", ylab = "Residuals", main = "LM Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(resid(lm_final)); qqline(resid(lm_final), col = "red")
par(mfrow = c(1, 1))

# Test metrics + scatter
lm_pred <- predict(lm_final, newdata = test_df)
lm_met  <- get_metrics(test_df$price, lm_pred)
cat("\nLinear Model (test): RMSE =", round(lm_met$RMSE, 3),
    "| MAE =", round(lm_met$MAE, 3),
    "| R^2 =", round(lm_met$R2, 3), "\n")

p_lm <- plot_actual_vs_pred(test_df$price, lm_pred, "Linear Model: Actual vs Predicted (Test)")

## ===================== 6) Decision Tree ====================
tree_fit <- fit_tree(model_formula, train_df)
rpart.plot(tree_fit, type = 2, extra = 101, fallen.leaves = TRUE, main = "Decision Tree (pruned)")

tree_pred <- predict(tree_fit, newdata = test_df)
tree_met  <- get_metrics(test_df$price, tree_pred)
cat("Decision Tree (test): RMSE =", round(tree_met$RMSE, 3),
    "| MAE =", round(tree_met$MAE, 3),
    "| R^2 =", round(tree_met$R2, 3), "\n")

tree_imp <- data.frame(Variable = names(tree_fit$variable.importance),
                       Importance = as.numeric(tree_fit$variable.importance))
p_tree_imp <- ggplot(tree_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#FFB347") +
  coord_flip() +
  labs(title = "Decision Tree Variable Importance", x = "", y = "Importance") +
  theme_minimal(base_size = 14)

## ===================== 7) Random Forest ====================
rf_fit <- fit_rf(model_formula, train_df)

rf_pred <- predict(rf_fit, newdata = test_df)
rf_met  <- get_metrics(test_df$price, rf_pred)
cat("Random Forest (test): RMSE =", round(rf_met$RMSE, 3),
    "| MAE =", round(rf_met$MAE, 3),
    "| R^2 =", round(rf_met$R2, 3), "\n")

rf_imp <- data.frame(Variable = rownames(importance(rf_fit)),
                     IncMSE   = importance(rf_fit)[ , "%IncMSE"])
p_rf_imp <- ggplot(rf_imp, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_col(fill = "#00C4CC") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance (%IncMSE)", x = "", y = "% Increase in MSE") +
  theme_minimal(base_size = 14)

## ===================== 8) Compare Models ===================
results_tbl <- data.frame(
  Model = c("Linear Regression","Decision Tree","Random Forest"),
  RMSE  = c(lm_met$RMSE, tree_met$RMSE, rf_met$RMSE),
  MAE   = c(lm_met$MAE,  tree_met$MAE,  rf_met$MAE),
  R2    = c(lm_met$R2,   tree_met$R2,   rf_met$R2)
)
print(results_tbl)

p_compare <- ggplot(results_tbl, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Model Comparison (RMSE on Test Set)", x = "", y = "RMSE") +
  theme_minimal(base_size = 14)

# Optional: display several key plots together
gridExtra::grid.arrange(p_lm, p_tree_imp, p_rf_imp, p_compare, ncol = 2)

## ===================== 9) Memo Notes =======================
cat("\n--- Notes for your memo ---\n")
cat("* Outliers: removed observations with price > $", PRICE_CUTOFF, " to stabilize models.\n", sep = "")
cat("* Missing data: listwise deletion on selected predictors; low missingness based on descriptive analysis.\n")
cat("* Linear regression: predictors pruned by p-values (alpha = ", P_THRESH, "); VIF checked; residual & QQ plots reviewed.\n", sep = "")
cat("* Tree / Random Forest: tuned by cp (min xerror) and mtry (OOB error); interpret using variable importance.\n")
cat("* Validation: 70/30 train–test split; metrics reported on test set (RMSE, MAE, R^2).\n") 


############################################################
### 10) K-FOLD CV (10-fold) FOR LM, DT, RF
############################################################

library(caret)

set.seed(SEED)

# Define CV control settings
cv_control <- trainControl(method = "cv", number = 10)

# Ensure modeling_data matches training variables (price cutoff + complete cases)
modeling_data <- model_df   # from your earlier cleaning step

### --- Linear Regression CV ---
lm_cv <- train(
  model_formula, 
  data = modeling_data,
  method = "lm",
  trControl = cv_control,
  metric = "RMSE"
)

cat("\n===== LINEAR REGRESSION: 10-Fold CV =====\n")
print(lm_cv)


### --- Decision Tree CV ---
dt_cv <- train(
  model_formula,
  data = modeling_data,
  method = "rpart",
  trControl = cv_control,
  tuneLength = 10,
  metric = "RMSE"
)

cat("\n===== DECISION TREE: 10-Fold CV =====\n")
print(dt_cv)



# --- OOB-based "CV" RMSE for Random Forest ---

# rf_fit$mse is the OOB MSE for each tree
rf_rmse_vec  <- sqrt(rf_fit$mse)

rf_rmse_mean <- mean(rf_rmse_vec)
rf_rmse_sd   <- sd(rf_rmse_vec) 

cat("\nRandom Forest OOB-based CV:")
cat("\n  RMSE =", round(rf_rmse_mean, 3), "±", round(rf_rmse_sd, 3), "\n")




# --- Simple OOB-based "CV" MAE for Random Forest (mean ± SD) ---

rf_oob_pred <- rf_fit$predicted
valid_idx <- !is.na(rf_oob_pred)

oob_abs_errors <- abs(model_df$price[valid_idx] - rf_oob_pred[valid_idx])

rf_mae_mean <- mean(oob_abs_errors)
rf_mae_sd   <- sd(oob_abs_errors)

cat("\nRandom Forest OOB-based CV:")
cat("\n  MAE =", round(rf_mae_mean, 3), "±", round(rf_mae_sd, 3), "\n")




### Extract CV mean ± SD for Linear Regression ###
lm_rmse_mean <- mean(lm_cv$resample$RMSE)
lm_rmse_sd   <- sd(lm_cv$resample$RMSE)

lm_mae_mean <- mean(lm_cv$resample$MAE)
lm_mae_sd   <- sd(lm_cv$resample$MAE)

cat("\nLinear Regression CV (10-fold):")
cat("\n  RMSE =", round(lm_rmse_mean, 3), "±", round(lm_rmse_sd, 3))
cat("\n  MAE  =", round(lm_mae_mean, 3), "±", round(lm_mae_sd, 3), "\n")



airbnb <- read.csv("C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv",
                   stringsAsFactors = FALSE)

model_df <- clean_data(airbnb)

rf_fit <- fit_rf(model_formula, train_df)




############################################################
## PRESCRIPTIVE ANALYSIS – DISTANCE-WEIGHTED RF PRICES
############################################################

# I already have: airbnb, model_df, rf_fit, PRICE_CUTOFF from above

# Make sure dplyr is available
if (!"dplyr" %in% rownames(installed.packages())) {
  install.packages("dplyr", dependencies = TRUE)
}
library(dplyr)

# ---- 1) Recreate the row index used in model_df ----
keep <- c("price","minimum_nights","number_of_reviews","reviews_per_month",
          "calculated_host_listings_count","availability_365",
          "room_type","neighbourhood_group")

tmp <- airbnb[ , keep]

numeric_vars <- c("price","minimum_nights","number_of_reviews","reviews_per_month",
                  "calculated_host_listings_count","availability_365")

for (v in numeric_vars) {
  tmp[[v]] <- suppressWarnings(as.numeric(tmp[[v]]))
}

keep_rows <- (!is.na(tmp$price) & tmp$price <= PRICE_CUTOFF) & complete.cases(tmp)

cat("Rows used in RF model:", sum(keep_rows), "\n")
cat("nrow(model_df):", nrow(model_df), "\n")  # should match

# ---- 2) Build prescriptive data frame: RF predictions + coords ----
rf_all_pred <- predict(rf_fit, newdata = model_df)

presc_df <- data.frame(
  rf_price            = rf_all_pred,
  neighbourhood_group = model_df$neighbourhood_group,
  latitude            = airbnb$latitude[keep_rows],
  longitude           = airbnb$longitude[keep_rows]
)

# ---- 3) Distance function (Haversine, miles) ----
deg2rad <- function(x) x * pi / 180

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  R <- 3959  # Earth radius in miles
  phi1 <- deg2rad(lat1); phi2 <- deg2rad(lat2)
  dphi <- deg2rad(lat2 - lat1)
  dlambda <- deg2rad(lon2 - lon1)
  
  a <- sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlambda/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# ---- 4) Anchor (Midtown Manhattan) and radius ----
anchor_lat   <- 40.73   # from your Executive Summary
anchor_lon   <- -73.97
RADIUS_MILES <- 1.75    # optimal radius from your prescriptive write-up

presc_df$distance_mi <- haversine_miles(
  lat1 = presc_df$latitude,
  lon1 = presc_df$longitude,
  lat2 = anchor_lat,
  lon2 = anchor_lon
)

inside <- presc_df %>% filter(distance_mi <= RADIUS_MILES)
cat("Listings inside", RADIUS_MILES, "mile radius:", nrow(inside), "\n")

# ---- 5) Compute weights and weighted contribution ----
inside <- inside %>%
  mutate(
    weight           = 1 / (1 + distance_mi),
    weighted_contrib = rf_price * weight
  )

# ---- 6) Neighborhood-level summary table ----
opt_table <- inside %>%
  group_by(neighbourhood_group) %>%
  summarise(
    mean_rf_price  = mean(rf_price),
    mean_distance  = mean(distance_mi),
    weighted_score = mean(weighted_contrib),
    n_listings     = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_score))

cat("\n===== Distance-Weighted RF Summary (1.75-mile radius) =====\n")
print(opt_table)

# If you only want the top 5 rows for your report:
cat("\nTop 5 neighborhoods for report:\n")
print(head(opt_table, 5))

# ---- 7) Overall vs anchor-area mean price (for % lift) ----
overall_mean  <- mean(presc_df$rf_price)
anchor_mean   <- mean(inside$rf_price)
improvement_pct <- (anchor_mean - overall_mean) / overall_mean * 100

cat("\nOverall RF mean price:   ", round(overall_mean, 2), "\n")
cat("Anchor-area RF mean price:", round(anchor_mean, 2), "\n")
cat("Percent lift (anchor vs overall):",
    round(improvement_pct, 1), "%\n")




############################################################
## PRESCRIPTIVE ANALYSIS – FULL BOROUGH SUMMARY
## Uses: airbnb, rf_fit, PRICE_CUTOFF
############################################################

library(dplyr)
library(geosphere)

### 1) Anchor coordinates (Midtown Manhattan)
anchor_lat <- 40.73
anchor_lon <- -73.97

### 2) Prepare data
prec_df <- airbnb %>% 
  filter(price <= PRICE_CUTOFF) %>% 
  filter(!is.na(latitude), !is.na(longitude),
         !is.na(neighbourhood_group)) %>%
  mutate(
    rf_price = predict(rf_fit, newdata = .),
    distance_m = distHaversine(
      cbind(longitude, latitude),
      c(anchor_lon, anchor_lat)
    ),
    distance_miles = distance_m * 0.000621371
  )

### 3) Weighted Score Function — same as before
wfun <- function(d) 1 / (1 + d)

### 4) Summaries by ALL boroughs (no radius filter)
opt_table_full <- prec_df %>%
  group_by(neighbourhood_group) %>%
  summarise(
    mean_rf_price   = round(mean(rf_price), 2),
    mean_distance   = round(mean(distance_miles), 2),
    weighted_score  = round(mean(rf_price * wfun(distance_miles)), 2),
    n_listings      = n()
  ) %>%
  arrange(desc(weighted_score))

cat("\n===== FULL PRESCRIPTIVE SUMMARY (ALL BOROUGHS) =====\n")
print(opt_table_full)






library(dplyr)

# presc_df should already contain:
# rf_price, neighbourhood_group, latitude, longitude, distance_mi

presc_all <- presc_df %>%
  filter(!is.na(neighbourhood_group), !is.na(distance_mi)) %>%
  mutate(
    weight           = 1 / (1 + distance_mi),
    weighted_contrib = rf_price * weight
  )

opt_table_full <- presc_all %>%
  group_by(neighbourhood_group) %>%
  summarise(
    mean_rf_price  = round(mean(rf_price), 2),
    mean_distance  = round(mean(distance_mi), 2),
    weighted_score = round(mean(weighted_contrib), 2),
    n_listings     = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_score))

cat("\n===== FULL PRESCRIPTIVE SUMMARY (ALL BOROUGHS) =====\n")
print(opt_table_full)











# ============================
# Static Map of Optimal Anchor
# ============================
library(dplyr)
install.packages("sf", dependencies = TRUE)

library(sf)

library(ggplot2)

# 0) Load data (if not already loaded)
# airbnb <- read.csv("C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv",
#                    stringsAsFactors = FALSE)

PRICE_CUTOFF <- 1000

airbnb_map <- airbnb %>%
  filter(!is.na(latitude),
         !is.na(longitude),
         !is.na(neighbourhood_group),
         price <= PRICE_CUTOFF)

# 1) Define optimal anchor and radius (your prescriptive result)
anchor_lat <- 40.73      # approx Midtown Manhattan
anchor_lon <- -73.97
radius_miles <- 1.75
radius_feet  <- radius_miles * 5280  # because EPSG:2263 uses feet

# 2) Convert listings and anchor to sf objects, project, and buffer
nyc_points <- st_as_sf(
  airbnb_map,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

anchor_pt <- st_as_sf(
  data.frame(lon = anchor_lon, lat = anchor_lat),
  coords = c("lon", "lat"),
  crs = 4326
)

# Project to a local CRS for NYC so buffer distance is in feet
nyc_points_2263 <- st_transform(nyc_points, 2263)
anchor_2263     <- st_transform(anchor_pt, 2263)

# Create circular buffer (1.75 mile radius)
anchor_buffer_2263 <- st_buffer(anchor_2263, dist = radius_feet)

# Transform back to WGS84 for plotting with lon/lat
anchor_buffer_4326 <- st_transform(anchor_buffer_2263, 4326)

# 3) Build the map
ggplot() +
  # All listings
  geom_point(
    data = airbnb_map,
    aes(x = longitude, y = latitude, color = neighbourhood_group),
    alpha = 0.4, size = 0.8
  ) +
  # Optimized radius (1.75 miles)
  geom_sf(
    data = anchor_buffer_4326,
    fill = NA,
    color = "red",
    linewidth = 1
  ) +
  # Anchor point
  geom_point(
    data = data.frame(lon = anchor_lon, lat = anchor_lat),
    aes(x = lon, y = lat),
    color = "red",
    size = 3
  ) +
  labs(
    title = "Optimal Airbnb Pricing Anchor – Midtown Manhattan",
    subtitle = "Red circle shows 1.75-mile radius around optimized anchor\nbased on distance-weighted Random Forest predicted prices",
    x = "Longitude",
    y = "Latitude",
    color = "Neighbourhood Group"
  ) +
  coord_sf(xlim = range(airbnb_map$longitude) + c(-0.05, 0.05),
           ylim = range(airbnb_map$latitude) + c(-0.05, 0.05)) +
  theme_minimal(base_size = 13)





# ============================
# Interactive Leaflet Map
# ============================
library(leaflet)

leaflet(data = airbnb_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 2,
    color = ~ifelse(neighbourhood_group == "Manhattan", "blue", "gray"),
    stroke = FALSE,
    fillOpacity = 0.4,
    group = "Listings"
  ) %>%
  # Anchor point
  addMarkers(
    lng = anchor_lon,
    lat = anchor_lat,
    popup = "Optimal Anchor (Midtown Manhattan)",
    label = "Optimal Anchor"
  ) %>%
  # Approximate circle (leaflet uses meters)
  addCircles(
    lng = anchor_lon,
    lat = anchor_lat,
    radius = radius_miles * 1609.34,  # miles -> meters
    color = "red",
    weight = 2,
    fill = FALSE,
    group = "Optimal Zone"
  ) %>%
  addLayersControl(
    overlayGroups = c("Listings", "Optimal Zone"),
    options = layersControlOptions(collapsed = FALSE)
  )



















library(dplyr)
library(leaflet)

# -----------------------------------
# Haversine distance in meters
# -----------------------------------
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000  # Earth radius in meters
  to_rad <- pi / 180
  
  phi1 <- lat1 * to_rad
  phi2 <- lat2 * to_rad
  dphi <- (lat2 - lat1) * to_rad
  dlambda <- (lon2 - lon1) * to_rad
  
  a <- sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlambda/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}



# Use your original Airbnb data frame
# airbnb <- read.csv("C:/Users/Willi/OneDrive/Desktop/Operational Analytics Project/new_york_listings_2024.csv",
#                    stringsAsFactors = FALSE)

PRICE_CUTOFF <- 1000

airbnb_presc <- airbnb %>%
  filter(price <= PRICE_CUTOFF,
         !is.na(latitude),
         !is.na(longitude),
         neighbourhood_group %in% c("Manhattan", "Brooklyn", "Queens"))

# Optimal anchor from your prescriptive work (Midtown Manhattan)
anchor_lat <- 40.73
anchor_lon <- -73.97
radius_miles  <- 1.75
radius_meters <- radius_miles * 1609.34

# Compute distance from anchor for each listing
airbnb_presc <- airbnb_presc %>%
  rowwise() %>%
  mutate(
    dist_m = haversine_m(anchor_lat, anchor_lon, latitude, longitude)
  ) %>%
  ungroup()

# Keep only listings within the 1.75-mile optimization radius
airbnb_in_radius <- airbnb_presc %>%
  filter(dist_m <= radius_meters)



leaflet(data = airbnb_in_radius) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Listings within 1.75 miles, colored by borough
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = ~case_when(
      neighbourhood_group == "Manhattan" ~ "blue",
      neighbourhood_group == "Brooklyn"  ~ "darkgreen",
      neighbourhood_group == "Queens"    ~ "purple",
      TRUE                               ~ "gray"
    ),
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Borough: </b>", neighbourhood_group,
      "<br><b>Price: </b>$", price
    ),
    group = "Listings in 1.75-mile Radius"
  ) %>%
  
  # Anchor point
  addMarkers(
    lng = anchor_lon,
    lat = anchor_lat,
    popup = "Optimal Anchor (Midtown Manhattan)",
    label = "Optimal Anchor"
  ) %>%
  
  # Optimization circle (1.75 miles)
  addCircles(
    lng = anchor_lon,
    lat = anchor_lat,
    radius = radius_meters,
    color = "red",
    weight = 2,
    fill = FALSE,
    group = "Optimization Radius"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Listings in 1.75-mile Radius", "Optimization Radius"),
    options = layersControlOptions(collapsed = FALSE)
  )






leaflet(data = airbnb_in_radius) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = ~case_when(
      neighbourhood_group == "Manhattan" ~ "blue",
      neighbourhood_group == "Brooklyn"  ~ "darkgreen",
      neighbourhood_group == "Queens"    ~ "purple",
      TRUE                               ~ "gray"
    ),
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Borough: </b>", neighbourhood_group,
      "<br><b>Price: </b>$", price
    ),
    label = ~neighbourhood_group,      # <-- borough label on hover
    group = "Listings in 1.75-mile Radius"
  ) %>%
  
  addMarkers(
    lng = anchor_lon,
    lat = anchor_lat,
    popup = "Optimal Anchor (Midtown Manhattan)",
    label = "Optimal Anchor"
  ) %>%
  
  addCircles(
    lng = anchor_lon,
    lat = anchor_lat,
    radius = radius_meters,
    color = "red",
    weight = 2,
    fill = FALSE,
    group = "Optimization Radius"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Listings in 1.75-mile Radius", "Optimization Radius"),
    options = layersControlOptions(collapsed = FALSE)
  )







library(dplyr)
library(leaflet)

# color palette for boroughs
pal <- colorFactor(
  palette = c("blue", "darkgreen", "purple"),
  domain  = c("Manhattan", "Brooklyn", "Queens")
)

leaflet(data = airbnb_in_radius) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color  = ~pal(neighbourhood_group),   # <-- use palette
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Borough: </b>", neighbourhood_group,
      "<br><b>Price: </b>$", price
    ),
    group = "Listings in 1.75-mile Radius"
  ) %>%
  
  # Anchor point
  addMarkers(
    lng = anchor_lon,
    lat = anchor_lat,
    popup = "Optimal Anchor (Midtown Manhattan)",
    label = "Optimal Anchor"
  ) %>%
  
  # Optimization circle
  addCircles(
    lng = anchor_lon,
    lat = anchor_lat,
    radius = radius_meters,
    color  = "red",
    weight = 2,
    fill   = FALSE,
    group  = "Optimization Radius"
  ) %>%
  
  # Legend for borough colors
  addLegend(
    position = "topright",
    pal      = pal,
    values   = ~neighbourhood_group,
    title    = "Neighbourhood Group",
    opacity  = 1
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Listings in 1.75-mile Radius", "Optimization Radius"),
    options = layersControlOptions(collapsed = FALSE)
  )



# Compute one representative point (centroid-ish) per borough
borough_labels <- airbnb_in_radius %>%
  group_by(neighbourhood_group) %>%
  summarise(
    lon = mean(longitude, na.rm = TRUE),
    lat = mean(latitude,  na.rm = TRUE),
    .groups = "drop"
  )

leaflet(data = airbnb_in_radius) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color  = ~pal(neighbourhood_group),
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Borough: </b>", neighbourhood_group,
      "<br><b>Price: </b>$", price
    ),
    group = "Listings in 1.75-mile Radius"
  ) %>%
  
  # Anchor + radius as before
  addMarkers(
    lng = anchor_lon,
    lat = anchor_lat,
    popup = "Optimal Anchor (Midtown Manhattan)",
    label = "Optimal Anchor"
  ) %>%
  addCircles(
    lng = anchor_lon,
    lat = anchor_lat,
    radius = radius_meters,
    color  = "red",
    weight = 2,
    fill   = FALSE,
    group  = "Optimization Radius"
  ) %>%
  
  # BIG borough labels on the map
  addLabelOnlyMarkers(
    data  = borough_labels,
    lng   = ~lon,
    lat   = ~lat,
    label = ~neighbourhood_group,
    labelOptions = labelOptions(
      noHide = TRUE,              # always visible
      textsize = "14px",
      direction = "center",
      style = list(
        "font-weight" = "bold",
        "color"       = "black",
        "text-shadow" = "1px 1px 2px white"
      )
    )
  ) %>%
  
  addLegend(
    position = "topright",
    pal      = pal,
    values   = ~neighbourhood_group,
    title    = "Neighbourhood Group",
    opacity  = 1
  ) %>%
  addLayersControl(
    overlayGroups = c("Listings in 1.75-mile Radius", "Optimization Radius"),
    options = layersControlOptions(collapsed = FALSE)
  )

