library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(pracma)
library(plotly)
library(reshape2)
library(ParBayesianOptimization)
library(xgboost)


train <- read.csv("train.csv")



## Feature Engineering ----------------------------------------------------


train_mod <- train %>%
  mutate(
    
    # Attack Zone
    attack_zone = 'waste',
    attack_zone = case_when(
      plate_x >= -0.558 & plate_x <= 0.558 &
        plate_z >= 1.833 & plate_z <= 3.166 ~ 'heart',
      plate_x >= -1.108 & plate_x <= 1.108 &
        plate_z >= 1.166 & plate_z <= 3.833 &
        attack_zone != 'heart' ~ 'shadow',
      plate_x >= -1.666 & plate_x <= 1.666 &
        plate_z >= 0.5 & plate_z <= 4.5 &
        !attack_zone %in% c('heart', 'shadow') ~ 'chase',
      TRUE ~ attack_zone),
    
    
    # Normalizing for Batter Handedness
    batter_platoon_adv = ifelse(is_lhp != is_lhb, 1, 0),
    
    across(c(spray_angle, release_pos_x, pfx_x, plate_x),
           ~ ifelse(is_lhb == 1, -.x, .x)),
    spray_angle = ifelse(spray_angle == 0, 0.01, spray_angle),
    
    direction = case_when(spray_angle <= -15 ~ "pull",
                          spray_angle >= 15 ~ "oppo",
                          TRUE ~ "center"),
    
    
    # Swing Metrics
    swing_efficiency = bat_speed / swing_length,
    swing_time = swing_length / ((bat_speed * 1.46667) / 2),
    swing_acceleration = (bat_speed * 1.46667) / swing_time,
    max_ev = (0.91 * release_speed * 0.23) + (bat_speed * (1 + 0.23)),
    
    
    movement_complexity = sqrt(pfx_x**2 + pfx_z**2),
    
    
    # Estimate center of swing based on the top of the strike zone
    swing_center_loc = map(sz_top, ~ c(-((17/12) / 2) - 6/12, 0, .)),
    
    
    # Calculate the normal vector based on the spray angle
    norm_vector = map(spray_angle, 
                      ~ c(1, 
                          1 / (sin(. * pi / 180) / cos(. * pi / 180)),
                          0)),
    
    
    # Calculate plane parameters
    plane_params = map2(norm_vector,
                        swing_center_loc,
                        ~ c(.x, sum(.y * .x))),
    
    
    # Contact Location (intersection of pitch path and plane)
    contact_loc = pmap(
      list(release_pos_x, plate_x, release_pos_y, 
           plate_z, release_pos_z,
           norm_vector, plane_params),
      function(a, b, c, d, e, norm_vector, plane_params) {
        b_x <- b - a
        d_y <- 0 - c
        f_z <- d - e
        t <- max(
          roots(
            c(norm_vector[1] * b_x + norm_vector[2] * d_y,
              norm_vector[1] * a + norm_vector[2] * c - plane_params[2])))
        contact <- c(a + b_x * t,
                     c + d_y * t,
                     e + f_z * t)
        return(contact)
        }),
    
    
    # Calculate distance to swing center
    contact_x = map_dbl(contact_loc, 1),
    contact_y = map_dbl(contact_loc, 2),
    contact_z = map_dbl(contact_loc, 3),
    dist_to_center = map2_dbl(swing_center_loc, 
                              contact_loc,
                              ~ sqrt(sum((.x - .y)^2))),
    
    
    # Calculate swing start location and bat vector
    swing_start_loc = map(dist_to_center, ~ c(-., 0, 0)),
    bat_vec = map2(contact_loc, swing_center_loc, ~ .x - .y),
    
    
    # Calculate central arc angle and swing radius
    central_arc_angle = map2_dbl(
      bat_vec,
      swing_start_loc,
      ~ acos(sum(.x * .y) / (sqrt(sum(.x^2)) * sqrt(sum(.y^2))))),
    
    swing_radius = swing_length / central_arc_angle,
    
    
    # distance to sweet spot
    dist_to_sweet_spot = pmax(pmin(swing_radius - dist_to_center, 3), -3),
    
    
    # great circle vector
    v_v1 = map(swing_start_loc, ~ . / sqrt(sum(.^2))),
    v_v2 = map(bat_vec, ~ . / sqrt(sum(.^2))),
    v_w = pmap(list(v_v1, v_v2),
               function(v1, v2) {
                 d <- sum(v1 * v2)
                 b <- roots(c(1 - d^2, 0, -1))[1]
                 a <- -d * b
                 w <- a * v1 + b * v2
                 return(w)
                 }),
         
         
     # Calculate swing path
     #swing_path = pmap(list(dist_to_center, 
      #                      central_arc_angle, 
       #                     v_v1, 
        #                    v_w, 
         #                   swing_center_loc), 
          #             function(dist_to_center, 
           #                     central_arc_angle, 
            #                    v_v1, 
             #                   v_w, 
              #                  swing_center_loc) {
      # t <- seq(0, central_arc_angle, length.out = 33
       #         )
      # path <- dist_to_center * 
       #  (v_v1 * cos(t) + v_w * sin(t)) + 
        # swing_center_loc
       
       
      # return(path)
     #}),
     
         
         
   # swing tangent at contact
   swing_tangent_at_contact = pmap(
     list(dist_to_center, v_v1, v_w, swing_center_loc),
     function(dist_to_center, v1, w, swing_center_loc) {
       dist_to_center * (-v1 * sin(0) + w * cos(0))
       }),
   
   
   centerline_angle = asin((12*abs(contact_z - plate_z)) / 2.7) * (180 / pi),
   

   # vertical bat angle (VBA)
   vertical_bat_angle = map_dbl(bat_vec, function(bat_vec) {
     vba <- atan2(bat_vec[3], sqrt(sum(bat_vec[1:2]^2))) * (180 / pi)
     return(vba)
   }),
   
   
   #vertical_bat_angle = atan2(bat_vec[3], 
    #                          sqrt(sum(bat_vec[1:2]^2))) * (180 / pi),
   
   
   # Vertical Approach Angle (VAA)
   vy_f = -sqrt(vy0**2 - (2 * ay * (50 - (17 /12)))),
   VAA = -atan((vz0 + (az * ((vy_f - vy0) / ay))) / vy_f) * (180 / pi),
   
   # Horizontal Approach Angle (HAA)
   vx_f = vx0 + (ax * ((vy_f - vy0) / ay)),
   HAA = -atan(vx_f / vy_f) * (180 / pi)
   ) %>%

  
  # Adjusting VAA for Pitch Type and Pitch Height
  # Adjusting HAA for Pitch Type, Location, Handedness and Release Pos 
  group_by(pitch_name) %>%
  group_modify(~ {
    vaa_model <- lm(VAA ~ plate_z, data = .x) 
    haa_model <- lm(HAA ~ plate_x + is_lhp + release_pos_x, data = .x) 
    .x %>% 
      mutate(VAAAA = resid(vaa_model),
             HAAAA = resid(haa_model))
    }) %>%
  ungroup() %>%
  mutate(outcome_woba = case_when(outcome_code == "0" ~ 0,
                                  outcome_code == "1" ~ 0.882,
                                  outcome_code == "2" ~ 1.253,
                                  outcome_code == "3" ~ 1.587,
                                  outcome_code == "4" ~ 2.044)) %>%
  
  # Cleaning
  as.data.frame() %>%
  arrange(uid) %>%
  select(-game_type, -game_year) %>% # irrelevant
  relocate(pitch_name, .before = pitch_type) %>%
  relocate(outcome, 
           #outcome_code, outcome_woba, 
           .after = last_col())




# Exploring the new features
train_mod %>%
  #group_by(outcome_code, attack_zone) %>%
  #group_by(pitch_name, is_lhp) %>%
  select(-uid, pitch_name, pitch_type, attack_zone,
         is_lhp, is_lhb, balls, strikes, on_3b, on_2b, on_1b) %>%
  group_by(outcome_code) %>%
  summarise(across(everything(), mean)) %>%
  select(-c(pitch_name, pitch_type, attack_zone,
            is_lhp, is_lhb, balls, strikes, 
            on_3b, on_2b, on_1b)) %>%
  #summarize(n = n(),
            #Avg_VAA = mean(VAA),
            #Avg_HAA = mean(HAA),
   #         avg_Attack_Angle = mean(attack_angle)) %>%
  #filter(is_lhp == 0) %>%
  as.data.frame()


train_mod %>%
  filter(pitch_type == "FF") %>%
  filter(HAA >= 1.295 & HAA <= 1.305) %>%
  select(uid, is_lhp, release_pos_x, plate_x, HAA, HAAAA)


train_mod %>%
  filter(outcome_code == 4) %>%
  #filter(adj_spray_angle >= -0.05 & adj_spray_angle <= 0.05) %>%
  head()


train_mod %>%
  group_by(outcome_code, direction) %>%
  summarize(n = n()) %>%
  as.data.frame()


train_mod %>%
  #filter(attack_zone == "heart") %>%
  filter(is_lhb == 0) %>%
  arrange(vertical_bat_angle) %>%
  #filter(outcome_code == 4) %>%
  head(n = 5)

train_mod %>%
  filter(centerline_angle == "NaN")





## Attack Zone Plot -------------------------------------------------------
ggplot(train_mod #%>% sample_n(2000),
       ,
       aes(x = plate_x, 
           y = plate_z,
           z = outcome_woba
           #shape = attack_zone,
           #color = outcome
           )) +
  

  stat_summary_hex(bins = 50, fun = mean) +
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      limits = c(0, 0.7), 
                      name = "Average wOBA") +
  
  
  # Draw strike zone (similar to draw_sz(ls='k--'))
  geom_rect(aes(xmin = -0.708, 
                xmax = 0.708, 
                ymin = 1.5, 
                ymax = 3.5), 
            color = 'black', 
            linetype = 'longdash',
            linewidth = 1,
            fill = NA) +
  

  # Draw the home plate (approximation)
  geom_segment(aes(x = -0.708, 
                   y = 0, 
                   xend = 0.708, 
                   yend = 0), 
               color = 'black') +  # Top side
  geom_segment(aes(x = -0.708, 
                   y = 0, 
                   xend = -0.708, 
                   yend = -0.3), 
               color = 'black') +  # Left side
  geom_segment(aes(x = 0.708, 
                   y = 0, 
                   xend = 0.708, 
                   yend = -0.3), 
               color = 'black') +  # Right side
  geom_segment(aes(x = -0.708, 
                   y = -0.3, 
                   xend = 0, 
                   yend = -0.6), 
               color = 'black') +  # Bottom-left side
  geom_segment(aes(x = 0.708, 
                   y = -0.3, 
                   xend = 0, 
                   yend = -0.6), 
               color = 'black') +  # Bottom-right side
  
  
  # Draw attack zones (heart, shadow, chase areas approximation)
  geom_rect(aes(xmin = -0.558, 
                xmax = 0.558, 
                ymin = 1.833, 
                ymax = 3.166), 
            color = 'brown', 
            linetype = 'solid',
            linewidth = 1,
            fill = NA) +
  geom_rect(aes(xmin = -1.108, 
                xmax = 1.108, 
                ymin = 1.166, 
                ymax = 3.833), 
            color = 'darkgreen', 
            linetype = 'solid',
            linewidth = 1,
            fill = NA) +
  geom_rect(aes(xmin = -1.666, 
                xmax = 1.666, 
                ymin = 0.5, 
                max = 4.5), 
            color = 'orange', 
            linetype = 'solid',
            linewidth = 1,
            fill = NA) +
  
  # Set fixed coordinate limits
  xlim(-2.5, 2.5) +
  ylim(-1, 5.5) +
  
  # Adjustments for equal aspect ratio and theme
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = c(1.3, 1),
        legend.justification = c(1, 1),
        legend.box.just = "right",
        legend.key = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  
  # De-spine the plot
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  
  # Add title and labels
  labs(color = "Outcome", shape = "Attack Zone")










## VAA Plot ---------------------------------------------------------------

ggplot(train_mod, aes(x = VAA, y = plate_z, color = outcome)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  
  # Adds some randomness to the x positions to avoid overplotting
  scale_color_manual(values = c("out" = "black", 
                                "single" = "darkgreen",
                                "double" = "blue",
                                "triple" = "purple",
                                "home_run" = "red")) +
  labs(title = "Vertical Approach Angle Relative to Average",
       x = "VAA",
       y = "Pitch Height",
       color = "Outcome") +
  theme_minimal() +
  xlim(-12.5, 0) +
  theme(legend.position = "right")



ggplot(train_mod %>% filter(pitch_name == "4-Seam Fastball"), 
       aes(x = VAAAA, y = plate_z, z = outcome_woba)) +
  stat_summary_hex(bins = 50, fun = mean) +
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      limits = c(0, 2), 
                      name = "Average wOBA") +
  labs(title = "Vertical Approach Angle Relative to Average",
       x = "VAAAA",
       y = "Pitch Height") +
  theme_minimal() +
  xlim(-1.2, 1.2) +
  theme(legend.position = "right")









## Swing Path and Contact Plot --------------------------------------------
plot_swing_and_stuff <- function(row) {
  plate_radius <- (17 / 12) / 2
  
  # Ensure vectors are numeric
  contact_loc <- unlist(row$contact_loc)
  norm_vector <- unlist(row$norm_vector)
  swing_center_loc <- unlist(row$swing_center_loc)
  swing_tangent_at_contact <- unlist(row$swing_tangent_at_contact)
  swing_path <- matrix(unlist(row$swing_path), nrow = 3, byrow = FALSE)
  
  fig <- plot_ly()
  
  # Plot strike zone
  fig <- fig %>%
    add_trace(
      x = c(-plate_radius, plate_radius, plate_radius, 
            -plate_radius, -plate_radius),
      y = c(0, 0, 0, 0, 0),
      z = c(row$sz_top, row$sz_top, row$sz_bot, row$sz_bot, row$sz_top),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "red")
    ) %>%
    add_trace(
      x = c(-plate_radius - 6 / 12, 
            -plate_radius - 6 / 12 - 4, 
            -plate_radius - 6 / 12 - 4, 
            -plate_radius - 6 / 12, 
            -plate_radius - 6 / 12),
      y = c(-3, -3, 3, 3, -3),
      z = c(0, 0, 0, 0, 0),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "red")
    )
  
  # Plot swing plane (green line)
  fig <- fig %>%
    add_trace(
      x = c(contact_loc[1] + norm_vector[1], 
            contact_loc[1] - norm_vector[1]),
      y = c(contact_loc[2] + norm_vector[2], 
            contact_loc[2] - norm_vector[2]),
      z = c(contact_loc[3] + norm_vector[3], 
            contact_loc[3] - norm_vector[3]),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "green")
    ) %>%
    add_trace(
      x = c(swing_center_loc[1], contact_loc[1]),
      y = c(swing_center_loc[2], contact_loc[2]),
      z = c(swing_center_loc[3], contact_loc[3]),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "green")
    )
  
  # Scatter plot for contact and swing center locations
  fig <- fig %>%
    add_trace(
      x = contact_loc[1],
      y = contact_loc[2],
      z = contact_loc[3],
      type = "scatter3d",
      mode = "markers",
      marker = list(color = "black")
    ) %>%
    add_trace(
      x = swing_center_loc[1],
      y = swing_center_loc[2],
      z = swing_center_loc[3],
      type = "scatter3d",
      mode = "markers",
      marker = list(color = "red")
    ) %>%
    add_trace(
      x = swing_center_loc[1] - row$dist_to_center,
      y = swing_center_loc[2],
      z = swing_center_loc[3],
      type = "scatter3d",
      mode = "markers",
      marker = list(color = "black")
    )
  
  # Plot swing sphere surface
  u <- seq(0, 2 * pi, length.out = 20)
  v <- seq(0, pi, length.out = 10)
  x <- row$dist_to_center * outer(cos(u), sin(v)) + swing_center_loc[1]
  y <- row$dist_to_center * outer(sin(u), sin(v)) + swing_center_loc[2]
  z <- row$dist_to_center * outer(rep(1, length(u)), cos(v)) + 
    swing_center_loc[3]
  
  fig <- fig %>%
    add_surface(x = x, 
                y = y, 
                z = z, 
                showscale = FALSE, 
                opacity = 0.1, 
                surfacecolor = matrix(rep(1, length(x) * length(y)), 
                                      nrow = length(x)), 
                colorscale = list(c(0, "red")))
  
  # Plot swing path (blue line)
  fig <- fig %>%
    add_trace(
      x = swing_path[1,],
      y = swing_path[2,],
      z = swing_path[3,],
      type = "scatter3d",
      mode = "lines",
      line = list(color = "blue")
    )
  
  # Plot swing tangent at contact (blue line)
  #t_ <- seq(-1, 1, length.out = 33)
  #tangent_line <- sweep(swing_tangent_at_contact %*% 
  #                        t(matrix(rep(1, length(t_)), 
  #                                 nrow = 1, 
  #                                 byrow = TRUE)), 
  #                      2, contact_loc, "+")
  
  fig <- fig %>%
    add_trace(
      x = tangent_line[1,],
      y = tangent_line[2,],
      z = tangent_line[3,],
      type = "scatter3d",
      mode = "lines",
      line = list(color = "blue")
    )
  
  # Set different view angles
  subplot(
    fig %>% layout(scene = list(camera = 
                                  list(eye = list(x = 1, y = 0, z = 1)))),
    fig %>% layout(scene = list(camera = 
                                  list(eye = list(x = 0, y = -1, z = 0)))),
    fig %>% layout(scene = list(camera = 
                                  list(eye = list(x = 0, y = 1, z = 0)))),
    fig %>% layout(scene = list(camera = 
                                  list(eye = list(x = 0, y = 0, z = 1)))),
    nrows = 1
  )
}

# Example usage
plot_swing_and_stuff(train_mod[train_mod$uid == "1", ])
t <- seq(0, train_mod$central_arc_angle[6], length.out = 33)
swing_path <- matrix(unlist(train_mod$swing_path[6]), 
                     nrow = 3, byrow = FALSE)





## XGBoost Model ----------------------------------------------------------

features <- c("effective_speed", 
              "release_pos_x", "release_pos_y", "release_pos_z",
              #"pfx_x", "pfx_z", 
              "release_spin_rate", "release_extension",
              "spin_axis", "spray_angle", #"direction", 
              "bat_speed", "swing_length",
              #"attack_zone", 
              "plate_x", "plate_z",
              "batter_platoon_adv",
              "swing_efficiency", "swing_time", "swing_acceleration",
              "max_ev", "movement_complexity",
              "contact_x", "contact_y", "contact_z", "central_arc_angle",
              "swing_radius", "dist_to_sweet_spot", "centerline_angle",
              "vertical_bat_angle", "VAA", "VAAAA", "HAA", "HAAAA")



# Prep data for XGBoost
X <- model.matrix(~ . - 1, data = train_mod[, features])
y <- train_mod$outcome_code
dtrain <- xgb.DMatrix(data = X, label = y)


# Set parameters for XGBoost
params <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "auc",   
  max_depth = 10,
  eta = 0.1,
  nthread = 4,
  subsample = 0.8,
  colsample_bytree = 0.8,
  num_class = 5
)


# Train XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain),
  print_every_n = 10
)


# Feature importance
importance <- xgb.importance(feature_names = colnames(X), 
                             model = xgb_model)
print(importance)
xgb.plot.importance(importance)


# K-fold Cross-Validation
set.seed(1234)
cv_model <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  stratified = TRUE,
  early_stopping_rounds = 10,
  print_every_n = 10
)


# AUC Plot
ggplot(melt(as.data.frame(cbind(
  Iteration = 1:nrow(cv_model$evaluation_log),
  Trainauc = cv_model$evaluation_log$train_auc_mean,
  Testauc = cv_model$evaluation_log$test_auc_mean)), 
  id.vars = "Iteration",
  variable.name = "Dataset",
  value.name = "auc"), 
  aes(x = Iteration, y = auc, color = Dataset)) +
  geom_line(linewidth = 1) +  
  labs(title = "XGBoost AUC Across Iterations", 
       x = "Iteration", 
       y = "auc") +
  theme_minimal() +  
  scale_color_manual(values = c("Trainauc" = "blue", 
                                "Testauc" = "red")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),  
        legend.position = "top") 





## Hyperparameter Tuning: Bayesian Optimization ---------------------------
set.seed(5678)
opt_result <- bayesOpt(
  FUN = function(nrounds, max_depth, eta, subsample, colsample_bytree,
                 lambda, alpha) {
    params_tuned <- list(
      booster = "gbtree",
      objective = "multi:softprob",
      eval_metric = "auc",
      max_depth = max_depth,
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      lambda = lambda,
      alpha = alpha
    )
    cv_result <- xgb.cv(
      params = params_tuned,
      data = dtrain,
      nfold = 5,
      nrounds = nrounds,
      verbose = 0,
      num_class = 5
    )
    list(Score = -min(cv_result$evaluation_log$test_auc_mean))
  },
  bounds <- list(
    nrounds = c(10L, 500L),
    eta = c(0.001, 1),
    max_depth = c(1L, 20L),
    colsample_bytree = c(0.1, 1),
    subsample = c(0.1, 1),
    lambda = c(1, 15),
    alpha = c(1, 15)
  ),
  initPoints = 25,
  plotProgress = TRUE
)


tuned_params <- append(list(
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "auc",  
  nthread = 4),
  getBestPars(opt_result)
)





## Re-fitting Tuned XGBoost Model -----------------------------------------
# K-fold Cross-Validation
set.seed(987)
tuned_cv_model <- xgb.cv(
  params = tuned_params,
  data = dtrain,
  nrounds = tuned_params$nrounds,
  nfold = 5,
  stratified = TRUE,
  early_stopping_rounds = 10,
  print_every_n = 10,
  num_class = 5
)


# Tuned AUC Plot
ggplot(melt(as.data.frame(cbind(
  Iteration = 1:nrow(tuned_cv_model$evaluation_log),
  TrainAUC = tuned_cv_model$evaluation_log$train_auc_mean,
  TestAUC = tuned_cv_model$evaluation_log$test_auc_mean)), 
  id.vars = "Iteration",
  variable.name = "Dataset",
  value.name = "auc"), 
  aes(x = Iteration, y = auc, color = Dataset)) +
  geom_line(size = 1) +  
  labs(title = "XGBoost AUC Across Iterations", 
       x = "Iteration", 
       y = "AUC") +
  theme_minimal() +  
  scale_color_manual(values = c("TrainAUC" = "blue", 
                                "TestAUC" = "red")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),  
        legend.position = "top") 


# Final Tuned Model using tuned hyperparams
tuned_xgb_model <- xgb.train(
  params = tuned_params,
  data = dtrain,
  nrounds = tuned_cv_model$best_iteration,
  watchlist = list(train = dtrain),
  print_every_n = 10,
  num_class = 5
)


# Tuned Feature importance
tuned_importance <- xgb.importance(feature_names = colnames(X), 
                                   model = tuned_xgb_model)
print(tuned_importance)
xgb.plot.importance(tuned_importance)




