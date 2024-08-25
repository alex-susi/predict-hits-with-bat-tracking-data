library(dplyr)
library(tidyr)
library(ggplot2)




train <- read.csv("train.csv")


## Feature Engineering ----------------------------------------------------

# Assumptions
q = 0.23 # Collision Efficiency
y0 = 50 # 50 feet
yf = 17 / 12 # home plate converted to inches


train_mod <- train %>%
  mutate(attack_zone = 'waste',
         attack_zone = case_when(
             plate_x >= -0.558 & 
               plate_x <= 0.558 & 
               plate_z >= 1.833 & 
               plate_z <= 3.166 ~ 'heart',
             plate_x >= -1.108 & 
               plate_x <= 1.108 & 
               plate_z >= 1.166 & 
               plate_z <= 3.833 & 
               attack_zone != 'heart' ~ 'shadow',
             plate_x >= -1.666 & 
               plate_x <= 1.666 & 
               plate_z >= 0.5 & 
               plate_z <= 4.5 & 
               !attack_zone %in% c('heart', 'shadow') ~ 'chase',
             TRUE ~ attack_zone),
         batter_platoon_adv = ifelse(is_lhp != is_lhb, 1, 0),
         swing_efficiency = bat_speed / swing_length,
         plate_speed = 0.91 * release_speed,
         bat_speed_fps = bat_speed * 1.46667,
         bat_speed_avg = bat_speed_fps / 2,
         swing_time = swing_length / bat_speed_avg,
         swing_acceleration = bat_speed_fps / swing_time,
         max_ev = (plate_speed * q) + (bat_speed * (1 + q)),
         movement_complexity = sqrt(pfx_x**2 + pfx_z**2),
         
         
         # Vertical Approach Angle (VAA)
         vy_f = -sqrt(vy0**2 - (2 * ay * (y0 - yf))),
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az * t),
         VAA = -atan(vz_f / vy_f) * (180 / pi),
         
         
         # Horizontal Approach Angle (HAA)
         vx_f = vx0 + (ax * t),
         HAA = -atan(vx_f / vy_f) * (180 / pi)) %>%
  
  
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
  relocate(outcome, outcome_code, outcome_woba, .after = last_col())




# Exploring the new features
train_mod %>%
  #group_by(outcome_code, attack_zone) %>%
  group_by(pitch_name, is_lhp) %>%
  summarize(n = n(),
            Avg_VAA = mean(VAA),
            Avg_HAA = mean(HAA)) %>%
  #filter(is_lhp == 0) %>%
  as.data.frame()


train_mod %>%
  filter(pitch_type == "FF") %>%
  filter(HAA >= 1.295 & HAA <= 1.305) %>%
  select(uid, is_lhp, release_pos_x, plate_x, HAA, HAAAA)










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
       aes(x = VAAAA, y = plate_z, color = outcome)) +
  geom_jitter(width = 0.2, alpha = 0.6) +  
  # Adds some randomness to the x positions to avoid overplotting
  scale_color_manual(values = c("out" = "black", 
                                "single" = "darkgreen",
                                "double" = "blue",
                                "triple" = "purple",
                                "home_run" = "red")) +
  labs(title = "Vertical Approach Angle Relative to Average",
       x = "VAAAA",
       y = "Pitch Height",
       color = "Outcome") +
  theme_minimal() +
  xlim(-2.5, 2.5) +
  theme(legend.position = "right")













