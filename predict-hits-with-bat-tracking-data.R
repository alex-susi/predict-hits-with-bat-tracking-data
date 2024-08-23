library(dplyr)
library(tidyr)
library(ggplot2)




train <- read.csv("train.csv")


## Feature Engineering ----------------------------------------------------

# Assumptions
q = 0.23 # Collision Efficiency
y0 = 50 # 50 feet
yf = 17/12 # home plate converted to inches


train <- train %>%
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
         swing_efficiency = bat_speed / swing_length,
         plate_speed = 0.91 * release_speed,
         bat_speed_fps = bat_speed * 1.46667,
         bat_speed_avg = bat_speed_fps / 2,
         swing_time = swing_length / bat_speed_avg,
         swing_acceleration = bat_speed_fps / swing_time,
         max_ev = (plate_speed * q) + (bat_speed * (1 + q)),
         movement_complexity = sqrt(pfx_x**2 + pfx_z**2),
         
         # Calculating Vertical Approach Angle (VAA)
         vy_f = -sqrt(vy0**2 - (2 * ay * (y0 - yf))),
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az * t),
         VAA = -atan(vz_f / vy_f) * (180 / pi))



train %>%
  group_by(attack_zone) %>%
  summarize(n = n())





## Attack Zone Plot -------------------------------------------------------
ggplot(train %>% sample_n(2000), 
       aes(x = plate_x, 
           y = plate_z,
           shape = attack_zone,
           color = outcome)) +
  geom_point(size = 1.5, alpha = 0.7) +
  
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
  
  # Set color palette
  scale_color_manual(values = c("out" = "gray", 
                                "home_run" = "red", 
                                "double" = "blue", 
                                "single" = "purple",
                                "triple" = "darkgreen")) +
  
  # Add title and labels
  labs(color = "Outcome", shape = "Attack Zone")























