rm(list=ls())
dev.off()
setwd("/Users/daisymitchell/MyRCoursework/Code")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(vegan)
library(stats)
library(lme4)
library(broom)
library(openxlsx)
library(ape)
library(ggplot2)
library(reshape2)
library(cluster)
library(ggalt)
library(ggrepel)
library(fclust)
library(ggforce)

totalbirdnet_df <- read.csv("/Users/daisymitchell/Desktop/final_bird_dataset.csv")

sites <- read.csv("/Users/daisymitchell/Desktop/sites_shannons.csv")
sites <- sites %>% dplyr::select(-X)




##### Convert into presence-absence matrix #####
bird_df <- dplyr::select(totalbirdnet_df, Common.Name, Site)

# Remove duplicate rows of same species in same site
bird_df <- bird_df %>%
  distinct(Common.Name, Site)

# Make presence-absence matrix
presence_absence_matrix <- bird_df %>%
  mutate(Presence = 1) %>%
  pivot_wider(names_from = Site, values_from = Presence, values_fill = list(Presence = 0))

# Set species names to row names
presence_absence_df <- as.data.frame(presence_absence_matrix)
species_row_names <- presence_absence_df$Common.Name
pa_no_row_names <- presence_absence_df[, -1]
rownames(pa_no_row_names) <- species_row_names

presence_absence_matrix <- as.matrix(pa_no_row_names)

# Transpose as dissimilarity analyses (to be between sites not species) = sites as rows and species as columns
presence_absence_matrix <- t(presence_absence_matrix)









####### Sørensen Index ########
sorensen <- vegdist(presence_absence_matrix, method = "bray", binary = TRUE)

## View as a heatmap
# Convert the dissimilarity matrix to a dataframe
sorensen_matrix <- as.matrix(sorensen)
sorensen_df <- as.data.frame(as.table(sorensen_matrix))

head(sorensen_df)

# Rename columns
colnames(sorensen_df) <- c("Site1", "Site2", "Sørensen_dissimilarity")
# Plot the heatmap
ggplot(sorensen_df, aes(x = Site1, y = Site2, fill = Sørensen_dissimilarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Sørensen Dissimilarity Heatmap",
       x = "Site",
       y = "Site",
       fill = "Sørensen dissimilarity")





##### PCoA on Sørensen dissimilarity ######
pcoa <- cmdscale(sorensen, k=10, eig=TRUE)
# Extract the scores 
pcoa_axes <- pcoa$points
colnames(pcoa_axes) <- paste0('sorensen_raw_pcoa_', 1:10)
# Look at the correlations between all axes
# using zapsmall to hide all correlations smaller than 10^-10
zapsmall(cor(pcoa_axes), digits=10)
# as expected, none of the axes are correlated (all 0 except 1 when compared to self)

# Convert the pcoa axis values to a data frame and label by site
pcoa_axes_df <- data.frame(pcoa_axes)
pcoa_axes_df$Site <- rownames(pcoa_axes)
# Merge onto the sites data
sites <- merge(sites, pcoa_axes_df, by='Site')

# Export 
# write.csv(sites, "/Users/daisymitchell/Desktop/PCoA_axes.csv")



######### Plots to see how the community metrics reflect site characteristics ###########
# Area
ggplot(sites, aes(sorensen_raw_pcoa_1, sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = Area_m2), size = 4) +  
  scale_colour_gradientn(colors = hcl.colors(20)) + 
  theme_classic() +
  theme(
    text = element_text(size = 16),           
    axis.title = element_text(size = 18),     
    axis.text = element_text(size = 14),      
    legend.title = element_text(size = 16),   
    legend.text = element_text(size = 14)     
  )
# Add site name labels
areaplot <- ggplot(sites, aes(x = sorensen_raw_pcoa_1, y = sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = Area_m2), size = 4) +  
  geom_text_repel(aes(label = Site), size = 6) +  
  scale_colour_gradientn(colors = hcl.colors(20)) + 
  labs(x = NULL, y = NULL, color=expression("Area"~(m^2))) +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )


# Green cover
ggplot(sites, aes(sorensen_raw_pcoa_1, sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = X.Green_cover)) + 
  scale_colour_gradientn(colors=hcl.colors(20)) + 
  theme_classic() 


# Height
ggplot(sites, aes(sorensen_raw_pcoa_1, sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = Height_m), size = 4) + 
  scale_colour_gradientn(colors=hcl.colors(20)) + 
  theme_classic() +
  theme(
    text = element_text(size = 16),          
    axis.title = element_text(size = 18),     
    axis.text = element_text(size = 14),      
    legend.title = element_text(size = 16),   
    legend.text = element_text(size = 14)     
  )
# Add site name labels
heightplot <- ggplot(sites, aes(sorensen_raw_pcoa_1, sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = Height_m), size = 4) + 
  geom_text_repel(aes(label = Site), size = 6) +  
  scale_colour_gradientn(colors=hcl.colors(20)) + 
  labs(x = NULL, y = NULL, color = "Height (m)") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16),     
    axis.text.x = element_blank()
  )

# Site type
ggplot(sites, aes(sorensen_raw_pcoa_1, sorensen_raw_pcoa_2)) +
  geom_point(aes(colour = Site_type)) + 
  theme_classic() 





########## Height and Area plots together #############
library(grid)
library(gridExtra)
grid.arrange(heightplot, areaplot, nrow = 2) 

grid.arrange(
  heightplot, areaplot,
  nrow = 2,
  bottom = textGrob("PCoA 1 (35.15%)", gp = gpar(fontsize = 20)),  # Common x-axis label
  left = textGrob("PCoA 2 (16.51%)", rot = 90, gp = gpar(fontsize = 20))  # Common y-axis label
)







### How many PCoA axes to consider? ###
par(mfrow=c(1,2))
eig <- pcoa$eig[pcoa$eig >0] 
barplot(eig / sum(eig), main='Axis variation')
barplot(cumsum(eig)/ sum(eig), main='Cumulative variation')
# Print the percentage variation of the first 10
head(sprintf('%0.2f%%', (eig / sum(eig)) * 100), n=10)
head(eig, n = 10)

## variance explained:
# PCoA 1 (35.15%)
# PCoA 2 (16.51%)









############ Linear model of PCoA axis 1 against site variables ################

# confirming that sorensen_raw_pcoa_1 is normally distributed
hist(sites$sorensen_raw_pcoa_1)
# yes, slight left skew but good
qqnorm(sites$sorensen_raw_pcoa_1)
qqline(sites$sorensen_raw_pcoa_1, col = "red")
# Q-Q plot for normality is good
shapiro.test(sites$sorensen_raw_pcoa_1)
# Shapiro-Wilk test for normality: W = 0.94951, p-value = 0.3334 is good


## Check for linearity via scatterplots
par(mfrow = c(1,2))

# Area
plot(sites$Area_m2, sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ Area_m2, data = sites), col = "red")
# log Area
plot(log(sites$Area_m2), sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ log(Area_m2), data = sites), col = "red")
######## log Area is more linear

# Green cover
plot(sites$X.Green_cover, sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ X.Green_cover, data = sites), col = "red")
# log Green cover
sites$greencoverplus <- sites$X.Green_cover + 1
plot(log(sites$greencoverplus), sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ log(greencoverplus), data = sites), col = "red")
####### non-log Green cover is linear already

# Height
plot(sites$Height_m, sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ Height_m, data = sites), col = "red")
# log Height
sites$heightplus <- sites$Height_m + 1
plot(log(sites$heightplus), sites$sorensen_raw_pcoa_1)
abline(lm(sorensen_raw_pcoa_1 ~ log(heightplus), data = sites), col = "red")
####### non-log Height is linear already




----------------------------------------------------------------------------------
  

################# ANOVA and TukeyHSD PCoA 1 by Site Type #####################
shapiro.test(sites$sorensen_raw_pcoa_1)
# normally distributed
leveneTest(sorensen_raw_pcoa_1 ~ Site_type, data=sites)
# homogeneity of variances between site types

pcoa_anova <- aov(sorensen_raw_pcoa_1 ~ Site_type, data=sites)
summary(pcoa_anova)

pcoa_tukey <- TukeyHSD(pcoa_anova)
pcoa_tukey




################# BOXPLOT WITH LABELS FOR SIG DIFF #########################
sites$Site_type <- as.factor(sites$Site_type)
sites$Site_type <- factor(sites$Site_type, levels = c("Garden", "Intensive", "Extensive", "Conventional"))
#### Boxplot of species richness between site types ####
y_limits <- range(sites$sorensen_raw_pcoa_1, na.rm = TRUE)
y_breaks <- seq(from = floor(y_limits[1]), to = ceiling(y_limits[2]), by = 0.1)

ggplot(sites, aes(x = Site_type, y = sorensen_raw_pcoa_1, fill = Site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray'
  )) +
  labs(
    x = "Site Type",
    y = 'PCoA 1 (35.15%)',
    fill = 'Site type'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 16)
  ) +
  scale_y_continuous(
    limits = y_limits,          # Set the y-axis limits
    breaks = y_breaks           # Set breaks
  )

################## adding sig labels #####################
max_y <- max(y_limits)  
label_y_position <- max_y + 0.1  # want the labels at the top of the plot

signif_labels <- data.frame(
  Site_type = c('Garden', 'Intensive', 'Extensive', 'Conventional'),
  label = c('A', 'A, B', 'B', 'A, B'),  
  y_position = label_y_position  # align labels in a straight row at top of graph
)

ggplot(sites, aes(x = Site_type, y = sorensen_raw_pcoa_1, fill = Site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray'
  )) +
  labs(
    x = "Site Type",
    y = 'PCoA 1 (35.15%)',
    fill = 'Site type'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 16)
  ) +
  scale_y_continuous(
    limits = c(y_limits[1], label_y_position),
    breaks = y_breaks           
  ) +
  geom_text(data = signif_labels, aes(x = Site_type, y = y_position, label = label), 
            vjust = -0.5, size = 6)


----------------------------------------------------------------------------------

########## linear model accounting for site type #############
model_type <- lm(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m + Site_type, data=sites)
summary(model_type)

AIC(model_type, model_all)
### including site type increases AIC - do not include
library(lme4)
random_model <- lmer(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m + (1 | Site_type), data = sites)
summary(random_model)
library(sjPlot)
tab_model(random_model)

################## Fitting linear model ######################
model_all <- lm(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m, data=sites)
summary(model_all)

par(mfrow = c(2,2))
plot(model_all)
#shapiro.test(residuals(model_all))



### gaussian GLM produces worse diagnostic plots ###
glm_model_all <- glm(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m, family = gaussian(link = "identity"), data = sites)
plot(glm_model_all)

## try log transformation
sites$pcoa1 <- sites$sorensen_raw_pcoa_1+1
sites$pcoa1 <- log(sites$pcoa1)

log_model_all <- lm(pcoa1 ~ log(Area_m2) + X.Green_cover + Height_m, data=sites)
summary(log_model_all)

plot(log_model_all)

AIC(model_all, log_model_all)
## this is worse than non-logged model



######### Checking relationships with PCoA Axis 2 ############

# normal distribution
hist(sites$sorensen_raw_pcoa_2)
# yes, slight left skew but good
qqnorm(sites$sorensen_raw_pcoa_2)
qqline(sites$sorensen_raw_pcoa_2, col = "red")
# Q-Q plot for normality is good
shapiro.test(sites$sorensen_raw_pcoa_2)
# Shapiro-Wilk test for normality: W = 0.95532, p-value = 0.4273 is good


# Check for linearity via scatterplots
par(mfrow = c(1,2))

# Area
plot(sites$Area_m2, sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ Area_m2, data = sites), col = "red")
# log Area
plot(log(sites$Area_m2), sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ log(Area_m2), data = sites), col = "red")
######## log Area is more linear

# Green cover
plot(sites$X.Green_cover, sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ X.Green_cover, data = sites), col = "red")
# log Green cover
sites$greencoverplus <- sites$X.Green_cover + 1
plot(log(sites$greencoverplus), sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ log(greencoverplus), data = sites), col = "red")
####### non-log Green cover is linear already

# Height
plot(sites$Height_m, sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ Height_m, data = sites), col = "red")
# log Height
sites$heightplus <- sites$Height_m + 1
plot(log(sites$heightplus), sites$sorensen_raw_pcoa_2)
abline(lm(sorensen_raw_pcoa_2 ~ log(heightplus), data = sites), col = "red")
####### non-log Height is linear already



###### Fitting linear model for axis 2 #########
pcoa2_model_all <- lm(sorensen_raw_pcoa_2 ~ log(Area_m2) + X.Green_cover + Height_m, data=sites)
summary(pcoa2_model_all)

par(mfrow = c(2,2))
plot(pcoa2_model_all)

######## no significant results - CHANGE PLOTS TO BE PCoA AXIS 1 VERSUS HEIGHT AND AREA #######




############# NEW PLOTS to see how the community metrics reflect site characteristics ##############
sites$Site_type <- as.factor(sites$Site_type)
sites$Site_type <- factor(sites$Site_type, levels = c("Garden", "Intensive", "Extensive", "Conventional"))

# log(Area)
axis1area_plot <- ggplot(sites, aes(log(Area_m2), sorensen_raw_pcoa_1, colour = Site_type)) +
  geom_point(size = 4) +  
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = expression("log(Area)"~(m^2)), y = NULL, colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )
axis1area_plot


# Height
axis1height_plot <- ggplot(sites, aes(Height_m, sorensen_raw_pcoa_1, colour = Site_type)) +
  geom_point(size = 4) +  
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = NULL, colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),  
    legend.text = element_text(size = 16)     
  )
axis1height_plot


library(grid)
library(gridExtra)
library(cowplot)


#### Plot both with a common color legend

# Function to extract the legend
extract_legend <- function(ggplot_obj) {
  g <- ggplot_gtable(ggplot_build(ggplot_obj))
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}
# Extract legend
legend <- extract_legend(axis1height_plot)

# Remove legends from original plots
noleg_axis1height_plot <- axis1height_plot + theme(legend.position = "none")
noleg_axis1area_plot <- axis1area_plot + theme(legend.position = "none")

# Combine the plots into a single grid
plots_combined <- plot_grid(
  noleg_axis1height_plot, 
  noleg_axis1area_plot,
  ncol = 1,        
  align = "v",     
  rel_heights = c(1, 1)  
)

# Combine the plots and the legend
final_plot <- plot_grid(
  plots_combined, 
  legend, 
  ncol = 2,        
  rel_widths = c(4, 1),  
  align = "v",     
  axis = "tb"      
)

# Add the common x-axis label
grid.arrange(
  final_plot,
  left = textGrob("PCoA 1 (35.15%)", rot = 90, gp = gpar(fontsize = 20))
)










----------------------------------------------------------------------------------------

######### For supp info: plots with site name labels added ###############
library(ggrepel)

# log(Area)
label_area_plot <- ggplot(sites, aes(log(Area_m2), sorensen_raw_pcoa_1, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_text_repel(aes(label = Site), size = 4, max.overlaps = 20) +  
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = expression("log(Area)"~(m^2)), y = NULL, colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )
label_area_plot

# Height
label_height_plot <- ggplot(sites, aes(Height_m, sorensen_raw_pcoa_1, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_text_repel(aes(label = Site), size = 4, max.overlaps = 20) +  
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = NULL, colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )
label_height_plot

# Extract legend
extract_legend <- function(ggplot_obj) {
  g <- ggplot_gtable(ggplot_build(ggplot_obj))
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}

legend <- extract_legend(label_height_plot)

# Remove legends from original plots
noleg_label_height_plot <- label_height_plot + theme(legend.position = "none")
noleg_label_area_plot <- label_area_plot + theme(legend.position = "none")

# Combine the plots
label_plots_combined <- plot_grid(
  noleg_label_height_plot, 
  noleg_label_area_plot,
  ncol = 1,        
  align = "v",     
  rel_heights = c(1, 1)  
)

# Combine the plots and legend
label_final_plot <- plot_grid(
  label_plots_combined, 
  legend, 
  ncol = 2,        
  rel_widths = c(4, 1),  
  align = "v",     
  axis = "tb"      
)

# Add the common x-axis label
grid.arrange(
  label_final_plot,
  left = textGrob("PCoA 1 (35.15%)", rot = 90, gp = gpar(fontsize = 20))
)










############# AREA MEAN AND SD PER SITE TYPE ##############
area_stats <- sites %>%
  group_by(Site_type) %>%
  summarise(
    mean_area = mean(Area_m2),
    sd_area = sd(Area_m2)
  )
area_stats


############ summary statistics of PCoA 1 values ##########
overview <- sites %>%
  group_by(Site_type) %>%
  summarise(across(c(sorensen_raw_pcoa_1), list(
    Mean = mean,
    SD = sd,
    Min = min,
    Q1 = ~ quantile(., 0.25),
    Median = median,
    Q3 = ~ quantile(., 0.75),
    Max = max
  )))
overview$IQR <- overview$sorensen_raw_pcoa_1_Q3-overview$sorensen_raw_pcoa_1_Q1
# Export table
write.csv(overview, file = "/Users/daisymitchell/Desktop/PCoA_1_overview.csv")






######### LINE OF BEST FIT ADDED TO NEW PLOTS: PCoA 1 versus height and area #####################

library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
library(grid)

# Fit the linear model
model_all <- lm(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m, data=sites)

# Calculate partial residuals for each predictor
partial_residuals_area <- residuals(model_all) + coef(model_all)["log(Area_m2)"] * log(sites$Area_m2)
partial_residuals_height <- residuals(model_all) + coef(model_all)["Height_m"] * sites$Height_m

# Add partial residuals to the original data frame
sites <- sites %>%
  mutate(partial_residuals_area = partial_residuals_area,
         partial_residuals_height = partial_residuals_height)

# Plot for log(Area_m2)
axis1area_plot <- ggplot(sites, aes(x = log(Area_m2), y = partial_residuals_area, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = expression("log(Area)"~(m^2)), y = "Partial Residuals", colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )

# Plot for Height_m
axis1height_plot <- ggplot(sites, aes(x = Height_m, y = partial_residuals_height, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Partial Residuals", colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),  
    legend.text = element_text(size = 16)     
  )

# Function to extract the legend
extract_legend <- function(ggplot_obj) {
  g <- ggplot_gtable(ggplot_build(ggplot_obj))
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}

# Extract legend
legend <- extract_legend(axis1height_plot)

# Remove legends from original plots
noleg_axis1height_plot <- axis1height_plot + theme(legend.position = "none")
noleg_axis1area_plot <- axis1area_plot + theme(legend.position = "none")

# Combine the plots into a single grid
plots_combined <- plot_grid(
  noleg_axis1height_plot, 
  noleg_axis1area_plot,
  ncol = 1,        
  align = "v",     
  rel_heights = c(1, 1)  
)

# Combine the plots and the legend
final_plot <- plot_grid(
  plots_combined, 
  legend, 
  ncol = 2,        
  rel_widths = c(4, 1),  
  align = "v",     
  axis = "tb"      
)

# Add the common y-axis label
grid.arrange(
  final_plot,
  left = textGrob("PCoA 1 (35.15%)", rot = 90, gp = gpar(fontsize = 20))
)














################### For supp info: plots with site name labels added ################
library(ggrepel)

# Plot for log(Area_m2)
lab_axis1area_plot <- ggplot(sites, aes(x = log(Area_m2), y = partial_residuals_area, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_text_repel(aes(label = Site), size = 4, max.overlaps = 20) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = expression("log(Area)"~(m^2)), y = "Partial Residuals", colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )

# Plot for Height_m
lab_axis1height_plot <- ggplot(sites, aes(x = Height_m, y = partial_residuals_height, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_text_repel(aes(label = Site), size = 4, max.overlaps = 20) +  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Partial Residuals", colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),  
    legend.text = element_text(size = 16)     
  )

# Function to extract the legend
extract_legend <- function(ggplot_obj) {
  g <- ggplot_gtable(ggplot_build(ggplot_obj))
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}

# Extract legend
legend <- extract_legend(lab_axis1height_plot)

# Remove legends from original plots
noleg_lab_axis1height_plot <- lab_axis1height_plot + theme(legend.position = "none")
noleg_lab_axis1area_plot <- lab_axis1area_plot + theme(legend.position = "none")

# Combine the plots into a single grid
plots_combined <- plot_grid(
  noleg_lab_axis1height_plot, 
  noleg_lab_axis1area_plot,
  ncol = 1,        
  align = "v",     
  rel_heights = c(1, 1)  
)

# Combine the plots and the legend
final_plot <- plot_grid(
  plots_combined, 
  legend, 
  ncol = 2,        
  rel_widths = c(4, 1),  
  align = "v",     
  axis = "tb"      
)

# Add the common y-axis label
grid.arrange(
  final_plot,
  left = textGrob("PCoA 1 (35.15%)", rot = 90, gp = gpar(fontsize = 20))
)






  
  
  
  
  
  
  
  









------------------------------------------------------------------------------------------

####### checking for fixed and random effects of site type ########
class(sites$Site_type)
as.factor(sites$Site_type)
model_fix <- lm(sorensen_raw_pcoa_1 ~ log(Area_m2) + X.Green_cover + Height_m + Site_type, data=sites)
summary(model_fix)
# no significant effects of site type and including it increases AIC - worse model than model_all below (without site type)
plot(model_fix)

######### model simplification ??? ##########
model_area_height <- lm(sorensen_raw_pcoa_1 ~ log(Area_m2) + Height_m, data = sites)
summary(model_area_height)

AIC(model_all, model_fix, model_area_height)

------------------------------------------------------------------------------------------------
  
  













########## K-means on PCoA data ##############

# Perform PCoA on Sørensen dissimilarity
pcoa <- cmdscale(sorensen, k = 10, eig = TRUE)

# Extract the PCoA scores for first 2 dimensions
pcoa_axes <- pcoa$points
colnames(pcoa_axes) <- paste0('sorensen_raw_pcoa_', 1:10)

# Determine  optimal number of clusters using Elbow plot
wss <- function(k) {
  kmeans(pcoa_axes[, 1:2], centers = k, nstart = 10)$tot.withinss  
}

# Compute WSS for k = 1 to k = 10
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# Plot Elbow curve
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Plot for K-means Clustering on PCoA Data")




# K-means Clustering on PCoA data
set.seed(123)  # For reproducibility
optimal_k <- 4  # Determined from the Elbow plot

# Perform K-means clustering
kmeans_result <- kmeans(pcoa_axes_df[, -ncol(pcoa_axes_df)], centers = optimal_k, nstart = 10)

# Add cluster results to PCoA data frame
pcoa_axes_df$cluster <- factor(kmeans_result$cluster)

# Merge with the sites data to include site types
pcoa_data_merged <- merge(pcoa_axes_df, sites, by = 'Site')

# Plot 
ggplot(pcoa_data_merged, aes(x = sorensen_raw_pcoa_1.x, y = sorensen_raw_pcoa_2.x, color = Site_type)) +
  geom_point(size = 4) +
  geom_text(aes(label = Site), vjust = -1, size = 4) +
  # Draw circles around clusters
  geom_encircle(aes(group = cluster), color = "black", size = 1, expand = 0.05) +
  labs(x = "PCoA 1 (35.15%)", y = "PCoA 2 (16.51%)", color = "Site type") +
  theme_minimal() +
  scale_color_manual(values = c('Garden' = 'chartreuse4', 'Intensive' = 'chartreuse2', 'Extensive' = 'darkorange2', 'Conventional' = 'darkgray')) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )






###### trying to find height and area thresholds #######

# for pcoa axis 1
ggplot(sites, aes(x=log(Area_m2), y=sorensen_raw_pcoa_1)) + 
  geom_point(aes(color = factor(Site_type))) +
  geom_smooth(aes(color = factor(Site_type)), method = "lm", se = FALSE) +
  labs(x = "Log of Area (m^2)", y = "Sorensen Raw PCoA 1", color = "Site Type") +
  theme_minimal()


ggplot(sites, aes(x=Height_m, y=sorensen_raw_pcoa_1))+
  geom_point(aes(color = factor(Site_type)))+
  geom_smooth(aes(color = factor(Site_type)), method = "lm", se = FALSE) +
  labs(x = "Height (m)", y = "Sorensen Raw PCoA 1", color = "Site Type") +
  theme_minimal()



# for species richness
ggplot(sites, aes(x=log(Area_m2), y=species_richness)) + 
  geom_point(aes(color = factor(Site_type))) +
  geom_smooth(aes(color = factor(Site_type)), method = "lm", se = FALSE) +
  labs(x = "Log of Area (m^2)", y = "Species richness", color = "Site Type") +
  theme_minimal()


ggplot(sites, aes(x=Height_m, y=species_richness))+
  geom_point(aes(color = factor(Site_type)))+
  geom_smooth(aes(color = factor(Site_type)), method = "lm", se = FALSE) +
  labs(x = "Height (m)", y = "Species richness", color = "Site Type") +
  theme_minimal()

#### do not have enough data for this









--------------------------------------------------------------------------------------------------


######### Fuzzy C-means clustering on PCoA data ##########


set.seed(123)  # For reproducibility
fcm_result <- FKM(pcoa_axes, k = 4)  

# Convert the PCoA axes to a data frame and add cluster results
fuzzy_pcoa_axes_df <- as.data.frame(pcoa_axes)
fuzzy_pcoa_axes_df$cluster <- factor(apply(fcm_result$U, 1, which.max))

# Merge with site types
site_names <- rownames(fuzzy_pcoa_axes_df)
fuzzy_pcoa_axes_df$Site <- site_names

fuzzy_pcoa_data_merged <- merge(fuzzy_pcoa_axes_df, sites, by.x = 'Site', by.y = 'Site')

# Plot
ggplot(fuzzy_pcoa_data_merged, aes(x = sorensen_raw_pcoa_1.x, y = sorensen_raw_pcoa_2.x, color = Site_type)) +
  geom_point(size = 3) +
  geom_text(aes(label = Site), vjust = -1, size = 3) +
  # Draw circles around clusters
  geom_encircle(aes(group = cluster), color = "black", size = 1, expand = 0.05) +
  labs(title = "PCoA Plot with Fuzzy C-Means Clustering", x = "PCoA Axis 1", y = "PCoA Axis 2", color = "Site type") +
  theme_minimal() +
  scale_color_manual(values = c('Garden' = 'chartreuse4', 'Intensive' = 'chartreuse2', 'Extensive' = 'darkorange2', 'Conventional' = 'darkgray'))











