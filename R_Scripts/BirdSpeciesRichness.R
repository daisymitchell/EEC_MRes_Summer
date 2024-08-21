rm(list=ls())
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
library(tibble)


totalbirdnet_df <- read.csv("/Users/daisymitchell/Desktop/final_bird_dataset.csv")

sites <- read.csv("/Users/daisymitchell/Desktop/SiteParameters.csv")


#### Calculate species richness and total occurrences per site ####
site_diversities <- totalbirdnet_df %>%
  group_by(Site) %>%
  summarise(
    species_richness = n_distinct(Common.Name),
    total_occurrences = n()
  )


##### Create abundance presence-absence matrix for biodiversity calcs #####
bird_abundance <- select(totalbirdnet_df, Common.Name, Site)
# count occurrences of each species at each site
abundance_df <- bird_abundance %>%
  group_by(Common.Name, Site) %>%
  summarise(Count = n(), .groups = 'drop')
# make abundance matrix
abundance_matrix <- abundance_df %>%
  pivot_wider(names_from = Site, values_from = Count, values_fill = list(Count = 0))
# transpose so sites are rows and species columns
abundance_matrix <- t(abundance_matrix)

# Convert to df to set rownames as sites
abundance_df <- as.data.frame(abundance_matrix, stringsAsFactors = FALSE)
colnames(abundance_df) <- abundance_df[1, ]
abundance_df <- abundance_df[-1, ]
row_names <- rownames(abundance_df)

# Convert all values to numeric
abundance_df <- lapply(abundance_df, function(x) as.numeric(as.character(x)))
# Restore row names
rownames(abundance_df) <- row_names
# Convert back to a matrix
abundance_matrix <- as.matrix(abundance_df)


#### Calculate Shannon diversity per site ####
shannondiv <- diversity(abundance_matrix, index = "shannon")
view(shannondiv)
shannondf <- as.data.frame(shannondiv)
# convert sites from rownames back to a column
shannondf <- rownames_to_column(shannondf, var = "Site")

## Add Shannon values to site_diversities 
site_diversities <- left_join(site_diversities, shannondf, by = "Site")


## Export site_diversities table
write.csv(site_diversities, file = "/Users/daisymitchell/Desktop/site_diversities.csv")






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
library(tibble)
library(GGally)
library(MASS)
library(car)



### This df contains site diversity data and site parameters ###
diversity <- read.csv("/Users/daisymitchell/Desktop/sites_shannons.csv")
diversity <- diversity %>% dplyr::select(-X)


## Shannon diversity and total occurrences are potentially uninformative
# both could be skewed by counting the same individual multiple times
# species richness more reliable and more intuitive to interpret diversity index



####### Explore how species richness varies with site type and then explain via environmental variables ##########
richdf <- diversity %>% dplyr::select(-total_occurrences, -shannondiv)

## Get an overview of species richness between sites
overview <- richdf %>%
  group_by(Site_type) %>%
  summarise(across(c(species_richness), list(
    Mean = mean,
    SD = sd,
    Min = min,
    Q1 = ~ quantile(., 0.25),
    Median = median,
    Q3 = ~ quantile(., 0.75),
    Max = max
  )))
overview$IQR <- overview$species_richness_Q3-overview$species_richness_Q1
# Export table
write.csv(overview, file = "/Users/daisymitchell/Desktop/species_richness_overview.csv")




## look at a boxplot of species richness to compare between site types ##

# Calculate range of y-axis values
y_limits <- range(richdf$species_richness, na.rm = TRUE)
# Select breaks between the min and max values
y_breaks <- seq(from = floor(y_limits[1]), to = ceiling(y_limits[2]), by = 4)


richdf$Site_type <- as.factor(richdf$Site_type)
richdf$Site_type <- factor(richdf$Site_type, levels = c("Garden", "Intensive", "Extensive", "Conventional"))
#### Boxplot of species richness between site types ####
ggplot(richdf, aes(x = Site_type, y = species_richness, fill = Site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray'
  )) +
  labs(
    x = "Site Type",
    y = 'Species Richness',
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




## Want to perform statistical test of differences seen in boxplot: first check data distribution ##

## Check if species richness is normally-distributed:
# Histogram of species richness
hist(richdf$species_richness)
# slightly left-skewed but pretty normal looking

# Q-Q plot for normality
qqnorm(richdf$species_richness)
qqline(richdf$species_richness, col = "red")
# this looks pretty good

# Shapiro-Wilk test for normality
shapiro.test(richdf$species_richness)
# W = 0.95072, p-value = 0.3515
# species richness is normally distributed 

# Testing for homogeneity of variance between groups
library(car)
leveneTest(species_richness ~ Site_type, data = richdf)
# p = 0.3666, assumption not violated - homogeneity of variances is confirmed
citation("car")





########## ANOVA of species richness between site types ############
anova_rich <- aov(species_richness ~ Site_type, data = richdf)
summary(anova_rich)
# significant effect of site type on species richness - do post-hoc pairwise comparisons

#### TukeyHSD #####
tukey_rich <- TukeyHSD(anova_rich)
tukey_rich
# only significant pairwise comparison is Garden-Extensive (p adj  = 0.0378057)







######## Adding letters above boxplots to denote signficant differences ##############

max_y <- max(y_limits)  
label_y_position <- max_y + 2  # want the labels at the top of the plot

signif_labels <- data.frame(
  Site_type = c('Garden', 'Intensive', 'Extensive', 'Conventional'),
  label = c('A', 'A, B', 'B', 'A, B'),  
  y_position = label_y_position  # align labels in a straight row at top of graph
)

ggplot(richdf, aes(x = Site_type, y = species_richness, fill = Site_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray'
  )) +
  labs(
    x = "Site Type",
    y = 'Species Richness',
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
    limits = c(y_limits[1], label_y_position + 2),
    breaks = y_breaks           
  ) +
  geom_text(data = signif_labels, aes(x = Site_type, y = y_position, label = label), 
            vjust = -0.5, size = 6)










##### What might be driving this difference? Model species richness against environmental variables ######

## Linear model validation:

## Check for correlation between explanatory variables:
cor(richdf[c("Area_m2", "Green_area", "X.Green_cover", "Height_m")])
# Green_area and Area_m2 are highly correlated (of course) => removed Green_area from model
cor(richdf[c("Area_m2", "X.Green_cover", "Height_m")])
# now all correlations below 0.8 - good to include all these variables


## Check for linearity via scatterplots:
par(mfrow = c(1,2))

# Area
plot(richdf$Area_m2, richdf$species_richness)
abline(lm(species_richness ~ Area_m2, data = richdf), col = "red")
# log Area
plot(log(richdf$Area_m2), richdf$species_richness)
abline(lm(species_richness ~ log(Area_m2), data = richdf), col = "red")
###### log Area is more linear

# Green cover
plot(richdf$X.Green_cover, richdf$species_richness)
abline(lm(species_richness ~ X.Green_cover, data = richdf), col = "red")
# log Green cover
# make column of green cover + 1 (for zero values)
richdf$greencoverplus <- richdf$X.Green_cover + 1
plot(log(richdf$greencoverplus), richdf$species_richness)
abline(lm(species_richness ~ log(greencoverplus), data = richdf), col = "red")
####### non-log Green cover is linear already

# Height
plot(richdf$Height_m, richdf$species_richness)
abline(lm(species_richness ~ Height_m, data = richdf), col = "red")
# log Height
# make column of height + 1 (for zero values)
richdf$heightplus <- richdf$Height_m + 1
plot(log(richdf$heightplus), richdf$species_richness)
abline(lm(species_richness ~ log(heightplus), data = richdf), col = "red")
####### non-log Height is linear already



#
random_lm <- lmer(species_richness ~ log(Area_m2) + X.Green_cover + Height_m + (1|Site_type), data=richdf)
summary(random_lm)
library(sjPlot)
tab_model(random_lm)
#



################### Fitting linear model ###################

lm_rich <- lm(species_richness ~ log(Area_m2) + X.Green_cover + Height_m, data = richdf)
summary(lm_rich)
# height is the only significant explanatory variable (p = 0.0063)

# diagnostic plots
par(mfrow = c(2,2))
plot(lm_rich)
# looks good

# Shapiro-Wilk test
shapiro.test(lm_rich$residuals)
# p-value = 0.6272 which is good

# Multicollinearity
vif(lm_rich)
# all low values below 2 which is good

# Adjusted R-squared:  0.306 



############ Comparing fit of different models #################

############ GLM Poisson ################
glm_pois <- glm(species_richness ~ log(Area_m2) + X.Green_cover + Height_m, 
                family = poisson(link = "log"), 
                data = richdf)
summary(glm_pois)
plot(glm_pois)


# Check for overdispersion (to see if negative binomial would fit)
dispersion <- sum(residuals(glm_pois, type = "pearson")^2) / glm_pois$df.residual
dispersion
# 1.424396
# slightly overdispersed maybe - try nb model

# Fit the Negative Binomial GLM
glm_nb <- glm.nb(species_richness ~ log(Area_m2) + X.Green_cover + Height_m, 
                 data = richdf)
summary(glm_nb)
plot(glm_nb)

AIC(lm_rich, glm_pois, glm_nb)
# poisson better fit than neg binomial
# cannot necessarily compare AIC between different model fitting types
# so check diagnostic plots
par(mfrow = c(2, 2))
plot(lm_rich)
plot(glm_pois)




################## GLM Poisson model is selected for use #######################

## check if area should still be logged - looking for log-linear instead of linear relationship
## poisson applies log to species richness
# Area
par(mfrow = c(1,2))
plot(richdf$Area_m2, log(richdf$species_richness))
abline(lm(log(species_richness) ~ Area_m2, data = richdf), col = "red")
# log Area
plot(log(richdf$Area_m2), log(richdf$species_richness))
abline(lm(log(species_richness) ~ log(Area_m2), data = richdf), col = "red")
###### log Area is more linear


### fitting model
glm_pois <- glm(species_richness ~ log(Area_m2) + X.Green_cover + Height_m, 
                family = poisson(link = "log"), 
                data = richdf)
summary(glm_pois)

## calculate pseudo R squared
deviance <- summary(glm_pois)$deviance
null_dev <- summary(glm_pois)$null.deviance
rsquared <- 1 - (deviance / null_dev)
rsquared

----------------------------------------------------------------------------------

####### accounting for site type as fixed effect ########
richdf$Site_type <- as.factor(richdf$Site_type)

all_glm_pois <- glm(species_richness ~ log(Area_m2) + X.Green_cover + Height_m + Site_type, 
                    family = poisson(link = "log"), 
                    data = richdf)
summary(all_glm_pois)

AIC(all_glm_pois, glm_pois)

par(mfrow = c(2,2))
plot(all_glm_pois)


----------------------------------------------------------------------------------
  
############# site type as random factor ############
random_glm <- glmer(species_richness ~ log(Area_m2) + X.Green_cover + Height_m + (1|Site_type), 
                    family = poisson(link = "log"),
                    data=richdf)
summary(random_glm)
library(sjPlot)
tab_model(random_glm)

AIC(random_glm, glm_pois)

--------------------------------------------------------------------------------

all_glm_anova <- anova(all_glm_pois)
all_glm_anova

library(multcomp)
summary(glht(all_glm_pois, linfct = mcp(Site_type = "Tukey")))


# Perform pairwise comparisons with Tukey adjustment
library(emmeans)
pairwise_comparisons <- emmeans(all_glm_pois, pairwise ~ Site_type, adjust = "tukey")
summary(pairwise_comparisons)

----------------------------------------------------------------------------------
  

  

######################### Plotting model ################################
##### significant effect of height so do species richness against height

# scatterplot
ggplot(richdf, aes(x = Height_m, y = species_richness, colour = Site_type)) +
  geom_point(size = 4) +  # Increase point size
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Species Richness", colour = "Site type") +  # Set axis and legend labels
  theme_light() +
  theme(
    text = element_text(size = 18),          
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)    
  )



############## For Supplementary Info: adding site name labels ##################
library(ggrepel)
# Create the scatter plot with site name labels
ggplot(richdf, aes(x = Height_m, y = species_richness, colour = Site_type)) +
  geom_point(size = 4) +  # Increase point size
  geom_text_repel(aes(label = Site), size = 5) +  # Add site name labels with repelling
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Species Richness", colour = "Site type") +  
  theme_light() +
  theme(
    text = element_text(size = 18),         
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )





###################### PARTIAL RESIDUALS SCATTERPLOT - ADDING LINE OF BEST FIT ##############################
residuals <- residuals(glm_pois, type = "pearson")
fitted_values <- predict(glm_pois, type = "link")
partial_residuals <- residuals + coef(glm_pois)["Height_m"] * richdf$Height_m

# Add partial residuals to the original data frame
richdf <- richdf %>%
  mutate(partial_residuals = partial_residuals)

# Create the plot
ggplot(richdf, aes(x = Height_m, y = partial_residuals, colour = Site_type)) +
  geom_point(size = 4) +  # Increase point size
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Species Richness (Partial Residuals)", colour = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),     
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )


## changes geom_point size from 4 to 6 and added 2 to every element_text
############# UNIQUE SHAPE OF POINTS SPECIFIC TO SITE TYPE ################
ggplot(richdf, aes(x = Height_m, y = partial_residuals)) +
  geom_point(aes(colour = Site_type, shape = Site_type), size = 6) +  # Map colour and shape to Site_type
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Single line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  scale_shape_manual(values = c(
    'Intensive' = 16,  # Solid circle
    'Garden' = 17,     # Solid triangle
    'Extensive' = 18,  # Solid diamond
    'Conventional' = 15  # Solid circle
  )) +
  labs(x = "Height (m)", y = "Species Richness (Partial Residuals)", colour = "Site type", shape = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 20),           
    axis.title = element_text(size = 22),     
    axis.text = element_text(size = 18),      
    legend.title = element_text(size = 20),   
    legend.text = element_text(size = 18)     
  )


############ ADDING SITE NAMES FOR SUPP INFO #################
library(ggrepel)

ggplot(richdf, aes(x = Height_m, y = partial_residuals)) +
  geom_point(aes(colour = Site_type, shape = Site_type), size = 6) +  # Map colour and shape to Site_type
  geom_text_repel(aes(label = Site, colour = Site_type), size = 5, show.legend = FALSE) +  # Add site name labels without adding them to the legend
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Single line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  scale_shape_manual(values = c(
    'Intensive' = 16,  # Solid circle
    'Garden' = 17,     # Solid triangle
    'Extensive' = 18,  # Solid diamond
    'Conventional' = 15  # Solid square
  )) +
  labs(x = "Height (m)", y = "Species Richness (Partial Residuals)", colour = "Site type", shape = "Site type") +
  theme_light() +
  theme(
    text = element_text(size = 20),           
    axis.title = element_text(size = 22),     
    axis.text = element_text(size = 18),      
    legend.title = element_text(size = 20),   
    legend.text = element_text(size = 18)     
  )








############## WITH LINE OF BEST FIT - For Supplementary Info: adding site name labels ##################
library(ggrepel)
# Create the scatter plot with site name labels
ggplot(richdf, aes(x = Height_m, y = partial_residuals, colour = Site_type)) +
  geom_point(size = 4) +  
  geom_text_repel(aes(label = Site), size = 5) +  # Add site name labels with repelling
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Line of best fit
  scale_colour_manual(values = c(
    'Intensive' = 'chartreuse2',
    'Garden' = 'chartreuse4',
    'Extensive' = 'darkorange2',
    'Conventional' = 'darkgray')
  ) +
  labs(x = "Height (m)", y = "Species Richness (Partial Residuals)", colour = "Site type") +  
  theme_light() +
  theme(
    text = element_text(size = 18),           
    axis.title = element_text(size = 20),   
    axis.text = element_text(size = 16),      
    legend.title = element_text(size = 18), 
    legend.text = element_text(size = 16)     
  )










################ MEAN AND SD OF HEIGHT OF EACH SITE TYPE ########################

height_stats <- richdf %>%
  group_by(Site_type) %>%  # Group by Site_type
  summarise(
    mean_height = mean(Height_m),  # Calculate mean height
    sd_height = sd(Height_m)       # Calculate standard deviation of height
  )

print(height_stats)



area_stats <- richdf %>%
  group_by(Site_type) %>%
  summarise(
    mean_area = mean(Area_m2),
    sd_area = sd(Area_m2)
  )
area_stats



