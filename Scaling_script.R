# Coding Club Tutorial - Transforming and scaling data
# Dexuan Zhu, dxzhu@ucdavis.edu
# 8/17/2025

library(tidyverse)  # contains dplyr (data manipulation), ggplot2 (data visualization) and other useful packages
#install.packages("cowplot")
library(cowplot)  # making effective plot grids
library(MASS)  # contains boxcox() function
library(ggeffects)  # model predictions
library(broom)  # extracting model summaries

# Import Data
setwd(normalizePath("C:/Users/Tobyz/Desktop/Toby在大学/Maloof Lab/Coding_Club_Tutorials/Modeling/CC-data-scaling"))
LPI_species <- read.csv("LPI_species.csv", stringsAsFactors = FALSE)  # remember to change the filepath appropriately   

str(LPI_species)
summary(LPI_species)

# Extract the white stork data from the main dataset and adjust the year variable
stork <- LPI_species %>%
  filter(Common.Name == 'White stork' & Sampling.method == 'Direct counts')%>%
  mutate(year = parse_number(as.character(year)))  # convert the year column to character and then parse the numeric part

# Define a custom plot theme

plot_theme <- function(...){
  theme_bw() +
    theme(
      # adjust axes
      axis.line = element_blank(),
      axis.text = element_text(size = 14,
                               color = "black"),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      axis.title = element_text(size = 14,
                                color = 'black'),
      axis.ticks = element_blank(),
      
      # add a subtle grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      
      # adjust background colors
      plot.background = element_rect(fill = "white",
                                     color = NA),
      panel.background = element_rect(fill = "white",
                                      color = NA),
      legend.background = element_rect(fill = NA,
                                       color = NA),
      # adjust titles
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14, hjust = 0,
                                 color = "black"),
      plot.title = element_text(size = 20,
                                color = 'black',
                                margin = margin(10, 10, 10, 10),
                                hjust = 0.5),
      
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "black",
                                   margin = margin(0, 0, 30, 0))
    )
  
}

# Remember, if you put the whole code in the brackets it will
# display in the plot viewer right away!

# Look at the distribution of the data
(stork_hist <- ggplot(data = stork) +
    geom_histogram(aes(x = pop),
                   alpha = 0.9,
                   fill = '#18a1db') +  # fill the histogram with a nice colour
    labs(x = 'Value',
         y = 'Density',
         title = 'Distribution of the white stork population data') +
    plot_theme())  # apply the custom theme

# Plot a scatter plot of the data
(stork_scatter <- ggplot(data = stork) +
    geom_point(aes(x = year, y = pop),  # change to geom_point() for scatter plot
               alpha = 0.9,
               color = '#18a1db') +
    labs(x = 'Year',
         y = 'Population Abundance',
         title = 'Population abundance of white stork') +
    plot_theme())  # apply the custom theme

# Log transform the data
stork <- stork %>%
  mutate(logpop = log(pop))

# Plot a scatter plot of the log transformed data
(stork_scatter <- ggplot(data = stork) +
    geom_point(aes(x = year, y = logpop),  # change pop -> logpop
               alpha = 0.9,
               color = '#18a1db') +
    labs(x = 'Year',
         y = 'Population Abundance (log transformed data)',
         title = 'Population abundance of white stork') +
    plot_theme())  # apply the custom theme

# Plot the histogram of log transformed data
(stork_log_hist <- ggplot(data = stork) +
    geom_histogram(aes(x = logpop),
                   alpha = 0.9,
                   fill = '#18a1db') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())

# Create a square-root transformed column
stork <- stork %>%
  mutate(sqrtpop = sqrt(pop))

# Plot the histogram of square root transformed data
(stork_hist_sqrt <- ggplot(data = stork) +
    geom_histogram(aes(x = sqrtpop),  # change pop -> sqrtpop
                   alpha = 0.9,
                   fill = '#18a1db') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())

# Build a model
stork.mod <- lm(pop ~ year, data = stork)
# Find the optimal lambda for Box-Cox
bc <- boxcox(stork.mod)
# Extract the optimal lambda value
(lambda <- bc$x[which.max(bc$y)])
# Transform the data using this lambda value
stork <- stork %>%
  mutate(bcpop = ((pop^lambda-1)/lambda))

# Plot a histogram of the Box-Cox transformed data
(stork_hist_bc <- ggplot(data = stork) +
    geom_histogram(aes(x = bcpop),
                   alpha = 0.9,
                   fill = '#18a1db') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())

# Panel of histograms for different transformations
(stork_dist_panel <- plot_grid(stork_hist + labs(title = 'Original data'),  # original data  
                               stork_log_hist + labs(title = 'Log transformation'),  # logarithmic transformation
                               stork_hist_sqrt + labs(title = 'Square-root transformation'),  # square-root transformation
                               stork_hist_bc + labs(title = 'Box-Cox transformation'),  # Box-Cox transformation
                               nrow = 2,  # number of row in the panel
                               ncol = 2))  # number of columns in the panel

# Fit new model using the Box-Cox transformed data
stork.bc.mod <- lm(bcpop ~ year, data = stork)

# Show the summary of the model outputs
summary(stork.bc.mod)

# Tell R to display two plots next to each other
par(mfrow = c(1, 2))

# Q-Q plot for the original data model
qqnorm(stork.mod$residuals, main = 'Q-Q Plot Original Data')
qqline(stork.mod$residuals)

# Q-Q plot for the Box-Cox transformed data model
qqnorm(stork.bc.mod$residuals, main = 'Q-Q Plot Box-Cox Transformed Data')
qqline(stork.bc.mod$residuals)

# Reset the plot display settings
par(mfrow = c(1, 1))

# Verify reverse transformations by creating new columns that should match the original
stork <- stork %>%
  mutate(back_log = exp(logpop),
         back_sqrt = sqrtpop^2,
         back_bc = (bcpop*lambda + 1)^(1/lambda)) %>%
  glimpse()  # displays a couple of observations from each column

# Get the predictions of our model
stork.pred <- ggpredict(stork.bc.mod, terms = c('year'))

# View the predictions dataframe
View(stork.pred)

# Apply the reverse transformation on the relevant columns
stork.pred$predicted <- (stork.pred$predicted*lambda + 1)^(1/lambda)
stork.pred$conf.low <- (stork.pred$conf.low*lambda + 1)^(1/lambda)
stork.pred$conf.high <- (stork.pred$conf.high*lambda + 1)^(1/lambda)

# Convert the summary table into a dataframe
mod.summary <- tidy(stork.bc.mod)

# slope
slope <- (mod.summary$estimate[2]*lambda + 1)^(1/lambda)
slope <- round(slope, 3)

# conf. intervals

# upper
# we extract the slope and add the standard error to get the upper CI
upper_ci <- ((mod.summary$estimate[2]+mod.summary$std.error[2])*lambda + 1)^(1/lambda)
upper_ci <- round(upper_ci, 3)

# lower
# we extract the slope and subtract the standard error to get the upper CI
lower_ci <- ((mod.summary$estimate[2]-mod.summary$std.error[2])*lambda + 1)^(1/lambda)
lower_ci <- round(lower_ci, 3)

# Plot the predictions
(stork_plot <- ggplot(stork.pred) +
    geom_line(aes(x = x, y = predicted), color = '#db1818') +  # add the prediction line          
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),  # add the ribbon
                fill = "#fc7777", alpha = 0.5) +  
    geom_point(data = stork,  # add the original data                      
               aes(y = pop, x = year)) +
    annotate("text", x = 1975, y = 180,  # annotate the plot with slope and CI info
             label = paste0('Slope = ', as.character(slope),
                            '\nUpper CI = ', as.character(upper_ci),
                            '\nLower CI = ', as.character(lower_ci))) +
    labs(x = '',
         y = 'Population Abundance',
         title = "Global white stork population increased between 1970-2008",
         caption = 'Data Source: Living Planet Index') +
    plot_theme()  +
    xlim(c(1970, 2008))  # we set a limit to the x-axis to show only the relevant years
)

# Save the figure
ggsave("stork_plot.png", stork_plot, width = 12, height = 9, units = "in")
       
#Standardization
# Extract the Atlantic salmon data from the main dataset
salmon <- LPI_species %>%
  filter(Common.Name == 'Atlantic salmon') %>%
  mutate(year = parse_number(as.character(year)))

# Look at the units in the dataset
unique(salmon$Units)
unique(salmon$Sampling.method)

# Look at the distribution of the data for each of the populations
(salmon_density_loc <- salmon %>%                            
    ggplot(aes(x = pop)) +
    geom_density() +  # we use geom_density() instead of geom_histogram in this case but they are interchangeable
    facet_wrap(~ id, scale = 'free') +  # create the grid based on the id, scale = 'free' allows different x and y scale for each population
    labs(y = 'Density',
         x = '\nValue\n',
         title = 'Distributions of individual Atlantic salmon populations\n',
         caption = 'Data Source: Living Planet Index') +
    plot_theme() +
    theme(axis.text.x = element_blank(),  # we remove the axis text to make the plots less clutered
          axis.text.y = element_blank()))

# Save the plot
ggsave(plot = salmon_density_loc,
       filename = 'salmon_hist_loc.png',
       width = 10, height = 12, units = 'in')

# Standardize the data
salmon <- salmon %>%
  group_by(id) %>%  # group the data by the study id
  mutate(scalepop_standard = (pop-mean(pop))/(sd(pop))) %>%  # apply standardization
  ungroup()  # ungroup the data to avoid issue with grouping later on

# Histogram of the original, unscaled data
salmon_hist <- ggplot(data = salmon) +
  geom_histogram(aes(x = pop),
                 alpha = 0.9,
                 fill = '#319450') +
  labs(x = 'Value',
       y = 'Density') +
  plot_theme()

# Look at the distribution of the scaled data
salmon_hist_scaled <- ggplot(data = salmon) +
  geom_histogram(aes(x = scalepop_standard),
                 alpha = 0.9,
                 fill = '#319450') +
  labs(x = 'Value',
       y = 'Density') +
  plot_theme()

# Panel of the histograms
(salmon_dist <- plot_grid(salmon_hist + labs(title = 'Original data'),  # original data  
                          salmon_hist_scaled + labs(title = 'Standardized data'),  # standardized data
                          nrow = 1,  # number of row in the panel
                          ncol = 2))  # number of columns in the panel

# Reverse transformation test of the salmon data
salmon <- salmon %>%
  group_by(id) %>%  # we group by id again
  mutate(pop_scaled_rev = (scalepop_standard * sd(pop) + mean(pop))) %>%  # apply the reverse transformation
  ungroup() %>%
  glimpse()  # look at the result

# Install the penguins package
#install.packages("palmerpenguins")

# Load the library
library(palmerpenguins)

# Import the data
penguins <- palmerpenguins::penguins

# Look at the variables in the dataset
str(penguins)
summary(penguins)

# Remove observations with NA for the variables we are considering
penguins <- penguins[complete.cases(penguins[ , 3:6]),]  # filter out only observations which have values in columns 3:6

# Scale the penguin data using normalization
penguins <- penguins %>%
  mutate(bill_length_mm_norm = (bill_length_mm - min(bill_length_mm))/(max(bill_length_mm)-min(bill_length_mm)),
         bill_depth_mm_norm = (bill_depth_mm - min(bill_depth_mm))/(max(bill_depth_mm)-min(bill_depth_mm)),
         flipper_length_mm_norm = (flipper_length_mm - min(flipper_length_mm))/(max(flipper_length_mm)-min(flipper_length_mm)),
         body_mass_g_norm = (body_mass_g - min(body_mass_g))/(max(body_mass_g)-min(body_mass_g)))

# Load the library
#install.packages("caret")
library(caret)

# Using preProcess to scale the data
penguins_mapping <- preProcess(penguins[, 3:6], method = c('range'))  # preProcess creates a mapping for the chosen variables
penguins_norm <- predict(penguins_mapping, penguins)  # we transform the data using predict() and the mapping

# Import packages
library(scales)

# Extract the data for the leatherback turtle from the LPI dataset
turtle <- LPI_species %>%
  filter(Common.Name == 'Leatherback turtle') %>%
  mutate(year = parse_number(as.character(year)))

# Look at the dataset
str(turtle)
summary(turtle)

# Plot a scatter plot of the turtle data
(turtle_scatter <- ggplot(data = turtle) +
    geom_point(aes(x = year, y = pop),  # change to geom_point() for scatter plot
               alpha = 0.9,
               color = '#ff4040') +
    labs(x = 'Year',
         y = 'Population Abundance',
         title = 'Population abundance of the leatherback turtle') +
    plot_theme())  # apply the custom theme

# Change the scale
(turtle_scatter_log <- ggplot(data = turtle) +
    geom_point(aes(x = year, y = pop),  # change to geom_point() for scatter plot
               alpha = 0.9,
               color = '#ff4040') +
    labs(x = 'Year',
         y = 'Population Abundance',
         title = 'Population abundance of the leatherback turtle') +
    scale_y_log10(labels = scales::label_number()) +  # line changes the scale on the y-axis and creates nice labels
    plot_theme())  # apply the custom theme

# Challenge!
# Data
cormorant <- LPI_species%>%
  filter(Common.Name=="Great cormorant / Cormorant")%>%
  mutate(year=parse_number(as.character(year)))

str(cormorant)
summary(cormorant)

#Data distribution
(cormorant_hist <- ggplot(data=cormorant)+
  geom_histogram(aes(x=pop))+
  labs(x='Value',
       y='Density')+
  plot_theme())

#Data transformation & Modeling  
cormorant <- cormorant%>%
  filter(pop!=0)
cormorant_lm <- lm(pop~year, data= cormorant)
bcx <- boxcox(cormorant_lm)
(lam <- bc$x[which.max(bc$y)])

cormorant <- cormorant%>%
  mutate(popbc = (pop^lam-1)/lam)

(cormorant_bc_hist <- ggplot(data=cormorant)+
    geom_histogram(aes(x=popbc))+
    labs(x='Value',
         y='Density')+
    plot_theme())

cormorant_bc_lm <- lm(popbc~year, data= cormorant)
plot(cormorant_bc_lm)

#Assumption test
cormorant.resid <- resid(cormorant_bc_lm)
shapiro.test(cormorant.resid)

bartlett.test(popbc~year, data=cormorant)

#histogram
(cormorant_density_loc <- cormorant%>%
  ggplot(aes(x=pop))+
  geom_density()+
  facet_wrap(~id, scale='free')+
  labs(y='Density',
       x='\nValue\n',
       title='Distributions of individual cormorant populations\n')+
  plot_theme()+
  theme(axis.text.x= element_blank(),
        axis.text.y= element_blank()))

ggsave(plot=cormorant_density_loc,
       filename="cormorant_hist_loc.png",
       width=10, height=12, units='in')

#standardization
cormorant <- cormorant%>%
  group_by(id)%>%
  mutate(scalepop_standard=(pop-mean(pop))/(sd(pop)))%>%
  ungroup()

(cormorant_hist <- ggplot(data=cormorant)+
  geom_histogram(aes(x=scalepop_standard),
                 alpha=0.9,
                 fill='#319450')+
  labs(y='Density',
       x='Value')+
  plot_theme())

#model
cormorant_std_mod <- lm(scalepop_standard~year, data=cormorant)
summary(cormorant_std_mod)

cormorant_pred <- ggpredict(cormorant_std_mod, terms=c('year'))
view(cormorant_pred)

#reverse transformation
cormorant_pred$predicted <- (cormorant_pred$predicted)*sd(cormorant$pop)+mean(cormorant$pop)
cormorant_pred$conf.low <- (cormorant_pred$conf.low)*sd(cormorant$pop)+mean(cormorant$pop)
