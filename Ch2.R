#Chapter 2
# Here I will be doing the projects on the ggplot2 book

# Upload Packages
library(tidyverse)
library(tibble)
library(data.table)
library(dplyr)
library(purrr)
library(ggplot2)
library(mgcv)

# This will list all of the data sets in a the above packages
data()
# This does the same thing
data(package = .packages(all.available = TRUE))

# Take a look at the data set
mpg

# Variable names
# Manufacturer (Chr) 15 different manufacturers of cars
# Model (Chr) 38 different models of cars
# Displ (Num) 35 different enginge displacements
# Year (int) only 1999 and 2008 years
# Cyl(int) 4 different types (4,5,6,8)
# trans (chr) 10 different types of transmisison with manual and auto being the main ones and then subsets of different ones
# drv (chr) 3 different types (f, 4, r)
# Cty (int) 21 different types of cty mpg
# hwy (int) 27 different types of hwy mpg
# fl (chr) 5 different types, but not sure what they mean
# class (chr)7 differnt classes of vehciles

# First graph 
ggplot(mpg, aes(displ, hwy)) + #mpg = dataset, displ=xaxis, hwy=yaxis
  geom_point() # Makes a scatter plot

# Exploring the relationship between city and hwy
# We see that cars with higher cty also have higher hwy mpg
ggplot(mpg, aes(cty, hwy)) + #mpg = dataset, displ=xaxis, hwy=yaxis
  geom_point()

# There are many manufactors and too many models 
# The graph like so, isn't useful
ggplot(mpg, aes(model, manufacturer)) +
  geom_point()

# This looks at the relationship between carat and price
# Generally the bigger the caret the higher the price
# Not a linear relationship, but a curve
ggplot(diamonds, aes(carat, price)) + 
  geom_point() +
  geom_smooth()

# This creates a histogram
ggplot(mpg, aes(cty)) +
  geom_histogram(binwidth = 1) # This adjusters the binwidth

# This makes each class a different color

ggplot(mpg, aes(displ, hwy, color = class)) + # creates a color for every class of vehicle
  geom_point() +
  xlab("Engine Displacement") + # X-axis 
  ylab("Hwy MPG") # Y-axis title

# Here we see that there are 7 classes of car
unique(mpg$class)

# See how drv is related to hwy mpg
# We see that front wheel drive has the best mpg
ggplot(mpg, aes(drv, hwy)) +
  geom_point()

# We have too many trans types, thus it becomes difficult
# to discriminate between the options
ggplot(mpg, aes(displ, hwy, shape=trans)) +
  geom_point()

# Here we can do faceting
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  facet_wrap(~class) # Creates a facet based on class of vehicle

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = lm) # lm creates a linear regression

# This adds a smoother to the graph
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE) # takes away the confidence interval

# The span controls the wigleness of the graph
# 0 being the most wiggle
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(span = 0.2)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'

# 1 being the least wiggle
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(span = 1)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'


# This fits a generalized additive models, which are better
# when the data is non-linear
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x))

# This adds jitter to help with over plotting
ggplot(mpg, aes(drv, hwy)) + 
  geom_jitter()

# This summarizs the shape of the distribution
ggplot(mpg, aes(drv, hwy)) + 
  geom_boxplot()

# This shows density
ggplot(mpg, aes(drv, hwy)) + 
  geom_violin()

# Histogram
ggplot(mpg, aes(hwy)) + 
  geom_histogram(binwidth = 1) # this adjustes the bin width

ggplot(mpg, aes(hwy)) + 
  geom_freqpoly(binwidth=1)

ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

# Bar Charts
ggplot(mpg, aes(class)) + 
  geom_bar() # Creates a bar chart

# Time series
# This shows the unemployment rate
ggplot(economics, aes(date, unemploy / pop)) +
  geom_line()

# Shows the median number of weeks unemployed
ggplot(economics, aes(date, uempmed)) +
  geom_line()

ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path() + # Creates a path between every point
  geom_point()

year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path(color = "grey50") +
  geom_point(aes(colour = year(date)))

ggplot(mpg, aes(cty, hwy)) + 
  geom_point() +
  geom_smooth(se=FALSE)


# This reorders the boxplot from small to large hwy
ggplot(mpg, aes(reorder(class, hwy), hwy)) + 
  geom_boxplot() +
  xlab("Class") 

# Now we take a look at the diamonds dataset
variable.names(diamonds)

diamonds

ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = .1) # Have to play around with the binwidth to find the correct one

# Modifying the axes
ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3)

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) + 
  xlab("City Driving (mpg)") + # Title the x-axis
  ylab("Highway Driving (mpg)") # Title the y-axis

# Remove the axes with Null
ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) + 
  xlab(NULL) + # Takes away the x-axis label
  ylab(NULL) # Takes away the y-axis label


# For continuous scales, use NA to set only one limit
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25, na.rm = TRUE) + 
  ylim(NA, 30)

# This takes out the 4 wheel drive cars
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) + 
  xlim("f", "r") + 
  ylim(20, 30)






