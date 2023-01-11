library(ggplot2)
mpg
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()