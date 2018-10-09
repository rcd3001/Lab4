### Lab 4 HW
# R DUggan 10.8.18    

rm(list = ls())
library('tidyverse')
# devtools::install_github('stefano-meschiari/latex2exp') #if you need to install latex2exp
library('latex2exp')

#Generating coords for alpha fill
coordX0 <- c(0, seq(0, qnorm(0.95), by = 0.01), qnorm(0.95)) #not sure what happened with the coordinates, but the geom_polygons won't work without this
coordX <- c(qnorm(0.05), seq(qnorm(0.05), 0, by = 0.01), 0)
coordY <- c(0,dnorm(coordX0[-c(1,length(coordX0))], mean=3.275), 0)
coord1DF <- as.data.frame(cbind(coordX, coordY))

#Generating coords for beta fill
coordX2 <- -(coordX)
coordY2 <- coordY
coord2DF <- as.data.frame(cbind(coordX2, coordY2))

ggplot(data = data.frame(x  = c(-166:167)), aes(x))+
  scale_x_continuous(breaks = c(qnorm(0.05), qnorm(0.95)), 
                     labels = c(expression(theta["0"]), expression(theta["a"])), 
                     limits = c(-5,5))+
  stat_function(fun = dnorm, aes(color = "Type II Error"), 
                args = list(mean = qnorm(0.95), sd = 1))+
  stat_function(fun = dnorm, aes(color = "Type I Error"), 
                args = list(mean = qnorm(0.05), sd = 1))+
  geom_vline(xintercept = mean(c(qnorm(0.05), qnorm(0.95))), linetype = "dashed")+
  geom_polygon(data = coord1DF, aes(x=coordX, y = coordY), fill = "red", alpha = 0.5)+
  geom_polygon(data = coord2DF, aes(x=coordX2, y = coordY2), fill = "blue", alpha = 0.5)+
  labs(x = NULL, y = NULL)+
  annotate("text", parse = TRUE, x = -0.4, y = .02, size = 6, 
           label = as.character(TeX('$\\beta')))+
  annotate("text", parse = TRUE, x = 0.4, y = .02, size = 6, color = "white", 
           label = as.character(TeX('$\\alpha')))+
  theme(text = element_text(size = 15), legend.text = element_text(size = 15), 
        legend.title = element_blank(), legend.position=c(.12, .92))

#Question 2-
 
