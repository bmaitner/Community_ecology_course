# Population growth and density dependence


# Function for malthusian growth
  malthusian_growth <- function(N0 = 2, r = 3, t =2){
    
    Nt <- N0*(exp(r*t))
  
    return(Nt)  
    
  }

#Plotting some examples to show how the equation works
  
library(ggplot2)  
library(tidyverse)
  library(scales)

  data.frame(time = 1:10) %>%
    mutate(population = malthusian_growth(N0 = 1,r = -.5,t = time))%>%
    ggplot(mapping = aes(x=time,y=population))+
    geom_line(color= "darkblue",linewidth=1)+
    theme_bw()
  
data.frame(time = 1:10) %>%
  mutate(population = malthusian_growth(N0 = 1,r = 0,t = time))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()

data.frame(time = 1:10) %>%
  mutate(population = malthusian_growth(N0 = 1,r = 0.5,t = time))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()

data.frame(time = 1:10) %>%
  mutate(population = malthusian_growth(N0 = 1,r = 1,t = time))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()


#Humans

#book gives = 0 (70k bce), 20k people
#-70000:2025
#.035 equates to doubling every ~ 20 years
data.frame(time = 1:200) %>%
  mutate(population = malthusian_growth(N0 = 10000,r = .035,t = time))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw()

data.frame(time = 1:500) %>%
  mutate(population = malthusian_growth(N0 = 10000,r = .035,t = time))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))+
  theme_bw()


###################################


# Logistic growth model

logistic_growth <- function(N0 = 2, r = 3, t =2,K=100){
  
  Nt <- N0*(K/(((K-N0)*exp(-r*t))+N0))
    
  return(Nt)  
  
}



data.frame(time = 1:100) %>%
  mutate(population = logistic_growth(N0 = 1,r = 0.5,t = time,K = 100))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()

data.frame(time = 1:100) %>%
  mutate(population = logistic_growth(N0 = 1,r = 0.5,t = time,K = 150))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()


data.frame(time = 1:100) %>%
  mutate(population = logistic_growth(N0 = 1,r = 0.5,t = time,K = 30),
         population2 = logistic_growth(N0 = 1,r = 0.5,t = time,K = 20))%>%
  ggplot(mapping = aes(x=time,y=population))+
  geom_line(color= "darkblue",linewidth=1)+
  geom_line(mapping = aes(x=time,y=population2),
            color="purple",linewidth=1)+
  theme_bw()







