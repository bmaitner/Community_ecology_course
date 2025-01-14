# History of community ecology

library(tidyverse)
library(ggplot2)

# Species in community type

set.seed(69)
comm_type <-
  bind_rows(
    data.frame(species = "A",
               env = runif(min = 0,max = 1,n = 1000)),
    data.frame(species = "B",
               env = runif(min = 0,max = 1,n = 2000)),
    data.frame(species = "C",
               env = runif(min = 0,max = 1,n = 500))
    )
  
  
ggplot(data = comm_type,
       mapping = aes(x=env,color=species,fill=species))+
  geom_freqpoly(lwd=2)+
    xlab("Environmental Gradient")+
    ylab("Abundance")



# Species along gradients

set.seed(69)
comm_gradient <-
  bind_rows(
    data.frame(species = "A",
               env = rnorm(mean = -4,sd = 2,n = 500)),
    data.frame(species = "B",
               env = rnorm(mean = 0,sd = 2,n = 2000)),
    data.frame(species = "C",
               env = rnorm(mean = 1,sd = .75,n = 500))
  )


ggplot(data = comm_gradient,
       mapping = aes(x=env,color=species))+
  geom_freqpoly(lwd=3)+
  xlab("Environmental Gradient")+
  ylab("Abundance")


# Grinnellian Niche

# gd <- data.frame(temp = runif(n = 100,min = 15,max = 25),
#                  precip = runif(n=100,min = 100,max =  1000))

gd <- data.frame(temp = rnorm(mean = 15,sd = 5,n = 100),
                 precip = rnorm(mean = 50,sd = 20,n = 100))


ggplot(data = gd,mapping = aes(x=temp,y=precip))+
  geom_point()+
  geom_polygon(data = gd %>%slice(chull(x = temp,precip)),
               alpha=0.5,
               fill="lightblue")+
  theme_bw()+
  xlab("Temperature")+
  ylab("Precipitation")

# Eltonian Niche

set.seed(2005)
elton_gradient <-
  bind_rows(
    data.frame(species = "A",
               env = rnorm(mean = -10,sd = 2,n = 500)),
    data.frame(species = "B",
               env = rnorm(mean = 0,sd = 2,n = 2000)),
    data.frame(species = "C",
               env = rnorm(mean = 1,sd = .75,n = 500))
  )


ggplot(data = elton_gradient,
       mapping = aes(x=env,color=species))+
  geom_freqpoly(lwd=3)+
  xlab("Seed size")+
  ylab("Consumption")+
  theme_bw()

# Hutchinsonian niche

library(hypervolume)

hd <- data.frame(temp = rnorm(mean = 15,sd = 5,n = 1000),
                 precip = rnorm(mean = 50,sd = 20,n = 1000),
                 seed_size = rnorm(mean = 1,sd = 1,n = 1000))

hd_hv<- hypervolume_gaussian(data = hd,name = "Hutchinsonian Niche")

hypervolume::plot.Hypervolume(hd_hv,show.3d=TRUE)

?plot.Hypervolume

hd_chull <- hypervolume::expectation_convex(input = hd,check.memory = FALSE)

hypervolume::plot.Hypervolume(hd_chull,
                              show.3d=TRUE)

# Competitive exclusion

library(portalr)

rod <- portalr::load_rodent_data()

rod$rodent_data
rod$rodent_data$wgt


rod$rodent_data %>%
  filter(!is.na(wgt))%>%
  filter(year == 2023) %>%
  filter(plot %in% 1)%>%
  ggplot(mapping = aes(x=wgt,color=species))+
  ggplot2::scale_x_log10()+
  #geom_freqpoly(lwd=3)+
  geom_density(lwd=2)+
  theme_bw()

# Limiting similarity

set.seed(69)
lsim <-
  bind_rows(
    data.frame(species = "A",
               env = rnorm(mean = -4,sd = 1,n = 500)),
    data.frame(species = "B",
               env = rnorm(mean = 0,sd = 1,n = 500)),
    data.frame(species = "C",
               env = rnorm(mean = 1,sd = 1,n = 500)),
    data.frame(species = "D",
               env = rnorm(mean = 2,sd = 1,n = 500)),
    data.frame(species = "E",
               env = rnorm(mean = 4,sd = 1,n = 500))
    
    
  )


ggplot(data = lsim,
       mapping = aes(x=env,color=species))+
  geom_density(lwd=3)+
  xlab("Resource Gradient")+
  ylab("Resource Usage")+
    theme_bw()

