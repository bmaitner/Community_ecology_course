# Equilibrium LV conditions per eq. 7.3 and 7.4

sp1_zngi <- function(k1,a12,N2){
  N1 <- k1 - (a12*N2)
  return(N1)
}

sp2_zngi <- function(k2,a21,N2){
  N1 <- (k2-N2)/a21
  return(N1)
}

library(ggplot2)
library(tidyverse)

data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 20,a12 = .5,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  # geom_line(data = data.frame(N2 = 1:100)%>%
  #             mutate(N1 = sp2_zngi(k2 = 20,a21 = .5,N2 = N2)),
  #           mapping = aes(x=N1,y=N2),
  #           color="darkred",
  #           linewidth = 1)+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme_bw()

data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 20,a12 = .5,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  #geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 20,a21 = .5,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme_bw()


#stable

data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 20,a12 = .5,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 20,a21 = .5,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,40))+
  ylim(c(0,40))+
  theme_bw()
  


#sp2 wins

data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 50,a12 = 2,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 50,a21 = .5,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,100))+
  theme_bw()

#sp1 wins
data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 50,a12 = .5,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 50,a21 = 2,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,100))+
  theme_bw()


#unstable
data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 100,a12 = 2,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 100,a21 = 2,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,100))+
  theme_bw()


data.frame(N2 = 1:100)%>%
  mutate(N1 = sp1_zngi(k1 = 100,a12 = .3,N2 = N2)) %>%
  ggplot(mapping = aes(x=N1,y=N2))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 100,a21 = .3,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,100))+
  theme_bw()


#################################

# R*
#' @param Ni number of species i
#' @param ai consumption rate of resource
#' @param di death rate
#' @param R resource availability
resource_dep_growth <- function(ai,di,R){
  
  per_capita_growth_rate <- (ai*R)-di
  return(per_capita_growth_rate)
}


data.frame(R = 1:10000)%>%
  mutate(pcgr = resource_dep_growth(ai = .1,di = .2,R = R)) %>%
  ggplot(mapping = aes(x=R,y=pcgr))+
  geom_line(color="darkblue",linewidth=1)+
  geom_line(data = data.frame(N2 = 1:100)%>%
              mutate(N1 = sp2_zngi(k2 = 100,a21 = .3,N2 = N2)),
            mapping = aes(x=N1,y=N2),
            color="darkred",
            linewidth = 1)+
  xlim(c(0,100))+
  theme_bw()







