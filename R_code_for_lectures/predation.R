library(ggplot2)  
library(tidyverse)
library(scales)

type_i <- function(a,N){return(a*N)}

type_ii <- function(a,h,N){return(a*N/(1+(a*h*N)))}

type_iii <- function(a,h,N){
  c <- 1/h
  d <- 1/(a*h)
  
  return((c*(N^2))/((d^2) + (N^2)))
  }


data.frame(prey_density = 1:10) %>%
  mutate(feeding_rate = type_i(a = 2,N = prey_density))%>%
  ggplot(mapping = aes(x=prey_density,y=feeding_rate))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()

data.frame(prey_density = 1:50) %>%
  mutate(feeding_rate = type_ii(a = 2,h = 2,N = prey_density))%>%
  ggplot(mapping = aes(x=prey_density,y=feeding_rate))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()


data.frame(prey_density = seq(from=0,to=1000,by=.001)) %>%
  mutate(feeding_rate = type_iii(a = .2,h = .2,N = prey_density))%>%
  ggplot(mapping = aes(x=prey_density,y=feeding_rate))+
  geom_line(color= "darkblue",linewidth=1)+
  theme_bw()


###################

#Lotka-Volterra

lotka_volterra_prey <- function(N,P,r,a){
  
  return((r*N)-(a*N*P))
  
}

lotka_volterra_prey_dd <- function(N,P,r,a,k){
  
  return((r*N*(1-(N/k)))-(a*N*P))
  
}


lotka_volterra_pred <- function(N,P,f,a,q){
  
  return((f*a*N*P)-(q*P))

}

lotka_volterra_pred_dd <- function(N,P,f,h,a,q){
  
  return(((f*a*N*P)/(1+(a*h*N)))-(q*P))
  
}



###########


lv <- function(time_length = 1000,
               N0=100,
               P0=1,
               r=2,
               a=0.1,
               f=1,
               q=1){
  
  out <- data.frame(N=rep(NA,1+time_length),
                    P=rep(NA,1+time_length),
                    t=0:time_length)
  
  out$N[1] <- N0
  out$P[1] <- P0
  
  for(i in 1:time_length){
    
    out$N[i+1] <- (lotka_volterra_prey(N = out$N[i],
                                      P = out$P[i],
                                      r = r,
                                      a = a)+out$N[i])
    out$P[i+1] <- (lotka_volterra_pred(N = out$N[i],
                                      P = out$P[i],
                                      f = f,
                                      a = a,
                                      q = q)+out$P[i])

  }
  
  return(out)

}


lv_dd <- function(time_length = 1000,
               N0=100,
               P0=1,
               r=2,
               a=0.1,
               f=1,
               q=1,
               k=20,
               h=1){
  
  out <- data.frame(N=rep(NA,1+time_length),
                    P=rep(NA,1+time_length),
                    t=0:time_length)
  
  out$N[1] <- N0
  out$P[1] <- P0
  
  for(i in 1:time_length){
    
    out$N[i+1] <- (lotka_volterra_prey_dd(N = out$N[i],
                                       P = out$P[i],
                                       r = r,
                                       a = a,
                                       k = k)+out$N[i])
    
    out$P[i+1] <- (lotka_volterra_pred_dd(N = out$N[i],
                                       P = out$P[i],
                                       f = f,
                                       a = a,
                                       q = q,
                                       h = h)+out$P[i])
    
  }
  
  return(out)
  
}



lv(time_length = 1000,
   N0 = 1,
   P0 = 10,
   r = 2,
   a = .2,
   f = .2,
   q = .01) %>%
  slice_head(n = 200)%>%
  ggplot(mapping = aes(x=t,y=N))+
  geom_line(color="green")+
  geom_line(mapping = aes(x=t,y=P),color="red")+
  theme_bw()+
  ylab("Abundance")




lv_dd(time_length = 1000,
   N0 = 10,
   P0 = 10,
   r = 3,
   a = .2,
   f = .1,
   q = .2,
   k = 20,
   h = .1) %>%
  slice_head(n = 200)%>%
  ggplot(mapping = aes(x=t,y=N))+
  geom_line(color="green",
            linewidth=1)+
  geom_line(mapping = aes(x=t,y=P),
            color="red",
            linewidth=1)+
  theme_bw()+
  ylab("Abundance")



lv_dd(time_length = 1000,
      N0 = 10,
      P0 = 10,
      r = 3,
      a = .11,
      f = .1,
      q = .14,
      k = 20,
      h = .1) %>%
  slice_head(n = 200)%>%
  ggplot(mapping = aes(x=t,y=N))+
  geom_line(color="green",
            linewidth=1)+
  geom_line(mapping = aes(x=t,y=P),
            color="red",
            linewidth=1)+
  theme_bw()+
  ylab("Abundance")


