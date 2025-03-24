# Community assembly

hist(rexp(n = 100000,rate = .1))

library(ggplot2)

ggplot(data = data.frame(x=rexp(n = 100000,rate = .1)),
       mapping = aes(x=x))+
  geom_density(fill="hotpink")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  xlab("Dispersal (km)")

max(rexp(n = 100000,rate = .1))
