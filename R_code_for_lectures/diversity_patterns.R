library(ggplot2)
library(vegan)
library(tidyverse)

# Functional richness

library(palmerpenguins)

data(package = 'palmerpenguins')
data("penguins")



library(sf)
library(swaRm)

penguins<-
penguins %>%
  select(species,bill_length_mm,flipper_length_mm)%>%
  na.omit()

penguins$bill_length_mm <- scale(penguins$bill_length_mm)
penguins$flipper_length_mm <- scale(penguins$flipper_length_mm)

# Calculate the hulls for each group
peng_hull<-penguins %>%
  group_by(species)%>%
  select(species,
         flipper_length_mm,
         bill_length_mm)%>%
  na.omit()%>%
  slice(chull(x = flipper_length_mm,
              y=bill_length_mm))

peng_hull_all<-penguins %>%
  select(species,
         flipper_length_mm,
         bill_length_mm)%>%
  mutate(species = "all")%>%
  na.omit()%>%
  slice(chull(x = flipper_length_mm,
              y=bill_length_mm))




peng1 <-penguins %>%
  ggplot(mapping = aes(x=flipper_length_mm,
                       y = bill_length_mm,
                       color=species,
                       fill=species))+
  geom_point()+
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  #geom_polygon(data = peng_hull,alpha=0.1)+
  xlab("Flipper length (z-score)")+
  ylab("Bill Length (z-score)")+
  theme_bw()

ggsave(filename = "figures/div_pattern_penguin_hv.png",
       plot = peng1,width = 10,height = 5,units = "in",dpi = 600)

ggsave(filename = "figures/div_pattern_penguin_hv.jpg",
       plot = peng1,width = 10,height = 5,units = "in",dpi = 600)


peng2<- penguins %>%
  ggplot(mapping = aes(x=flipper_length_mm,
                       y = bill_length_mm,
                       color=species,
                       fill=species))+
  geom_point()+
  geom_polygon(data = peng_hull,alpha=0.1)+
  scale_color_manual(values = c("darkorange","purple","cyan4"))+
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  xlab("Flipper length (z-score)")+
  ylab("Bill Length (z-score)")+theme_bw()


penguins %>%
  group_by(species)%>%
  select(species,
         flipper_length_mm,
         bill_length_mm)%>%
  summarise(chull_area = chull_area(flipper_length_mm,bill_length_mm))


ggsave(filename = "figures/div_pattern_penguin_hv2.png",
       plot = peng2,
       width = 10,
       height = 5,
       units = "in",
       dpi = 600)

ggsave(filename = "figures/div_pattern_penguin_hv2.jpg",
       plot = peng2,
       width = 10,
       height = 5,
       units = "in",
       dpi = 600)

peng3<- penguins %>%
  ggplot(mapping = aes(x=flipper_length_mm,
                       y = bill_length_mm,
                       color=species,
                       fill=species))+
  geom_point()+
  geom_polygon(data = peng_hull_all,alpha=0.4,fill="grey")+
  scale_color_manual(values = c("darkorange","purple","cyan4","grey"))+
  xlab("Flipper length (z-score)")+
  ylab("Bill Length (z-score)")+
  theme_bw()+
    scale_fill_discrete(guide="none")


penguins %>%
  select(flipper_length_mm,
         bill_length_mm)%>%
  summarise(chull_area = chull_area(flipper_length_mm,bill_length_mm))


ggsave(filename = "figures/div_pattern_penguin_hv3.png",
       plot = peng3,
       width = 10,
       height = 5,
       units = "in",
       dpi = 600)

ggsave(filename = "figures/div_pattern_penguin_hv3.jpg",
       plot = peng3,
       width = 10,
       height = 5,
       units = "in",
       dpi = 600)



################################################  
  
# SAR

data(BCI)
BCI2 <- BCI[,1:200]
BCI3 <- BCI

BCI3 <-
BCI3 %>%
  rownames_to_column()%>%
  pivot_longer(cols = 2:(ncol(BCI3)+1))

BCI3$value[sample(x = 1:nrow(BCI3),
                  size = 0.3*nrow(BCI3),
                  replace = FALSE)] <- 0

BCI3<-BCI3 %>%
  pivot_wider(values_from = value,names_from = name)%>%
  select(-rowname)



sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI2)
sp3 <- specaccum(BCI3)

sp_df<- bind_rows(data.frame(region = "A",
                             sites = sp1$sites,
                             richness = sp1$richness),
                  data.frame(region = "B",
                             sites = sp2$sites,
                             richness = sp2$richness),
                  data.frame(region = "C",
                             sites = sp3$sites,
                             richness = sp3$richness))


sp_df %>%
  #filter(region %in% c("A","B"))%>%
  ggplot(mapping = aes(x=sites,y=richness,color=region))+
  geom_line(lwd=2)

sp_df %>%
  #filter(region %in% c("A","B"))%>%
  ggplot(mapping = aes(x=sites,y=richness,color=region))+
  #geom_point(lwd=2)+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()



