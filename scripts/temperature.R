library(ggplot2)
library(mgcv)

# Load data ---------------------------------------------------------------
temp <- read.csv("data/Song_et_al_2019.csv")
temp <- temp[temp$age < 253,]
temp <- temp[order(temp$age),]

pal <- c("#ffd60a",
         "#003566",
         "#000814")


# Smooth ------------------------------------------------------------------
mod <- gam(temp~s(age, k=20), data=temp)
gam.check(mod)

pred <- predict(mod, newdata=data.frame(age=0:260))

temp2 <- data.frame(age=0:260,
                    temp=pred)

# Plot temperature --------------------------------------------------------
ggplot() +
  geom_point(data=temp, aes(x=age, y=temp-14), col="lightgrey", size=0.5)+
  geom_line(data=temp2, aes(x=age, y=temp-14, col=temp-14), size=0.8) +
  scale_color_gradient2(low=pal[3],
                        mid=pal[2],
                       midpoint = 2,
                        high=pal[1]
                        
                       )+
  xlim(260,0)+
  ggthemes::theme_hc() 
                         