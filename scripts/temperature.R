library(ggplot2)
library(mgcv)

source("scripts/functions.R")

# Load data ---------------------------------------------------------------
temp <- read.csv("data/Song_et_al_2019.csv")

temp$bin <- find_stg(age=temp$age,
          timescale=2012)

hyp <- read.csv("data/hyp.csv")
colnames(hyp)[1] <- "event"



pal <- c("#ffd60a",
         "#003566",
         "#000814")

mod_avg <- 13.79

# Calculate average per bin
meant <- data.frame(temp=tapply(temp$temp, temp$bin, mean, na.rm=T))
meant$stg <- as.numeric(row.names(meant))

gts("2020")

# Smooth ------------------------------------------------------------------
mod <- gam(temp~s(age), data=temp)
gam.check(mod)

ymod <- seq(0,260,1)
pred <- predict(mod, newdata=data.frame(age=ymod), se.fit = T)

temp2 <- data.frame(age=ymod,
                    temp=pred$fit-mod_avg)
hyp$pred <- predict(mod, newdata = data.frame(age=hyp$age)) - mod_avg

hyp

write.csv(hyp[,-1], "hyp.csv", row.names = F)

temp2 <- temp2[-which(temp2$age %in% round(hyp$age)),]
temp2 <- rbind(temp2,
               cbind(age=hyp$age,
                     temp=hyp$warming))

# Plot temperature --------------------------------------------------------
p <- ggplot() +
  geom_point(data=temp, aes(x=age, y=temp-mod_avg), col="lightgrey", size=1)+
  geom_line(data=temp, aes(x=age, y=temp-mod_avg), size=1) +
  # geom_ribbon(data=temp2, aes(x=age, ymin=temp-ci, ymax=temp+ci), alpha=0.5) +
  xlim(260,0)+
  geom_point(data=hyp, aes(x=age, y=warming), col="red", size=3) +
  ggthemes::theme_hc()

x11();p

png("figs/temp.png", w=8, h=5, res=300, units="in")
deeptime::gggeo_scale(p,size=3, height = unit(1, "lines"))
dev.off()
