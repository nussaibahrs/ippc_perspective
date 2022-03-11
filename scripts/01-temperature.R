library(ggplot)
library(ggforce)
library(cowplot)
library(patchwork)
# Last 66 million years ---------------------------------------------------

cenozoicT <- readxl::read_excel("data/data_paleofig1.xlsx", sheet=1)
colnames(cenozoicT) <- c("age", "deltaT")

# plot(cenozoicT$age, cenozoicT$deltaT, type="l", xlim=c(67,0))


##### load data ####
temp.df <- readxl::read_excel("data/data_paleofig1.xlsx", sheet=2)
temp.df <- temp.df[temp.df$age <=300 & temp.df$age > max(cenozoicT$age),] # remove cenozoic and older ones

#calculate anomaly
temp.df$deltaT <- temp.df$all  - 13.79

#merge new cenozoic data
temp.df <- plyr::rbind.fill(temp.df, cenozoicT)
temp.df <- temp.df[order(temp.df$age),]

# plot(temp.df$age, temp.df$deltaT, type="l")
# PETM - add values again
n <- which.min(abs(temp.df$age - 56))
temp.df$stage[n] <- "PETM"
temp.df$anomaly[n] <- 5
temp.df$min.anomaly[n] <- 3
temp.df$max.anomaly[n] <- 9

### hyperthermals
hyperthermals <- readxl::read_excel("data/data_paleofig1.xlsx", sheet=3)
hyperthermals <- merge(hyperthermals[,c("event", "name")], temp.df, by.x="name", by.y="stage")

# for uncertainties - hypterthermals only
mins <- temp.df$deltaT - temp.df$anomaly + temp.df$min.anomaly 
maxs <- temp.df$deltaT - temp.df$anomaly + temp.df$max.anomaly 

#omit to have ci for everything
mins <- mins[which(temp.df$stage %in% hyperthermals$name)] 
maxs <- maxs[which(temp.df$stage %in% hyperthermals$name)] 

hyp_ages <- temp.df$age[which(temp.df$stage %in% hyperthermals$name)]

hyp_ci <- data.frame(age=hyp_ages,
                     deltaT =hyperthermals$deltaT[order(hyperthermals$age)],
                     min=mins,
                     max=maxs)


# Future ------------------------------------------------------------------
temp.future <- readxl::read_excel("data/data_paleofig1.xlsx", sheet=4)

# Plot --------------------------------------------------------------------
pal <- c("#ffcf67",
         "#d3321d")

p1 <- ggplot()+
  geom_line(data=temp.df, aes(x=age, y=deltaT, col=deltaT), size=0.8) +
  geom_errorbar(data=hyp_ci, aes(x=age, ymin=min, ymax=max), width=2) +
  geom_point(data=hyperthermals, aes(x=age, y=deltaT, col=deltaT), fill="white", 
             shape=21, size=3) +
  scale_x_continuous(trans="reverse", limits=c(300,0),
                     expand = expansion(mult = 0.01))+
  labs(x="Age (Ma)", 
       y=expression(bold("Temperature  anomaly ("*degree*"C)")))+
  scale_color_gradient(low=pal[1],
                       high=pal[2]) +
  ggthemes::theme_few() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face=2),
        legend.position ="none")

p1

# Future Scenario ---------------------------------------------------------
temp.future <- temp.future[temp.future$time %in% c(2.6, 4.5,7,8.5),]

future_pal <- c("2.6"="#223555ff", "4.5"="#e7db5dff", "7.0"="#e33426ff", "8.5"="#781b25ff")
names(future_pal) <- paste0("SSP", c(1:3,5), "-", names(future_pal))
labs <- names(future_pal)
names(future_pal) <-NULL

box_rng <- temp.future %>%  group_by(year) %>%  summarise(min=min(lwr), max=max(upr))
posd <- 20
boxoff <- 0.02

miy = 0
may=6

p2 <- ggplot(temp.future)+
  geom_line(aes(x=year, y=mean, col=Scenario), linetype="dashed", alpha=0.5, position=position_dodge(width = posd)) +
  geom_point(aes(x=year, y=mean, col=Scenario), position=position_dodge(width = posd)) +
  geom_errorbar(aes(x=year, ymin=lwr, ymax=upr, col=Scenario), position=position_dodge(width = posd), width=5)+
  scale_color_manual(values=future_pal)+
  coord_cartesian(ylim=c(miy, may))+
  scale_x_continuous(breaks=c(2050, 2090), labels=c("2041-2060", "2081-2100")) +
  labs(x="Year", y="Temperature anomaly") +
  ggthemes::theme_few()+
  theme(panel.grid = element_blank(),
        legend.title = element_text(face="bold"),
        axis.text.x = element_text(hjust=0),
        legend.position = c(0.2,0.85),
        legend.box = "none",
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.title = element_text(face="bold")) +
  guides(col = guide_legend(nrow = 4))+
  annotate("rect", xmin=2040, xmax=2060, ymin=box_rng$min[box_rng$year==2050]-boxoff, ymax=box_rng$max[box_rng$year==2050]+boxoff, alpha=0.1) +
  annotate("rect", xmin=2080, xmax=2100, ymin=box_rng$min[box_rng$year==2090]-boxoff, ymax=box_rng$max[box_rng$year==2090]+boxoff, alpha=0.1) +
  annotate("rect", xmin=2035, xmax=2105, ymin=miy+0.3, ymax=miy, 
           fill="darkgrey", col="white") +
  annotate("text", x=2070, y=miy+0.15, label="Future Projections", fontface="bold",
           col="white")



p3 <- deeptime::gggeo_scale(p1,size=3, height = unit(1, "lines"), color="white")

svg("figs/fig01.svg", w=10, h=5)
cowplot::ggdraw(p3) + cowplot::ggdraw(p2) +
  plot_layout(widths = c(0.7,0.3))
dev.off()

