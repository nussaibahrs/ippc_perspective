library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(patchwork)

# Set theme ---------------------------------------------------------------
theme_set(theme_hc())
theme_replace(axis.title.y=element_text(angle=90),
              axis.title = element_text(face="bold"),
              plot.tag=element_text(size=10))

pal <- c("#efc88bff",
         "#cf5c36ff",
         "#050517ff")

# Load data ---------------------------------------------------------------
ipcc <- read.csv("data/ipcc_pubs.csv")
ipcc$type <- "IPCC"
ipcc$paleo <- 0
ipcc$paleo[ipcc$category %in% c("deep-time", "near-time", "historical")] <- 1

random <- read.csv("data/random_pubs.csv")
random$type <- "WoS"
random$paleo <- 2

cols <- c("type", "scored_sum", "paleo")

dat <- rbind(ipcc[,cols], random[,cols])

# Compare modern vs paleo -------------------------------------------------
count <- table(dat$paleo)

p1 <- ggplot(dat, aes(x=paleo, y=scored_sum, col=factor(paleo))) +
  geom_boxplot() +
  geom_jitter(alpha=0.5) +
  scale_x_continuous(breaks=c(0,1,2),
                     labels = c("modern", "paleo", "random")) +
  scale_y_continuous(breaks=seq(0,11, 2), limits=c(0,11)) +
  labs(x="", y="Relevance score", tag="(a)") +
  scale_color_manual(values=pal)+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10),
        legend.position ="none") +
  annotate("text", x=0-0.25, y=0.5, label=paste0("n=", count[1]), fontface=3,
           size=3)+
  annotate("text", x=1-0.25, y=0.5, label=paste0("n=", count[2]), fontface=3,
           size=3)+
  annotate("text", x=2-0.25, y=0.5, label=paste0("n=", count[3]), fontface=3,
           size=3)

ggsave("figs/fig02.svg", p1, w=6, h=5)

# By categories -----------------------------------------------------------
labs <- read.csv("data/cats.csv")
cats <- rbind(ipcc[ipcc$paleo==1, c(labs$short, "paleo")], random[,c(labs$short, "paleo")])

cats <- reshape2::melt(cats, id.vars="paleo")

cats <- cats %>%  group_by(variable, paleo) %>% 
  summarise(sum=sum(value, na.rm = T)) %>% 
  left_join(data.frame(paleo=c(1,2),
                       tot=c(67,100))) %>% 
  ungroup() %>% 
  mutate(perc = sum/tot)

cats <- merge(cats, labs, by.x="variable", by.y="short")
cats$paleo <- factor(cats$paleo, levels=c(2,1))
cats$category <- factor(cats$category, levels=c("Framing", "Taxonomic Scope", "Reporting", "Climate Change", "Relevance for Prediction"))

palf <- pal[2:3]
names(palf) <- c(1,2)

palf[2] <- scales::alpha(palf[2], 0.8)
labsf <- c("2"="WoS","1"="IPCC")

p2 <- ggplot(cats, aes(x=reorder(long, perc), y=perc*100, fill=paleo)) +
  geom_bar(stat="identity", position = "dodge", width = 0.6) +
  #geom_text(aes(label=sum), size=3, hjust=1.2, col="white", fontface=2, position = position_dodge(width = .9))+
  coord_flip() +
  scale_fill_manual(values=palf, labels=labsf) +
  labs(x="", y="Percentage of publications", tag="(b)", fill="Publication type") +
  facet_grid(category~., scale="free_y", space="free")


p3 <- plot_grid(p1, p2, ncol=1, rel_heights = c(0.6,1))

save_plot("figs/fig02.svg", p3, base_width=6, base_height = 6)

# Scales ------------------------------------------------------------------
cols <- c("first_author", "spatial_scale", "source", "scope", "category", 
          "temporal_resolution")

dat2 <- rbind(ipcc[,cols],
      random[,cols]) %>%
filter(!category %in% c("modern", "modern/future"))

dat2$paleo <- 2
dat2$paleo[dat2$source=="Web of Science"] <- 1

dat2$spatial_scale[dat2$spatial_scale %in% c("na", "")] <- NA
dat2$spatial_scale[is.na(dat2$spatial_scale)] <- "Not specified"

dat2$spatial_scale[dat2$spatial_scale %in% c("multiple", "various")] <- "Multiple"


dat2$category[dat2$category=="historical"] <- "near-time"



spatial <- dat2 %>% 
  group_by(paleo, spatial_scale) %>%
  tally() %>% 
  ungroup() %>% 
  add_row(paleo=1, 
          spatial_scale="Multiple", 
          n=0)

spatial <- spatial %>% 
  left_join(
    dat2 %>% group_by(paleo) %>% 
  tally(name="sum")
) %>% 
  mutate(prop=n/sum)

spatial$spatial_scale <- factor(spatial$spatial_scale,
                                levels=rev(c("Local", "Regional", "Basin", "Continent",
                                         "Global", "Multiple", "Not specified")))

names(palf) <- c(2,1)
names(labsf) <- rev(names(labsf))

p4 <- ggplot(data=spatial, aes(x=spatial_scale, y=prop*100, fill=factor(paleo))) +
  geom_bar(stat="identity", position="dodge", width=0.6) +
  scale_fill_manual(values=palf, labels=labsf) +
  labs(x="", y="", fill="")+
  coord_flip() 

dat2$category[dat2$category %in% c("", "na")] <- "not specified"
dat2$category[dat2$category == "deep-time/near-time"] <- "both"

temporal <- dat2 %>% 
  group_by(paleo, category) %>% 
  tally() %>% 
  group_by(paleo) %>% 
  mutate(prop = n/sum(n))

temporal$category <- factor(temporal$category,
                            levels=c("not specified", "both", "deep-time", "near-time"))
  
p5 <- ggplot(data=temporal, aes(x=category, y=prop*100, fill=factor(paleo))) +
  geom_bar(stat="identity", position="dodge", width=0.6) +
  scale_fill_manual(values=palf, labels=labsf) +
  labs(x="", y="Percentage of publications", fill="")+
  coord_flip() 


dat2 %>% 
  group_by(paleo, temporal_resolution) %>% 
  tally() %>% 
  ggplot(aes(x=temporal_resolution, y=n, fill=paleo)) +
  geom_bar(stat="identity")

unique(dat2$temporal_resolution)

svg("figs/fig_03.svg", w=8, h=4)
p4 + p5 +
  plot_layout(ncol=1, guides = "collect", heights=c(7,4)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
dev.off()
