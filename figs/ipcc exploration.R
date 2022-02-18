
IPCC <- read.csv("IPCC data.csv", header = TRUE)

library(tidyverse)
library(magrittr)

summarise(IPCC, group_by(source, category), count(spatial_scale))

space <- IPCC %>%
  group_by(category) %>%
  count(spatial_scale)
space
IPCC$source <- replace(IPCC$source, IPCC$source == "", "Web of Science")
IPCC$category <- replace(IPCC$category, IPCC$category == "historical", "near-time")
IPCC$paper_type <- replace(IPCC$paper_type, IPCC$paper_type == "Review (nothing new)", "Review")
IPCC$paper_type <- replace(IPCC$paper_type, IPCC$paper_type == "Review (new analysis)", "Review")

IPCC$scored_sum <- as.numeric(IPCC$scored_sum)

IPCCpaleo <- IPCC[(IPCC$category == "near-time"|IPCC$category == "deep-time"),]
IPCCpaleo$source <- replace(IPCCpaleo$source, IPCCpaleo$source == "AR5 Ch4", "ipcc")
IPCCpaleo$source <- replace(IPCCpaleo$source, IPCCpaleo$source == "AR5 Ch6", "ipcc")

ggplot(IPCCpaleo) +
  facet_grid(source~.) +
  geom_bar(aes(x = spatial_scale))

ggplot(IPCCpaleo) +
  facet_grid(source~.) +
  geom_bar(aes(x = category))

ggplot(IPCCpaleo, (aes())) +
  facet_grid(source~.) +
  geom_boxplot(aes(x = category, y = scored_sum))

ggplot(IPCCpaleo, (aes())) +
  facet_grid(source~.) +
  geom_boxplot(aes(x = paper_type, y = scored_sum))

ggplot(IPCCpaleo) +
  facet_grid(source~.) +
  geom_bar(aes(x = paper_type))

