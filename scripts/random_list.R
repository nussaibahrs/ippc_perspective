pubs <- read.csv("data/webofscience.csv")

tot <- 45+21

set.seed(42)

sub.df <- pubs[sample(1:nrow(pubs), nrow(pubs)),]

sub.df <- data.frame(
first_author=unlist(lapply(sub.df$Authors, function(x) strsplit(as.character(x), split=";")[[1]][1])),
year=sub.df$Publication.Year,
title=sub.df$Article.Title,
doi=sub.df$DOI)

write.csv(sub.df, "output/random_pubs.csv", row.names=F)


