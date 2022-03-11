
# Merge xls files --------------------------------------------------------------
f <- list.files("data", "wos")
f <- f[3:5]

wos <- lapply(f, function(x) readxl::read_excel(file.path("data", x)))
wos <- do.call(rbind, wos)
write.csv(wos,"data/webofscience2.csv")


# Load --------------------------------------------------------------------
pubs <- read.csv("data/webofscience2.csv")

tot <- 45+21

sub.df <- pubs[sample(1:nrow(pubs), nrow(pubs)),]

sub.df <- data.frame(
first_author=unlist(lapply(sub.df$Authors, function(x) strsplit(as.character(x), split=";")[[1]][1])),
year=sub.df$Publication.Year,
title=sub.df$Article.Title,
doi=sub.df$DOI)

write.csv(sub.df, "output/random_pubs2.csv", row.names=F)


