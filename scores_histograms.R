setwd("~/Desktop/Tools/contests_analysis")

library(dplyr)
library(ggplot2)

online1 <- read.csv("data/onlinequal1.csv", sep = ",")
online2 <- read.csv("data/onlinequal2.csv", sep = ",")
onsite1 <- read.csv("data/onsitequal.csv", sep = ",")
info <- read.csv("data/participantinfo.csv", sep = ",")

plot_histogram <- function(values, name, lim = 85, mybreak = 20, limx = 100){
  name = paste0(name, "(Average:", mean(values) %>% signif(4) %>% as.character(), ")")
  values %>% hist(ylim = c(0, lim),
                  xlim = c(0, limx),
                  breaks = mybreak,
                  main = name,
                  xlab = "Score")
}

online1 <- online1 %>% select(-P, -P.1, -P.2, -P.3)
names(online1)[names(online1)=="Global"] <- "online1"
online2 <- online2 %>% select(-P, -P.1, -P.2, -P.3)
names(online2)[names(online2)=="Global"] <- "online2"
online <- merge(online1, online2, by=c("Username", "User"))
online <- online %>% transform(total = online1 + online2)
online <- online %>% filter(total > 0)

onsite1 <- onsite1 %>% select(-P, -P.1, -P.2, -P.3)

png("plots/online1.png")
par(mfrow=c(1,3)) 
online$warning %>% plot_histogram("Warning")
online$mosquitoes %>% plot_histogram("Mosquitoes")
online$salamat %>% plot_histogram("Salamat")
dev.off()

png("plots/online2.png")
par(mfrow=c(1,3)) 
online$attendance %>% plot_histogram("Attendance")
online$compression %>% plot_histogram("Compression")
online$dawry %>% plot_histogram("Dawry")
dev.off()

png("plots/onlinetotals.png")
par(mfrow=c(1,3)) 
online$online1 %>% plot_histogram("Online 1 Global", 20, 30, 300)
online$online2 %>% plot_histogram("Online 2 Global", 20, 30, 300)
x <- online$total %>% plot_histogram("Online Total", 20, 30, 600)
abline(v = 249,col = "red")
text(20,20, "Rank 50", col = "red", adj = c(0, -.1))
dev.off()


onsite1 <- onsite1 %>% filter(Global > 0)
png("plots/onsite1.png")
par(mfrow=c(1,3)) 
onsite1$clients %>% plot_histogram("Clients", 35, 30, 100)
onsite1$tulips %>% plot_histogram("Tulips", 35, 30, 100)
onsite1$mines %>% plot_histogram("Mines", 35, 25, 100)
dev.off()

png("plots/onsite1total.png")
onsite1$Global %>% plot_histogram("Onsite Total", 20, 100, 300)
abline(v = 62,col = "red")
text(20,20, "Rank 20", col = "red", adj = c(0, -.1))
dev.off()

