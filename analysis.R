setwd("~/Desktop/Tools/contests_analysis")

library(sets)
library(dplyr)
library(ggplot2)

online1 <- read.csv("data/onlinequal1.csv", sep = ",")
online2 <- read.csv("data/onlinequal2.csv", sep = ",")
onsite1 <- read.csv("data/onsitequal.csv", sep = ",")
info <- read.csv("data/participantinfo.csv", sep = ",")

# Generate the onsite and online dataset

online1 <- online1 %>% select(-P, -P.1, -P.2, -P.3)
names(online1)[names(online1)=="Global"] <- "online1"

online2 <- online2 %>% select(-P, -P.1, -P.2, -P.3)
names(online2)[names(online2)=="Global"] <- "online2"

online <- merge(online1, online2, by=c("Username", "User"))
online <- online %>% transform(total = online1 + online2)
onsite1 <- onsite1 %>% select(-P, -P.1, -P.2, -P.3)

# Clean the "City" column
info$City <- info$City %>% as.character()
info$City <- info$City %>% trimws()
info$City[info$City %in% c("giza-6october", "6 October", "6 October city", "6october", "6th october", "6th October city")] <- "6 October City"
info$City[info$City %in% c("Al-Qaluynia", "Al-Qalyubia", "Qalubia")] <- "Al Qalubia"
info$City[info$City %in% c("الاسكندرية", "Egypt_Alex", "alex", "Alex", "Alexabdria", "Alexanderia", "alexandria", "Alexandria", "alix")] <- "Alexandria"
info$City[info$City %in% c("assiut", "Assiut", "Assuit", "Asyut")] <- "Assiut"
info$City[info$City %in% c("Bani seuif", "bani suef", "bani-suef", "beni suef", "Beni suef", "beni swief", "Beni-Suef")] <- "Beni Sweif"
info$City[info$City %in% c("nasr city", "Elobour", "Cairo ", "cario", "nasr city ", "sharabia", "new cairo", "New cairo", "New Cairo", "cairo", "Cairo", "cairo-egypt", "cairo")] <- "Cairo"
info$City[info$City %in% c("cairo 15 may", "15may city")] <- "15 May City"
info$City[info$City %in% c("El-Behira, Damanhour", "Damanhur ", "El behera, Damnhour", "Damanhour", "Damanhur", "damnhour")] <- "Damanhour"
info$City[info$City %in% c("gyza", "Giza-elbadrashen", "Giza-Aiyt", "Al Giza", "El Giza", "geza", "giza", "Giza")] <- "Al Giza"
info$City[info$City %in% c("desouk,kafrelsheikh", "desouqe")] <- "Desouq"
info$City[info$City %in% c("el-arish", "العريش")] <- "Al Arish"
info$City[info$City %in% c("elfayoum", "fayoum", "Fayoum")] <- "Al Fayoum"
info$City[info$City %in% c("Hurgada", "Hurgada ", "الغردقه", "hurghada", "Hurghada", "hurghada", "Hurghada , Red Sea")] <- "Hurghada"
info$City[info$City %in% c("lsmailia", "Ismailia ", "ismailia", "Ismailia", "Ismailia")] <- "Ismailia"
info$City[info$City %in% c("Kafer Alziat - Tanta", "kafr el-zayat", "Tanta")] <- "Tanta"
info$City[info$City %in% c("mansoura", "Mansoura", "El Mansoura")] <- "Mansoura"
info$City[info$City %in% c("menofia", "Menouf", "shibin-elkom _ monifia", "SHIBEIN", "shbeen")] <- "Menofia"
info$City[info$City %in% c("Naga Hammadi", "qena", " Qena", "kena", "Naga Hammadi, Qena")] <- "Qena"
info$City[info$City %in% c("portsaid", "Portsaid")] <- "Portsaid"
info$City[info$City %in% c("Qussair", "Qussier")] <- "Qussair"
info$City[info$City %in% c("zefta", "Zifta", "زفتى")] <- "Zefty"

# Merge participants' information with their scores

info <- info %>% transform(User = paste(First.Name, Last.Name))
info <- info %>% select(City, School, School.Year, Date.of.Birth, User)
info <- info %>% unique()

online <- online %>% merge(info, by = "User")

# User was capitalized in onsite data
info$User <- info$User %>% toupper()
info <- info %>% unique()
onsite1$User <- onsite1$User %>% toupper()
onsite1 <- onsite1 %>% merge(info, by = "User")

online <- online[!duplicated(online[,"Username"]),]
onsite1 <- onsite1[!duplicated(onsite1[,"Username"]),]

# Get list of frequencies
get_freqs <- function(values){
  freqs <- values %>% table() %>% as.data.frame()
  names(freqs) <- c("val", "freq")
  result <- values %>% sapply(function(value){
    filter(freqs, val == value)$freq
  })
  result %>% as.numeric()
}

onsite1 <- onsite1 %>% transform(perm = paste0(City, Global))
onsitefreq <- onsite1$perm %>% get_freqs()

online <- online %>% transform(perm = paste0(City, total))
onlinefreq <- online$perm %>% get_freqs()

# Plot scores against City
png("plots/score_vs_city_onsite.png")
ggplot(onsite1, aes(x = City, y = Global)) + xlab("City") + ylab("Score") + geom_point(aes(colour = Global, size = onsitefreq)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) + scale_size("Frequency", range = c(4, 7))
dev.off()

png("plots/score_vs_city_online.png")
ggplot(online, aes(x = City, y = total)) + xlab("City") + ylab("Score") + geom_point(aes(colour = total, size = onlinefreq)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) + scale_size("Frequency", range = c(2, 7))
dev.off()

# Plot scores against year of birth
onsite1 <- onsite1 %>% transform(year_of_birth = (2016 - as.numeric(format(as.Date(Date.of.Birth, "%m/%d/%Y"), "%Y"))) %>% as.factor())
online <- online %>% transform(year_of_birth = (2016 - as.numeric(format(as.Date(Date.of.Birth, "%m/%d/%Y"), "%Y"))) %>% as.factor())

png("plots/score_vs_age_onsite.png")
ggplot(onsite1, aes(x = year_of_birth, y = Global)) + xlab("Age") + ylab("Score") + geom_point(aes(colour = Global, size = onsitefreq)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + scale_size("Frequency", range = c(4, 7))
dev.off()

png("plots/score_vs_age_online.png")
ggplot(online %>% filter(total > 0), aes(x = year_of_birth, y = total)) + xlab("Age") + ylab("Score") + geom_point(aes(colour = total, size = get_freqs(filter(online, total > 0)$perm))) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + scale_size("Frequency", range = c(2, 7))
dev.off()

