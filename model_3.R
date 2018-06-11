library(tidyverse)
library(lubridate)
library(googlesheets)
library(stringr)
library(rtweet)
library(plotly)
library(scales)

# Définition des fonctions pour le calcul des mesures sur les numéros de série.

N1 <- function(ech){
  return(round(2*median(ech)-1))
}

N2 <- function(ech){
  return(round(2*mean(ech)-1))
}

N3 <- function(ech){
  if (length(ech) == 1) return(ech)
  else return(round(max(ech)+min(ech)-1))
}

N4 <- function(ech){
  if (length(ech) == 1) return(ech)
  else{
    l <- length(unique(ech))
    return(round((l+1)/l*max(ech, na.rm = TRUE)-1))
  }
}

N5 <- function(ech){
  if (length(ech) == 1) return(ech)
  else{
    l <- length(unique(ech))
    return(round((max(ech, na.rm = TRUE)-min(ech, na.rm = TRUE))*(l+1)/(l-1)))
  }
}

tesla <- gs_key("188iQarE-7M49xKcTaCxEkQVd1A1FH5bFgJ5d9sQfJck")

data <- tesla %>%
  gs_read(ws = "All Entries")
alldata <- data[3:nrow(data),]
colnames(alldata) <- data[2,]
proddata <- tesla %>%
  gs_read(ws = "Production")

date <- c("Reservation date",
          "Invite date or access date to the Configurator",
          "Configuration date",
          "VIN assignment date",
          "Scheduled delivery date",
          "Last update",
          "Calculated delivery date")
time <- c("Reservation time")
int <- c("VIN (clean version x=5)")
cleanalldata <- alldata %>%
  mutate_at(date, funs(dmy)) %>%
  #mutate_at(time, funs(hm)) %>%
  mutate_at(int, funs(as.integer))
cleanproddata <- proddata %>%
  mutate_at("Date", funs(dmy)) %>%
  mutate_at("Total units_1", funs(as.integer))

INVITES <- cleanalldata %>%
  filter(!is.na(`VIN (clean version x=5)`) & !is.na(`VIN assignment date`)) %>%
  rename(vin_invites = `VIN (clean version x=5)`, date = `VIN assignment date`) %>%
  arrange(date)
  #mutate(N4 = N4(lag(vin_invites)), N5 = N5(lag(vin_invites)))
n4 <- vector(mode = "double", length = nrow(INVITES))
n5 <- vector(mode = "double", length = nrow(INVITES))
for (i in seq(length(n4))){
  n4[i] <- N4(INVITES$vin_invites[1:i])
  n5[i] <- N5(INVITES$vin_invites[1:i])
}
INVITES <- as.tibble(cbind(INVITES, tibble(N4 = n4, N5 = n5)))

PRODUCTION <- cleanproddata %>%
  filter(!is.na(`Date`) & !is.na(`Total units_1`)) %>%
  rename(date = `Date`, total_unites = `Total units_1`) %>%
  arrange(date)

Model3VINs <- get_timeline("Model3VINs", n = 3200)
Model3VINs$text <- str_to_lower(Model3VINs$text)

NHSTA <- Model3VINs %>%
  filter(str_detect(text, "highest vin is (\\d+)") == TRUE) %>%
  mutate(vin_nhsta = as.integer(str_match(text, "highest vin is (\\d+)")[,2])) %>%
  mutate(n_nhsta = as.integer(str_replace(str_match(text, "registered ([:graph:]*) new")[,2], ",", ""))) %>%
  mutate(date = as_date(created_at)) %>%
  select(date, n_nhsta, vin_nhsta) %>%
  arrange(date)

g <- ggplot()+
  geom_point(data = INVITES, mapping = aes(x = date, y = vin_invites), alpha = 0.5, size = 0.75)+
  geom_line(data = INVITES, mapping = aes(x = date, y = N4), color = "green")+
  geom_line(data = INVITES, mapping = aes(x = date, y = N5), color = "yellow")+
  geom_point(data = PRODUCTION, mapping = aes(x = date, y = total_unites), color = "red")+
  geom_line(data = PRODUCTION, mapping = aes(x = date, y = total_unites), color = "red")+
  geom_step(data = NHSTA, mapping = aes(x = date, y = vin_nhsta), color = "blue", size = 1)+
  labs(
    x = "Date d'assignation du numéro VIN",
    y = "Numéro VIN"
  )+
  scale_x_date(breaks = date_breaks("1 month"), limits = c(as.Date("2017-11-01"),NA))
g

STAT <- INVITES %>%
  mutate(semaine = week(date), mois = month(date), année = year(date)) %>%
  group_by(année, mois, semaine) %>%
  select(N4, N5)
