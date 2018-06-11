library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)
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
    return(round((l+1)/l*max(ech)-1))
  }
}

N5 <- function(ech){
  if (length(ech) == 1) return(ech)
  else{
    l <- length(unique(ech))
    return(round((max(ech)-min(ech))*(l+1)/(l-1)))
  }
}

url <- "https://c64preservation.com/index.php/dp.php?pg=registry"
c64 <- read_html(url)

case_serial <- c64 %>%
  html_nodes(xpath = '//*[@class="lightercell"]') %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

board_assembly <- c64 %>%
  html_nodes(xpath = '//*[@class="lightercell"]') %>%
  html_nodes("td:nth-child(4)") %>%
  html_text()

board_rev <- c64 %>%
  html_nodes(xpath = '//*[@class="lightercell"]') %>%
  html_nodes("td:nth-child(5)") %>%
  html_text() %>%
  str_to_lower()

# Clean up data

## case_serial
case_serial <- str_replace_all(case_serial, "[:alpha:][\\d]?$", "")
case_serial <- str_replace_all(case_serial, "^[:alpha:]*\\d", "")
#case_serial <- str_replace_all(case_serial, "[:alpha:]", "")
case_serial <- str_replace_all(case_serial, "^0*", "")

## board_assembly
board_assembly <- str_replace_all(board_assembly, "[^\\d]", "")
board_assembly <- str_replace_all(board_assembly, "326[\\d]*", "326298")
board_assembly <- str_replace_all(board_assembly, "250407[\\d]*", "250407")
board_assembly <- str_replace_all(board_assembly, "250425[\\d]*", "250425")
board_assembly <- str_replace_all(board_assembly, "250466[\\d]*", "250466")
board_assembly <- str_replace_all(board_assembly, "250469[\\d]*", "250469")
board_assembly <- str_replace_all(board_assembly, "376298", "326298")
board_assembly <- str_replace_all(board_assembly, "329268", "326298")
board_assembly <- str_replace_all(board_assembly, "260407", "250407")
board_assembly <- str_replace_all(board_assembly, "1983250407", "250407")
board_assembly <- str_replace_all(board_assembly, "25407", "250407")
board_assembly <- str_replace_all(board_assembly, "25047", "250407")
board_assembly <- str_replace_all(board_assembly, "250047", "250407")
board_assembly <- str_replace_all(board_assembly, "2504077", "250407")
board_assembly <- str_replace_all(board_assembly, "250424", "250425")
board_assembly <- str_replace_all(board_assembly, "240407", "250407")
board_assembly <- str_replace_all(board_assembly, "240425", "250425")
board_assembly <- str_replace_all(board_assembly, "250457", "250407")
board_assembly <- str_replace_all(board_assembly, "25040", "250407")
board_assembly <- str_replace_all(board_assembly, "240469", "250469")
board_assembly <- str_replace_all(board_assembly, "240496", "250469")
board_assembly <- str_replace_all(board_assembly, "254069", "250469")
board_assembly <- str_replace_all(board_assembly, "250489", "250469")
board_assembly <- str_replace_all(board_assembly, "250496", "250469")
board_assembly <- str_replace_all(board_assembly, "2504077", "250407")
board_assembly <- str_replace_all(board_assembly, "1419[\\d]*", "KU-14194HB")
board_assembly <- str_replace_all(board_assembly, "104684", "KU-14194HB")
board_assembly <- str_replace_all(board_assembly, "251137", "")
board_assembly <- str_replace_all(board_assembly, "2511[\\d]*", "")
board_assembly <- str_replace_all(board_assembly, "2504$", "")
board_assembly <- str_replace_all(board_assembly, "252311$", "")


## board_rev
board_rev <- str_replace_all(board_rev, "rev[.|\\s|-]*(\\d|\\w)","\\1")
board_rev <- str_replace_all(board_rev, "(\\d|\\w)[\\s|\\d|.|-]*","\\1")
board_rev <- str_replace_all(board_rev,"^a$","A")
board_rev <- str_replace_all(board_rev,"^b$","B")
board_rev <- str_replace_all(board_rev,"^c$","C")
board_rev <- str_replace_all(board_rev,"[^ABC\\d{1}]","")
board_rev <- str_replace_all(board_rev,"6","")
board_rev <- str_replace_all(board_rev,"2","")
board_rev <- str_replace_all(board_rev,"0","")
board_rev <- str_replace_all(board_rev,"1","")
board_rev <- str_replace_all(board_rev,"8","")
board_rev <- str_replace_all(board_rev,"9","")

commodore64 <-  tibble(
  case_serial = as.double(case_serial),
  board_assembly = board_assembly,
  board_rev = board_rev
)

clean64 <- commodore64 %>%
  mutate(rev = if_else(board_assembly == "KU-14194HB", "", board_rev)) %>%
  mutate(board = if_else(rev == "" | board_assembly == "",board_assembly,paste0(board_assembly,"/",rev))) %>%
  filter(board %in% c("326298", "326298/A", "326298/B", "326298/C", "KU-14194HB", "250407/A", "250407/B", "250407/C", "250425", "250425/A", "250425/B",
                      "250466", "250469/3", "250469/4", "250469/A", "250469/B")) %>%
  filter(!is.na(case_serial))

clean64 %>%
  group_by(board) %>%
  summarise(n = n(), min = min(case_serial), max = max(case_serial))
