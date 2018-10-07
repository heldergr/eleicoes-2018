library(httr)
library(XML)
library(dplyr)
library(jsonlite)

Sys.setlocale("LC_ALL", "en_US.ISO8859-1")

ap <- fromJSON("https://d2thur3zpak26x.cloudfront.net/2018/1-turno/presidente_brasil.json?t=1538953595061")
m <- ap$municipios

zoeira <- NULL
for (m1 in m) {
    nome <- m1$nm
    c <- m1$c 
    c$Municipio <- nome
    c$v <- gsub("\\.", "", c$v)
    c$v <- as.numeric(c$v)
    c$Total <- sum(c$v)
    if (is.null(zoeira)) {
        zoeira <- c
    } else {
        zoeira <- do.call(rbind, list(zoeira, c))
    }
}

zoeira <- mutate(zoeira, Perc = v / Total)
zoeira <- arrange(zoeira, desc(Perc))

noHaddad <- filter(zoeira, nc != "FERNANDO HADDAD")

View(zoeira)
View(noHaddad)
