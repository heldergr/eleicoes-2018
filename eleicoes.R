library(httr)
library(XML)
library(dplyr)
library(jsonlite)

Sys.setlocale("LC_ALL", "en_US.ISO8859-1")

apuracao <- fromJSON("https://d2thur3zpak26x.cloudfront.net/2018/1-turno/presidente_brasil.json?t=1538953595061")
print(apuracao$atualizacao$apuracao_total)
municipios <- apuracao$municipios

apuracaoMunicipio <- NULL
codigosIbge <- attributes(municipios)
for (codigoIbge in codigosIbge$names) {
    if (codigoIbge != "") {
        municipio <- municipios[[codigoIbge]]
        c <- municipio$c 
        c$CodigoIbge <- codigoIbge
        c$Municipio <- municipio$nm
        c$v <- gsub("\\.", "", c$v)
        c$v <- as.numeric(c$v)
        c$Total <- sum(c$v)
        if (is.null(apuracaoMunicipio)) {
            apuracaoMunicipio <- c
        } else {
            apuracaoMunicipio <- do.call(rbind, list(apuracaoMunicipio, c))
        }
    }
}

municipios <- read.csv("municipios.csv", row.names = NULL, sep = ";")
municipios <- municipios[, c(4,5)]
colnames(municipios) <- c("UF", "CodigoIbge")

apuracaoMunicipio <- merge(apuracaoMunicipio, municipios)

apuracaoMunicipio <- mutate(apuracaoMunicipio, Perc = v / Total)
apuracaoMunicipio <- arrange(apuracaoMunicipio, desc(Perc))

apuracaoSemHaddad <- filter(apuracaoMunicipio, nc != "FERNANDO HADDAD")

View(apuracaoMunicipio)
View(apuracaoSemHaddad)
