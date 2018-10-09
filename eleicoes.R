library(dplyr)
library(jsonlite)
library(ggplot2)

# Required for Mac Os
# Sys.setlocale("LC_ALL", "en_US.ISO8859-1")

prepararDadosMunicipio <- function(municipio, codigoIbge = "Vazio") {
    c <- municipio$c
    c$CodigoIbge <- codigoIbge
    c$Municipio <- municipio$nm
    c$v <- gsub("\\.", "", c$v)
    c$v <- as.numeric(c$v)
    c$Total <- sum(c$v)
    c <- subset( c, select = -c(cp, tc) )
    c    
}

adicionarRegiao <- function(apuracao) {
    apuracao %>%
        mutate(Regiao = "NE") %>%
        mutate(Regiao = if_else(UF %in% c("EXT"), "EXT", Regiao)) %>%
        mutate(Regiao = if_else(UF %in% c("MG", "SP", "RJ", "ES"), "SE", Regiao)) %>%
        mutate(Regiao = if_else(UF %in% c("RS", "SC", "PR"), "S", Regiao)) %>%
        mutate(Regiao = if_else(UF %in% c("MS", "MT", "GO"), "CO", Regiao)) %>%
        mutate(Regiao = if_else(UF %in% c("TO", "PA", "AM", "AP", "RR", "RO", "AC"), "N", Regiao)) -> apuracao
    apuracao
}

analisarApuracaoEstado <- function(apuracao, estado) {
    apuracaoEstado <- filter(apuracao, UF == estado)
    gg <- ggplot(apuracaoEstado, aes(x=nc, y=Perc))
    gg + geom_violin() + theme(axis.text.x = element_text(angle=90))
}

apuracao <- fromJSON("https://d2thur3zpak26x.cloudfront.net/2018/1-turno/presidente_brasil.json?t=1538953595061")
municipios <- apuracao$municipios

apuracaoMunicipio <- NULL
codigosIbge <- attributes(municipios)

for (codigoIbge in codigosIbge$names) {
    if (codigoIbge != "") {
        municipio <- municipios[[codigoIbge]]
        c <- prepararDadosMunicipio(municipio, codigoIbge)
        if (is.null(apuracaoMunicipio)) {
            apuracaoMunicipio <- c
        } else {
            apuracaoMunicipio <- do.call(rbind, list(apuracaoMunicipio, c))
        }
        municipios[[codigoIbge]] <- NULL
    }
}

codIbgeMunicipios <- read.csv("municipios.csv", row.names = NULL, sep = ";")
codIbgeMunicipios <- codIbgeMunicipios[, c(4,5)]
colnames(codIbgeMunicipios) <- c("UF", "CodigoIbge")
apuracaoMunicipio <- merge(apuracaoMunicipio, codIbgeMunicipios)

# Agora itera pelas cidade de fora do pais
for (municipio in municipios) {
    c <- prepararDadosMunicipio(municipio, "Vazio")
    c$UF <- "EXT"
    apuracaoMunicipio <- do.call(rbind, list(apuracaoMunicipio, c))
}

apuracaoMunicipio %>% 
    mutate(Perc = v / Total) %>%
    arrange(desc(Perc)) %>%
    adicionarRegiao() -> apuracaoMunicipio

# Filtra por candidatos mais votados
apuracaoMunicipioCandidatos <- filter(apuracaoMunicipio, 
    nc %in% c("FERNANDO HADDAD", "JAIR BOLSONARO", "CIRO GOMES"))

# Analise de apuracao por regiao
gg <- ggplot(apuracaoMunicipioCandidatos, aes(x=nc, y=Perc))
gg + geom_violin() + theme(axis.text.x = element_text(angle=90)) + facet_grid(. ~ Regiao)

analisarApuracaoEstado(apuracaoMunicipioCandidatos, "MG")
