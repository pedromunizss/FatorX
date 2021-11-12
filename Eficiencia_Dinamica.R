library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(ipeadatar)
library(reshape2)

setwd('C:/Users/pedro.silva/OneDrive - Agencia Reguladora de Aguas Energia e Saneamento Basico do Distrito Federal/FatorX/ED')

# COLETANDO DADOS DO IPCA DE 1999 A 2019
IPCA <- ipeadata(code = 'PAN_IPCAG', language = 'br')
IPCA$value <- round(IPCA$value, digits = 2)
IPCA$date <- year(IPCA$date)
IPCA <- filter(IPCA, date >= 1999 & date <= 2019)

# AJUSTE PELO IPCA
for (i in nrow(IPCA):1) {
  if (i == nrow(IPCA))
    IPCA$ajuste[i] <- 1
  else{
    IPCA$ajuste[i] <- IPCA$ajuste[i+1]/(1+IPCA$value[i+1]/100)
  }
}

IPCA <- IPCA[, c(2,3,6)]
names(IPCA) <- c('ANO', 'IPCA', 'IPCA_AJUSTADO')
#### Dados SNIS ####

# Coleta das informacoes a serem utilizadas
# SNIS - Serie Historica, Informacoes de indicadores agregados
## Filtros aplicados:
### Dados Gerais:
#### Ano de referencia - 2000 a 2019
#### Abrangencia - Regional
#### Tipo de servico - Agua e Esgoto
#### Natureza juridica - Todos
### Localizacao geografica:
#### Regiao - Todos (remover SANEMAT)
#### Estado - Todos
#### Prestadores - Todos (ate o momento, 15/10/21, sao 26 prestadores)

## Tabelas utilizadas e colunas selecionadas
### Informacoes Finaceiras:
#### FN002 - Receita Operacional Direta de Agua
#### FN003 - Receita Operacional Direta de Esgoto
#### FN010 - Despesa com pessoal proprio e provisao para devedores duvidosos
#### FN015 - Despesas de explaracao (Dex)
#### NF016 - Despesas com juros e encargos do servico da divida
#### FN019 - Despesas com depreciacao, amortizacao
#### FN026 - Quantidade total de empregados proprios
### Informacoes de Agua:
#### AG011 - Volume de agua faturado
### Informacoes de Esgoto:
#### ES007 - Volume de esgoto faturado
### Informacoes do Balanco:
#### BL002 - Ativo Total

dados <- read.table(file = 'Agregado-20211103161719.csv',
                   header = TRUE,
                   dec = ',',
                   sep = ';',
                   na.strings = '',
                   encoding = 'UTF-8',
                   nrows = 527)

names(dados) <- c('CO_MUNICIPIO', 'NM_MUNICIPIO', 'SG_UF', 'ANO', 'CO_PRESTADOR',
                  'NM_PRESTADOR', 'SG_PRESTADOR', 'ABRANGENCIA', 'TP_SERVICO',
                  'NATUREZA_JURIDICA', 'AG011', 'BL002', 'ES007', 'FN002', 'FN003',
                  'FN010', 'FN015', 'FN016', 'FN019','FN026')


for (i in 11:20) {
  dados[, i] <- gsub(pattern = '\\.', '', dados[, i])
  dados[, i] <- gsub(pattern = ',', '\\.', dados[, i]) |> as.numeric()
}

####### CORRIGINDO INFORMACOES DA SANESUL ##########
# O Ativo total da empresa nos anos de 2018 e 2019 precisam ser multiplicados por 1000
dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO %in% c(2018,2019), 'BL002'] <-
  1000 * dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO %in% c(2018,2019), 'BL002']
dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO == 2018, 'BL002'] <- 995199



dados <- dados[dados$SG_PRESTADOR != 'SANEMAT',]
dados <- dados[dados$SG_PRESTADOR != 'COSAMA',]
dados <- dados[dados$SG_PRESTADOR != 'DEPASA',]

dados$ANO <- as.Date(as.character(dados$ANO), format = "%Y") |>
  year()

dados <- merge(dados, IPCA, by = 'ANO')
dados <- data.table(dados)


# REMOVENDO OUTLIERS DA FN026
dados[, IQR_FN026 := (quantile(FN026, 0.75, na.rm = T, type = 6) - quantile(FN026, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN026 := mean(FN026, na.rm = T), by = SG_PRESTADOR]

dados[, FN026_RMOUTLIERS := ifelse(is.na(FN026),
                                   yes = NA,
                                   no = ifelse(FN026 < MEDIA_FN026-1.5*IQR_FN026,
                                               yes = NA,
                                               no = ifelse(FN026 > MEDIA_FN026+1.5*IQR_FN026,
                                                           yes = NA,
                                                           no = FN026)))]
dados[, IQR_FN026 := NULL][, MEDIA_FN026 := NULL]


# REMOVENDO OUTLIERS DA FN010
dados[, IQR_FN010 := (quantile(FN010, 0.75, na.rm = T, type = 6) - quantile(FN010, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN010 := mean(FN010, na.rm = T), by = SG_PRESTADOR]

dados[, FN010_RMOUTLIERS := ifelse(is.na(FN010),
                                   yes = 0,
                                   no = ifelse(FN010 < MEDIA_FN010-1.5*IQR_FN010,
                                               yes = 0,
                                               no = ifelse(FN010 > MEDIA_FN010+1.5*IQR_FN010,
                                                           yes = 0,
                                                           no = FN010)))]
dados[, IQR_FN010 := NULL][, MEDIA_FN010 := NULL]


# AJUSTANDO FN010 PELO IPCA
dados[, FN010_AJUSTADO := FN010_RMOUTLIERS/IPCA_AJUSTADO]


# Despesas(ajustadas) por empregados
dados <- dados[, DESPESA_POR_EMPREGADO := (FN010_RMOUTLIERS/FN026_RMOUTLIERS)/IPCA_AJUSTADO]


# Ajuste pelo IPCA das Despesas com depreciação
dados <- dados[, FN019_AJUSTADO := FN019/IPCA_AJUSTADO]

# Ajuste pelo IPCA das Despesas financeiras
# removendo valores fora dos limimites mean +(-) 1.5*distancia_interquartil
dados[, IQR_FN019 := (quantile(FN019, 0.75, na.rm = T, type = 6) - quantile(FN019, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN019 := mean(FN019, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN019_RMOUTLIERS := ifelse(is.na(FN019),
                                   yes = 0,
                                   no = ifelse(FN019 < MEDIA_FN019-1.5*IQR_FN019,
                                               yes = 0,
                                               no = ifelse(FN019 > MEDIA_FN019+1.5*IQR_FN019,
                                                           yes = 0,
                                                           no = FN019)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN019
dados[, IQR_FN019 := NULL][, MEDIA_FN019 := NULL]


# susbtitui os NAs por zeros
#dados$FN016 <- ifelse(is.na(dados$FN016), yes = 0, no = dados$FN016)

dados[, IQR_FN016 := (quantile(FN016, 0.75, na.rm = T, type = 6) - quantile(FN016, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN016 := mean(FN016, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN016_RMOUTLIERS := ifelse(is.na(FN016), yes = 0,
                                   no = ifelse(FN016 < MEDIA_FN016-1.5*IQR_FN016,
                                               yes = 0,
                                               no = ifelse(FN016 > MEDIA_FN016+1.5*IQR_FN016,
                                                           yes = 0,
                                                           no = FN016)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN016
dados[, IQR_FN016 := NULL][, MEDIA_FN016 := NULL]

# ativos 

dados[, IQR_BL002 := (quantile(BL002, 0.75, na.rm = T, type = 6) - quantile(BL002, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_BL002 := mean(BL002, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, BL002_RMOUTLIERS := ifelse(is.na(BL002),
                                   yes = 0,
                                   no = ifelse(BL002 < MEDIA_BL002-1.5*IQR_BL002,
                                               yes = 0,
                                               no = ifelse(BL002 > MEDIA_BL002+1.5*IQR_BL002,
                                                           yes = 0,
                                                           no = BL002)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE BL002
dados[, IQR_BL002 := NULL][, MEDIA_BL002 := NULL]

# AJUSTANDO PELO IPCA PARA CALCULAR O CAPEX
dados[, FN019_RMOUTLIERS_IPCA := FN019_RMOUTLIERS/IPCA_AJUSTADO]
dados[, FN016_RMOUTLIERS_IPCA := FN016_RMOUTLIERS/IPCA_AJUSTADO]
dados[, BL002_RMOUTLIERS_IPCA := (BL002_RMOUTLIERS/IPCA_AJUSTADO)*1000]

# CAPEX
dados[, CAPEX := (FN019_RMOUTLIERS_IPCA+FN016_RMOUTLIERS_IPCA)/BL002_RMOUTLIERS_IPCA]




# CALCULO DESPESAS OPERACIONAIS (REMOVENDO DESPESAS COM PESSOAL PROPRIO) - DESPESAS DE EXPLORACAO
dados[, IQR_FN015 := (quantile(FN015, 0.75, na.rm = T, type = 6) - quantile(FN015, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN015 := mean(FN015, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN015_RMOUTLIERS := ifelse(is.na(FN015),
                                   yes = 0,
                                   no = ifelse(FN015 < MEDIA_FN015-1.5*IQR_FN015,
                                               yes = 0,
                                               no = ifelse(FN015 > MEDIA_FN015+1.5*IQR_FN015,
                                                           yes = 0,
                                                           no = FN015)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN015
dados[, IQR_FN015 := NULL][, MEDIA_FN015 := NULL]
# AJUSTANDO PELO IPCA
dados[, DEX := FN015_RMOUTLIERS/IPCA_AJUSTADO]


# CALCULO DESPESAS OPERACIONAIS - DESPESAS COM PESSOAL PROPRIO
dados[, IQR_FN010 := (quantile(FN010, 0.75, na.rm = T, type = 6) - quantile(FN010, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN010 := mean(FN010, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN010_RMOUTLIERS := ifelse(is.na(FN010),
                                   yes = 0,
                                   no = ifelse(FN010 < MEDIA_FN010-1.5*IQR_FN010,
                                               yes = 0,
                                               no = ifelse(FN010 > MEDIA_FN010+1.5*IQR_FN010,
                                                           yes = 0,
                                                           no = FN010)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN010
dados[, IQR_FN010 := NULL][, MEDIA_FN010 := NULL]
# AJUSTANDO PELO IPCA
dados[, DES_PESSOAL_PROP := FN010_RMOUTLIERS/IPCA_AJUSTADO]


# OUTRAS DESPESAS OPERACIONAIS (DEX MENOS DESPESAS COM PESSOAL PROPRIO)
dados[, qOG := DEX - DES_PESSOAL_PROP]


prestador <- unique(dados$SG_PRESTADOR)

# PTF IN PESSOAL
ptf_input_pessoal <- data.frame(ANO = c(2000:2019))

PTF_input <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_input[t-1999] <- NA
    }
    else{
      PTF_input[t-1999] <- (aux[ANO == t, FN026_RMOUTLIERS]/aux[ANO == t-1, FN026_RMOUTLIERS])^
        (.5*((aux[ANO==t-1, DESPESA_POR_EMPREGADO]*aux[ANO==t-1, FN026_RMOUTLIERS]/
               (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_RMOUTLIERS] +
                  aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]
               )
        ) + (aux[ANO==t, DESPESA_POR_EMPREGADO]*aux[ANO==t, FN026_RMOUTLIERS]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_RMOUTLIERS] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO])
        ))
        )
    }
  }
  
  ptf_input_pessoal <- cbind(ptf_input_pessoal, PTF_input)
  names(ptf_input_pessoal)[i+1] <- prestador[i]
}

# PTF IN OUTROS GASTOS
ptf_input_outros_gastos <- data.frame(ANO = c(2000:2019))

PTF_input <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_input[t-1999] <- NA
    }
    else{
      PTF_input[t-1999] <- (aux[ANO == t, qOG]/aux[ANO == t-1, qOG])^
        (.5*((aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]/
               (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_RMOUTLIERS] +
                  aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]
               )
        ) + (aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_RMOUTLIERS] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO])
        ))
        )
    }
  }
  
  ptf_input_outros_gastos <- cbind(ptf_input_outros_gastos, PTF_input)
  names(ptf_input_outros_gastos)[i+1] <- prestador[i]
}

# PTF IN CAPEX

ptf_input_capex <- data.frame(ANO = c(2000:2019))

PTF_input <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_input[t-1999] <- NA
    }
    else{
      PTF_input[t-1999] <- (aux[ANO == t, BL002_RMOUTLIERS_IPCA]/aux[ANO == t-1, BL002_RMOUTLIERS_IPCA])^
        (.5*((aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_RMOUTLIERS_IPCA]/
               (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_RMOUTLIERS] +
                  aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]
               )
        ) + (aux[ANO==t, CAPEX] * aux[ANO==t, BL002_RMOUTLIERS_IPCA]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_RMOUTLIERS] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_RMOUTLIERS_IPCA] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO])
        ))
        )
    }
  }
  
  ptf_input_capex <- cbind(ptf_input_capex, PTF_input)
  names(ptf_input_capex)[i+1] <- prestador[i]
}


PTF_INPUT <- ptf_input_pessoal*ptf_input_outros_gastos*ptf_input_capex
PTF_INPUT <- PTF_INPUT[, c("ANO", sort(names(PTF_INPUT[,2:26])))]
PTF_INPUT$ANO <- 2000:2019

# transformando NaN e Inf em NA
for (i in 2:26) {
  PTF_INPUT[,i] <- ifelse(is.infinite(PTF_INPUT[,i]), NA, PTF_INPUT[,i])
  PTF_INPUT[,i] <- ifelse(is.nan(PTF_INPUT[,i]), NA, PTF_INPUT[,i])
}


################################################################################
################################### PTF OUTPUT #################################
################################################################################

# PRODUTO - AGUA
# Volume de agua Faturado(AG011)

dados[, IQR_AG011 := (quantile(AG011, 0.75, na.rm = T, type = 6) - quantile(AG011, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_AG011 := mean(AG011, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, AG011_RMOUTLIERS := ifelse(is.na(AG011),
                                   yes = NA,
                                   no = ifelse(AG011 < MEDIA_AG011-1.5*IQR_AG011,
                                               yes = NA,
                                               no = ifelse(AG011 > MEDIA_AG011+1.5*IQR_AG011,
                                                           yes = NA,
                                                           no = AG011)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE AG011
dados[, IQR_AG011 := NULL][, MEDIA_AG011 := NULL]

# Volume de esgoto Faturado(ES007)

dados[, IQR_ES007 := (quantile(ES007, 0.75, na.rm = T, type = 6) - quantile(ES007, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_ES007 := mean(ES007, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, ES007_RMOUTLIERS := ifelse(is.na(ES007),
                                   yes = NA,
                                   no = ifelse(ES007 < MEDIA_ES007-1.5*IQR_ES007,
                                               yes = NA,
                                               no = ifelse(ES007 > MEDIA_ES007+1.5*IQR_ES007,
                                                           yes = NA,
                                                           no = ES007)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE ES007
dados[, IQR_ES007 := NULL][, MEDIA_ES007 := NULL]

# Receita Operacional direta de agua
dados[, IQR_FN002 := (quantile(FN002, 0.75, na.rm = T, type = 6) - quantile(FN002, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN002 := mean(FN002, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN002_RMOUTLIERS := ifelse(is.na(FN002),
                                   yes = NA,
                                   no = ifelse(FN002 < MEDIA_FN002-1.5*IQR_FN002,
                                               yes = NA,
                                               no = ifelse(FN002 > MEDIA_FN002+1.5*IQR_FN002,
                                                           yes = NA,
                                                           no = FN002)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN002
dados[, IQR_FN002 := NULL][, MEDIA_FN002 := NULL]
# ajuste pelo ipca
dados[, FN002_RMOUTLIERS := FN002_RMOUTLIERS/IPCA_AJUSTADO]


# Preco da agua
dados[, pAg := FN002_RMOUTLIERS/AG011_RMOUTLIERS]

###################################
# dados[dados$SG_PRESTADOR == 'CASAL' & dados$ANO == 2019, 'pAg'] <- 0
# dados[dados$SG_PRESTADOR == 'COSANPA' & dados$ANO == 2019, 'pAg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANEPAR' & dados$ANO == 2019, 'pAg'] <- 0
###################################

# Receita Operacional direta de ESGOTO
dados[, IQR_FN003 := (quantile(FN003, 0.75, na.rm = T, type = 6) - quantile(FN003, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]
dados[, MEDIA_FN003 := mean(FN003, na.rm = T), by = SG_PRESTADOR]

# REMOVENDO OUTLIERS
dados[, FN003_RMOUTLIERS := ifelse(is.na(FN003),
                                   yes = NA,
                                   no = ifelse(FN003 < MEDIA_FN003-1.5*IQR_FN003,
                                               yes = NA,
                                               no = ifelse(FN003 > MEDIA_FN003+1.5*IQR_FN003,
                                                           yes = NA,
                                                           no = FN003)))]
# APAGA AS COLUNAS INTERQUATIL E MEDIA DE FN003
dados[, IQR_FN003 := NULL][, MEDIA_FN003 := NULL]
# ajuste pelo ipca
dados[, FN003_RMOUTLIERS := FN003_RMOUTLIERS/IPCA_AJUSTADO]

# Preco do esgoto
dados[, pEg := FN003_RMOUTLIERS/ES007_RMOUTLIERS]


###################################
# dados[dados$SG_PRESTADOR == 'CAGECE' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'CORSAN' & dados$ANO == 2018, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'CORSAN' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'COSANPA' & dados$ANO == 2018, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'COSANPA' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'DESO' & dados$ANO == 2018, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'DESO' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SABESP' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANEAGO' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANEATINS' & dados$ANO == 2018, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANEATINS' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANEPAR' & dados$ANO == 2019, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO == 2018, 'pEg'] <- 0
# dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO == 2019, 'pEg'] <- 0
###################################

ptf_output_agua <- data.frame(ANO = c(2000:2019))

PTF_output <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_output[t-1999] <- NA
    }
    else{
      PTF_output[t-1999] <- (aux[ANO == t, AG011_RMOUTLIERS]/aux[ANO == t-1, AG011_RMOUTLIERS])^
        (.5*((aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_RMOUTLIERS]/
               (aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_RMOUTLIERS] + aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_RMOUTLIERS])
        ) + (aux[ANO==t, pAg] * aux[ANO == t, AG011_RMOUTLIERS]/
               (aux[ANO==t, pAg] * aux[ANO == t, AG011_RMOUTLIERS] + aux[ANO==t, pEg] * aux[ANO == t, ES007_RMOUTLIERS])
        ))
        )
    }
  }
  
  ptf_output_agua <- cbind(ptf_output_agua, PTF_output)
  names(ptf_output_agua)[i+1] <- prestador[i]
}



ptf_output_esgoto <- data.frame(ANO = c(2000:2019))

PTF_output <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_output[t-1999] <- NA
    }
    else{
      PTF_output[t-1999] <- (aux[ANO == t, ES007_RMOUTLIERS]/aux[ANO == t-1, ES007_RMOUTLIERS])^
        (.5*((aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_RMOUTLIERS]/
               (aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_RMOUTLIERS] + aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_RMOUTLIERS])
        ) + (aux[ANO==t, pEg] * aux[ANO == t, ES007_RMOUTLIERS]/
               (aux[ANO==t, pAg] * aux[ANO == t, AG011_RMOUTLIERS] + aux[ANO==t, pEg] * aux[ANO == t, ES007_RMOUTLIERS])
        ))
        )
    }
  }
  
  ptf_output_esgoto <- cbind(ptf_output_esgoto, PTF_output)
  names(ptf_output_esgoto)[i+1] <- prestador[i]
}


PTF_OUTPUT <- ptf_output_agua*ptf_output_esgoto
PTF_OUTPUT <- PTF_OUTPUT[, c("ANO", sort(names(PTF_OUTPUT[,2:26])))]
PTF_OUTPUT$ANO <- 2000:2019



PTF <- PTF_OUTPUT/PTF_INPUT
PTF$ANO <- 2000:2019
# ordenando as empresas em ordem alfabetica
PTF <- data.frame(PTF[, c('ANO',sort(names(PTF[, 2:26])))])

for (i in 2:26) {
  PTF[,i] <- ifelse(is.infinite(PTF[,i]), NA, PTF[,i])
  PTF[,i] <- ifelse(is.nan(PTF[,i]), NA, PTF[,i])
}
PTF <- data.table(PTF)
########### Final ################

vol_faturado_ag <- dados[, .(ANO, SG_PRESTADOR, AG011_RMOUTLIERS)]
vol_faturado_ag <- dcast.data.table(vol_faturado_ag, ANO ~ SG_PRESTADOR, value.var = 'AG011_RMOUTLIERS')
vol_faturado_ag <- vol_faturado_ag[ANO != 1999]


vol_faturado_eg <- dados[, .(ANO, SG_PRESTADOR, ES007_RMOUTLIERS)]
vol_faturado_eg <- dcast.data.table(vol_faturado_eg, ANO ~ SG_PRESTADOR, value.var = 'ES007_RMOUTLIERS')
vol_faturado_eg <- vol_faturado_eg[ANO != 1999]

# CONFERINDO SE A COLUNAS (EMPRESAS) ESTAO NA MESMA ORDEM
names(vol_faturado_ag) == names(PTF)


# PONDERANDO PELOS VALORES FATURADOS
vol_faturado <- vol_faturado_ag + vol_faturado_eg
vol_faturado$ANO <- 2000:2019
# REMOVENDO O VALOR FATURADO DAS EMPRESAS QUE N�O FORAM CALCULADOS O PTF DO RESPECTIVO ANO
aux <- c(is.na(PTF))
vol_faturado <- vol_faturado*ifelse((aux), NA, 1)



numerador <- data.frame(PTF * vol_faturado)
numerador$ANO <- 2000:2019
for (i in 2:26) {
  numerador[,i] <- ifelse(is.infinite(numerador[,i]), NA, numerador[,i])
  numerador[,i] <- ifelse(is.nan(numerador[,i]), NA, numerador[,i])
}


mean(rowSums(numerador[,2:26], na.rm = T)/rowSums(vol_faturado[,2:26], na.rm = T))









