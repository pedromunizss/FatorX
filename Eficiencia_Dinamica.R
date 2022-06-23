# Verificando e instalando os pacotes necessários
pkg <- c('dplyr', 'data.table', 'lubridate', 'ipeadatar', 'reshape2', 'scales')

pkg_install <- pkg[!(pkg %in% installed.packages())]
if(length(pkg_install)){
    for (i in 1:length(pkg_install)) {
        install.packages(pkg_install[i])
    }
}

library(dplyr)
library(data.table) # manipulacao de dados
library(lubridate) # ajuste das datas
library(ipeadatar) # dados do ipca
library(reshape2) # transformacao da base de dados

#setwd('ED')

##########################################################
############## DADOS DO IPCA DE 1999 A 2019 ##############
##########################################################

IPCA <- ipeadata(code = 'PAN_IPCAG', language = 'br')
IPCA$value <- round(IPCA$value, digits = 2)
IPCA$date <- year(IPCA$date)
IPCA <- filter(IPCA, date >= 1999 & date <= 2019)

# AJUSTE IPCA
for (i in nrow(IPCA):1) {
  if (i == nrow(IPCA))
    IPCA$ajuste[i] <- 1
  else{
    IPCA$ajuste[i] <- IPCA$ajuste[i+1]/(1+IPCA$value[i+1]/100)
  }
}

IPCA <- IPCA[, c(2,3,6)]
names(IPCA) <- c('ANO', 'IPCA', 'IPCA_AJUSTADO')

##########################################################
############## Dados SNIS de 1999 a 2019 #################
##########################################################

# Coleta das informacoes a serem utilizadas
# SNIS - Serie Historica, Informacoes de indicadores agregados
## Filtros aplicados:
### Dados Gerais:
#### Ano de referencia - 2000 a 2019
#### Abrangencia - Regional
#### Tipo de servico - Agua e Esgoto
#### Natureza juridica - Todos
### Localizacao geografica:
#### Regiao - Todos (remover SANEMAT, COSAMA, DEPASA)
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


# Apos baixar os dados do SNIS, salva-lo no formato csv separado por ';'
dados <- read.table(file = 'dados_eficiencia_dinamica.csv',
                    header = TRUE,
                    dec = ',',
                    sep = ';',
                    na.strings = '',
                    encoding = 'UTF-8',
                    nrows = 527) #remove a ultima linha com os totais 

# alterando o nome das variaveis
names(dados) <- c('CO_MUNICIPIO', 'NM_MUNICIPIO', 'SG_UF', 'ANO', 'CO_PRESTADOR',
                  'NM_PRESTADOR', 'SG_PRESTADOR', 'ABRANGENCIA', 'TP_SERVICO',
                  'NATUREZA_JURIDICA', 'AG011', 'BL002', 'ES007', 'FN002', 'FN003',
                  'FN010', 'FN015', 'FN016', 'FN019','FN026')

# Removendo as emrpesas que nao participarao do calculo
dados <- dados[dados$SG_PRESTADOR != 'SANEMAT',]
dados <- dados[dados$SG_PRESTADOR != 'COSAMA',]
dados <- dados[dados$SG_PRESTADOR != 'DEPASA',]

# Transformando as variveis de texto para numerica
for (i in 11:20) {
  dados[, i] <- gsub(pattern = '\\.', '', dados[, i]) # remove o separador de milhar 
  dados[, i] <- gsub(pattern = ',', '\\.', dados[, i]) |> as.numeric() # transforma o separador decima de ',' para '.'
}

####### CORRIGINDO INFORMACOES DA SANESUL ##########
# O Ativo total da empresa nos anos de 2018 e 2019 precisam ser multiplicados por 1000

dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO %in% c(2018,2019), 'BL002'] <-
  1000 * dados[dados$SG_PRESTADOR == 'SANESUL' & dados$ANO %in% c(2018,2019), 'BL002']

# Transformando a variavel Ano em "Date"
dados$ANO <- as.Date(as.character(dados$ANO), format = "%Y") |>
  year()

# Junta os dados das empresas com os dados do IPCA
dados <- merge(dados, IPCA, by = 'ANO')
dados <- data.table(dados)


################################################################################
######################### Tratando os Dados ####################################
################################################################################

# Calculo da distancia interquartil das variaveis
dados[, `:=` (IQR_AG011 = quantile(AG011, 0.75, na.rm = T, type = 6) -
                quantile(AG011, 0.25, na.rm = T, type = 6),
              
              IQR_ES007 = quantile(ES007, 0.75, na.rm = T, type = 6) -
                quantile(ES007, 0.25, na.rm = T, type = 6),
              
              IQR_BL002 = quantile(BL002, 0.75, na.rm = T, type = 6) -
                quantile(BL002, 0.25, na.rm = T, type = 6),
              
              IQR_FN002 = quantile(FN002, 0.75, na.rm = T, type = 6) -
                quantile(FN002, 0.25, na.rm = T, type = 6),
              
              IQR_FN003 = quantile(FN003, 0.75, na.rm = T, type = 6) -
                quantile(FN003, 0.25, na.rm = T, type = 6),
              
              IQR_FN010 = quantile(FN010, 0.75, na.rm = T, type = 6) -
                quantile(FN010, 0.25, na.rm = T, type = 6),
              
              IQR_FN015 = quantile(FN015, 0.75, na.rm = T, type = 6) -
                quantile(FN015, 0.25, na.rm = T, type = 6),
              
              IQR_FN016 = quantile(FN016, 0.75, na.rm = T, type = 6) -
                quantile(FN016, 0.25, na.rm = T, type = 6),
              
              IQR_FN019 = quantile(FN019, 0.75, na.rm = T, type = 6) -
                quantile(FN019, 0.25, na.rm = T, type = 6),
              
              IQR_FN026 = quantile(FN026, 0.75, na.rm = T, type = 6) -
                quantile(FN026, 0.25, na.rm = T, type = 6)),
      by = SG_PRESTADOR]

# Calculo da media das variaveis
dados[, `:=` (MEDIA_AG011 = mean(AG011, na.rm = T),
              MEDIA_ES007 = mean(ES007, na.rm = T),
              MEDIA_BL002 = mean(BL002, na.rm = T),
              MEDIA_FN002 = mean(FN002, na.rm = T),
              MEDIA_FN003 = mean(FN003, na.rm = T),
              MEDIA_FN010 = mean(FN010, na.rm = T),
              MEDIA_FN015 = mean(FN015, na.rm = T),
              MEDIA_FN016 = mean(FN016, na.rm = T),
              MEDIA_FN019 = mean(FN019, na.rm = T),
              MEDIA_FN026 = mean(FN026, na.rm = T)),
      by = SG_PRESTADOR]

# Funcao para remover os outliers
# removendo valores fora dos limimites mean +(-) 1.5*distancia_interquartil

rmoutliers <- function(x, media, iqr){
  ifelse(is.na(x), yes = NA,
         no = ifelse(x < media-1.5*iqr, yes = NA,
                     no = ifelse(x > media+1.5*iqr, yes = NA,
                                 no = x)))
}

rmoutliers_0 <- function(x, media, iqr){
  ifelse(is.na(x), yes = 0,
         no = ifelse(x < media-1.5*iqr, yes = 0,
                     no = ifelse(x > media+1.5*iqr, yes = 0,
                                 no = x)))
}

# REMOVENDO OUTLIERS DA BL002
dados[, BL002_BOXPLOT := rmoutliers(BL002, MEDIA_BL002, IQR_BL002)]
dados[, IQR_BL002 := NULL][, MEDIA_BL002 := NULL]

# REMOVENDO OUTLIERS DA FN002
dados[, FN002_BOXPLOT := rmoutliers(FN002, MEDIA_FN002, IQR_FN002)]
dados[, IQR_FN002 := NULL][, MEDIA_FN002 := NULL]

# REMOVENDO OUTLIERS
dados[, FN003_BOXPLOT := rmoutliers(FN003, MEDIA_FN003, IQR_FN003)]
dados[, IQR_FN003 := NULL][, MEDIA_FN003 := NULL]

# REMOVENDO OUTLIERS DA FN026
dados[, FN026_BOXPLOT := rmoutliers(FN026, MEDIA_FN026, IQR_FN026)]
dados[, IQR_FN026 := NULL][, MEDIA_FN026 := NULL]

# REMOVENDO OUTLIERS DA FN010
dados[, FN010_BOXPLOT := rmoutliers(FN010, MEDIA_FN010, IQR_FN010)]
dados[, IQR_FN010 := NULL][, MEDIA_FN010 := NULL]

# REMOVENDO OUTLIERS DA FN015
dados[, FN015_BOXPLOT := rmoutliers(FN015, MEDIA_FN015, IQR_FN015)]
dados[, IQR_FN015 := NULL][, MEDIA_FN015 := NULL]

# REMOVENDO OUTLIERS DA FN016
dados[, FN016_BOXPLOT := rmoutliers(FN016, MEDIA_FN016, IQR_FN016)]
dados[, IQR_FN016 := NULL][, MEDIA_FN016 := NULL]

# REMOVENDO OUTLIERS DA FN019
dados[, FN019_BOXPLOT := rmoutliers(FN019, MEDIA_FN019, IQR_FN019)]
dados[, IQR_FN019 := NULL][, MEDIA_FN019 := NULL]

# REMOVENDO OUTLIERS DA AG011
dados[, AG011_BOXPLOT := rmoutliers(AG011, MEDIA_AG011, IQR_AG011)]
dados[, IQR_AG011 := NULL][, MEDIA_AG011 := NULL]

# REMOVENDO OUTLIERS DA ES007
dados[, ES007_BOXPLOT := rmoutliers(ES007, MEDIA_ES007, IQR_ES007)]
dados[, IQR_ES007 := NULL][, MEDIA_ES007 := NULL]

################################################################################
###################### AJUSTES DAS RECEITAS PELO IPCA ##########################
################################################################################

dados[, BL002_AJUSTADO := (BL002_BOXPLOT/IPCA_AJUSTADO)*1000]
dados[, FN019_AJUSTADO := FN019_BOXPLOT/IPCA_AJUSTADO]
dados[, FN016_AJUSTADO := FN016_BOXPLOT/IPCA_AJUSTADO]
dados[, DES_PESSOAL_PROP := FN010_BOXPLOT/IPCA_AJUSTADO] # DESPESAS COM PESSOAL PROPRIO
dados[, DEX := FN015_BOXPLOT/IPCA_AJUSTADO]
dados[, FN002_AJUSTADO := FN002_BOXPLOT/IPCA_AJUSTADO]
dados[, FN003_AJUSTADO := FN003_BOXPLOT/IPCA_AJUSTADO]

################################################################################
################################ INSUMOS #######################################
################################################################################

# Despesas por empregados (ajustado pelo IPCA)
dados[, DESPESA_POR_EMPREGADO := (FN010_BOXPLOT/FN026_BOXPLOT)/IPCA_AJUSTADO]

# CAPEX
dados[, CAPEX := (FN019_AJUSTADO+FN016_AJUSTADO)/BL002_AJUSTADO]

# OUTRAS DESPESAS OPERACIONAIS (DEX MENOS DESPESAS COM PESSOAL PROPRIO)
dados[, qOG := DEX - DES_PESSOAL_PROP]


################################################################################
################################ PRODUTOS ######################################
################################################################################

# Preco da agua
dados[, pAg := FN002_AJUSTADO/AG011_BOXPLOT]

# Preco do esgoto
dados[, pEg := FN003_AJUSTADO/ES007_BOXPLOT]


################################################################################
############################### PTF - INSUMOS ##################################
################################################################################

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
      PTF_input[t-1999] <- (aux[ANO == t, FN026_BOXPLOT]/aux[ANO == t-1, FN026_BOXPLOT])^
        (.5 * (
          (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_BOXPLOT]/
             (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_BOXPLOT] +
                aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_AJUSTADO] +
                aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]))
          + 
            (aux[ANO==t, DESPESA_POR_EMPREGADO]*aux[ANO==t, FN026_BOXPLOT]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_BOXPLOT] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_AJUSTADO] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO]))
        )
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
        (.5 * (
          (aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]/
             (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_BOXPLOT] +
                aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_AJUSTADO] +
                aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]))
          + 
            (aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_BOXPLOT] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_AJUSTADO] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO])
            )
        )
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
      PTF_input[t-1999] <- (aux[ANO == t, BL002_AJUSTADO]/aux[ANO == t-1, BL002_AJUSTADO])^
        (.5 * (
           (aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_AJUSTADO]/
               (aux[ANO==t-1, DESPESA_POR_EMPREGADO] * aux[ANO==t-1, FN026_BOXPLOT] +
                  aux[ANO==t-1, CAPEX] * aux[ANO==t-1, BL002_AJUSTADO] +
                  aux[ANO==t-1, qOG] * aux[ANO==t-1, IPCA_AJUSTADO]))
        +
          (aux[ANO==t, CAPEX] * aux[ANO==t, BL002_AJUSTADO]/
               (aux[ANO==t, DESPESA_POR_EMPREGADO] * aux[ANO==t, FN026_BOXPLOT] +
                  aux[ANO==t, CAPEX] * aux[ANO==t, BL002_AJUSTADO] +
                  aux[ANO==t, qOG] * aux[ANO==t, IPCA_AJUSTADO]))
        )
        )
    }
  }
  
  ptf_input_capex <- cbind(ptf_input_capex, PTF_input)
  names(ptf_input_capex)[i+1] <- prestador[i]
}

# TOTAL - INPUT

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

# PTF OUT - AGUA

ptf_output_agua <- data.frame(ANO = c(2000:2019))

PTF_output <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_output[t-1999] <- NA
    }
    else{
      PTF_output[t-1999] <- (aux[ANO == t, AG011_BOXPLOT]/aux[ANO == t-1, AG011_BOXPLOT])^
        (.5 * (
          (aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_BOXPLOT]/
             (aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_BOXPLOT] +
                aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_BOXPLOT]))
          +
            (aux[ANO==t, pAg] * aux[ANO == t, AG011_BOXPLOT]/
               (aux[ANO==t, pAg] * aux[ANO == t, AG011_BOXPLOT] +
                  aux[ANO==t, pEg] * aux[ANO == t, ES007_BOXPLOT]))
        )
        )
    }
  }
  
  ptf_output_agua <- cbind(ptf_output_agua, PTF_output)
  names(ptf_output_agua)[i+1] <- prestador[i]
}

# PTF OUT - ESGOTO

ptf_output_esgoto <- data.frame(ANO = c(2000:2019))

PTF_output <- vector()

for (i in 1:length(prestador)) {
  
  aux <- dados[SG_PRESTADOR == prestador[i]]
  
  for (t in 2000:2019) {
    if (!(t-1) %in% aux$ANO){
      PTF_output[t-1999] <- NA
    }
    else{
      PTF_output[t-1999] <- (aux[ANO == t, ES007_BOXPLOT]/aux[ANO == t-1, ES007_BOXPLOT])^
        (.5 * (
          (aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_BOXPLOT]/
             (aux[ANO==t-1, pAg] * aux[ANO == t-1, AG011_BOXPLOT] +
                aux[ANO==t-1, pEg] * aux[ANO == t-1, ES007_BOXPLOT]))
          +
            (aux[ANO==t, pEg] * aux[ANO == t, ES007_BOXPLOT]/
               (aux[ANO==t, pAg] * aux[ANO == t, AG011_BOXPLOT] +
                  aux[ANO==t, pEg] * aux[ANO == t, ES007_BOXPLOT]))
        )
        )
    }
  }
  
  ptf_output_esgoto <- cbind(ptf_output_esgoto, PTF_output)
  names(ptf_output_esgoto)[i+1] <- prestador[i]
}

# TOTAL - OUTPUT

PTF_OUTPUT <- ptf_output_agua*ptf_output_esgoto
PTF_OUTPUT <- PTF_OUTPUT[, c("ANO", sort(names(PTF_OUTPUT[,2:26])))]
PTF_OUTPUT$ANO <- 2000:2019


################################################################################
################################### PTF TOTAL ##################################
################################################################################

PTF <- PTF_OUTPUT/PTF_INPUT
PTF$ANO <- 2000:2019
# ordenando as empresas em ordem alfabetica
PTF <- data.frame(PTF[, c('ANO',sort(names(PTF[, 2:26])))])

for (i in 2:26) {
  PTF[,i] <- ifelse(is.infinite(PTF[,i]), NA, PTF[,i])
  PTF[,i] <- ifelse(is.nan(PTF[,i]), NA, PTF[,i])
}

empresas_remover <- c()
for (i in 1:length(prestador)) {
  if( sum(is.na(select(PTF, prestador[i]))) > 10)
    empresas_remover <- c(empresas_remover, print(prestador[i]))
}
PTF <- select(PTF, -empresas_remover)


PTF <- data.table(PTF)

# empresas que continuaram
col_ptf <- names(PTF)

########### Final ################

vol_faturado_ag <- dados[, .(ANO, SG_PRESTADOR, AG011_BOXPLOT)]
vol_faturado_ag <- dcast.data.table(vol_faturado_ag, ANO ~ SG_PRESTADOR, value.var = 'AG011_BOXPLOT')
vol_faturado_ag <- vol_faturado_ag[ANO != 1999]
vol_faturado_ag <- select(vol_faturado_ag, col_ptf)


vol_faturado_eg <- dados[, .(ANO, SG_PRESTADOR, ES007_BOXPLOT)]
vol_faturado_eg <- dcast.data.table(vol_faturado_eg, ANO ~ SG_PRESTADOR, value.var = 'ES007_BOXPLOT')
vol_faturado_eg <- vol_faturado_eg[ANO != 1999]
vol_faturado_eg <- select(vol_faturado_eg, col_ptf)

# CONFERINDO SE A COLUNAS (EMPRESAS) ESTAO NA MESMA ORDEM

names(vol_faturado_ag) == names(PTF)


# PONDERANDO PELOS VALORES FATURADOS
vol_faturado <- vol_faturado_ag + vol_faturado_eg
vol_faturado$ANO <- 2000:2019
# REMOVENDO O VALOR FATURADO DAS EMPRESAS QUE N?O FORAM CALCULADOS O PTF DO RESPECTIVO ANO
aux <- c(is.na(PTF))
vol_faturado <- vol_faturado*ifelse((aux), NA, 1)



numerador <- data.frame(PTF * vol_faturado)
numerador$ANO <- 2000:2019
for (i in 2:(length(PTF))) {
  numerador[,i] <- ifelse(is.infinite(numerador[,i]), NA, numerador[,i])
  numerador[,i] <- ifelse(is.nan(numerador[,i]), NA, numerador[,i])
}

# VARIACAO DA EFICIENCIA DINAMICA
mean(rowSums(numerador[,2:length(numerador)], na.rm = T)/rowSums(vol_faturado[,2:length(numerador)], na.rm = T))
