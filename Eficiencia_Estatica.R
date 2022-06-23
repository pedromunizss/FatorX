# verifica e instala os pacotes necessários
pkg <- c('dplyr', 'Benchmarking')

pkg_install <- pkg[!(pkg %in% installed.packages())]
if(length(pkg_install)){
    for (i in 1:length(pkg_install)) {
        install.packages(pkg_install[i])
    }
}

library(Benchmarking) #biblioteca com a funcao dea
library(dplyr)

#### Dados SNIS ####

# Coleta das informacoes a serem utilizadas
# SNIS - Serie Historica, Informacoes de indicadores agregados
## Filtros aplicados:
### Dados Gerais:
#### Ano de referencia - 2017, 2018 e 2019
#### Abrangencia - Regional
#### Tipo de servico - Agua e Esgoto
#### Natureza juridica - Todos
### Localizacao geografica:
#### Regiao - Todos
#### Estado - Todos
#### Prestadores - Todos (ate o momento, 15/10/21, sao 26 prestadores)

## Tabelas utilizadas e colunas selecionadas
### Informacoes Finaceiras:
#### FN010 - Despesa com pessoal proprio
#### FN013 - Despesa com energia eletrica
#### FN015 - Despesas de explaracao (Dex)
#### FN026 - Quantidade total de empregados proprios
### Informacoes de Agua:
#### AG010 - Volume de agua consumido
### Informacoes de Esgoto:
#### ES006 - Volume de esgoto tratado

dados <- read.csv2(file = 'Agregado-20211015141947.csv', nrows = 78, encoding = 'UTF-8')

# Preparacao dos dados
dados <- dados[, c(2, 3, 4, 6, 7, 11:16)]
names(dados) <- c('NM_MUNICIPIO', 'CO_UF', 'ANO', 'NM_PRESTADOR', 'SG_PRESTADOR',
                  'AG010', 'ES006', 'FN010', 'FN013', 'FN015', 'FN026')
dados$AG010 <- gsub(pattern = '\\.', '', dados$AG010)
dados$AG010 <- gsub(pattern = ',', '\\.', dados$AG010) |>
  as.numeric()  

dados$ES006 <- gsub(pattern = '\\.', '', dados$ES006)
dados$ES006 <- gsub(pattern = ',', '\\.', dados$ES006) |>
  as.numeric()  

dados$FN010 <- gsub(pattern = '\\.', '', dados$FN010)
dados$FN010 <- gsub(pattern = ',', '\\.', dados$FN010) |>
  as.numeric()

dados$FN013 <- gsub(pattern = '\\.', '', dados$FN013)
dados$FN013 <- gsub(pattern = ',', '\\.', dados$FN013) |>
  as.numeric()  

dados$FN015 <- gsub(pattern = '\\.', '', dados$FN015)
dados$FN015 <- gsub(pattern = ',', '\\.', dados$FN015) |>
  as.numeric()  

dados$FN026 <- gsub(pattern = '\\.', '', dados$FN026)
dados$FN026 <- gsub(pattern = ',', '\\.', dados$FN026) |>
  as.numeric()  

# cria a variavel Custo Operacional
dados$CUSTO_OP <- dados$FN015 - dados$FN013 - dados$FN010


# Calculo DEA
inputs_2017 <- dados[dados$ANO == '2017', c('FN026','CUSTO_OP')] #seleciona as colunas com os inputs
outputs_2017 <- dados[dados$ANO == '2017', c('AG010','ES006')] #seleciona as colunas com os outputs
dea_2017 <- dea(X = inputs_2017, Y = outputs_2017, RTS = 'vrs', ORIENTATION = 'in')

inputs_2018 <- dados[dados$ANO == '2018', c('FN026','CUSTO_OP')] #seleciona as colunas com os inputs
outputs_2018 <- dados[dados$ANO == '2018', c('AG010','ES006')] #seleciona as colunas com os outputs
dea_2018 <- dea(X = inputs_2018, Y = outputs_2018, RTS = 'vrs', ORIENTATION = 'in')

inputs_2019 <- dados[dados$ANO == '2019', c('FN026','CUSTO_OP')] #seleciona as colunas com os inputs
outputs_2019 <- dados[dados$ANO == '2019', c('AG010','ES006')] #seleciona as colunas com os outputs
dea_2019 <- dea(X = inputs_2019, Y = outputs_2019, RTS = 'vrs', ORIENTATION = 'in')

eficiencia <- c(dea_2017$eff, dea_2018$eff, dea_2019$eff)
eficiencia <- data.frame(eficiencia)
dados <- merge(dados, eficiencia, by = 'row.names')

eficiencia_2017 <- dados[dados$ANO == '2017', c('SG_PRESTADOR', 'eficiencia')]
eficiencia_2017[order(eficiencia_2017$eficiencia, decreasing = T),]

final <- dados %>%
  group_by(SG_PRESTADOR) %>%
  summarise(eficiencia_media = mean(eficiencia))
