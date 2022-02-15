# Fator X - Adasa

O Fator X é o mecanismo regulatório que permite o compartilhamento dos ganhos de produtividade com os usuários.

# Objetivo

O objetivo deste código é dispor sobre a metodologia de apuração do Fator X nos processos de Revisão Tarifária Periódica – RTP da Concessionária dos serviços públicos de abastecimento de água e esgotamento sanitário do Distrito Federal.

# Metodologia

O Fator X é composto por três parcelas distintas, conforme formula a seguir
$$ Fator X = Fator X_O + Fator X_Q + Fator X_H$$ 
sendo
* $Fator X_O$: Fator de eficiência operacional;
* $Fator X_Q$: Fator de Qualidade;
* $Fator X_H$: Fator de Eficiência Hídrica

**Disclaimer**
Este projeto se limita apenas ao cálculo do Fator de Eficiência Operacional 

## Fator de Eficiência Operacional

É calculado por meio da aplicação de técnicas de benchmarking, e compreende duas dimensões: eficiência estática (catch-up fator) e eficiência dinâmica (frontier shift), conforme formula a seguir
$$ Fator X_O = \Delta_{EE} + \Delta_{ED} $$
sendo
$\Delta_{EE}:$ Variação da eficiência estática
$\Delta_{ED}:$ Variação da eficiência dinâmica

### Variação da eficiência estática

A variação da eficiência estática é calculada pelo método de Análise Envoltória de Dados (DEA), que calcula um escore de eficiência usado para comparar a combinação de vários insumos (inputs) e produtos (outputs) de cada empresa, com as combinações das empresas mais eficientes da amostra.

Para o cáculo do DEA será utilizdo o modelo VRS (*variable returns to scale*) orientado aos inputs. 

O número de prestadores de serviços utilizados no benchmarking deverá ser, idealmente, pelo menos o triplo do número de variáveis consideradas como insumos e produtos.

Para o cálculo da eficiência estática devem ser consideradas as seguintes variáveis:
|        | Variável                                                                              | unidade | SNIS                                                                                                                |
|--------|---------------------------------------------------------------------------------------|---------|---------------------------------------------------------------------------------------------------------------------|
| Input  | Quantidade de Pessoal Próprio                                                         | nº      | Quantidade total de empregados próprios (FN026)                                                                     |
| Input  | Custos Operacionais, excluídas as despesas com pessoal próprio e com energia elétrica | R$      | Despesas de Exploração - DEX (FN015);  Despesas com Pessoal Próprio (FN010);  Despesas com energia elétrica (FN013) |
| Output | Volume consumido de Água                                                              | m³      | Volume de água consumida (AG010)                                                                                    |
| Output | Volume Tratado de Esgoto                                                              | m³      | Volume de Esgoto Tratado (ES006)                                                                                    |

Serão utilizadas no benchmarking todas as companhias estaduais prestadoras de serviços de abastecimento de água e esgotamento sanitário do Brasil.

**Fonte de informações** é o Sistema Nacional de Informações sobre Saneamento (SNIS)

Para o cálculo serão utilizadas as informações dos **três últimos anos** com dados disponíveis.

Caso não haja variação considerada significativa nos valores, será utilizado o valor do último ano disponível.

Caso haja variação considerada significativa nos valores, será utilizado a média dos 3 últimos anos.

Os resultados obtidos das eficiências estáticas das empresas da amostra serão classificados em ordem crescente e posteriormente divididos em quartis, sendo o primeiro das menos eficientes e o último, das mais eficientes.

### Variação da eficiência dinâmica

