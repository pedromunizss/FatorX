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

Os resultados obtidos das eficiências estáticas das empresas da amostra serão classificados em ordem crescente e posteriormente divididos em quartis, sendo o primeiro das menos eficientes e o último, das mais eficientes, coforme tabela a seguir

| Grupo                    | $\Delta_{EE}$ |
|--------------------------|---------------|
| [mín, 1º Quartil)        | 2,0%          |
| [1º Quatil, 2º Quartil)  | 1,5%          |
| [2º Quartil, 3º Quartil) | 1,0%          |
| [3º Quartil, máx]        | 0,5%          |

### Variação da eficiência dinâmica

A variação da eficiência dinâmica (Δ𝐸𝐷) será calculada pelo cálculo da Produtividade Total dos Fatores (PTF), por meio da aplicação do índice de Tornqvist.

A Produtividade Total dos Fatores de uma empresa 𝑘 em um ano 𝑡 (𝑃𝑇𝐹𝑘𝑡) é obtida por meio da seguinte fórmula:
𝑃𝑇𝐹𝑘𝑡 = 𝑃𝑇𝐹𝑘𝑡 𝑑𝑜𝑠 𝑜𝑢𝑡𝑝𝑢𝑡𝑠 / 𝑃𝑇𝐹𝑘𝑡 𝑑𝑜𝑠 𝑖𝑛𝑝𝑢𝑡𝑠

A Variação da Eficiência Dinâmica (Δ𝐸𝐷𝑡), para cada ano 𝑡, é obtida por meio da média do PTF de todas as prestadoras, ponderada pelos seus respectivos volumes faturados totais, somando água e esgoto, isto é:

Δ𝐸𝐷𝑡 = Σ(𝑃𝑇𝐹𝑘𝑡×𝑉𝐹𝑘𝑡) / Σ𝑉𝐹𝑘𝑡

A Variação da Eficiência Dinâmica (Δ𝐸𝐷) será obtida por meio da média aritmética dos valores da Variação da Eficiência Dinâmica (Δ𝐸𝐷𝑡), obtida em cada ano 𝑡, menos um, isto é:

Δ𝐸𝐷=(ΣΔ𝐸𝐷 / 20) − 1

Para o cálculo da eficiência dinâmica devem ser utilizadas as seguintes variáveis:
