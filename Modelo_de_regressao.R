setwd("C:/Users/Gabriel/Desktop/Cursos/BigDataReAzure/cap11 - MachineLearning")
getwd()

### MACHINE LEARNING - REGRESSAO ###

# Problema de Negócio: Previsão de despesas hospitalares #

# Para esta análise, vamos usar um conjunto de dados simulando dispesas
# médicas hipotéticas para um conjunto de pacientes espalhados por 4 regioes do Brasil
# Esse dataset possui 1.338 observações e 7 variaveis


# Etapa 1 - Coletando os dados
despesas_hospitalares <- read.csv("despesas.csv")
View(despesas_hospitalares)


# Etapa 2 - Explorando e Preparando os Dados
# Visualizando as variáveis 
str(despesas_hospitalares)


# Medias de Têndencia Central da Varivel gastos
summary(despesas_hospitalares$gastos)

# Construindo um histograma
hist(despesas_hospitalares$gastos, main = "Grafico de Despesas",
     xlab = "Gastos", ylab = "Frequencia")

# Tabela de Contingência das Regioes
table(despesas_hospitalares$regiao)


# Explorando relacionamento entre as variaveis: Matriz de Correlação
cor(despesas_hospitalares[c("idade", "bmi", "filhos", "gastos")])


# Nenhuma das correlções na matriz e considerada forte, mas existem algumas associações
# interessantes. Por exemplo, a idade e o bmi (IMC) parecem ter uma correlação positiva fraca
# o que significa que, com o aumento da idade, a massa corporal tende a aumentar. Há também uma
# correlaçao positiva moderada entre a idade e os gastos, além do número de filhos e os gastos.
# Estas associações implicam que, a medida que a idade, massa corporal e número de filhos aumenta
# o custo esperado do seguro saúde sobe.


# Visualizando relacionamento entre as variáveis: ScatterPlot
pairs(despesas_hospitalares[c("idade", "bmi", "filhos", "gastos")])


# ScatterPlot Matrix
install.packages("psych")
library(psych)

# Este gráfico fornece um pouco mais de informações sobre o relacionamento entre as variáveis
pairs.panels(despesas_hospitalares[c("idade", "bmi", "filhos", "gastos")])


# Etapa 3: Treinando o Modelo
modelo <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao, data = despesas_hospitalares)

# Similar ao item anterior
modelo <- lm(gastos ~ ., data = despesas_hospitalares)

# Visualizando os coeficientes
modelo

# Prevendo despesas medicas
?predict

# Aqui verificamos os gastos previstos pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)
View(despesas_hospitalares)


# Prevendo os gastos com Dados de teste
despesas_teste <- read.csv("despesas-teste.csv")
View(despesas_teste)
previsao2 <- predict(modelo, despesas_teste)
despesas_teste$gastos <- previsao2
View(despesas_teste)


# Etapa 4 - Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modelo)



# ******************************************************
# **** ESTAS INFORMAÇÕES ABAIXO É QUE FARÃO DE VOCÊ ****
# **** UM VERDADEIRO CONHECEDOR DE MACHINE LEARNING ****
# ******************************************************


# Equação da Regressão
# y = a + bx (simples)
# y = a + b0x0 + b1x1 + b2x2... bnxn (multipla) 

# Resíduos 
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribução normal, o que indica
# que a média entre os valores previstos e os observados é próximo de 0 (o que é muito bom)

# Coeficiente - Intercept - a (alfa)
# valor de a na equaçao de regressao linear

# Coeficientes - Nomes das Variáveis - b (beta)
# Valor de b na equação de regressão 

# OBS: A questão é que lm() ou summary() têm diferentes convenções de
# rotulagem para cada variável explicativa.
# Em vez de escrever slope_1, slope_2 ....
# Simplesmente usam o nome das variáveis em qualquer saida
# indicar quais coeficientes pertencem a qual variável

# Erro Padrão
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal é que este valor
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer

# Asteriscos
# Os arteriscos representam os níveis de significância de acordo com o p-value
# quanto mais estrelas maior o nível de significância.
# Atenção --> Muitos asteriscos indicam que é improvável que não exista relação
# entre as variáveis

# Valor T
# Define se o coeficiente da variável é significativo ou não para o modelo.
# Ele é usado para calcular o p-value e os níveis de significância

# Valor P
# O p-value representa a probabilidade que a variável não seja relevante,
# Deve ser o menor valor possível
# Se este valor for realmente pequeno, o R irá mostrar o valor 
# como notação cientifica

# Significância 
# São aquelas legendas próximas a suas variáveis
# Espaço em Branco - RUIM
# Pontos - RAZOÁVEL
# Asteriscos - BOM
# Muitos Asteriscos - MUITO BOM

# Residual Standard Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento 
# e o número de variáveis no seu modelo (Observações - Variaveis)

# R-squared (R^2) (coeficiente de determinação)
# Ajuda avaliar o nível de precisão do nosso modelo. Quanto maior, 
# melhor, sendo 1 o valor ideal. 

# F-statistics
# É o teste F do modelo. Esse teste obtém os parametros do nosso modelo
# e compara com um modelo que tenham mais parâmetros
# Em teoria, um modelo com mais parametros tem um desempenho melhor.

# Se o seu modelo com mais parâmetros NÃO tiver um desempenho melhor
# que um modelo com menos parâmetros o valor do p-value será bem alto

# Se o modelo com mais parâmetros tiver performance melhor que um modelo
# com menos parâmetros, o valor do p-value será mais baixo

# LEMBRE-SE: Correlação não implica causalidade



# Etapa 5: Otimizando a Performance do Modelo

# Adicionando uma variável com o dobro do valor das idades
despesas_hospitalares$idade2 <- despesas_hospitalares$idade^2

# Adicionando um indicador para BMI >= 30
despesas_hospitalares$bmi30 <- ifelse(despesas_hospitalares$bmi >= 30, 1, 0)

View(despesas_hospitalares)


# Criando o modelo otimizado
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                  bmi30 * fumante + regiao, data = despesas_hospitalares)

summary(modelo_v2)


# Dados para teste
despesas_teste2 <- read.csv("despesas-teste.csv")
View(despesas_teste2)

previsao3 <- predict(modelo, despesas_teste2)
despesas_teste2$gastos_previstos <- previsao3
View(despesas_teste2)
