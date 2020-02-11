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