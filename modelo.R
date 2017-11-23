# Nesse ponto, os arquivos da base e dos fatores ja foram importados
# Isso pode ser feito facilmente no Rstudio, do lado direito, na opçao "Import Dataset"
# Os dados estao nas tabelas: base e Factors

# Criacao da coluna Rmt que estava faltando
# O valor correto deve ser subtituido no arquivo Factors.xlsx original
# Essa linha nao precisa ser executada depois disso
Factors$Rmt <- rnorm(4042)

# Renomear as colunas da base - colovar apenas o nome da açao
nomes <- NULL
for(i in 1:ncol(base)){
  nomes <- c(nomes, tail(strsplit(colnames(base),split="\n")[[i]],1))
}
colnames(base) <- nomes

# Criacao de uma lista com os modelos de regressao linear multipla
# Modelo: Rpt_Rft ~ a_pt + b_p*Rmt_Rft + s_p*SMB + h_p*HML
modelos <- NULL
for(i in 2:ncol(base)){
  Rpt_Rft <- base[[i]] - Factors$Rft
  Rmt_Rft <- Factors$Rmt - Factors$Rft
  mod <- lm(Rpt_Rft ~ Rmt_Rft + Factors$SMB + Factors$HML)
  modelos <- append(modelos, list(mod))
}

# Tabela com as constantes de cada um dos modelos
# Importante: para cada açao ha uma regressao que gera diferentes coeficientes
tabela.coef <- NULL
for(i in 1:length(modelos)){
  tabela.coef <- cbind(tabela.coef, modelos[[i]]$coefficients)
}
row.names(tabela.coef) <- c("a_pt","b_p","s_p","h_p")
colnames(tabela.coef) <- colnames(base)[-1]
write.csv(tabela.coef, "coloque aqui o seu diretorio + nome do arquivo", row.names = TRUE, col.names = TRUE)

# Tabela com os valores do ajustados pelo modelo
tabela.fit <- NULL
for(i in 1:length(modelos)){
  tabela.fit <- cbind(tabela.fit, predict(modelos[[i]]) + Factors$Rft)  # Previsao de Rpt de acordo com o modelo
}
row.names(tabela.fit) <- as.character(base[[1]])
colnames(tabela.fit) <- colnames(base)[-1]
write.csv(tabela.fit, "coloque aqui o seu diretorio + nome do arquivo", row.names = TRUE, col.names = TRUE)
