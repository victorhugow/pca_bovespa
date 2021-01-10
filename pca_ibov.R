#Introduçao a Teoria do Aprendizado Estatístico
#Trabalho final
#Autor: Victor Hugo


library(tidyverse) #Organizar daodos, gráficos etc
library(quantmod) #Baixar dados do Yahoo Finance
library(FactoMineR) # pca 
library(factoextra) # gráficos
library(rvest) #webscraping
library(stargazer) #tabelas


#Função para calcular os retornos
calret=function(y){
  return(diff(log(y)))
}

#Dados do indice iBovespa (BVSP) que queremos replicar via PCA -----

ibov <- getSymbols(Symbols = "^BVSP", start_date="2010-01-01", index.class = "POSIXct")

ibov <- fortify(BVSP)

ibov <- ibov %>% dplyr::select(Index, BVSP.Close)

names(ibov) <- c("data", "ibov")

ibov_ret <- calret(ibov$ibov)

ibov_ret <- cbind(as.character(ibov$data[-c(1)]), ibov_ret) %>% as.data.frame()

names(ibov_ret) <- c("data", "ibov")

ibov_ret <- apply(ibov_ret, 2, function(x) ifelse(is.na(x), 0, x)) %>% as.data.frame()

ibov_ret$data <- as.character(ibov_ret$data)
ibov_ret$data <- as.Date(ibov_ret$data)
ibov_ret$ibov  <- ibov_ret$ibov %>% as.character() %>% as.numeric()

ibov <- ibov_ret 

ibov <- ibov %>% filter(data >= '2015-01-01' & data <='2020-12-30')

pdf('output/retornos_indice.pdf')
ggplot(ibov) +
  geom_line(aes(x=as.Date(data), y=ibov), color="#264653") +
  theme_light() +
  labs(x="Data", y="Retorno")
dev.off()

pdf('output/retornos_acu_indice.pdf')
ibov %>% mutate(ret_acu= cumprod(1+ibov)) %>%
  ggplot() +
  geom_line(aes(x=as.Date(data), y=ret_acu), color="#2a9d8f") +
  theme_light() +
  labs(x="Data", y="Retorno acumulado")
dev.off()

#Webscrapping dos tickers das ações que compõe o indice e baixando os retornos via yahoo finance ------
wiki_ibov <- read_html("https://en.wikipedia.org/wiki/List_of_companies_listed_on_B3")

tabela_ibov <- wiki_ibov %>% html_table()
tabela_ibov <- do.call(rbind, tabela_ibov)

tickers <- tabela_ibov$Ticker
tickers <- paste0(tickers, ".SA")
tickers[34] <- "PCAR3.SA" #Ação preferwencial nao baixa...


#Pegando retornos das açoes do ibov
stocks = lapply(tickers, function(sym) {
  
  dailyReturn(na.omit(getSymbols(sym, start_date="2010-01-01", auto.assign=FALSE)))
  
})

ibov_stocks <- do.call(merge, stocks)
ibov_stocks <- fortify(ibov_stocks)

names(ibov_stocks)[2:74] <- tickers

colnames(ibov_stocks)[1] <- "data"

ibov_stocks$data <- as.Date(ibov_stocks$data)

ibov_stocks <- ibov_stocks %>% filter(data >= '2015-01-01' & data <='2020-12-30')

pdf("output/ret_stocks.pdf")
pivot_longer(ibov_stocks, cols = 2:dim(ibov_stocks)[2]) %>%
  ggplot() +
  geom_line(aes(x=data, y=value, color=name), show.legend = F, alpha=.5) +
  theme_light() +
  labs(x="Data", y="Retorno") +
  ylim(-1,1)
dev.off()

pdf("output/ret_acu_stocks.pdf")
pivot_longer(ibov_stocks, cols = 2:dim(ibov_stocks)[2]) %>% group_by(name) %>% mutate(ret_acu=cumprod(1+value)) %>%
  ggplot() +
  geom_line(aes(x=data, y=ret_acu, color=name), show.legend = F, alpha=.5) +
  theme_light() +
  labs(x="Data", y="Retorno acumulado") +
  ylim(0,10)
dev.off()

#PCA-------
ibov_stocks_pca <- ibov_stocks[,c(-1)] #Deixando apenas as ações, tirando data

ibov_stocks_pca <- apply(ibov_stocks_pca, 2, function(x) ifelse(is.na(x), 0, x) %>% scale()) #Missings

#Computo do PCA
pca <- prcomp(ibov_stocks_pca)


pdf("output/explained_var.pdf")
fviz_eig(pca, addlabels=TRUE, hjust = -0.3, barfill="#e9c46a", barcolor = "#e9c46a") +
  ylim(0, 40) +
  scale_color_manual("", values = c("#e9c46a")) +
  labs(title = "", y="Variação explicada", x="Dimensões")
dev.off()

pdf("output/eigenvalues.pdf")
fviz_eig(pca, choice = "eigenvalue", 
         addlabels=F) +
  labs(title = "", y="Autovalores", x="Dimensões")
dev.off()

#Guardando os PCs
componentes <- pca$rotation

PC1 <- componentes[,1]

#Calculando os pesos
pesos = abs(PC1)/sum(abs(PC1))
sum(pesos) #soma tem q dar 1

#Multiplicando os pesos pelas ações wR'
ibov_stocks_2 <- apply(ibov_stocks[,c(-1)], 2, function(x) ifelse(is.na(x), 0, x))
ret_ibov_pca <- pesos %*% t(ibov_stocks_2)

#Arrumando os dados para comparaçao
ret_ibov_pca <- cbind(as.character(ibov_stocks$data), as.character(ret_ibov_pca)) %>% as.data.frame()

names(ret_ibov_pca) <- c("data", "ret_ibov_pca")

ret_ibov_pca$data <- as.character(ret_ibov_pca$data)
ibov$data <- as.character(ibov$data)

ibovespa_vs_pca <- merge(ret_ibov_pca, ibov, by=c("data"))

ibovespa_vs_pca$ibov <- ibovespa_vs_pca$ibov %>% as.character() %>% as.numeric()
ibovespa_vs_pca$ret_ibov_pca <- ibovespa_vs_pca$ret_ibov_pca %>% as.character() %>% as.numeric()
ibovespa_vs_pca$data <- as.Date(ibovespa_vs_pca$data)

ibovespa_vs_pca <- ibovespa_vs_pca %>% mutate(cr_ibovespa=cumprod(1+ibov),
                                              cr_pca_ibov=cumprod(1+ret_ibov_pca))
pdf("output/ibov_vs_pca.pdf")
ggplot(ibovespa_vs_pca) +
  geom_line(aes(y=cr_pca_ibov, x=data, colour="Retorno acum. do PC1"), size=1) +
  geom_line(aes(y=cr_ibovespa, x=data, colour="Retorno acum. do índice"), size=1) +
  theme_light() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank()) +
  labs(x="", y="Retorno acumulado")
dev.off()

#retorno da carteira pca
pdf("output/ret_carteira_pca.pdf")  
ggplot(ret_ibov_pca) +
  geom_line(aes(x=as.Date(data), y=as.numeric(as.character(ret_ibov_pca))), color="#2a9d8f",size=.5) +
  theme_light() +
  labs(x="Data", y="Retorno acumulado")
dev.off()  



#Tabelas ------
m_cov <- cov(ibov_stocks_pca)
stargazer(m_cov[1:4, 1:4])


tickers_table <- cbind(colnames(ibov_stocks_pca)[1:20],
                       colnames(ibov_stocks_pca)[21:40],
                       colnames(ibov_stocks_pca)[41:60],
                       c(colnames(ibov_stocks_pca)[61:73], rep(NA, 20-length(colnames(ibov_stocks_pca)[61:73]))))

stargazer(tickers_table)


#Construindo um portfolio winner com top10 ações ---------
wining_portfolio <- sort(PC1,decreasing = T)[1:10]  %>% as.data.frame()
wining_portfolio <- wining_portfolio %>% fortify()
wining_portfolio$pesos <- abs(wining_portfolio$.)/sum(abs(wining_portfolio$.))



wining_stocks <- ibov_stocks[,c('BBDC3.SA', 
                                'BBDC4.SA' ,
                                'BBAS3.SA' ,
                                'ITSA4.SA' ,
                                'ITUB4.SA' ,
                                'CYRE3.SA' ,
                                'B3SA3.SA' ,
                                'SANB11.SA',
                                'MULT3.SA',
                                'BRML3.SA')]

wining_portfolio <- wining_portfolio$pesos %*% t(wining_stocks) %>% t()

wining_portfolio_ret <- cbind(wining_stocks$data,wining_portfolio) %>% as.data.frame()
wining_portfolio_ret$cum_ret_win <- cumprod(1+wining_portfolio_ret$V1)


wining_portfolio_with_ibov <- cbind(ibovespa_vs_pca, wining_portfolio_ret)


pdf("output/wining_port.pdf")  
ggplot(wining_portfolio_with_ibov) +
  geom_line(aes(y=cr_ibovespa, x=data, colour="Retorno acum. do índice"), size=1) +
  geom_line(aes(y=cum_ret_win, x=data, colour="Retorno acum. de portfolio (top 10 ações)"), size=1) +
  theme_light() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank()) +
  labs(x="", y="Retorno acumulado")
dev.off()

#Construindo um portfolio loser com top10 ações ---------
losing_portfolio <- sort(PC1)[1:10]  %>% as.data.frame()
losing_portfolio$pesos <- abs(losing_portfolio$.)/sum(abs(losing_portfolio$.))

losing_stocks <- ibov_stocks[,c('PCAR3.SA', 
                                'SUZB3.SA' ,
                                'HGTX3.SA' ,
                                'KLBN11.SA',
                                'HAPV3.SA' ,
                                'BEEF3.SA' ,
                                'IRBR3.SA' ,
                                'JBSS3.SA' ,
                                'TOTS3.SA' ,
                                'CRFB3.SA' 
)]


losing_stocks <- apply(losing_stocks, 2, function(x) ifelse(is.na(x), 0, x))

losing_portfolio <- losing_portfolio$pesos %*% t(losing_stocks) %>% t() %>% as.data.frame()

losing_portfolio$cum_ret_los <- cumprod(1+losing_portfolio$V1)

losing_portfolio_with_ibov <- cbind(ibovespa_vs_pca, losing_portfolio)

ggplot(losing_portfolio_with_ibov) +
  geom_line(aes(y=cr_ibovespa, x=data, colour="Retorno acum. do índice"), size=1) +
  geom_line(aes(y=cum_ret_los, x=data, colour="Retorno acum. de portfolio (top 10 piores)"), size=1) +
  theme_light() +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank()) +
  labs(x="", y="Retorno acumulado")

