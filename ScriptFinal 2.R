################################################################################
##                   INDICADORES PAISES SELECIONADOS                          ##
################################################################################

#INSTALAR BIBLIOTECAS

install.packages("tidyverse")
install.packages("plotly")
install.packages("gapminder")
install.packages("readxl")

# BAIXANDO AS BIBLIOTECAS
library(tidyverse)
library(plotly)
library(gapminder)
library(readxl)

dados <- read_xlsx("banco final.xlsx")
names(dados)
dim(dados)
head(dados)

glimpse(dados)

# Tranformando as variaveis pais como fator, desnut1ano e desnu5anos como numerica

dados$pais<-as.factor(dados$pais)
dados$desnut1ano<-as.numeric(dados$desnut1ano)
dados$desnut5anos<-as.numeric(dados$desnut5anos)

glimpse(dados)
summary(dados)

# Calculo das medidas de tendecia central
mean(dados$tmi)
median(dados$tmi)

mean(dados$neonatal)
median(dados$neonatal)

mean(dados$baixop, na.rm = TRUE)
median (dados$baixop, na.rm = TRUE)

mean (dados$desnut1ano, na.rm = TRUE)
median (dados$desnut1ano, na.rm = TRUE)

mean(dados$desnut5anos, na.rm = TRUE)
median(dados$desnut5anos, na.rm = TRUE)

mean(dados$infresp, na.rm = TRUE)
median(dados$infresp, na.rm = TRUE)

mean(dados$asfixia, na.rm = TRUE)
median(dados$asfixia, na.rm = TRUE)

mean(dados$prematuridade, na.rm = TRUE)
median(dados$prematuridade, na.rm = TRUE)

# Calculo das medidas de dispersao

# Desvio-padrao

sd(dados$tmi)
sd (dados$neonatal)
sd (dados$baixop, na.rm = TRUE)
sd (dados$desnut1ano, na.rm = TRUE)
sd (dados$desnut5anos, na.rm = TRUE)
sd (dados$infresp, na.rm = TRUE)
sd (dados$asfixia, na.rm = TRUE)
sd (dados$prematuridade, na.rm = TRUE)

# Quartil

quantile(dados$tmi, probs = 0.25, na.rm = TRUE)
quantile(dados$neonatal, probs = 0.25, na.rm = TRUE)
quantile(dados$baixop, probs = 0.25, na.rm = TRUE)
quantile(dados$desnut1ano, probs = 0.25, na.rm = TRUE)
quantile(dados$desnut5anos, probs = 0.25, na.rm = TRUE)
quantile(dados$infresp, probs = 0.25, na.rm = TRUE)
quantile(dados$asfixia, probs = 0.25, na.rm = TRUE)
quantile(dados$prematuridade, probs = 0.25, na.rm = TRUE)

quantile(dados$tmi, probs = 0.75, na.rm = TRUE)
quantile(dados$neonatal, probs = 0.75, na.rm = TRUE)
quantile(dados$baixop, probs = 0.75, na.rm = TRUE)
quantile(dados$desnut1ano, probs = 0.75, na.rm = TRUE)
quantile(dados$desnut5anos, probs = 0.75, na.rm = TRUE)
quantile(dados$infresp, probs = 0.75, na.rm = TRUE)
quantile(dados$asfixia, probs = 0.75, na.rm = TRUE)
quantile(dados$prematuridade, probs = 0.75, na.rm = TRUE)

quantile(dados$tmi, probs = 0.20, na.rm = TRUE)
quantile(dados$neonatal, probs = 0.20, na.rm = TRUE)
quantile(dados$baixop, probs = 0.20, na.rm = TRUE)
quantile(dados$desnut1ano, probs = 0.20, na.rm = TRUE)
quantile(dados$desnut5anos, probs = 0.20, na.rm = TRUE)
quantile(dados$infresp, probs = 0.20, na.rm = TRUE)
quantile(dados$asfixia, probs = 0.20, na.rm = TRUE)
quantile(dados$prematuridade, probs = 0.20, na.rm = TRUE)

quantile(dados$tmi, probs = 0.80, na.rm = TRUE)
quantile(dados$neonatal, probs = 0.80, na.rm = TRUE)
quantile(dados$baixop, probs = 0.80, na.rm = TRUE)
quantile(dados$desnut1ano, probs = 0.80, na.rm = TRUE)
quantile(dados$desnut5anos, probs = 0.80, na.rm = TRUE)
quantile(dados$infresp, probs = 0.80, na.rm = TRUE)
quantile(dados$asfixia, probs = 0.80, na.rm = TRUE)
quantile(dados$prematuridade, probs = 0.80, na.rm = TRUE)

# Funcao para calcular a moda

calcular_moda <- function(vetor) {
  valores_unicos <- unique(vetor)              # Valores unicos
  frequencias <- tabulate(match(vetor, valores_unicos))  # Frequencias
  moda <- valores_unicos[frequencias == max(frequencias)] # Moda
  return(moda)
}

moda_tmi <- calcular_moda(na.omit(dados$tmi))
moda_tmi

moda_neonatal <- calcular_moda(na.omit(dados$neonatal))
moda_neonatal

# Construcao dos graficos

# Grafico de evolucao da TMI (2010, 2015, 2021) (GRAFICO 1)

dados_tmi <- dados %>%
  filter(ano %in% c(2010, 2015, 2021)) %>%
  select(pais, ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

grafico_tmi <- plot_ly(dados_tmi, 
                       x = ~ano, 
                       y = ~tmi, 
                       color = ~pais, 
                       type = 'scatter', 
                       mode = 'lines+markers',
                       text = ~paste("Pais:", pais, "Ano:", ano, "TMI:", tmi),
                       line = list(width = 6),
                       marker = list(size = 10)) %>%  # Tamanho da bolinha
  layout(
    title = list(
      text = "Taxa de Mortalidade Infantil (TMI): 2010, 2015 e 2021",
      font = list(size = 14)
    ),
    xaxis = list(
      title = list(text = "Ano", font = list(size = 14)),
      tickfont = list(size = 14)
    ),
    yaxis = list(
      title = list(text = "TMI (por 1000 nascimentos)", font = list(size = 14)),
      tickfont = list(size = 14)
    ),
    legend = list(
      title = list(text = "Paises", font = list(size = 14)),
      font = list(size = 14)
    )
  )

grafico_tmi

grafico_interativo_tmi <- ggplotly(grafico_tmi, tooltip = "text")
grafico_interativo_tmi

# Grafico Barras TMI 2010 (GRAFICO 2)

dados_2010 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, tmi) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Infantil
  mutate(tmi = as.numeric(tmi)) # Converter TMI para numerico 


grafico14 <- ggplot(dados_2010, aes(x = reorder(pais, tmi), y = tmi, text = paste("Pais:", pais, "TMI:", tmi))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Infantil (TMI) em 2010",
       x = "pais",
       y = "TMI (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico14

grafico_int_tmi_2010 <- ggplotly(grafico14, tooltip = "text")
grafico_int_tmi_2010

# Grafico Barras TMI 2015 (GRAFICO 3)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, tmi) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Infantil
  mutate(tmi = as.numeric(tmi)) # Converter TMI para numerico 

grafico15 <- ggplot(dados_2015, aes(x = reorder(pais, tmi), y = tmi, text = paste("Pais:", pais, "TMI:", tmi))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Infantil (TMI) em 2015",
       x = "pais",
       y = "TMI (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico15

grafico_int_tmi_2015 <- ggplotly(grafico15, tooltip = "text")
grafico_int_tmi_2015

# Grafico Barras TMI 2021 (GRAFICO 4)

dados_2021 <- banco_final %>%
  filter(ano == 2021) %>%
  select(pais, tmi) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Infantil
  mutate(tmi = as.numeric(tmi)) # Converter TMI para numerico 

grafico24 <- ggplot(dados_2021, aes(x = reorder(pais, tmi), y = tmi, text = paste("Pais:", pais, "TMI:", tmi))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Infantil (TMI) em 2021",
       x = "pais",
       y = "TMI (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico24

grafico_int_tmi_2021 <- ggplotly(grafico24, tooltip = "text")
grafico_int_tmi_2021

# Grafico de evolucao da TMN (2010, 2015, 2021) (GRAFICO 5)

dados_neonatal <- dados %>%
  filter(ano %in% c(2010, 2015, 2021)) %>%
  select(pais, ano, neonatal) %>%
  mutate(ano = as.numeric(ano),
         neonatal = as.numeric(neonatal))

# Criar o grafico
grafico_neonatal <- plot_ly(dados_neonatal, 
                            x = ~ano, 
                            y = ~neonatal, 
                            color = ~pais, 
                            type = 'scatter', 
                            mode = 'lines+markers',
                            text = ~paste("Pais:", pais, "Ano:", ano, "Neonatal:", neonatal),
                            line = list(width = 6),
                            marker = list(size = 10)) %>%
  layout(
    title = list(
      text = "Taxa de Mortalidade Neonatal: 2010, 2015 e 2021",
      font = list(size = 20)
    ),
    xaxis = list(
      title = list(text = "Ano", font = list(size = 16)),
      tickfont = list(size = 14)
    ),
    yaxis = list(
      title = list(text = "Neonatal (por 1000 nascimentos)", font = list(size = 16)),
      tickfont = list(size = 14)
    ),
    legend = list(
      title = list(text = "Paises", font = list(size = 16)),
      font = list(size = 14)
    )
  )

grafico_neonatal

grafico_neonatal_interativo <- ggplotly(grafico_neonatal, tooltip = "text")
grafico_neonatal_interativo

# Grafico Barras TMN 2010 (GRAFICO 6)

dados_20103 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, neonatal) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Neonatal
  mutate(tmi = as.numeric(neonatal)) # Converter TMN para numerico

# Criar o gráfico interativo
grafico21 <- ggplot(dados_20103, aes(x = reorder(pais, neonatal), y = neonatal, text = paste("Pais:", pais, "TMN:", neonatal))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Neonatal (TMN) em 2010",
       x = "pais",
       y = "TMN (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico21

grafico_int_tmn_2010 <- ggplotly(grafico21, tooltip = "text")
grafico_int_tmn_2010

# Grafico Barras TMN 2015 (GRAFICO 7)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, neonatal) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Neonatal
  mutate(tmi = as.numeric(neonatal)) # Converter TMN para numerico

# Criar o grafico interativo
grafico23 <- ggplot(dados_2015, aes(x = reorder(pais, neonatal), y = neonatal, text = paste("Pais:", pais, "TMN:", neonatal))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Neonatal (TMN) em 2015",
       x = "pais",
       y = "TMN (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico23

grafico_int_tmn_2015 <- ggplotly(grafico23, tooltip = "text")
grafico_int_tmn_2015

# Grafico Barras TMN 2021 (GRAFICO 8)

dados_2021 <- banco_final %>%
  filter(ano == 2021) %>%
  select(pais, neonatal) %>%          # Selecionar apenas Pais e Taxa de Mortalidade Neonatal
  mutate(tmi = as.numeric(neonatal)) # Converter TMN para numerico 


# Criar o grafico interativo
grafico25 <- ggplot(dados_2021, aes(x = reorder(pais, neonatal), y = neonatal, text = paste("Pais:", pais, "TMN:", neonatal))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Taxa de Mortalidade Neonatal (TMN) em 2021",
       x = "pais",
       y = "TMN (por 1000 nascimentos)") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico25

grafico_int_tmn_2021 <- ggplotly(grafico25, tooltip = "text")
grafico_int_tmn_2021

# Grafico de evolucao do baixo peso (2010, 2015) (GRAFICO 9)

dados_baixop <- dados %>%
  filter(ano %in% c(2010, 2015, 2021)) %>%
  select(pais, ano, baixop) %>%
  mutate(ano = as.numeric(ano),
         baixop = as.numeric(baixop))

# Criar o grafico
grafico_baixop <- plot_ly(dados_baixop, 
                          x = ~ano, 
                          y = ~baixop, 
                          color = ~pais, 
                          type = 'scatter', 
                          mode = 'lines+markers',
                          text = ~paste("Pais:", pais, "Ano:", ano, "Baixop:", baixop),
                          line = list(width = 6),
                          marker = list(size = 10)) %>%
  layout(
    title = list(
      text = "Baixo Peso ao Nascer: 2010 e 2015",
      font = list(width = 24)
    ),
    xaxis = list(
      title = list(text = "Ano", font = list(size = 20)),
      tickfont = list(width = 20)
    ),
    yaxis = list(
      title = list(text = "Baixo Peso (%)", font = list(size = 20)),
      tickfont = list(width = 20)
    ),
    legend = list(
      title = list(text = "Paises", font = list(size = 20)),
      font = list(width = 20)
    )
  )

grafico_baixop

grafico_baixop_interativo <- ggplotly(grafico_baixop, tooltip = "text")
grafico_baixop_interativo

# Grafico Barras Baixo Peso 2010 (GRAFICO 10)

dados_20102 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, baixop) %>%          
  mutate(tmi = as.numeric(baixop)) 

# Criar o grafico interativo
grafico22 <- ggplot(dados_20102, aes(x = reorder(pais, baixop), y = baixop, text = paste("Pais:", pais, "Baixo Peso:", baixop))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Baixo Peso ao Nascer em 2010",
       x = "pais",
       y = "Baixo Peso") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico22

grafico_int_baixop_2010 <- ggplotly(grafico22, tooltip = "text")
grafico_int_baixop_2010

# Grafico Barras Baixo Peso 2015 (GRAFICO 11)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, baixop) %>%          
  mutate(tmi = as.numeric(baixop)) 

# Criar o grafico interativo
grafico26 <- ggplot(dados_2015, aes(x = reorder(pais, baixop), y = baixop, text = paste("Pais:", pais, "Baixo Peso:", baixop))) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(title = "Baixo Peso ao Nascer em 2015",
       x = "pais",
       y = "Baixo Peso") +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),  # fundo branco do painel
        plot.background = element_rect(fill = "white"), axis.text.y = element_text(size = 14),             # aumenta nomes dos paises
        axis.text.x = element_text(size = 14),             # aumenta os numeros do eixo
        axis.title = element_text(size = 16), # aumenta e deixa negrito os titulos
        plot.title = element_text(size = 18, hjust = 0.5) )

grafico26

grafico_int_baixop_2015 <- ggplotly(grafico26, tooltip = "text")
grafico_int_baixop_2015

# Grafico Barras Desnut 2010 (GRAFICO 12)

dados_20101 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, desnut1ano) %>%          
  mutate(desnut1ano = as.numeric(desnut1ano)) 


grafico28 <- ggplot(dados_20101, aes(
  x = reorder(pais, desnut1ano),
  y = desnut1ano,
  text = paste("Pais:", pais, "Desnutricao:", desnut1ano)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Desnutricao em 2010",
    x = "Pais",
    y = "Desnutricao"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico28

grafico_int_desnut_2010 <- ggplotly(grafico28, tooltip = "text")
grafico_int_desnut_2010

# Grafico Barras Desnut 2015 (GRAFICO 13)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, desnut1ano) %>%          
  mutate(desnut1ano = as.numeric(desnut1ano))  


grafico29 <- ggplot(dados_2015, aes(
  x = reorder(pais, desnut1ano),
  y = desnut1ano,
  text = paste("Pais:", pais, "Desnutricao:", desnut1ano)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Desnutricao em 2015",
    x = "Pais",
    y = "Desnutricao"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico29

grafico_int_desnut_2015 <- ggplotly(grafico29, tooltip = "text")
grafico_int_desnut_2015

# Grafico Barras Desnut 2021 (GRAFICO 14)

dados_2021 <- banco_final %>%
  filter(ano == 2021) %>%
  select(pais, desnut1ano) %>%        
  mutate(desnut1ano = as.numeric(desnut1ano)) 


grafico30 <- ggplot(dados_2021, aes(
  x = reorder(pais, desnut1ano),
  y = desnut1ano,
  text = paste("Pais:", pais, "Desnutricao:", desnut1ano)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Desnutricao em 2021",
    x = "Pais",
    y = "Desnutricao"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico30

grafico_int_desnut_2021 <- ggplotly(grafico30, tooltip = "text")
grafico_int_desnut_2021

# Grafico Barras Infeccoes 2010 (GRAFICO 15)

dados_20101 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, infresp) %>%          
  mutate(infresp = as.numeric(infresp))

grafico31 <- ggplot(dados_20101, aes(
  x = reorder(pais, infresp),
  y = infresp,
  text = paste("Pais:", pais, "Infeccoes Respiratorias Inferiores Agudas:", infresp)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Infeccoes Respiratorias Inferiores Agudas em 2010",
    x = "Pais",
    y = "Infeccoes Respiratorias Inferiores Agudas"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico31

grafico_int_infresp_2010 <- ggplotly(grafico31, tooltip = "text")
grafico_int_infresp_2010

# Grafico Barras Infeccoes 2015 (GRAFICO 16)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, infresp) %>%          
  mutate(infresp = as.numeric(infresp)) 

grafico32 <- ggplot(dados_2015, aes(
  x = reorder(pais, infresp),
  y = infresp,
  text = paste("Pais:", pais, "Infeccoes Respiratorias Inferiores Agudas:", infresp)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Infeccoes Respiratorias Inferiores Agudas em 2015",
    x = "Pais",
    y = "Infeccoes Respiratorias Inferiores Agudas"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico32

grafico_int_infresp_2015 <- ggplotly(grafico32, tooltip = "text")
grafico_int_infresp_2015

# Grafico Barras Infeccoes 2021 (GRAFICO 17)

dados_2021 <- banco_final %>%
  filter(ano == 2021) %>%
  select(pais, infresp) %>%          
  mutate(infresp = as.numeric(infresp)) 

grafico33 <- ggplot(dados_2021, aes(
  x = reorder(pais, infresp),
  y = infresp,
  text = paste("Pais:", pais, "Infeccoes Respiratorias Inferiores Agudas:", infresp)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Infeccoes Respiratorias Inferiores Agudas em 2021",
    x = "Pais",
    y = "Infeccoes Respiratorias Inferiores Agudas"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico33

grafico_int_infresp_2021 <- ggplotly(grafico33, tooltip = "text")
grafico_int_infresp_2021

# Grafico Barras Asfixia 2010 (GRAFICO 18)

dados_20101 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, asfixia) %>%          
  mutate(asfixia = as.numeric(asfixia)) 

grafico34 <- ggplot(dados_20101, aes(
  x = reorder(pais, asfixia),
  y = asfixia,
  text = paste("Pais:", pais, "Asfixia e Trauma no Nascimento:", asfixia)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Asfixia e Trauma no Nascimento em 2010",
    x = "Pais",
    y = "Asfixia e Trauma no Nascimento"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico34

grafico_int_asfixia_2010 <- ggplotly(grafico34, tooltip = "text")
grafico_int_asfixia_2010

# Grafico Barras Asfixia 2015 (GRAFICO 19)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, asfixia) %>%          
  mutate(asfixia = as.numeric(asfixia)) 

grafico35 <- ggplot(dados_2015, aes(
  x = reorder(pais, asfixia),
  y = asfixia,
  text = paste("Pais:", pais, "Asfixia e Trauma no Nascimento:", asfixia)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Asfixia e Trauma no Nascimento em 2015",
    x = "Pais",
    y = "Asfixia e Trauma no Nascimento"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico35

grafico_int_asfixia_2015 <- ggplotly(grafico35, tooltip = "text")
grafico_int_asfixia_2015

# Grafico Barras Asfixia 2021 (GRAFICO 20)

dados_2021 <- banco_final %>%
  filter(ano == 2021) %>%
  select(pais, asfixia) %>%         
  mutate(asfixia = as.numeric(asfixia)) 

grafico36 <- ggplot(dados_2021, aes(
  x = reorder(pais, asfixia),
  y = asfixia,
  text = paste("Pais:", pais, "Asfixia e Trauma no Nascimento:", asfixia)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Asfixia e Trauma no Nascimento em 2021",
    x = "Pais",
    y = "Asfixia e Trauma no Nascimento"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico36

grafico_int_asfixia_2021 <- ggplotly(grafico36, tooltip = "text")
grafico_int_asfixia_2021

# Grafico Barras Prematuridade 2010 (GRAFICO 21)

dados_20101 <- banco_final %>%
  filter(ano == 2010) %>%
  select(pais, prematuridade) %>%          
  mutate(prematuridade = as.numeric(prematuridade)) 

grafico37 <- ggplot(dados_20101, aes(
  x = reorder(pais, prematuridade),
  y = prematuridade,
  text = paste("Pais:", pais, "Prematuridade:", prematuridade)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Prematuridade em 2010",
    x = "Pais",
    y = "Prematuridade"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico37

grafico_int_prematuridade_2010 <- ggplotly(grafico37, tooltip = "text")
grafico_int_prematuridade_2010

# Grafico Barras Prematuridade 2015 (GRAFICO 22)

dados_2015 <- banco_final %>%
  filter(ano == 2015) %>%
  select(pais, prematuridade) %>%          
  mutate(prematuridade = as.numeric(prematuridade)) 

grafico38 <- ggplot(dados_2015, aes(
  x = reorder(pais, prematuridade),
  y = prematuridade,
  text = paste("Pais:", pais, "Prematuridade:", prematuridade)
)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  coord_flip() +
  labs(
    title = "Prematuridade em 2015",
    x = "Pais",
    y = "Prematuridade"
  ) +
  theme(
    panel.grid = element_blank(),                        # remove as linhas de grade
    panel.background = element_rect(fill = "white", colour = NA),  # fundo branco do painel
    plot.background  = element_rect(fill = "white", colour = NA),  # fundo branco fora do painel
    axis.text.y = element_text(size = 14),               # nomes dos paises
    axis.text.x = element_text(size = 14),               # numeros do eixo x 
    axis.title = element_text(size = 16), # titulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5) # titulo centralizado
  )

grafico38

grafico_int_prematuridade_2015 <- ggplotly(grafico38, tooltip = "text")
grafico_int_prematuridade_2015

###### COMPARACAO COM O GDP #######

# Instalando e chamando as bibliotecas
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(dplyr)
library(plotly)
library(readxl)
library(tidyverse)
library(ggplot2)

df <- read_excel("gdp_R.xlsx")

glimpse(df)

df <- df %>% 
  mutate(Paises = as.factor(Paises))

df <- df %>% mutate(
  GDP = as.numeric(gsub("[^0-9,\\.]", "", GDP)), # tirar simbolos estranhos
  TMI = as.numeric(TMI),
  TMN = as.numeric(TMN),
  Ano = as.numeric(Ano)
)

glimpse(df)
summary(df)
head(df)

### GRAFICO GDP, TMI ###

grafico_gdp <- ggplot(df, aes(
  x = Ano, 
  y = TMI, 
  size = GDP, 
  color = Paises,
  label = Paises,
  text = paste(
    "Pais:", Paises,
    "Ano:", Ano,
    "GDP:", round(GDP, 2),
    "TMI:", round(TMI, 2)
  )
)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -1.2, size = 3) +
  scale_size_continuous(range = c(5,20)) +
  labs(title = "TMI por ano relacionada ao GDP", 
       x = "Ano",
       y = "TMI (por 1000 nascidos vivos)",
       size = "GDP por milhoes de dolares") +
  theme_minimal()

grafico_gdp_interativo <- ggplotly(grafico_gdp, tooltip = "text")
grafico_gdp_interativo

### GRAFICO GDP, TMN ###

grafico_gdp_tmn <- ggplot(df, aes(
  x = Ano, 
  y = TMN, 
  size = GDP, 
  color = Paises,
  label = Paises,
  text = paste(
    "Pais:", Paises,
    "Ano:", Ano,
    "GDP:", round(GDP, 2),
    "TMN:", round(TMN, 2)
  )
)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -1.2, size = 3) +
  scale_size_continuous(range = c(5,20)) +
  labs(title = "TMN por ano relacionada ao GDP", 
       x = "Ano",
       y = "TMN (por 1000 nascidos vivos)",
       size = "GDP por milhoes de dolares") +
  theme_minimal()

grafico_gdp_interativo_tmn <- ggplotly(grafico_gdp_tmn, tooltip = "text")
grafico_gdp_interativo_tmn

############# SERIES TEMPORAIS, ARIMA ############

#Instalando e chamando bibliotecas

install.packages("forecast")

library(dplyr)
library(forecast)

banco <- read_xlsx("banco final.xlsx")
str(banco)
banco$tmi <- as.numeric(as.character(banco$tmi))
banco$neonatal <- as.numeric(as.character(banco$neonatal))
banco$baixop <- as.numeric(as.character(banco$baixop))
banco$desnut1ano <- as.numeric(as.character(banco$desnut1ano))
banco$desnut5anos <- as.numeric(as.character(banco$desnut5anos))
banco$infresp <- as.numeric(as.character(banco$infresp))
banco$asfixia <- as.numeric(as.character(banco$asfixia))
banco$prematuridade <- as.numeric(as.character(banco$prematuridade))

# TMI BRASIL 

# Filtrar os dados apenas para o pais desejado
serie_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

# Criar o objeto de serie temporal 
ts_brasil <- ts(serie_brasil$tmi, start = min(serie_brasil$ano), frequency = 1)

# Visualizar a serie
plot(ts_brasil, main = "TMI - Brasil", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

# Ajustar o modelo ARIMA automaticamente
modelo_arima <- auto.arima(ts_brasil)

# Resumo do modelo
summary(modelo_arima)

# Prever os proximos 5 anos
previsao <- forecast(modelo_arima, h = 5)

# Plotar previsao
plot(previsao, main = "Previsao da TMI - Brasil")

previsao$mean

# TMI ARGENTINA 

serie_argentina <- banco %>%
  filter(pais == "Argentina") %>%  
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_argentina <- ts(serie_argentina$tmi, start = min(serie_argentina$ano), frequency = 1)

plot(ts_argentina, main = "TMI - Argentina", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_arg <- auto.arima(ts_argentina)

summary(modelo_arima_arg)

previsao_arg <- forecast(modelo_arima_arg, h = 5)

plot(previsao_arg, main = "Previsao da TMI - Argentina")

previsao_arg$mean

# TMI Africa do Sul

serie_africa_sul <- banco %>%
  filter(pais == "Africa do Sul") %>%  
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_africa_sul <- ts(serie_africa_sul$tmi, start = min(serie_africa_sul$ano), frequency = 1)

plot(ts_africa_sul, main = "TMI - Africa do Sul", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_afs <- auto.arima(ts_africa_sul)

summary(modelo_arima_afs)

previsao_afs <- forecast(modelo_arima_afs, h = 5)

plot(previsao_afs, main = "Previsao da TMI - Africa do Sul")

previsao_afs$mean

# TMI ANGOLA

serie_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_angola <- ts(serie_angola$tmi, start = min(serie_angola$ano), frequency = 1)

plot(ts_angola, main = "TMI - Angola", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_angola <- auto.arima(ts_angola)

summary(modelo_arima_angola)

previsao_angola <- forecast(modelo_arima_angola, h = 5)

plot(previsao_angola, main = "Previsao da TMI - Angola")

previsao_angola$mean

# TMI ARGELIA

serie_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_argelia <- ts(serie_argelia$tmi, start = min(serie_argelia$ano), frequency = 1)

plot(ts_argelia, main = "TMI - Argelia", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_argelia <- auto.arima(ts_argelia)

summary(modelo_arima_argelia)

previsao_argelia <- forecast(modelo_arima_argelia, h = 5)

plot(previsao_argelia, main = "Previsao da TMI - Argelia")

previsao_argelia$mean

# TMI COLOMBIA

serie_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_colombia <- ts(serie_colombia$tmi, start = min(serie_colombia$ano), frequency = 1)

plot(ts_colombia, main = "TMI - Colombia", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_colombia <- auto.arima(ts_colombia)

summary(modelo_arima_colombia)

previsao_colombia <- forecast(modelo_arima_colombia, h = 5)

plot(previsao_colombia, main = "Previsao da TMI - Colombia")

previsao_colombia$mean

# TMI GABAO

serie_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_gabao <- ts(serie_gabao$tmi, start = min(serie_gabao$ano), frequency = 1)

plot(ts_gabao, main = "TMI - Gabao", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_gabao <- auto.arima(ts_gabao)

summary(modelo_arima_gabao)

previsao_gabao <- forecast(modelo_arima_gabao, h = 5)

plot(previsao_gabao, main = "Previsao da TMI - Gabao")

previsao_gabao$mean   # valores previstos
prev$lower  # limites inferiores dos intervalos 
prev$upper  # limites superiores dos intervalos 

# TMI NIGERIA

serie_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_nigeria <- ts(serie_nigeria$tmi, start = min(serie_nigeria$ano), frequency = 1)

plot(ts_nigeria, main = "TMI - Nigeria", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_nigeria <- auto.arima(ts_nigeria)

summary(modelo_arima_nigeria)

previsao_nigeria <- forecast(modelo_arima_nigeria, h = 5)

plot(previsao_nigeria, main = "Previsao da TMI - Nigeria")

previsao_nigeria$mean

# TMI URUGUAI

serie_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, tmi) %>%
  mutate(tmi = as.numeric(tmi))

ts_uruguai <- ts(serie_uruguai$tmi, start = min(serie_uruguai$ano), frequency = 1)

plot(ts_uruguai, main = "TMI - Uruguai", ylab = "Taxa de Mortalidade Infantil", xlab = "Ano")

modelo_arima_uruguai <- auto.arima(ts_uruguai)

summary(modelo_arima_uruguai)

previsao_uruguai <- forecast(modelo_arima_uruguai, h = 5)

plot(previsao_uruguai, main = "Previsao da TMI - Uruguai")

previsao_uruguai$mean

###### NEONATAL #######

# TMN AFRICA DO SUL

neo_africa_sul <- banco %>%
  filter(pais == "Africa do Sul") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_africa_sul <- ts(neo_africa_sul$neonatal, start = min(neo_africa_sul$ano), frequency = 1)

plot(ts_neo_africa_sul, main = "Mortalidade Neonatal - Africa do Sul", ylab = "Taxa", xlab = "Ano")

modelo_neo_afs <- auto.arima(ts_neo_africa_sul)

summary(modelo_neo_afs)

previsao_neo_afs <- forecast(modelo_neo_afs, h = 5)

plot(previsao_neo_afs, main = "Previsao - Mortalidade Neonatal - Africa do Sul")

previsao_neo_afs$mean

# TMN ANGOLA

neo_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_angola <- ts(neo_angola$neonatal, start = min(neo_angola$ano), frequency = 1)

plot(ts_neo_angola, main = "Mortalidade Neonatal - Angola", ylab = "Taxa", xlab = "Ano")

modelo_neo_angola <- auto.arima(ts_neo_angola)

summary(modelo_neo_angola)

previsao_neo_angola <- forecast(modelo_neo_angola, h = 5)

plot(previsao_neo_angola, main = "Previsao - Mortalidade Neonatal - Angola")

previsao_neo_angola$mean

# TMN ARGELIA

neo_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_argelia <- ts(neo_argelia$neonatal, start = min(neo_argelia$ano), frequency = 1)

plot(ts_neo_argelia, main = "Mortalidade Neonatal - Argelia", ylab = "Taxa", xlab = "Ano")

modelo_neo_argelia <- auto.arima(ts_neo_argelia)

summary(modelo_neo_argelia)

previsao_neo_argelia <- forecast(modelo_neo_argelia, h = 5)

plot(previsao_neo_argelia, main = "Previsao - Mortalidade Neonatal - Argelia")

previsao_neo_argelia$mean

# TMN ARGENTINA 

neo_argentina <- banco %>%
  filter(pais == "Argentina") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_argentina <- ts(neo_argentina$neonatal, start = min(neo_argentina$ano), frequency = 1)

plot(ts_neo_argentina, main = "Mortalidade Neonatal - Argentina", ylab = "Taxa", xlab = "Ano")

modelo_neo_argentina <- auto.arima(ts_neo_argentina)

summary(modelo_neo_argentina)

previsao_neo_argentina <- forecast(modelo_neo_argentina, h = 5)

plot(previsao_neo_argentina, main = "Previsao - Mortalidade Neonatal - Argentina")

previsao_neo_argentina$mean

# TMN BRASIL

neo_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_brasil <- ts(neo_brasil$neonatal, start = min(neo_brasil$ano), frequency = 1)

plot(ts_neo_brasil, main = "Mortalidade Neonatal - Brasil", ylab = "Taxa", xlab = "Ano")

modelo_neo_brasil <- auto.arima(ts_neo_brasil)

summary(modelo_neo_brasil)

previsao_neo_brasil <- forecast(modelo_neo_brasil, h = 5)

plot(previsao_neo_brasil, main = "Previsao - Mortalidade Neonatal - Brasil")

previsao_neo_brasil$mean

# TMN COLOMBIA

neo_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_colombia <- ts(neo_colombia$neonatal, start = min(neo_colombia$ano), frequency = 1)

plot(ts_neo_colombia, main = "Mortalidade Neonatal - Colombia", ylab = "Taxa", xlab = "Ano")

modelo_neo_colombia <- auto.arima(ts_neo_colombia)

summary(modelo_neo_colombia)

previsao_neo_colombia <- forecast(modelo_neo_colombia, h = 5)

plot(previsao_neo_colombia, main = "Previsao - Mortalidade Neonatal - Colombia")

previsao_neo_colombia$mean

# TMN GABAO

neo_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_gabao <- ts(neo_gabao$neonatal, start = min(neo_gabao$ano), frequency = 1)

plot(ts_neo_gabao, main = "Mortalidade Neonatal - Gabao", ylab = "Taxa", xlab = "Ano")

modelo_neo_gabao <- auto.arima(ts_neo_gabao)

summary(modelo_neo_gabao)

previsao_neo_gabao <- forecast(modelo_neo_gabao, h = 5)

plot(previsao_neo_gabao, main = "Previsao - Mortalidade Neonatal - Gabao")

previsao_neo_gabao$mean

# TMN NIGERIA

neo_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_nigeria <- ts(neo_nigeria$neonatal, start = min(neo_nigeria$ano), frequency = 1)

plot(ts_neo_nigeria, main = "Mortalidade Neonatal - Nigeria", ylab = "Taxa", xlab = "Ano")

modelo_neo_nigeria <- auto.arima(ts_neo_nigeria)

summary(modelo_neo_nigeria)

previsao_neo_nigeria <- forecast(modelo_neo_nigeria, h = 5)

plot(previsao_neo_nigeria, main = "Previsao - Mortalidade Neonatal - Nigeria")

previsao_neo_nigeria$mean

# TMN URUGUAI

neo_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, neonatal) %>%
  mutate(neonatal = as.numeric(neonatal))

ts_neo_uruguai <- ts(neo_uruguai$neonatal, start = min(neo_uruguai$ano), frequency = 1)

plot(ts_neo_uruguai, main = "Mortalidade Neonatal - Uruguai", ylab = "Taxa", xlab = "Ano")

modelo_neo_uruguai <- auto.arima(ts_neo_uruguai)

summary(modelo_neo_uruguai)

previsao_neo_uruguai <- forecast(modelo_neo_uruguai, h = 5)

plot(previsao_neo_uruguai, main = "Previsao - Mortalidade Neonatal - Uruguai")

previsao_neo_uruguai$mean

############## BAIXO PESO #############

# BAIXO PESO Africa do Sul

baixop_africa_sul <- banco %>%
  filter(pais == "Africa do Sul") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_africa_sul <- ts(baixop_africa_sul$baixop, start = min(baixop_africa_sul$ano), frequency = 1)
plot(ts_baixop_africa_sul, main = "Baixo Peso ao Nascer - Africa do Sul", ylab = "Taxa", xlab = "Ano")
modelo_baixop_africa_sul <- auto.arima(ts_baixop_africa_sul)
summary(modelo_baixop_africa_sul)
previsao_baixop_africa_sul <- forecast(modelo_baixop_africa_sul, h = 5)
plot(previsao_baixop_africa_sul, main = "Previsao - Baixo Peso - Africa do Sul")

previsao_baixop_africa_sul$mean

# BAIXO PESO Angola

baixop_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_angola <- ts(baixop_angola$baixop, start = min(baixop_angola$ano), frequency = 1)
plot(ts_baixop_angola, main = "Baixo Peso ao Nascer - Angola", ylab = "Taxa", xlab = "Ano")
modelo_baixop_angola <- auto.arima(ts_baixop_angola)
summary(modelo_baixop_angola)
previsao_baixop_angola <- forecast(modelo_baixop_angola, h = 5)
plot(previsao_baixop_angola, main = "Previsao - Baixo Peso - Angola")

previsao_baixop_angola$mean

# BAIXO PESO Argelia

baixop_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_argelia <- ts(baixop_argelia$baixop, start = min(baixop_argelia$ano), frequency = 1)
plot(ts_baixop_argelia, main = "Baixo Peso ao Nascer - Argelia", ylab = "Taxa", xlab = "Ano")
modelo_baixop_argelia <- auto.arima(ts_baixop_argelia)
summary(modelo_baixop_argelia)
previsao_baixop_argelia <- forecast(modelo_baixop_argelia, h = 5)
plot(previsao_baixop_argelia, main = "Previsao - Baixo Peso - Argelia")

previsao_baixop_argelia$mean

# BAIXO PESO Argentina

baixop_argentina <- banco %>%
  filter(pais == "Argentina") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_argentina <- ts(baixop_argentina$baixop, start = min(baixop_argentina$ano), frequency = 1)
plot(ts_baixop_argentina, main = "Baixo Peso ao Nascer - Argentina", ylab = "Taxa", xlab = "Ano")
modelo_baixop_argentina <- auto.arima(ts_baixop_argentina)
summary(modelo_baixop_argentina)
previsao_baixop_argentina <- forecast(modelo_baixop_argentina, h = 5)
plot(previsao_baixop_argentina, main = "Previsao - Baixo Peso - Argentina")

previsao_baixop_argentina$mean

# BAIXO PESO Brasil

baixop_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_brasil <- ts(baixop_brasil$baixop, start = min(baixop_brasil$ano), frequency = 1)
plot(ts_baixop_brasil, main = "Baixo Peso ao Nascer - Brasil", ylab = "Taxa", xlab = "Ano")
modelo_baixop_brasil <- auto.arima(ts_baixop_brasil)
summary(modelo_baixop_brasil)
previsao_baixop_brasil <- forecast(modelo_baixop_brasil, h = 5)
plot(previsao_baixop_brasil, main = "Previsao - Baixo Peso - Brasil")

previsao_baixop_brasil$mean

# BAIXO PESO Colombia

baixop_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_colombia <- ts(baixop_colombia$baixop, start = min(baixop_colombia$ano), frequency = 1)
plot(ts_baixop_colombia, main = "Baixo Peso ao Nascer - Colombia", ylab = "Taxa", xlab = "Ano")
modelo_baixop_colombia <- auto.arima(ts_baixop_colombia)
summary(modelo_baixop_colombia)
previsao_baixop_colombia <- forecast(modelo_baixop_colombia, h = 5)
plot(previsao_baixop_colombia, main = "Previsao - Baixo Peso - Colombia")

previsao_baixop_colombia$mean

# BAIXO PESO Gabao

baixop_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_gabao <- ts(baixop_gabao$baixop, start = min(baixop_gabao$ano), frequency = 1)
plot(ts_baixop_gabao, main = "Baixo Peso ao Nascer - Gabao", ylab = "Taxa", xlab = "Ano")
modelo_baixop_gabao <- auto.arima(ts_baixop_gabao)
summary(modelo_baixop_gabao)
previsao_baixop_gabao <- forecast(modelo_baixop_gabao, h = 5)
plot(previsao_baixop_gabao, main = "Previsao - Baixo Peso - Gabao")

previsao_baixop_gabao$mean

# BAIXO PESO Nigeria

baixop_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_nigeria <- ts(baixop_nigeria$baixop, start = min(baixop_nigeria$ano), frequency = 1)
plot(ts_baixop_nigeria, main = "Baixo Peso ao Nascer - Nigeria", ylab = "Taxa", xlab = "Ano")
modelo_baixop_nigeria <- auto.arima(ts_baixop_nigeria)
summary(modelo_baixop_nigeria)
previsao_baixop_nigeria <- forecast(modelo_baixop_nigeria, h = 5)
plot(previsao_baixop_nigeria, main = "Previsao - Baixo Peso - Nigeria")

previsao_baixop_nigeria$mean

# BAIXO PESO Uruguai

baixop_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, baixop) %>%
  mutate(baixop = as.numeric(baixop))
ts_baixop_uruguai <- ts(baixop_uruguai$baixop, start = min(baixop_uruguai$ano), frequency = 1)
plot(ts_baixop_uruguai, main = "Baixo Peso ao Nascer - Uruguai", ylab = "Taxa", xlab = "Ano")
modelo_baixop_uruguai <- auto.arima(ts_baixop_uruguai)
summary(modelo_baixop_uruguai)
previsao_baixop_uruguai <- forecast(modelo_baixop_uruguai, h = 5)
plot(previsao_baixop_uruguai, main = "Previsao - Baixo Peso - Uruguai")

previsao_baixop_uruguai$mean

########### DESNUT1ANO ############

# DESNUT Africa do Sul

desnut1ano_africa_sul <- banco %>%
  filter(pais == "Africa do Sul") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_africa_sul <- ts(desnut1ano_africa_sul$desnut1ano, start = min(desnut1ano_africa_sul$ano), frequency = 1)
plot(ts_desnut1ano_africa_sul, main = "Desnutricao ate 1 ano - Africa do Sul", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_africa_sul <- auto.arima(ts_desnut1ano_africa_sul)
summary(modelo_desnut1ano_africa_sul)
previsao_desnut1ano_africa_sul <- forecast(modelo_desnut1ano_africa_sul, h = 5)
plot(previsao_desnut1ano_africa_sul, main = "Previsao - Desnutricao ate 1 ano - Africa do Sul")

previsao_desnut1ano_africa_sul$mean

# DESNUT Angola

desnut1ano_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_angola <- ts(desnut1ano_angola$desnut1ano, start = min(desnut1ano_angola$ano), frequency = 1)
plot(ts_desnut1ano_angola, main = "Desnutricao ate 1 ano - Angola", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_angola <- auto.arima(ts_desnut1ano_angola)
summary(modelo_desnut1ano_angola)
previsao_desnut1ano_angola <- forecast(modelo_desnut1ano_angola, h = 5)
plot(previsao_desnut1ano_angola, main = "Previsao - Desnutricao ate 1 ano - Angola")

# DESNUT Argelia

desnut1ano_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_argelia <- ts(desnut1ano_argelia$desnut1ano, start = min(desnut1ano_argelia$ano), frequency = 1)
plot(ts_desnut1ano_argelia, main = "Desnutricao ate 1 ano - Argelia", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_argelia <- auto.arima(ts_desnut1ano_argelia)
summary(modelo_desnut1ano_argelia)
previsao_desnut1ano_argelia <- forecast(modelo_desnut1ano_argelia, h = 5)
plot(previsao_desnut1ano_argelia, main = "Previsao - Desnutricao ate 1 ano - Argelia")

# DESNUT Argentina

desnut1ano_argentina <- banco %>%
  filter(pais == "Argentina") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_argentina <- ts(desnut1ano_argentina$desnut1ano, start = min(desnut1ano_argentina$ano), frequency = 1)
plot(ts_desnut1ano_argentina, main = "Desnutricao ate 1 ano - Argentina", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_argentina <- auto.arima(ts_desnut1ano_argentina)
summary(modelo_desnut1ano_argentina)
previsao_desnut1ano_argentina <- forecast(modelo_desnut1ano_argentina, h = 5)
plot(previsao_desnut1ano_argentina, main = "Previsao - Desnutricao ate 1 ano - Argentina")

previsao_desnut1ano_argentina$mean

# DESNUT Brasil

desnut1ano_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_brasil <- ts(desnut1ano_brasil$desnut1ano, start = min(desnut1ano_brasil$ano), frequency = 1)
plot(ts_desnut1ano_brasil, main = "Desnutricao ate 1 ano - Brasil", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_brasil <- auto.arima(ts_desnut1ano_brasil)
summary(modelo_desnut1ano_brasil)
previsao_desnut1ano_brasil <- forecast(modelo_desnut1ano_brasil, h = 5)
plot(previsao_desnut1ano_brasil, main = "Previsao - Desnutricao ate 1 ano - Brasil")

previsao_desnut1ano_brasil$mean

# DESNUT Colombia

desnut1ano_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_colombia <- ts(desnut1ano_colombia$desnut1ano, start = min(desnut1ano_colombia$ano), frequency = 1)
plot(ts_desnut1ano_colombia, main = "Desnutricao ate 1 ano - Colombia", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_colombia <- auto.arima(ts_desnut1ano_colombia)
summary(modelo_desnut1ano_colombia)
previsao_desnut1ano_colombia <- forecast(modelo_desnut1ano_colombia, h = 5)
plot(previsao_desnut1ano_colombia, main = "Previsao - Desnutricao ate 1 ano - Colombia")

previsao_desnut1ano_colombia$mean

# DESNUT Gabao

desnut1ano_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_gabao <- ts(desnut1ano_gabao$desnut1ano, start = min(desnut1ano_gabao$ano), frequency = 1)
plot(ts_desnut1ano_gabao, main = "Desnutricao ate 1 ano - Gabao", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_gabao <- auto.arima(ts_desnut1ano_gabao)
summary(modelo_desnut1ano_gabao)
previsao_desnut1ano_gabao <- forecast(modelo_desnut1ano_gabao, h = 5)
plot(previsao_desnut1ano_gabao, main = "Previsao - Desnutricao ate 1 ano - Gabao")

# DESNUT Nigeria

desnut1ano_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_nigeria <- ts(desnut1ano_nigeria$desnut1ano, start = min(desnut1ano_nigeria$ano), frequency = 1)
plot(ts_desnut1ano_nigeria, main = "Desnutricao ate 1 ano - Nigeria", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_nigeria <- auto.arima(ts_desnut1ano_nigeria)
summary(modelo_desnut1ano_nigeria)
previsao_desnut1ano_nigeria <- forecast(modelo_desnut1ano_nigeria, h = 5)
plot(previsao_desnut1ano_nigeria, main = "Previsao - Desnutricao ate 1 ano - Nigeria")

# DESNUT Uruguai

desnut1ano_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, desnut1ano) %>%
  mutate(desnut1ano = as.numeric(desnut1ano))
ts_desnut1ano_uruguai <- ts(desnut1ano_uruguai$desnut1ano, start = min(desnut1ano_uruguai$ano), frequency = 1)
plot(ts_desnut1ano_uruguai, main = "Desnutricao ate 1 ano - Uruguai", ylab = "Taxa", xlab = "Ano")
modelo_desnut1ano_uruguai <- auto.arima(ts_desnut1ano_uruguai)
summary(modelo_desnut1ano_uruguai)
previsao_desnut1ano_uruguai <- forecast(modelo_desnut1ano_uruguai, h = 5)
plot(previsao_desnut1ano_uruguai, main = "Previsao - Desnutricao ate 1 ano - Uruguai")

previsao_desnut1ano_uruguai$mean

##### INFRESP ######

# INFECCOES AFRICA DO SUL

infresp_africa_sul <- banco %>% filter(pais == "Africa do Sul") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_africa_sul <- ts(infresp_africa_sul$infresp, start = min(infresp_africa_sul$ano), frequency = 1)
plot(ts_infresp_africa_sul, main = "Infeccoes Respiratorias - Africa do Sul", ylab = "Taxa", xlab = "Ano")
modelo_infresp_africa_sul <- auto.arima(ts_infresp_africa_sul)
summary(modelo_infresp_africa_sul)
previsao_infresp_africa_sul <- forecast(modelo_infresp_africa_sul, h = 5)
plot(previsao_infresp_africa_sul, main = "Previsao - Infeccoes Respiratorias - Africa do Sul")

previsao_infresp_africa_sul$mean

# INFECCOES ANGOLA

infresp_angola <- banco %>% filter(pais == "Angola") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_angola <- ts(infresp_angola$infresp, start = min(infresp_angola$ano), frequency = 1)
plot(ts_infresp_angola, main = "Infeccoes Respiratorias - Angola", ylab = "Taxa", xlab = "Ano")
modelo_infresp_angola <- auto.arima(ts_infresp_angola)
summary(modelo_infresp_angola)
previsao_infresp_angola <- forecast(modelo_infresp_angola, h = 5)
plot(previsao_infresp_angola, main = "Previsao - Infeccoes Respiratorias - Angola")

previsao_infresp_angola$mean

# INFECCOES ARGELIA

infresp_argelia <- banco %>% filter(pais == "Argelia") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_argelia <- ts(infresp_argelia$infresp, start = min(infresp_argelia$ano), frequency = 1)
plot(ts_infresp_argelia, main = "Infeccoes Respiratorias - Argelia", ylab = "Taxa", xlab = "Ano")
modelo_infresp_argelia <- auto.arima(ts_infresp_argelia)
summary(modelo_infresp_argelia)
previsao_infresp_argelia <- forecast(modelo_infresp_argelia, h = 5)
plot(previsao_infresp_argelia, main = "Previsao - Infeccoes Respiratorias - Argelia")

previsao_infresp_argelia$mean

# INFECCOES ARGENTINA

infresp_argentina <- banco %>% filter(pais == "Argentina") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_argentina <- ts(infresp_argentina$infresp, start = min(infresp_argentina$ano), frequency = 1)
plot(ts_infresp_argentina, main = "Infeccoes Respiratorias - Argentina", ylab = "Taxa", xlab = "Ano")
modelo_infresp_argentina <- auto.arima(ts_infresp_argentina)
summary(modelo_infresp_argentina)
previsao_infresp_argentina <- forecast(modelo_infresp_argentina, h = 5)
plot(previsao_infresp_argentina, main = "Previsao - Infeccoes Respiratorias - Argentina")

previsao_infresp_argentina$mean

# INFECCOES BRASIL

infresp_brasil <- banco %>% filter(pais == "Brasil") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_brasil <- ts(infresp_brasil$infresp, start = min(infresp_brasil$ano), frequency = 1)
plot(ts_infresp_brasil, main = "Infeccoes Respiratorias - Brasil", ylab = "Taxa", xlab = "Ano")
modelo_infresp_brasil <- auto.arima(ts_infresp_brasil)
summary(modelo_infresp_brasil)
previsao_infresp_brasil <- forecast(modelo_infresp_brasil, h = 5)
plot(previsao_infresp_brasil, main = "Previsao - Infeccoes Respiratorias - Brasil")

previsao_infresp_brasil$mean

# INFECCOES COLOMBIA

infresp_colombia <- banco %>% filter(pais == "Colombia") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_colombia <- ts(infresp_colombia$infresp, start = min(infresp_colombia$ano), frequency = 1)
plot(ts_infresp_colombia, main = "Infeccoes Respiratorias - Colombia", ylab = "Taxa", xlab = "Ano")
modelo_infresp_colombia <- auto.arima(ts_infresp_colombia)
summary(modelo_infresp_colombia)
previsao_infresp_colombia <- forecast(modelo_infresp_colombia, h = 5)
plot(previsao_infresp_colombia, main = "Previsao - Infeccoes Respiratorias - Colombia")

previsao_infresp_colombia$mean

# INFECCOES GABAO

infresp_gabao <- banco %>% filter(pais == "Gabao") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_gabao <- ts(infresp_gabao$infresp, start = min(infresp_gabao$ano), frequency = 1)
plot(ts_infresp_gabao, main = "Infeccoes Respiratorias - Gabao", ylab = "Taxa", xlab = "Ano")
modelo_infresp_gabao <- auto.arima(ts_infresp_gabao)
summary(modelo_infresp_gabao)
previsao_infresp_gabao <- forecast(modelo_infresp_gabao, h = 5)
plot(previsao_infresp_gabao, main = "Previsao - Infeccoes Respiratorias - Gabao")

previsao_infresp_gabao$mean

# INFECCOES NIGERIA

infresp_nigeria <- banco %>% filter(pais == "Nigeria") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_nigeria <- ts(infresp_nigeria$infresp, start = min(infresp_nigeria$ano), frequency = 1)
plot(ts_infresp_nigeria, main = "Infeccoes Respiratorias - Nigeria", ylab = "Taxa", xlab = "Ano")
modelo_infresp_nigeria <- auto.arima(ts_infresp_nigeria)
summary(modelo_infresp_nigeria)
previsao_infresp_nigeria <- forecast(modelo_infresp_nigeria, h = 5)
plot(previsao_infresp_nigeria, main = "Previsao - Infeccoes Respiratorias - Nigeria")

previsao_infresp_nigeria$mean

# INFECCOES URUGUAI

infresp_uruguai <- banco %>% filter(pais == "Uruguai") %>% arrange(ano) %>% select(ano, infresp) %>% mutate(infresp = as.numeric(infresp))
ts_infresp_uruguai <- ts(infresp_uruguai$infresp, start = min(infresp_uruguai$ano), frequency = 1)
plot(ts_infresp_uruguai, main = "Infeccoes Respiratorias - Uruguai", ylab = "Taxa", xlab = "Ano")
modelo_infresp_uruguai <- auto.arima(ts_infresp_uruguai)
summary(modelo_infresp_uruguai)
previsao_infresp_uruguai <- forecast(modelo_infresp_uruguai, h = 5)
plot(previsao_infresp_uruguai, main = "Previsao - Infeccoes Respiratorias - Uruguai")

previsao_infresp_uruguai$mean

######## ASFIXIA ##########

# ASFIXIA AFRICA DO SUL

asfixia_africa_sul <- banco %>% 
  filter(pais == "Africa do Sul") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_africa_sul <- ts(asfixia_africa_sul$asfixia, start = min(asfixia_africa_sul$ano), frequency = 1)

plot(ts_asfixia_africa_sul, main = "Asfixia ao Nascer - Africa do Sul", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_africa_sul <- auto.arima(ts_asfixia_africa_sul)

summary(modelo_asfixia_africa_sul)

previsao_asfixia_africa_sul <- forecast(modelo_asfixia_africa_sul, h = 5)

plot(previsao_asfixia_africa_sul, main = "Previsao - Asfixia - Africa do Sul")

previsao_asfixia_africa_sul$mean

# ASFIXIA ANGOLA

asfixia_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_angola <- ts(asfixia_angola$asfixia, start = min(asfixia_angola$ano), frequency = 1)

plot(ts_asfixia_angola, main = "Asfixia ao Nascer - Angola", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_angola <- auto.arima(ts_asfixia_angola)

summary(modelo_asfixia_angola)

previsao_asfixia_angola <- forecast(modelo_asfixia_angola, h = 5)

plot(previsao_asfixia_angola, main = "Previsao - Asfixia - Angola")

previsao_asfixia_angola$mean

# ASFIXIA ARGELIA

asfixia_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_argelia <- ts(asfixia_argelia$asfixia, start = min(asfixia_argelia$ano), frequency = 1)

plot(ts_asfixia_argelia, main = "Asfixia ao Nascer - Argelia", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_argelia <- auto.arima(ts_asfixia_argelia)

summary(modelo_asfixia_argelia)

previsao_asfixia_argelia <- forecast(modelo_asfixia_argelia, h = 5)

plot(previsao_asfixia_argelia, main = "Previsao - Asfixia - Argelia")

previsao_asfixia_argelia$mean

# ASFIXIA ARGENTINA

asfixia_argentina <- banco %>%
  filter(pais == "Argentina") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_argentina <- ts(asfixia_argentina$asfixia, start = min(asfixia_argentina$ano), frequency = 1)

plot(ts_asfixia_argentina, main = "Asfixia ao Nascer - Argentina", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_argentina <- auto.arima(ts_asfixia_argentina)

summary(modelo_asfixia_argentina)

previsao_asfixia_argentina <- forecast(modelo_asfixia_argentina, h = 5)

plot(previsao_asfixia_argentina, main = "Previsao - Asfixia - Argentina")

previsao_asfixia_argentina$mean

# ASFIXIA BRASIL

asfixia_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_brasil <- ts(asfixia_brasil$asfixia, start = min(asfixia_brasil$ano), frequency = 1)

plot(ts_asfixia_brasil, main = "Asfixia ao Nascer - Brasil", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_brasil <- auto.arima(ts_asfixia_brasil)

summary(modelo_asfixia_brasil)

previsao_asfixia_brasil <- forecast(modelo_asfixia_brasil, h = 5)

plot(previsao_asfixia_brasil, main = "Previsao - Asfixia - Brasil")

previsao_asfixia_brasil$mean

# ASFIXIA COLOMBIA

asfixia_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_colombia <- ts(asfixia_colombia$asfixia, start = min(asfixia_colombia$ano), frequency = 1)

plot(ts_asfixia_colombia, main = "Asfixia ao Nascer - Colombia", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_colombia <- auto.arima(ts_asfixia_colombia)

summary(modelo_asfixia_colombia)

previsao_asfixia_colombia <- forecast(modelo_asfixia_colombia, h = 5)

plot(previsao_asfixia_colombia, main = "Previsao - Asfixia - Colombia")

previsao_asfixia_colombia$mean

# ASFIXIA GABAO

asfixia_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_gabao <- ts(asfixia_gabao$asfixia, start = min(asfixia_gabao$ano), frequency = 1)

plot(ts_asfixia_gabao, main = "Asfixia ao Nascer - Gabao", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_gabao <- auto.arima(ts_asfixia_gabao)

summary(modelo_asfixia_gabao)

previsao_asfixia_gabao <- forecast(modelo_asfixia_gabao, h = 5)

plot(previsao_asfixia_gabao, main = "Previsao - Asfixia - Gabao")

previsao_asfixia_gabao$mean

# ASFIXIA NIGERIA

asfixia_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_nigeria <- ts(asfixia_nigeria$asfixia, start = min(asfixia_nigeria$ano), frequency = 1)

plot(ts_asfixia_nigeria, main = "Asfixia ao Nascer - Nigeria", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_nigeria <- auto.arima(ts_asfixia_nigeria)

summary(modelo_asfixia_nigeria)

previsao_asfixia_nigeria <- forecast(modelo_asfixia_nigeria, h = 5)

plot(previsao_asfixia_nigeria, main = "Previsao - Asfixia - Nigeria")

previsao_asfixia_nigeria$mean

# ASFIXIA URUGUAI

asfixia_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, asfixia) %>%
  mutate(asfixia = as.numeric(asfixia))

ts_asfixia_uruguai <- ts(asfixia_uruguai$asfixia, start = min(asfixia_uruguai$ano), frequency = 1)

plot(ts_asfixia_uruguai, main = "Asfixia ao Nascer - Uruguai", ylab = "Taxa", xlab = "Ano")

modelo_asfixia_uruguai <- auto.arima(ts_asfixia_uruguai)

summary(modelo_asfixia_uruguai)

previsao_asfixia_uruguai <- forecast(modelo_asfixia_uruguai, h = 5)

plot(previsao_asfixia_uruguai, main = "Previsao - Asfixia - Uruguai")

previsao_asfixia_uruguai$mean

######### PREMATURIDADE ############

# PREMATURIDADE Africa do Sul

prematuridade_africa_sul <- banco %>%
  filter(pais == "Africa do Sul") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_africa_sul <- ts(prematuridade_africa_sul$prematuridade, start = min(prematuridade_africa_sul$ano), frequency = 1)
plot(ts_prematuridade_africa_sul, main = "Prematuridade - Africa do Sul", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_africa_sul <- auto.arima(ts_prematuridade_africa_sul)
summary(modelo_prematuridade_africa_sul)
previsao_prematuridade_africa_sul <- forecast(modelo_prematuridade_africa_sul, h = 5)
plot(previsao_prematuridade_africa_sul, main = "Previsao - Prematuridade - Africa do Sul")

previsao_prematuridade_africa_sul$mean

# PREMATURIDADE Angola

prematuridade_angola <- banco %>%
  filter(pais == "Angola") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_angola <- ts(prematuridade_angola$prematuridade, start = min(prematuridade_angola$ano), frequency = 1)
plot(ts_prematuridade_angola, main = "Prematuridade - Angola", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_angola <- auto.arima(ts_prematuridade_angola)
summary(modelo_prematuridade_angola)
previsao_prematuridade_angola <- forecast(modelo_prematuridade_angola, h = 5)
plot(previsao_prematuridade_angola, main = "Previsao - Prematuridade - Angola")

# PREMATURIDADE Argelia

prematuridade_argelia <- banco %>%
  filter(pais == "Argelia") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_argelia <- ts(prematuridade_argelia$prematuridade, start = min(prematuridade_argelia$ano), frequency = 1)
plot(ts_prematuridade_argelia, main = "Prematuridade - Argelia", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_argelia <- auto.arima(ts_prematuridade_argelia)
summary(modelo_prematuridade_argelia)
previsao_prematuridade_argelia <- forecast(modelo_prematuridade_argelia, h = 5)
plot(previsao_prematuridade_argelia, main = "Previsao - Prematuridade - Argelia")

# PREMATURIDADE Argentina

prematuridade_argentina <- banco %>%
  filter(pais == "Argentina") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_argentina <- ts(prematuridade_argentina$prematuridade, start = min(prematuridade_argentina$ano), frequency = 1)
plot(ts_prematuridade_argentina, main = "Prematuridade - Argentina", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_argentina <- auto.arima(ts_prematuridade_argentina)
summary(modelo_prematuridade_argentina)
previsao_prematuridade_argentina <- forecast(modelo_prematuridade_argentina, h = 5)
plot(previsao_prematuridade_argentina, main = "Previsao - Prematuridade - Argentina")

previsao_prematuridade_argentina$mean

# PREMATURIDADE Brasil

prematuridade_brasil <- banco %>%
  filter(pais == "Brasil") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_brasil <- ts(prematuridade_brasil$prematuridade, start = min(prematuridade_brasil$ano), frequency = 1)
plot(ts_prematuridade_brasil, main = "Prematuridade - Brasil", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_brasil <- auto.arima(ts_prematuridade_brasil)
summary(modelo_prematuridade_brasil)
previsao_prematuridade_brasil <- forecast(modelo_prematuridade_brasil, h = 5)
plot(previsao_prematuridade_brasil, main = "Previsao - Prematuridade - Brasil")

previsao_prematuridade_brasil$mean

# PREMATURIDADE Colombia

prematuridade_colombia <- banco %>%
  filter(pais == "Colombia") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_colombia <- ts(prematuridade_colombia$prematuridade, start = min(prematuridade_colombia$ano), frequency = 1)
plot(ts_prematuridade_colombia, main = "Prematuridade - Colombia", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_colombia <- auto.arima(ts_prematuridade_colombia)
summary(modelo_prematuridade_colombia)
previsao_prematuridade_colombia <- forecast(modelo_prematuridade_colombia, h = 5)
plot(previsao_prematuridade_colombia, main = "Previsao - Prematuridade - Colombia")

previsao_prematuridade_colombia$mean

# PREMATURIDADE Gabao

prematuridade_gabao <- banco %>%
  filter(pais == "Gabao") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_gabao <- ts(prematuridade_gabao$prematuridade, start = min(prematuridade_gabao$ano), frequency = 1)
plot(ts_prematuridade_gabao, main = "Prematuridade - Gabao", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_gabao <- auto.arima(ts_prematuridade_gabao)
summary(modelo_prematuridade_gabao)
previsao_prematuridade_gabao <- forecast(modelo_prematuridade_gabao, h = 5)
plot(previsao_prematuridade_gabao, main = "Previsao - Prematuridade - Gabao")

previsao_prematuridade_gabao$mean

# PREMATURIDADE Nigeria

prematuridade_nigeria <- banco %>%
  filter(pais == "Nigeria") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_nigeria <- ts(prematuridade_nigeria$prematuridade, start = min(prematuridade_nigeria$ano), frequency = 1)
plot(ts_prematuridade_nigeria, main = "Prematuridade - Nigeria", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_nigeria <- auto.arima(ts_prematuridade_nigeria)
summary(modelo_prematuridade_nigeria)
previsao_prematuridade_nigeria <- forecast(modelo_prematuridade_nigeria, h = 5)
plot(previsao_prematuridade_nigeria, main = "Previsao - Prematuridade - Nigeria")

previsao_prematuridade_nigeria$mean

# PREMATURIDADE Uruguai

prematuridade_uruguai <- banco %>%
  filter(pais == "Uruguai") %>%
  arrange(ano) %>%
  select(ano, prematuridade) %>%
  mutate(prematuridade = as.numeric(prematuridade))
ts_prematuridade_uruguai <- ts(prematuridade_uruguai$prematuridade, start = min(prematuridade_uruguai$ano), frequency = 1)
plot(ts_prematuridade_uruguai, main = "Prematuridade - Uruguai", ylab = "Taxa", xlab = "Ano")
modelo_prematuridade_uruguai <- auto.arima(ts_prematuridade_uruguai)
summary(modelo_prematuridade_uruguai)
previsao_prematuridade_uruguai <- forecast(modelo_prematuridade_uruguai, h = 5)
plot(previsao_prematuridade_uruguai, main = "Previsao - Prematuridade - Uruguai")

previsao_prematuridade_uruguai$mean  


















 


  


  