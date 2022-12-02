
# Horas de trabalho em países democratas e autocratas --------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 01/12/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/working-hours -------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Trabalho é uma parte central de nossas vidas. É algo que nós fazemos quase todos os
### dias, por muitos dias, e por décadas. Por ser uma questão central, observar de perto
### quanto tempo nós gastamos trabalhando pode nos dizer muito sobre nossas vidas e sobre
### a sociedade em que vivemos.

### O quanto as pessoas no mundo trabalham? Em quantos países hoje as pessoas trabalham
### muito menos que nos passados 150 anos? Trabalhar menos significa ser capaz de gastar
### tempo se tornando mais educado, ou simplesmentes tendo mais tempo de lazer. Isso é um
### substancial progresso, mas existem ainda muitas desigualdades entre e dentro de países.

### Aqui nós apresentamos dados sobre horas de trabalho. Nós exploramos como isso ocorre
### nos países e ao longo do tempo, e como essas diferenças importam na vida das pessoas.

### Os dados consideram apenas a média de horas dos trabalhadores em tempo integral e que 
### não participam de atividades da agricultura.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

hor_trab <- read.csv("annual-working-hours-per-worker.csv")
view(hor_trab)
names(hor_trab)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

hor_trab <- hor_trab %>%
  select(-Code) %>%
  rename(horas = Average.annual.working.hours.per.worker) %>%
  filter(between(Year, 1970, 2017)) %>%
  view()

hor_trab1 <- hor_trab %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China")) %>%
  group_by(Entity) %>%
  summarise(media = mean(horas), sd = sd(horas),
            n = n(), se = sd/sqrt(n)) %>%
  view()

hor_trab2 <- hor_trab %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China")) %>%
  view()

hor_trab3 <- hor_trab %>%
  filter(Entity %in% c("United States", "Brazil", "China")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 4)

ggplot(hor_trab1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c( "#88CCEE", "#CC6677",
                                "#DDCC77", "#117733")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Alemanha", "Estados Unidos", "Japão", "China")) +
  labs(x = "Países", y = "Tempo médio anual\n de trabalho (horas)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"))

ggplot(hor_trab2, aes(x = Year, y = horas, 
                      group = Entity, color = Entity)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (anos)", y = "Tempo médio anual\n de trabalho (horas)",
       color = "Países") +
    theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none", axis.text = element_text(colour = "black"))





