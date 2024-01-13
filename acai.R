library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext); library(ggplot2)

#### Mapa ####

raw_data <- read_xlsx('~/Downloads/SearchKeywords-amapuri.com,belfanstore.com,selvanevada.co,youaresavvy.com,naturela.com-(170)-(2022_02-2023_02) (2).xlsx', 
                      sheet = 2, col_names = T)

data <- raw_data %>% 
  select(1, 3, 8:12)
colnames(data)[1] <- "terms"

data %>% 
  pivot_longer(cols = 3:7) %>% 
  mutate(v = Traffic * value) %>% 
  select(3:5) %>% 
  group_by(name) %>% 
  summarise(z = sum(v, na.rm = T)) %>% 
  mutate(per = round(z/sum(z)*100, 2)) %>% 
  ungroup() %>% 
  ggplot(., aes(area = z, fill = name, 
                label = paste(name, paste(per, "%", sep = ""),sep = "\n"))) +
  geom_treemap(color = 'grey60') + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  ggtitle('Share de búsqueda Categoría Acai - Paid Search') +
  scale_fill_discrete(labels = c("Amapuri", "Belfan Store", "Naturela", 
                                 "Selva Nevada", "YouAreSavvy")) +
  labs(fill = "Website") + 
  theme(plot.title = element_text(face = 'bold', size = 20))
  

#### Time Series ####
