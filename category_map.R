library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext); library(ggplot2)

raw_data <- read_xlsx('~/Downloads/SearchKeywords-pacifico.com.pe,rimac.com,lapositiva.com.pe,mapfre.com.pe,interseguro.pe-(604)-(2022_02-2023_02) (2).xlsx', 
                      sheet = 2, col_names = T)

data <- raw_data %>% 
  select(1, 3, 8:12)
colnames(data)[1] <- "terms"

# Define the categories
category <- c("soat", "vida", "vehicular", "eps", "salud", "viaje", "sctr")

# Function to assign a category
assign_category <- function(string) {
  for (cat in category) {
    if (grepl(cat, string, ignore.case = TRUE)) {
      return(cat)
    }
  }
  return("none")
}

data <- data %>%
  mutate(category = sapply(terms, assign_category))

# Define the column to multiply with others
main_col <- "Traffic"

# Define the set of 5 additional columns
additional_cols <- names(data[,3:7])

# Multiply the main column with the additional columns and create new columns 
# with the results
for (i in seq_along(additional_cols)) {
  new_col_name <- additional_cols[i]
  data <- data %>%
    mutate(!!new_col_name := !!sym(main_col) * !!sym(additional_cols[i]))
}

data %>% 
  group_by(category) %>% 
  pivot_longer(cols = 3:7) %>% 
  select(3:5) %>% 
  ungroup() %>% 
  group_by(name, category) %>% 
  summarise(v = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(per = round(v/sum(v)*100,2)) %>% 
  ungroup() %>% 
  filter(category != "none") %>% 
  ggplot(., aes(area = v, 
                fill = category, 
                label = paste(name, paste(per, "%", sep = ""),sep = "\n"),
                subgroup = category)) +
  geom_treemap(color = 'grey60') + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(color = 'black', alpha = 0.1, grow = T, 
                             place = 'center') +
  ggtitle('Share de búsqueda Categoría Seguros') +
  scale_fill_discrete(labels = c("EPS", "Salud", "SCTR", "SOAT", "Vehicular",
                                 "Viaje", "Vida")) +
  labs(fill = "Categoría") + 
  theme(plot.title = element_text(face = 'bold', size = 20))
  

