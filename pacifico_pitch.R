library(tidyverse); library(treemapify); library(wordcloud)
library(topicmodels); library(tm); library(readxl); library(RColorBrewer)
library(tidytext); library(foreach); library(doParallel)

#### map ####
kws_desktop <- read_xlsx(path = '~/Downloads/info_pacifico/kws_desktop.xlsx', 
                         sheet = 2, trim_ws = T)
kws_mobile <- read_xlsx(path = '~/Downloads/info_pacifico/kws_mobile.xlsx', 
                         sheet = 2, trim_ws = T)
df_1 <- kws_desktop %>% 
  select(1, 3, 8:12)
df_1$fuente <- 'desktop'
  
df_2 <- kws_mobile %>% 
  select(1, 3, 6:10)
df_2$fuente <- 'mobile'

data <- bind_rows(df_1, df_2)

Hello! How can I assist you today with your R programming?
# To find clusters in the Search Terms keywords based on the traffic column,
# we can use the k-means clustering algorithm.

# First, we need to extract the relevant columns from the data dataframe.
# Assuming the columns are named "SearchTerms" and "Traffic":

search_terms <- data$SearchTerms
traffic <- data$Traffic

# Next, we can create a data matrix using the traffic column.
# We will use the traffic column as the only variable for our clustering.

data_matrix <- matrix(traffic, ncol = 1)

# Now, let's perform the k-means clustering using the kmeans() function.

k <- 3  # number of clusters
kmeans_result <- kmeans(data_matrix, centers = k)

# Finally, we can retrieve the cluster assignments for each search term.

cluster_assignments <- kmeans_result$cluster

# We can now analyze the clusters and the search terms they contain.
    
data %>% 
  pivot_longer(cols = 3:7) %>% 
  mutate(v = Traffic * value) %>% 
  select(3:6) %>% 
  group_by(name, fuente) %>% 
  summarise(z = sum(v, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(per_fuente = round(z/sum(z)*100, 2)) %>% 
  group_by(name) %>% 
  mutate(x = sum(z)) %>% 
  ungroup() %>% 
  mutate(per_name = round(x/sum(x)*200, 2)) %>% 
  ggplot(., aes(area = z, 
                fill = name, 
                label = paste(fuente, paste(per_fuente, "%", sep = ""), 
                              sep = "\n"),
                subgroup = paste(name, paste(per_name, "%", sep = ""), 
                                 sep = '\n'))) +
  geom_treemap(color = 'grey60') + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(color = 'black', alpha = 0.15, grow = T,
                             place = 'center', size = 20) +
  ggtitle('Share de búsqueda Categoría Seguros Vehiculares') +
  scale_fill_discrete(labels = c('Pacifico', 'Interseguro', 'Rimac', 
                                 'La Positiva', 'Mapfre')) +
  labs(fill = "Marcas") + 
  theme(plot.title = element_text(face = 'bold', size = 20))

#### linea de tiempo ####
kws_desktop_tl <- read_xlsx(path = '~/Downloads/info_pacifico/kws_desktop.xlsx', 
                         sheet = 3, trim_ws = T)
kws_mobile_tl <- read_xlsx(path = '~/Downloads/info_pacifico/kws_mobile.xlsx', 
                        sheet = 3, trim_ws = T)

df_1_tl <- kws_desktop_tl %>% 
  select(1, 2, 4, 7:11)
df_1_tl$fuente <- 'desktop'

df_2_tl <- kws_mobile_tl %>% 
  select(1, 2, 4:9)
df_2_tl$fuente <- 'mobile'

data <- bind_rows(df_1_tl, df_2_tl)

data %>% 
  pivot_longer(cols = 4:8) %>% 
  mutate(v = Traffic * value) %>% 
  select(3:6) %>% 
  group_by(name, fuente) %>% 
  summarise(z = sum(v, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(per_fuente = round(z/sum(z)*100, 2)) %>% 
  group_by(name) %>% 
  mutate(x = sum(z)) %>% 
  ungroup() %>% 
  mutate(per_name = round(x/sum(x)*200, 2))