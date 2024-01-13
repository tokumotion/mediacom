library(tidyverse); library(readxl); library(ggplot2); library(topicmodels)
library(treemapify)

#### Mapa ####

df1 <- read_excel(path = '~/Downloads/SearchKeywords-makro.com.co,tiendasjumbo.co,exito.com,alkosto.com,aratiendas.com-(170)-(2022_05-2023_07).xlsx', 
                  sheet = 2)
df2 <- read_excel(path = '~/Downloads/SearchKeywords-makro.com.co,d1.com.co,tiendasmetro.co,pricesmart.com-(170)-(2022_05-2023_07).xlsx', 
                  sheet = 2)

df1_n <- df1 %>% 
  select(1, 3, 8:12) %>% 
  pivot_longer(3:7) %>% 
  replace(is.na(.), 0) %>% 
  mutate(n = value * Traffic) %>% 
  group_by(name) %>% 
  summarise(sum_tr = sum(n), .groups = "drop")

df2_n <- df2 %>% 
  select(1, 3, 8:11) %>% 
  pivot_longer(3:6) %>% 
  replace(is.na(.), 0) %>% 
  mutate(n = value * Traffic) %>% 
  group_by(name) %>% 
  summarise(sum_tr = sum(n), .groups = "drop")

full_join(df1_n, df2_n, by = c("name", "sum_tr")) %>% 
  filter(!row_number() %in% c(7)) %>% 
  mutate(per = round(sum_tr/sum(sum_tr)*100,2)) %>% 
  ggplot(., aes(area = sum_tr, fill = name,
                label = paste(name, paste(per, "%", sep = ""),sep = "\n"))) +
  geom_treemap(color = 'grey60') + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  ggtitle('Share de tráfico total Categoría Supermercados') +
  scale_fill_discrete(labels = c("AlKosto", "Ara Tiendas", "D1", "Exito", 
                                 "Makro", "PriceSmart", "Tiendas Jumbo",
                                 'Tiendas Metro')) +
  labs(fill = "Supermercados") + 
  theme(plot.title = element_text(face = 'bold', size = 20))
 
full_join(df1 %>% 
  select(1, 3:5, 8:12) %>% 
  pivot_longer(5:9), 
  df2 %>% 
    select(1, 3:5, 8:11) %>% 
    pivot_longer(5:8) %>% 
    filter(name != "makro.com.co")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(n_org = Traffic * Organic * value, 
         n_paid = Traffic * Paid * value) %>% 
  group_by(name) %>% 
  summarise(traffic_org = sum(n_org), traffic_paid = sum(n_paid), 
            .groups = "drop") %>% 
  pivot_longer(cols = c('traffic_org', 'traffic_paid'), 
               names_to = 'type',
               values_to = 'traffic') %>% 
  group_by(type) %>% 
  mutate(per = round(traffic/sum(traffic)*100,2)) %>% 
  ungroup() %>% 
  ggplot(., aes(area = traffic, 
                fill = name, 
                label = paste(type, 
                              paste(per, "%", sep = ""),sep = "\n"),
                subgroup = name)) +
  geom_treemap(color = 'grey20', size = 0.2) + 
  geom_treemap_text(color = "white",
                    place = 'topleft',
                    size = 15) +
  geom_treemap_subgroup_border(color = 'grey60', size = 0.5) + 
  geom_treemap_subgroup_text(color = 'black', alpha = 0.2, grow = T, 
                             place = 'center') +
  ggtitle('Share de búsqueda Categoría Supermercado') +
  scale_fill_discrete(labels = c("AlKosto", "Ara", "D1", "Exito", "Makro",
                                 "Pricesmart", "Jumbo", "Metro")) +
  labs(fill = "Supermercado") + 
  theme(plot.title = element_text(face = 'bold', size = 20))sip, me di cuenta por su lomit despues

  

#### Time Series ####
df3 <- read_excel(path = '~/Downloads/SearchKeywords-makro.com.co,tiendasjumbo.co,exito.com,alkosto.com,aratiendas.com-(170)-(2022_05-2023_07).xlsx', 
                  sheet = 3)
df4 <- read_excel(path = '~/Downloads/SearchKeywords-makro.com.co,d1.com.co,tiendasmetro.co,pricesmart.com-(170)-(2022_05-2023_07).xlsx', 
                  sheet = 3)

full_join(df3 %>% 
            select(1, 2, 4:11) %>% 
            pivot_longer(6:10),
          df4 %>% 
            select(1, 2, 4:10) %>% 
            pivot_longer(6:9)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(trf = Traffic * value, 
         trf_org = Traffic * value * Organic, 
         trf_paid = Traffic * value * Paid) %>% 
  group_by(`Time Period`, name) %>% 
  summarise(traffic_org = sum(trf_org), traffic_paid = sum(trf_paid), 
            traffic = sum(trf), .groups = 'drop') %>% 
  pivot_longer(cols = c('traffic_org', 'traffic_paid', 'traffic'), 
               names_to = 'type',
               values_to = 'trf') %>% 
  filter(type != "traffic") %>% 
  ggplot(aes(x = `Time Period`, y = trf, fill = type)) +
  geom_area() + 
  facet_wrap(~ name, nrow = 2) +
  labs(fill = "Tipo de Tráfico") + 
  theme(plot.title = element_text(face = 'bold', size = 20)) +
  ggtitle('Histórico de Tráfico - Mayo 2022 hasta Agosto 2023') +
  scale_fill_discrete(labels = c("Tráfico Orgánico", "Tráfico Pago")) +
  scale_y_continuous(label = scales::comma) +
  labs(x = "Meses", y = "Tráfico")

full_join(df3 %>% 
            select(1, 2, 4:11) %>% 
            pivot_longer(6:10),
          df4 %>% 
            select(1, 2, 4:10) %>% 
            pivot_longer(6:9)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(trf = Traffic * value, 
         trf_org = Traffic * value * Organic, 
         trf_paid = Traffic * value * Paid) %>% 
  group_by(`Time Period`, name) %>% 
  summarise(traffic_org = sum(trf_org), traffic_paid = sum(trf_paid), 
            traffic = sum(trf), .groups = 'drop') %>% 
  pivot_longer(cols = c('traffic_org', 'traffic_paid', 'traffic'), 
               names_to = 'type',
               values_to = 'trf') %>% 
  filter(type == "traffic") %>% 
  ggplot(aes(x = `Time Period`, y = trf, fill = name)) +
  geom_area() + 
  labs(fill = "Supermercados") + 
  theme(plot.title = element_text(face = 'bold', size = 20)) +
  ggtitle('Histórico de Tráfico - Mayo 2022 hasta Agosto 2023') +
  scale_fill_discrete(labels = c("AlKosto", "Ara", "D1", "Exito", "Makro",
                                 "Pricesmart", "Jumbo", "Metro")) +
  scale_y_continuous(label = scales::comma) +
  labs(x = "Meses", y = "Tráfico")
#### Keywords ####
df5 <- read_excel(path = '~/Downloads/SearchKeywords-nonbrand.xlsx', 
                  sheet = 2)
df6 <- read_excel(path = '~/Downloads/SearchKeywords-nonbran2.xlsx', 
                  sheet = 2)

df7 <- full_join(df5, df6, by = "Search Terms") %>% 
  select(1, 3)

# Assuming your data is in a dataframe named df

# Normalize the traffic data
df7$Traffic.x <- scale(df7$Traffic.x)
df7 <- df7 %>% 
  filter(!is.na(x = Traffic.x))

# Convert your text data into numerical form using the text2vec package
library(text2vec)
it = itoken(df7$`Search Terms`, progressbar = FALSE)
v = create_vocabulary(it) 
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)

# Combine the dtm with the traffic data
df_final = cbind(dtm, df7$Traffic.x)

# Determine the optimal number of clusters (k) using the elbow method
wss <- (nrow(df_final)-1)*sum(apply(df_final,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df_final, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Run K-means clustering (replace 'k' with your chosen number of clusters)
set.seed(123) # Setting seed to reproduce results of random sampling
kmeans_result <- kmeans(df_final, centers = 4)

# Print the results
print(kmeans_result)
