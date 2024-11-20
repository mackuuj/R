here::i_am("R/clustering.R")
library(here)
library(data.table)
library(dplyr)
library(countrycode)
library(cluster)
library(factoextra)
library(ggalluvial)
source(here("R/country_classification.R"))
base_data <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))

# What about the source/greeness of energy production?
# TODO: employment?
# TODO: Green products

first_year <- 2014
last_year <- 2018

reduced_data <- base_data %>% 
  filter(
    year<=last_year, year>=first_year
  ) %>% 
  mutate(population = population*1000) %>%
  mutate(# Here the normalization is done
    GWP_trade_normed = (GWP_Imports - GWP_Exports)/population, # GWP net imports per capita
    GWP_normed = GWP_pba/population, # GWP per capita
    ValueAdded_normed = ValueAdded_pba/population, # ValueAdded per capita
    EnergyProduction_normed = PrimaryEnergyProduction/population, # PrimaryEnergyProduction per capita
    EnergyConsumption_normed = FinalEnergyConsumption / population, # FinalEnergyConsumption per capita
    EnergyExports_normed = EnergyNetTrade / population, # EnergyNetExports per capita
    GreenPatents_normed = GreenPatents_n / (population/1000000) # Green patents per million capita
  ) %>%
  select(all_of(c("country")), contains("_normed")) %>%
  summarise(across(.cols = everything(), .fns = mean),.by = "country") %>% 
  mutate(country=countrycode(country, "iso3c", "country.name"))

# Coerce into data.frame structure:
reduced_data_df <- as.data.frame(reduced_data)
rownames(reduced_data_df) <- reduced_data_df$country
reduced_data_df$country <- NULL

# Scale data
reduced_data_scaled <- scale(reduced_data_df)

# Compute dissimilarity matrix
reduced_data_dist <- dist(reduced_data_scaled, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc_complete <- hclust(reduced_data_dist, method = "complete" )
hc_complete_agnes <- agnes(reduced_data_dist, method = "complete")

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(reduced_data_dist, method = x)$ac
}
map_dbl(m, ac)
# 
hc_ward_agnes <- agnes(reduced_data_dist, method = "ward")
# pltree(hc_complete_agnes, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# TODO: HIER BIST DU BEI DER ÜBERSETZUNG:

fviz_dend(hc_ward_agnes,
          main = "Titel",
          xlab = "Countries", ylab = "",
          k = 4, # Cut in groups
          cex = 0.75, # label size
          rect = TRUE, # Add rectangle around groups
          rect_fill = TRUE,
          color_labels_by_k = TRUE, # color labels by groups
          # k_colors = RColorBrewer::brewer.pal(n_groups, "Dark2"),
          # rect_border = RColorBrewer::brewer.pal(n_groups, "Dark2"),
          horiz = TRUE
)

# Visualization of the results
clusters_obtained <- cutree(as.hclust(hc_ward_agnes), k = 4)
clusters_obtained_tb <- tibble(
  "country" = names(clusters_obtained),
  "Ecological model" = as.character(clusters_obtained),
  "Development model" = get_country_classification(
    countrycode(country, "country.name", "iso3c"), "jee")
  ) %>% 
  pivot_longer(
    cols = -country, names_to = "group", values_to = "code"
    )%>%
  mutate(time_=ifelse(group=="Ecological model", 2, 1))

# Relationship to development models

ggplot(clusters_obtained_tb,
       aes(x = group, stratum = code, alluvium = country,
           fill = code, label = code)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray", curve_type = "linear") +
  geom_stratum() +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters") +
  theme_bw()



# NOTES FROM STRUC CHANGE--------------

# Conduct the clustering and create the plot object =========================
work_data_v1 <- select(work_data_v1, -country)
clustering_object <- work_data_v1 %>%
  get_diff_matrix(raw_dat = FALSE) %>% # Scale the data
  agnes(method = "ward") # Compute hierachical clustering
dendo_plot <- fviz_dend(clustering_object,
                        main = dendo_title,
                        xlab = "Countries", ylab = "",
                        k = n_groups, # Cut in groups
                        cex = 0.75, # label size
                        rect = TRUE, # Add rectangle around groups
                        rect_fill = TRUE,
                        color_labels_by_k = TRUE, # color labels by groups
                        k_colors = RColorBrewer::brewer.pal(n_groups, "Dark2"),
                        rect_border = RColorBrewer::brewer.pal(n_groups, "Dark2"),
                        horiz = TRUE
)
sub_grp <- cutree(as.hclust(clustering_object), k = n_groups)

# Create the factor map =======================================================  
factor_map <- fviz_cluster(list(
  data = get_diff_matrix(work_data_v1, raw_dat = T),
  cluster = sub_grp
),
repel = TRUE, # Avoid label overlapping
show.clust.cent = TRUE, # Show cluster centers
palette = "jco", # Color palette see ?ggpubr::ggpar
ggtheme = theme_minimal(),
main = factor_title
)

