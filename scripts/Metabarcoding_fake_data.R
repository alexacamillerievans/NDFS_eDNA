## Metabarcoding methods comparison experiment
## 
##  Authors: 
##    Alexa Camilleri Evans, alexa.camillerievans@water.ca.gov
##    Eric Holmes, eric.holmes@water.ca.gov
##
##  Steps:
##    1) Develop fake community data
##    2) Set assumptions for biases from traditional and metabarcoding methods
##    3) Assess sampling design to determine if it is sufficient to detect differences
##    4) Cool stats and visualizations

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(vegan)
library(reshape2)

# 1) Create fake data -----------------------------------------------------

set.seed(1)
species_lookup <- data.frame(species = paste0("s", 1:15),
                             classification = rep(c("a", "b", "c"), each = 5),
                             rarity = runif(15))

sites <- c("sttd", "dwsc", "shr")
visits <- c("v1", "v2", "v3")
site_detection_prob <- c(0.6, 0.7, 0.2)
season_detection_prob <- c(0.9, 0.7, 0.5)

site_variance = .5
seas_variance = .1

truecomm <- data.frame(site = rep(sites, 3),
                      sample = rep(visits, each = 3),
                      site_prob = rep(site_detection_prob, 3),
                      season_prob = rep(season_detection_prob, each = 3))

for(s in species_lookup$species){

  truecomm[, ncol(truecomm) + 1] <- 1000 * 
                                 species_lookup[species_lookup$species == s, "rarity"] *
                                 (truecomm$site_prob * rnorm(9, 1, site_variance)) *
                                 (truecomm$season_prob * rnorm(9, 1, seas_variance))
  
  colnames(truecomm)[ncol(truecomm)] <- s
}

truecomm <- truecomm %>% replace(. < 0, 0)

# True comm visualization
truecommmelt <- reshape2::melt(truecomm, id.vars = c("site", "sample", "site_prob", "season_prob"),
                               value.name = "count", variable.name = "species")

truecommmelt <- merge(truecommmelt, species_lookup, by = "species")

ggplot(truecommmelt, aes(x = reorder(species, -count), y = count, fill = classification)) + 
  geom_bar(stat = "identity") +
  facet_grid(site ~ sample) + 
  labs(x = "Species", y = "Abundance") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2) Set assumptions for method biases ------------------------------------

tradcomm <- truecommmelt
# Assume class B is underrepresented in the data
tradcomm$count <- ifelse(tradcomm$classification == "b", tradcomm$count * 0.1, tradcomm$count)

metacomm <- truecommmelt
set.seed(1)
# Assume only 10 of the 15 identified species exist in the metabarcode library
metacomm <- metacomm[metacomm$species %in% species_lookup$species[sample(1:15, 10)],]

ggplot(tradcomm, aes(x = reorder(species, count), y = -count, fill = classification)) + 
  geom_bar(stat = "identity", fill = "brown") +
  geom_bar(data = metacomm, aes(y = count), stat = "identity", fill = "forestgreen") +
  facet_grid(site ~ sample) + 
  labs(x = "Species", y = "Abundance") + coord_flip() +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3) assess sampling design -----------------------------------------------


# 4) Stats and visualizations ---------------------------------------------

