### SVEsplit doktorat ###

# CILJ: mali DF s indeksom kvalitete # qual.complete #
# CILJ: i rekodiranim česticama koje ga čine #

source("IvanP/!!!Doktorat/doktorat.code/03__kriterij_construct.R")

qual <- sveST %>% 
  select(qol1x01 : qol1x14) %>% 
  na_if(98)

qual %>% var_label() %>% unlist() %>% unname()



# # BASIC CHECKS
# qual %>%
#   # mutate_all(na_if, y = 98) %>%
#   sapply(table, useNA = "always")
#   # cor(use = "pairwise.complete.obs")
# 
# # nice histograms
# qual %>%
#   tidyr::gather(kljuc, velju, everything()) %>%
#   # mutate(velju = na_if(velju, 98)) %>%
#   ggplot(aes(velju)) +
#   geom_bar() +
#   facet_grid(kljuc ~ .)

# šira selekcija, samo tematski i po distribucijama
qual.2 <- select(qual, -qol1x03, -qol1x10, -qol1x11)

kratka.imena <- c("zrak","buka.promet","smrad","kriminal","smece","guzva","zp.nedostatak","zp.neodrzavanje","setnja","ulice.neodrzavanje","kom.infrastruktura")
# identical(length(names(qual.2)), length(kratka.imena))

cor(qual.2, use = "complete.obs") %>% 
  corrplot::corrplot()

# ALPHA #
psych::alpha(qual.2)


# FAKTORICA #
library(GPArotation)
qual.2 %>%
  # select(-) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  pca(nfactors = 2, rotate = "varimax")

# FAKTORICA # split by klaster
faktorica.po.klasterima <-
  qual.2 %>%
  add_column(klaster = sveST$klaster) %>%
  split(.$klaster) %>%
  lapply(select, -klaster) %>%
  lapply(cor, y = NULL, use = "complete.obs") %>%
  lapply(pca, nfactors = 2, rotate = "varimax")

map(faktorica.po.klasterima, "loadings") %>%
  map(round, 2) %>% 
  map(as.data.frame.matrix, .id = "klaster") %>% 
  map(add_column, cestice = kratka.imena, .before = 1) %>% 
  lapply(arrange, -RC1, -RC2)

# faktorska po kvartovima ima puno smisla
# vežu se različiti problemi. trebam novu podjelu.

alfa.po.klasterima <-
  qual.2 %>%
  add_column(klaster = sveST$klaster) %>%
  split(.$klaster) %>%
  lapply(select, -klaster) %>%
  lapply(psych::alpha)

alfa.po.klasterima %>% 
  map("total") %>% 
  sapply(function(x) round(x[["std.alpha"]], 2))

# KUL #

qual.skala <- rowSums(qual.2, na.rm = TRUE)
var_label(qual.skala) <- "Skala percepcije kvalitete okoliša"

qual.complete <- add_column(qual.2, qual.skala) %>% as_data_frame()


# # PLOT
# 
# parametri <- c(
#   min = mean(qual.complete$qual.skala),
#   esde = sd(qual.complete$qual.skala)
#   )
# 
# 
# ggplot(qual.complete, aes(x = qual.skala)) +
#   geom_histogram(aes(y = ..density..), bins = 39, fill = "gray52") +
#   stat_function(fun = dnorm,
#                 n = 700, args = list(mean = parametri["min"], sd = parametri["esde"]),
#                 colour = "blue")
# 
