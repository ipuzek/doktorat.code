### SVEsplit doktorat ###

# CILJ: mali DF sa konačnom NEP skalom # NEP.complete #
# CILJ: i rekodiranim / obrnutim česticama koje ju čine #

source("IvanP/!!!Doktorat/doktorat.code/02__kotar_klaster.R")
source("IvanP/!!!Doktorat/doktorat.code/FUNS/clone_R.R")

# recode_98 <- function(x) {
#   x <- as.numeric(x)
#   recode(x,`98` = NA_real_)
#   }

library(questionr)

nmz <- sveST %>% select(vrij14x01:vrij14x09) %>% var_label() %>% 
  unlist() %>% unname()

NEP.1 <- sveST %>% select(vrij14x01:vrij14x09)

# BASIC CHECKS
# NEP.1 %>% 
#   mutate_all(na_if, y = 98) %>%
#   sapply(table, useNA = "always")
#   cor(use = "pairwise.complete.obs") %>% 
#   corrplot::corrplot()

# nice histograms
# NEP.1 %>% 
#   tidyr::gather(kljuc, velju, everything()) %>%
#   mutate(velju = na_if(velju, 98)) %>% 
#   ggplot(aes(velju)) +
#   geom_bar() +
#   facet_grid(kljuc ~ .)

# NEP skala recodes -------------------------------------------------------

rec_okreni <- function(x) {
  
  x.recoded <- sjmisc::rec(x,
                              
                              rec = " 1=5 [rec_uopće se ne slažem];
                              2=4 [rec_ne slažem se];
                              3=3 [niti se slažem niti se ne slažem];
                              4=2 [rec_slažem se];
                              5=1 [rec_u potpunosti se slažem];
                              98 = NA ",
    
    var.label = "OBRNUTE")
  
  return(x.recoded)

  }
  
NEP.2 <- NEP.1 %>% 
  mutate_at(vars(vrij14x01, vrij14x05, vrij14x07, vrij14x08), rec_okreni) %>% 
  rename(
    vrij14x01_r = vrij14x01,
    vrij14x05_r = vrij14x05,
    vrij14x07_r = vrij14x07,
    vrij14x08_r = vrij14x08) %>% 
  
  mutate_all(na_if, y = 98)

var_label(NEP.2$vrij14x01_r) <- paste0("OBR_", var_label(NEP.1$vrij14x01))
var_label(NEP.2$vrij14x05_r) <- paste0("OBR_", var_label(NEP.1$vrij14x05))
var_label(NEP.2$vrij14x07_r) <- paste0("OBR_", var_label(NEP.1$vrij14x07))
var_label(NEP.2$vrij14x08_r) <- paste0("OBR_", var_label(NEP.1$vrij14x08))
#

for (i in c("vrij14x01_r", "vrij14x05_r", "vrij14x07_r", "vrij14x08_r")) {
  class(NEP.2[[i]]) <- "labelled"
}


# ALPHA #
psych::alpha(NEP.2)


# FAKTORICA #
library(GPArotation)
NEP.2 %>%
  # select(-) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  psych::pca(nfactors = 2, rotate = "varimax")


# KONAČNA SKALA #
NEP.skala <- rowSums(NEP.2, na.rm = TRUE)
var_label(NEP.skala) <- "NEP skala"

NEP.complete <- add_column(NEP.2, NEP.skala)
rm(NEP.skala)




# PLOT

# 
# parametri <- c(
#   min = mean(NEP.complete$NEP.skala),
#   esde = sd(NEP.complete$NEP.skala)
#   )
# 
# 
# ggplot(NEP.complete, aes(x = NEP.skala)) +
#   geom_histogram(aes(y = ..density..), bins = 39, fill = "gray52") +
#   stat_function(fun = dnorm,
#                 n = 700, args = list(mean = parametri["min"], sd = parametri["esde"]),
#                 colour = "blue")
