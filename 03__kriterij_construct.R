# skala
#
source("IvanP/!!!Doktorat/doktorat.code/01_recode.R")
source("IvanP/!!!Doktorat/doktorat.code/FUNS/clone_R.R")

recode_98 <- function(x) {
  x <- as.numeric(x)
  recode(x,`98` = NA_real_)
  }

library(psych)
library(questionr)
library(tibble)

nmz <- sveST %>% select(vrij14x01:vrij14x10) %>% var_label() %>% unlist() %>% unname()

# BASIC CHECKS
# sveST %>%
#   select(vrij14x01:vrij14x09) %>% 
#   mutate_all(recode_98) %>%
#   # lapply(table, useNA = "always")
#   cor(use = "pairwise.complete.obs")

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
  
sveST %>%
  select(vrij14x01 : vrij14x09) %>%
  mutate_at(vars(vrij14x01, vrij14x05, vrij14x07, vrij14x08), rec_okreni) %>% 
  rename(
    vrij14x01_r = vrij14x01,
    vrij14x05_r = vrij14x05,
    vrij14x07_r = vrij14x07,
    vrij14x08_r = vrij14x08)

var_label(sveST.NEP$vrij14x01_r) <- paste0("OBR_", var_label(sveST.NEP$vrij14x01))
var_label(sveST.NEP$vrij14x05_r) <- paste0("OBR_", var_label(sveST.NEP$vrij14x05))
var_label(sveST.NEP$vrij14x07_r) <- paste0("OBR_", var_label(sveST.NEP$vrij14x07))
var_label(sveST.NEP$vrij14x08_r) <- paste0("OBR_", var_label(sveST.NEP$vrij14x08))
#

for (i in c("vrij14x01_r", "vrij14x05_r", "vrij14x07_r", "vrij14x08_r")) {
  class(sveST.NEP[[i]]) <- "labelled"
}

NEP.cestice <- sveST.NEP %>%
  select(
    vrij14x01_r,
    vrij14x02,
    vrij14x03,
    vrij14x04,
    vrij14x05_r,
    vrij14x06,
    vrij14x07_r,
    vrij14x08_r,
    vrij14x09)

psych::alpha(NEP.cestice)




NEP.skala <- mutate_all(NEP.cestice, recode_98) %>% rowSums(na.rm = TRUE)
var_label(NEP.skala) <- "NEP skala"







sveST.NEP.skala <- add_column(sveST.NEP, NEP.skala)
rm(NEP.skala)

parametri <- c(
  min = mean(sveST.NEP.skala$NEP.skala),
  esde = sd(sveST.NEP.skala$NEP.skala)
  )


ggplot(sveST.NEP.skala, aes(x = NEP.skala)) +
  geom_histogram(aes(y = ..density..), bins = 39, fill = "gray52") +
  stat_function(fun = dnorm,
                n = 700, args = list(mean = parametri["min"], sd = parametri["esde"]),
                colour = "blue")

NEP.cestice %>%
  select(-vrij14x09) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  pca(nfactors = 2, rotate = "oblimin") -> faktorica.oblimin

faktorica.oblimin %>% str

NEP.cestice %>% var_label()

  
# shared histogram in ggplot ----------------------------------------------
scopes.bg <- select(scopes.2015, - country)

scopes.2015 %>% 
  group_by(country) %>%
  mutate(gr_mean_scale.gend = mean(scale.gend, na.rm = TRUE)) %>%
  ggplot(aes(x = scale.gend)) +
  geom_bar(data = scopes.bg, fill = "grey") +
  geom_bar(aes(fill = country)) +
  geom_vline(aes(xintercept =  gr_mean_scale.gend), colour = "darkgrey") +
  facet_wrap(~ country) +
  guides(fill = FALSE)  # to remove the legend


mean_se.df <- scopes.2015 %>%
  split(.$country) %>%              # group_by(country) %>% 
  map("scale.gend") %>%             # do(mean_se(.$scale.gend))
  map_df(mean_se, .id = "country")  


scopes.2015 %>%
  ggplot(aes(x = scale.gend)) +
  geom_histogram(data = scopes.bg, fill = "grey", binwidth = 2) +
  geom_histogram(aes(fill = country), binwidth = 2) +
  lajna(mean_se.df, size = 2) +
  lajna_min(mean_se.df) + lajna_max(mean_se.df) +
  facet_grid(country ~ .) +
  guides(fill = FALSE)  # to remove the legend

lajna <- function(data, size) {
  geom_vline(data = data, aes_string(xintercept = "y"), 
             alpha = .7, colour = "darkgrey", size = size) }
lajna_min <- function(data) {
  geom_vline(data = data, aes_string(xintercept = "ymin"),
             colour = "red", size = .2) }
lajna_max <- function(data) {    
  geom_vline(data = data, aes_string(xintercept = "ymax"),
             colour = "red", size = .2) }


