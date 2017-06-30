### SVEsplit doktorat ###

# CILJ: tablice!

source("IvanP/!!!Doktorat/doktorat.code/01__recode.R")
source("IvanP/!!!Doktorat/doktorat.code/02__kotar_klaster.R")
source("IvanP/!!!Doktorat/doktorat.code/03__kriterij_construct.R")
source("IvanP/!!!Doktorat/doktorat.code/04__inglehart_index.R")
source("IvanP/!!!Doktorat/doktorat.code/05__quality_index.R")


sveST.6 <- sveST.5 %>%
  add_column(
    NEP.skala = NEP.complete$NEP.skala,
    ingl.skala = ingl.complete$ingl.skala,
    qual.skala = qual.complete$qual.skala
  )

rbind(qual.complete %>% summarise_all(mean, na.rm = TRUE),
      qual.complete %>% summarise_all(sd, na.rm = TRUE)
) %>% write.csv2("IvanP/!!!Doktorat/doktorat.tekst/OUT/qual.csv")

qual.alfa <- qual.complete %>% 
  select(-ends_with(".skala")) %>% 
  psych::alpha()

# r.cor	# Item whole correlation corrected for item overlap and scale reliability

qual.alfa$item.stats %>% write.csv2("IvanP/!!!Doktorat/doktorat.tekst/OUT/nep_alfa.csv")


NEP.complete %>% 
  select(-ends_with(".skala")) %>%
  var_label() %>% unname() %>% unlist()

is.na(qual.complete$qual.skala) %>% sum
