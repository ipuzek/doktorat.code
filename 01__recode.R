### SVEsplit doktorat ###

# CILJ: rekodiram i sređujem demografske varijable #
# spol; dob.10, dob.5; obraz; zanimanje #
# prihod (obični prihod kućanstva) #
# prihod.cont (prihod - kontinuirani) # 
# prihod.PC (prihod po članu kućanstva) #
# imovina.kucanstva #
# DODATNI: rast i okoliš #

library(stringr)
library(purrr)
library(labelled); library(forcats)
library(ggplot2)

library(broom); library(intubate)

library(tidyr); library(tibble); library(dplyr)

source("IvanP/R/pocetni.R")
source("IvanP/!!!Doktorat/doktorat.code/FUNS/transf_cont.R")


ntbt_ltabs <- ntbt_function_formula_data

haven::read_spss(
  "IvanP/!Istrazivanja/Split - kulturne potrebe/Podaci/web0903_13_lab_v1_encoding.sav"
  ) -> sveST

# recodes #


# SPOL --------------------------------------------------------------------

sveST %>%
  mutate(
    spol.num = dmg1 - 1,
    spol.num = labelled(spol.num, c(Muškarac = 0, Žena = 1)),
    spol = to_factor(spol.num)
  ) -> sveST  # %>% ntbt_ltabs(~spol+spol.num)

# DOB ---------------------------------------------------------------------

sveST %>% 
  mutate(
    dob.10 = cut(dmg2, include.lowest=TRUE,  right=TRUE,
                 breaks=c(16, 25, 35, 45, 55, 65, 86),
                 labels = c("16-25","26-35","36-45","46-55","56-65","66+"),
                 ordered_result = FALSE),
    dob.5 = cut(dmg2, include.lowest=TRUE,  right=TRUE,
                breaks=c(16, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 86),
                labels = c("16-20", "21-25", "26-30", "31-35", "36-40", "41-45", 
                           "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-86"),
                ordered_result = FALSE)
  ) -> sveST   # %>% ntbt_ltabs(~dmg2 + dob.10)


var_label(sveST$dob.10) <- "10-godišnje dobne skupine"
var_label(sveST$dob.5) <- "5-godišnje dobne skupine"


# OBRAZOVANJE -------------------------------------------------------------

sveST$obraz <- cut(sveST$dmg3, include.lowest=TRUE,  right=TRUE,
    breaks=c(1,3,4,6,7,8,9),
    labels = c("OŠ", "SŠ_3god", "SŠ_4god", "Viša", "Fakultet", "Mag+"),
    ordered_result = FALSE)


var_label(sveST$obraz) <- "Stupanj obrazovanja"

# ZANIMANJE ---------------------------------------------------------------

recode_factor(
  as.numeric(sveST$dmg6),
  `1` = "Briga o kućanstvu",
  `2` = "U procesu školovanja",
  `3` = "Nezaposlen/a",
  `4` = "U mirovini",
  `6` = "Samostalni i zaposleni stručnjaci",
  `7` = "Vlasnici poduzeća i obrtnici",
  `8` = "Vlasnici poduzeća i obrtnici",
  `9` = "Samostalni i zaposleni stručnjaci",
  `10` = "Menadžment (sve razine)",
  `11` = "Menadžment (sve razine)",
  `12` = "Ostali zaposlenici",
  `13` = "Ostali zaposlenici",
  `14` = "Ostali zaposlenici",
  `15` = "KV i NKV radnici",
  `16` = "KV i NKV radnici",
  `17` = "KV i NKV radnici",
  `98` = "NZBO"
) -> sveST$zanimanje
sveST$zanimanje <- na_if(sveST$zanimanje, "NZBO")


var_label(sveST$zanimanje) <- "Zanimanje"


# PRIHODI KUĆANSTVA -------------------------------------------------------

recode_factor(
  to_fac_drop(sveST$dmg11),
  "Bez prihoda u kućanstvu prošli mjesec" = "Do 2.000 kn",
  "Do 1000 kuna" = "Do 2.000 kn",
  "Od 1001 do 2000 kuna" = "Do 2.000 kn",
  "Od 2001 do 4000 kuna" = "2.001-4.000 kn",
  "Od 4001 do 6000 kuna" = "4.001-6.000 kn",
  "Od 6001 do 8000 kuna" = "6.001-8.000 kn",
  "Od 8001 do 10000 kuna" = "8.001-10.000 kn",
  "Od 10001 do 12000 kuna" = "10.001-14.000 kn",
  "Od 12001 do 14000 kuna" = "10.001-14.000 kn",
  "Od 14001 do 16000 kuna" = "14.001-18.000 kn",
  "Od 16001 do 18000 kuna" = "14.001-18.000 kn",
  "Od 18001 do 20000 kuna" = "18.001+ kn",
  "20001 kuna ili više" = "18.001+ kn",
  "Ne zna" = "NZBO",
  "Ne želi odgovoriti" = "NZBO"
) -> sveST$prihod

sveST$prihod <- na_if(sveST$prihod, "NZBO")


var_label(sveST$prihod) <- "Prihod kućanstva"

#

sveST$prih.tmp <- sveST$dmg11

sveST %>%
  add_value_labels(prih.tmp = c(
    "0 do 0" = 1,
    "1 do 1000" = 2,
    "20001 do 30000" = 13
  )) -> sveST


# prihod_cont

sveST %>%
  mutate(
    prihod.cont = transf_cont(prih.tmp, no_valid_labs = 13),
    prihod.cont = recode(prihod.cont,
                         `98` = NA_real_ ,
                         `99` = NA_real_),
    broj.odraslih = na_if(dmg12, 98),
    broj.djece = recode(as.numeric(dmg13), `98` = 0),
    prihod.PC = prihod.cont / (broj.odraslih + sqrt(broj.djece)),
    prihod.PC = as.numeric(prihod.PC)
  ) -> sveST # %>% ntbt_ltabs(~broj.djece)

sveST$prih.tmp <- NULL


var_label(sveST$prihod.cont) <- "Prihod - kontinuirani"
var_label(sveST$prihod.PC) <- "Prihod - po članu kućanstva"


# IMOVINA KUĆANSTVA -------------------------------------------------------
## u ovom slučaju, vrijednost indeksa je broj validnih odgovora ##

sveST <- sveST %>%

  mutate_at(vars(starts_with("dmg14_"), -dmg14_6), funs(v = isnt_na)) %>%
  mutate(imovina.kucanstva = dmg14_1_v + dmg14_2_v + dmg14_3_v + dmg14_4_v + dmg14_5_v) %>% 
  select(-dmg14_1_v, -dmg14_2_v, -dmg14_3_v, -dmg14_4_v, -dmg14_5_v)

var_label(sveST$imovina.kucanstva) <- "Imovina - indeks"


# DODATNI PREDIKTORI ----------------------------------------------------------

source("IvanP/!!!Doktorat/doktorat.code/01_1__dodatni_prediktori.R")
