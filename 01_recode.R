### SVEsplit doktorat ###

library(dplyr); library(tidyr); library(questionr); library(labelled)
library(ggplot2); library(scales)
library(stringr); library(magrittr)
library(broom)
source("/IvanP/R/pocetni.R")

transf_cont <- function(x, no_valid_labs, min = "auto", max = "auto", digits = 0) {
  
  if (identical(min, "auto") && identical(max, "auto")) {
    nejmz <- names(val_labels(x))[1:no_valid_labs]
    nejmz.brojke <- str_extract_all(nejmz, "[0-9]+")
    mini <- lapply(nejmz.brojke, function(x) x[1]) %>% unlist %>% as.numeric()
    maxi <- lapply(nejmz.brojke, function(x) x[2]) %>% unlist %>% as.numeric()
  } else {
    if (length(min) == length(max)) {
      mini <- min
      maxi <- max
      warning("auto not used - check min and max carefully", call. = FALSE)  
    } else stop("min-max lengths unequal")
  }
  
  set.seed(666)
  x <- as.numeric(x)                         # 0. input
  
  for (i in seq_along(1:no_valid_labs)) {    # 2. sequence
    
    x[x == i] <- runif(length(x[x == i]), min = mini[i], max = maxi[i])
    
  }
  
  round(x, digits = digits)                  # 3. output
  
}

sveST <- haven::read_spss("/IvanP/!Istrazivanja/Split - kulturne potrebe/Podaci/web0903_13_lab_v1_encoding.sav")

# recodes #


# SPOL --------------------------------------------------------------------

sveST$spol.num <- sveST$dmg1 - 1
val_labels(sveST$spol.num) <- c(Muškarac = 0, Žena = 1)

sveST$spol <- to_factor(sveST$spol.num)

# DOB ---------------------------------------------------------------------

sveST$dob.10 <- cut(sveST$dmg2, include.lowest=TRUE,  right=TRUE,
    breaks=c(16, 25, 35, 45, 55, 65, 86),
    labels = c("16-25","26-35","36-45","46-55","56-65","66+"),
    ordered_result = TRUE)

sveST$dob.5 <- cut(sveST$dmg2, include.lowest=TRUE,  right=TRUE,
                   breaks=c(16, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 86),
                   labels = c("16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-86"),
                   ordered_result = TRUE)

# OBRAZOVANJE -------------------------------------------------------------

sveST$obraz <- cut(sveST$dmg3, include.lowest=TRUE,  right=TRUE,
    breaks=c(1,3,4,6,7,8,9),
    labels = c("OŠ", "SŠ_3god", "SŠ_4god", "Viša", "Fakultet", "Mag+"),
    ordered_result = TRUE)

# ZANIMANJE ---------------------------------------------------------------

sveST$zanimanje <- recode_factor(as.numeric(sveST$dmg6),
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
                          `98` = "NZBO")
sveST$zanimanje <- na_if(sveST$zanimanje, "NZBO")
#frre(sveST$zanimanje)


# PRIHODI KUĆANSTVA -------------------------------------------------------

sveST$prihod <- recode_factor(to_fac_drop(sveST$dmg11),
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
                              "Ne želi odgovoriti" = "NZBO")
sveST$prihod <- na_if(sveST$prihod, "NZBO")

# pomoćni za transform
sveST$prih.tmp <- sveST$dmg11
names(val_labels(sveST$prih.tmp))[1] <- "0 do 0"
names(val_labels(sveST$prih.tmp))[2] <- "1 do 1000"
names(val_labels(sveST$prih.tmp))[13] <- "20001 do 30000"

# prihod_cont

sveST$prihod.cont <- transf_cont(sveST$prih.tmp, no_valid_labs = 13) %>%
  recode( `98` = NA_real_ ,
          `99` = NA_real_ )
sveST$prih.tmp <- NULL

sveST$broj.odraslih <- na_if(sveST$dmg12, 98)

sveST$broj.djece <- sveST$dmg13
sveST$broj.djece[sveST$broj.djece == 98] <- 0

sveST$prihod.PC <- sveST$prihod.cont / 
  (sveST$broj.odraslih + sqrt(sveST$broj.djece)) %>%
  as.numeric()

# EXPLORE
# qplot(sveST$prihod.cont, sveST$prihod.PC,
#       main = cor(sveST$prihod.cont, sveST$prihod.PC, use = "complete.obs"))


# IMOVINA KUĆANSTVA -------------------------------------------------------
## u ovom slučaju, vrijednost indeksa je broj validnih odgovora ##

sveST <- sveST %>%
  mutate_at(vars(starts_with("dmg14_"), -dmg14_6), funs(v = is.not.na)) %>%
  mutate(imovina.kucanstva = dmg14_1_v + dmg14_2_v + dmg14_3_v + dmg14_4_v + dmg14_5_v)

# EXPLORE
ggplot(sveST, aes(x = to_factor(imovina.kucanstva), y = prihod.PC)) +
  geom_boxplot() + 
  geom_jitter(alpha = .2) + ylim(c(0,10000))

ggplot(sveST, aes(x = obraz, y = prihod.cont)) +
  geom_boxplot() + 
  geom_jitter(alpha = .2) + ylim(c(0,20000))

# prihod.PC pegla učinak dobi na prihod
ggplot(sveST, aes(x = dmg2, y = prihod.PC, colour = to_factor(dmg1))) +
  geom_jitter(alpha = .2) + 
  ylim(c(0,10000)) + xlim(c(16,70)) +
  geom_smooth(method = "lm")




  
  

