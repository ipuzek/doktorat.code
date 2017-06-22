### MIRKO tablice

library(tibble)
library(knitr)
library(forcats)
library(questionr)


# source("IvanP/R/pocetni.R")
# source("IvanP/!!!Doktorat/doktorat.code/01_recode.R", encoding = "UTF-8")

sveST$svi <- as.factor("%")
sveST$swi <- as.factor("Cijeli uzorak")

### razlika u najvažnijoj karakteristici odgoja izmežu mužke i ženske djece.

sveST$odgoj.diff <-  sveST$vrij4abx1 - sveST$vrij4abx2
# table(sveST$odgoj.diff, useNA = "always")

sveST$odgoj.mz <- if_else(sveST$odgoj.diff == 0, "Bez razlike u odgoju djece",
        if_else(sveST$odgoj.diff < -87, NA_character_,
                if_else(sveST$odgoj.diff > 88, NA_character_, "Različit odgoj djece")))

sveST$odgoj.mz <- factor(sveST$odgoj.mz)

var_label(sveST$odgoj.mz) <- "Odgoj - razlika muško-žensko dijete"

sveST$odgoj.diff <- NULL

###

source("IvanP/!!!Doktorat/doktorat.code/FUNS/clone_R.R", encoding = "UTF-8")

vrij3.lbz <- select(sveST, num_range("vrij3x", 1:11, width = 2)) %>% var_label()


select(sveST, num_range("vrij3x", 1:11, width = 2)) %>%
  mutate_all(to_factor) %>% 
  map_df(~ fct_relevel(.x,
                         c("Nevažno",
                           "Niti važno niti nevažno",
                           "Važno",
                           "Ne zna - Bez odgovora"))) %>% 

  
  set_var_labs(vrij3.lbz) %>% 
  
  clone_R %>%
  
  cbind(sveST, .) -> sveST.2
  
# # check
# cor.1 <- select(sveST.2, num_range("vrij3x", 1:11, width = 2)) %>%
#   mutate_all(as.numeric) %>%
#   mutate_all(na_if, y = 8) %>% # summary()
#   cor(use = "pairwise.complete.obs")
# 
# cor.2 <- select(sveST.2, vrij3x01_R : vrij3x11_R) %>%
#   mutate_all(as.numeric) %>%
#   mutate_all(na_if, y = 4) %>% # summary()
#   cor(use = "pairwise.complete.obs")
# 
# sum(cor.1 - cor.2)


labelled_to_sorted_factor <- function(x) to_factor(x) %>% fct_infreq()

gt.lbz <- select(sveST, gt1 : gt2b) %>% var_label()


select(sveST, gt1 : gt2b) %>%
  mutate_all(labelled_to_sorted_factor) %>% 

  set_var_labs(gt.lbz) %>% clone %>% 
  
  cbind(sveST.2, .) -> sveST.3


# INGLEHART ---------------------------------------------------------------
# inglehart recode bejzik

recode_inglehart <- function(x) {
  to_factor(x) %>%
  fct_collapse(
    mat = c(
      "Održavanje reda u državi",
      "Borba za radna mjesta",
      "Visok stupanj ekonomskog rasta",
      "Osiguranje obrane zemlje"
    ),
    postmat = c(
      "Sudjelovanje ljudi u važnim vladinim odlukama",
      "Zaštita slobode govora",
      "Uljepšavanje gradova i sela"
    ),
    NZBO = c("Ne znam", "Bez odgovora")
  )
}

select(sveST.3, vrij5a : vrij5b) %>% 
  mutate_all(recode_inglehart) %>%
  clone %>%
  cbind(sveST.3, .) -> sveST.4


# inglehart tipovi

sveST.4$post.mat <- "mješoviti tip"
sveST.4$post.mat[sveST.4$vrij5a_R == "mat" & sveST.4$vrij5b_R == "mat"] <- "materijalist"
sveST.4$post.mat[sveST.4$vrij5a_R == "postmat" & sveST.4$vrij5b_R == "postmat"] <- "postmaterijalist"
sveST.4$post.mat <- factor(sveST.4$post.mat, levels = c("materijalist", "mješoviti tip", "postmaterijalist"))
var_label(sveST.4$post.mat) <- "Tip: mat-mješoviti-postmat"

# table(sveST.4$post.mat)

fct_collapse_nzbo <- function (x) {
  
  fct_collapse(x, NZBO = c("Ne zna", "Ne znam", "Bez odgovora"))
  
}

sveST.4$vrij6_R <- sveST.4$vrij6 %>% to_factor %>% fct_collapse_nzbo
         
# ltabs(~ vrij6, sveST.4)
# ffre(sveST.4$vrij6, levels = "prefixed")

isnt_nan <- function(x) {!is.nan(x)}

sveST.5 <- sveST.4 %>%

  mutate_at(vars(starts_with("vrij8_"), -vrij8_8, -vrij8_9), funs(vVRIJ8 = isnt_nan)) %>%
  
  mutate(indeks.tolerancije = 
           vrij8_1_vVRIJ8 + 
           vrij8_2_vVRIJ8 + 
           vrij8_3_vVRIJ8 + 
           vrij8_4_vVRIJ8 + 
           vrij8_5_vVRIJ8 + 
           vrij8_6_vVRIJ8 + 
           vrij8_7_vVRIJ8) %>% 
  
  select(-ends_with("vVRIJ8"))

# table(sveST.5$indeks.tolerancije, useNA = "always")
# ako je vrij8_8 jednak 8, index treba biti jednak 0.5

sveST.5$indeks.tolerancije[sveST.5$indeks.tolerancije == 0] <- NA
sveST.5$indeks.tolerancije <- sveST.5$indeks.tolerancije + 1
sveST.5$indeks.tolerancije[sveST.5$vrij8_8 == 8] <- 1
sveST.5$indeks.tolerancije[is.na(sveST.5$indeks.tolerancije)] <- 0

sveST.5$indeks.tolerancije <- 
  as.character(sveST.5$indeks.tolerancije) %>% 
  fct_collapse(`Niti jedna grupa nije nepoželjna` = "0",
               `Ne zna je li koja grupa nepoželjna` = "1",
               `Jedna grupa je nepoželjna` = "2",
               `Više od jedne grupe je nepoželjno` = c(as.character(3:8))
               )

var_label(sveST.5$indeks.tolerancije) <- "Indeks tolerancije"

###

recode_98 <- function(x) {
  x <- as.numeric(x)
  recode(x,
         `98` = NA_real_)
  
}

recode_rod <- function(x) {
  x <- as.numeric(x)
  
  recode(x,
         `1` = 5,
         `2` = 4,
         `4` = 2,
         `5` = 1)
  
}


sveST.6 <- sveST.5 %>%
  
  mutate_at(vars(vrij9x1, vrij9x3, vrij9x4, vrij9x6), funs(miss = recode_98)) %>%
  
  mutate_at(vars(vrij9x1_miss, vrij9x3_miss, vrij9x6_miss), funs(vVRIJ9 = recode_rod)) %>%
  
  mutate(indeks.rodne.ravnopravnosti = 
           vrij9x1_miss_vVRIJ9 + 
           vrij9x3_miss_vVRIJ9 +
           vrij9x4_miss +
           vrij9x6_miss_vVRIJ9) %>% 
  
  select(-ends_with("_miss"),
         -ends_with("vVRIJ9"),
         -vrij9x4_miss)

#

sveST.6$vrij10_R <- 
  sjmisc::rec(sveST.6$vrij10,
              
    recodes = " 2=0 [Ekonomski rast i radna mjesta su važnija];
                1=1 [Zaštita okoliša je važnija];
                3=98 [NZBO] ",
    
    var.label = "Okoliš vs ekonomski rast")

class(sveST.6$vrij10_R) <- "labelled"

#

sveST.6$vrij12_R <- 
  sjmisc::rec(sveST.6$vrij12,
              
    recodes = " 1=0 [Ljudi se mogu obogatiti samo na račun drugih.];
                2=1 [Bogatstvo se može povećavati tako da ima dovoljno za sve.];
                8=98 [NZBO] ",
    
    var.label = "Bogatstvo za sve VS zero-sum")

class(sveST.6$vrij12_R) <- "labelled"


# NEP skala recodes -------------------------------------------------------

rec_okreni <- function(x.df) {
  
  x.df.recoded <- sjmisc::rec(x.df,
              
    recodes = " 1=5 [rec_uopće se ne slažem];
                2=4 [rec_ne slažem se];
                3=3 [niti se slažem niti se ne slažem];
                4=2 [rec_slažem se];
                5=1 [rec_u potpunosti se slažem];
                98 = NA ",
    
    var.label = "OBRNUTE")
  
  return(x.df.recoded)

  }
  
sveST.7 <- sveST.6 %>%
  select(vrij14x01, vrij14x05, vrij14x07, vrij14x08) %>%
  rec_okreni %>% 
  cbind(sveST.6, .)

var_label(sveST.7$vrij14x01_r) <- paste0("OBR_", var_label(sveST.7$vrij14x01))
var_label(sveST.7$vrij14x05_r) <- paste0("OBR_", var_label(sveST.7$vrij14x05))
var_label(sveST.7$vrij14x07_r) <- paste0("OBR_", var_label(sveST.7$vrij14x07))
var_label(sveST.7$vrij14x08_r) <- paste0("OBR_", var_label(sveST.7$vrij14x08))
#

for (i in c("vrij14x01_r", "vrij14x05_r", "vrij14x07_r", "vrij14x08_r")) {
  class(sveST.7[[i]]) <- "labelled"
}

NEP.cestice <- sveST.7 %>%
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

NEP.skala <- mutate_all(NEP.cestice, recode_98) %>% rowSums(na.rm = TRUE)
var_label(NEP.skala) <- "NEP skala"

miss_args <- function(x.df) {
  
  recode_na_to_1 <- function(x) if_else(is.na(x), 1, 0)

  mutate_all(x.df, recode_na_to_1) %>% rowSums()
  
}

NEP.miss  <- mutate_all(NEP.cestice, recode_98) %>% miss_args()

sveST.7.NEP <- add_column(sveST.7,
                          NEP.skala, NEP.miss)


# rel

sveST.7.NEP$rel1_r <- sveST.7.NEP$rel1 %>% to_factor() %>% fct_collapse(NZBO = c("Ne zna", "Ne želi odgovoriti"))
freq(sveST.7.NEP$rel2)

sveST.7.NEP$rel2_r <- sveST.7.NEP$rel2 %>% 
  to_factor() %>% 
  fct_recode(`Gotovo nikad ili nikad` = "Rjeđe od toga - nikada",
             NZBO = "Ne zna - bez odgovora")


# neformalna politika # TODO = bind(pp1x, sveST)

pp1x <- sveST.7.NEP %>% 
  select(starts_with("pp1x")) %>% 
  mutate_all(to_factor) %>% 
  map_df(~fct_recode(.x, `Već sam to radio-la` = "Već sam to radio - la")) %>% 
  map_df(~fct_collapse(.x, NZBO = "Ne znam", "Bez odgovora"))
  
pp1x.lab <- copy_labels(from = select(sveST.7.NEP, starts_with("pp1x")),
            to   = pp1x)

names(pp1x.lab) %<>%  paste0("_r")

sveST.8 <- cbind(sveST.7.NEP, pp1x.lab)

# glazbeni žanrovi

kp1x <- sveST.7.NEP %>% 
  select(starts_with("kp1x")) %>% 
  mutate_all(to_factor) %>% 
  map_df(~fct_collapse(.x, NZBO = "Ne zna - bez odgovora", "8"))

kp1x.lab <- copy_labels(from = select(sveST.7.NEP, starts_with("kp1x")),
            to   = kp1x)

names(kp1x.lab) %<>%  paste0("_r")

sveST.8.1 <- cbind(sveST.8, kp1x.lab)

# slobodno vrijeme

kp2x <- sveST.7.NEP %>% 
  select(starts_with("kp2x")) %>% 
  mutate_all(to_factor) %>%
  map_df(~fct_recode(.x, NZBO = "Ne zna - bez odgovora"))

kp2x.lab <- copy_labels(from = select(sveST.7.NEP, starts_with("kp2x")),
            to   = kp2x)

names(kp2x.lab) %<>%  paste0("_r")

sveST.8.2 <- cbind(sveST.8.1, kp2x.lab)

#
# kulturne priredbe

kp3x <- sveST.7.NEP %>% 
  select(starts_with("kp3x")) %>% 
  mutate_all(to_factor) %>%
  map_df(~fct_recode(.x, NZBO = "Ne zna - bez odgovora"))

kp3x.lab <- copy_labels(from = select(sveST.7.NEP, starts_with("kp3x")),
            to   = kp3x)

names(kp3x.lab) %<>%  paste0("_r")

sveST.8.3 <- cbind(sveST.8.2, kp3x.lab)

#
# čitanje knjiga
# NE BINDATI!
kp4 <- sveST.7.NEP %>% 
  select(starts_with("kp4")) %>%
  mutate_all(to_factor)

#  

# ružni recode index čitanja ----------------------------------------------
kp4$kp4a_br[kp4$kp4a == "1-3 knjige"] <- 2
kp4$kp4b_br[kp4$kp4b == "1-3 knjige"] <- 2
kp4$kp4c_br[kp4$kp4c == "1-3 knjige"] <- 2

kp4$kp4a_br[kp4$kp4a == "1-3 knjige"] <- 2
kp4$kp4b_br[kp4$kp4b == "1-3 knjige"] <- 2
kp4$kp4c_br[kp4$kp4c == "1-3 knjige"] <- 2

kp4$kp4a_br[kp4$kp4a == "1-3 knjige"] <- 2
kp4$kp4b_br[kp4$kp4b == "1-3 knjige"] <- 2
kp4$kp4c_br[kp4$kp4c == "1-3 knjige"] <- 2

kp4$kp4a_br[kp4$kp4a == "Više od 3 - manje od 10"] <- 7
kp4$kp4b_br[kp4$kp4b == "Više od 3 - manje od 10"] <- 7
kp4$kp4c_br[kp4$kp4c == "Više od 3 - manje od 10"] <- 7

kp4$kp4a_br[kp4$kp4a == "Više od 3 - manje od 10"] <- 7
kp4$kp4b_br[kp4$kp4b == "Više od 3 - manje od 10"] <- 7
kp4$kp4c_br[kp4$kp4c == "Više od 3 - manje od 10"] <- 7

kp4$kp4a_br[kp4$kp4a == "Više od 3 - manje od 10"] <- 7
kp4$kp4b_br[kp4$kp4b == "Više od 3 - manje od 10"] <- 7
kp4$kp4c_br[kp4$kp4c == "Više od 3 - manje od 10"] <- 7

kp4$kp4a_br[kp4$kp4a == "10+"] <- 12
kp4$kp4b_br[kp4$kp4b == "10+"] <- 12
kp4$kp4c_br[kp4$kp4c == "10+"] <- 12

kp4$kp4a_br[kp4$kp4a == "10+"] <- 12
kp4$kp4b_br[kp4$kp4b == "10+"] <- 12
kp4$kp4c_br[kp4$kp4c == "10+"] <- 12

kp4$kp4a_br[kp4$kp4a == "10+"] <- 12
kp4$kp4b_br[kp4$kp4b == "10+"] <- 12
kp4$kp4c_br[kp4$kp4c == "10+"] <- 12

# KRAJ --------------------------------------------------------------------


sveST.8.3$citanje_index <- select(kp4, ends_with("_br")) %>%
  rowSums(na.rm = TRUE) %>%
  sjmisc::rec(recodes =
      "   0=0 [Ne čita knjige];
        2:4=1 [Do 6 knjiga godišnje];
        6:9=2 [Do 12 knjiga godišnje];
      11:16=3 [Do 20 knjiga godišnje];
      19:24=4 [20 i više knjiga godišnje];
      26:36=5 [20 i više knjiga iz raznih područja godišnje] ")

class(sveST.8.3$citanje_index) <- "labelled" # treba da bi labelled i haven funkali

rm(kp4)

# labelled::to_factor(citanje_index)


#

# freq(sveST.7.NEP$potpr2)

# freq(sveST.7.NEP$qol3)
pozz <- names(val_labels(sveST.7.NEP$qol3))[str_detect(names(val_labels(sveST.7.NEP$qol3)), "Da,")]

qol3_r <- to_factor(sveST.7.NEP$qol3) %>%
  fct_collapse(Da = pozz) %>% 
  fct_recode(NZBO = "Ne zna - bez odgovora")

qol3_r_rec <- sjmisc::rec(sveST.7.NEP$qol3, 
                  recodes =
      "   1=0 [NE namjeravam preseliti];
        2:5=1 [Namjeravam preseliti];
        8=98  [NZBO] ")

class(qol3_r_rec) <- "labelled" # treba da bi labelled i haven funkali

sveST.8.4 <- sveST.8.3 %>% add_column(qol3_r, qol3_r_rec)

# USTAŠE I PARTIZANI #

poltZ <- sveST.8.4 %>%
  select(polt3:polt5) %>%
  mutate_all(to_factor) %>%
  map_df( ~ fct_collapse(.x, NZBO = c("Ne zna", "Ne želi odgovoriti")))

var_label(poltZ) <- var_label(select(sveST.8.4, polt3:polt5))
names(poltZ) %<>% paste0("_r")

sveST.8.5 <- cbind(sveST.8.4, poltZ)

# for (i in c("polt3_r", "polt4_r", "polt5_r")) {
#   class(sveST.8.5[[i]]) <- "labelled"
# }
# sveST.8.5 %>% select(polt3_r:polt5_r) %>% sapply(class)




# PIŠI FILE ---------------------------------------------------------------

# haven::write_sav(sveST.8.5, "/Dropbox/sveST_8_5.sav")



# usputno i ostalo --------------------------------------------------------

# skuži malo bolje ifelse
# hoću uz uvjet u jednoj varijabli mijenjati drugu varijablu

# set.seed(666)
# brojke <- sample(0:1, 10, replace = TRUE)
# 
# slova <- ifelse(brojke == 0, "A", "B")

# krucijalna socdem -------------------------------------------------------

# frre(sveST$spol, N = FALSE)
# frre(sveST$dob.10, N = FALSE)
# frre(sveST$obraz, N = FALSE)
# 
# # OKK 1
# 
# sveST %>%
#   mutate(okk1 = to_fac_drop(okk1)) %>%
#   tab_x("okk1", "obraz", reci = FALSE) %>%
#   kable(digits = 0, caption = "Broj knjiga u kućnoj biblioteci")
# 
# # odgoj
# 
# select(sveST, starts_with("vrij3x")) %>%
#   mutate_all(na_if, y = 8) %>%
#   mutate_all(to_factor) %>%
#   lab_tab %>%
#   arrange(Važno)

###
