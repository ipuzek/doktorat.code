### SVEsplit doktorat ###

# CILJ: mali DF s konačnim Inglehartovim indeksom # ingl.complete #
# CILJ: i rekodiranim česticama koje ga čine #
# INGLEHART je zeznutiji nego sam mislio

library(forcats)
# source("IvanP/!!!Doktorat/doktorat.code/01__recode.R")


# 1. dio - materijalizam - postmaterijalizam ------------------------------
# inglehart recode bejzik

recode_inglehart <- function(x) {
  to_factor(x) %>%
    forcats::fct_collapse(
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

select(sveST, vrij5a : vrij5b) %>% 
  mutate_all(recode_inglehart) %>%
  clone_R() %>%
  cbind(sveST, .) -> sveST.4


# inglehart tipovi - NE VALJA INSTRUMENt - ovo je pomoć za aditivni

sveST.4$post.mat <- "mješoviti tip"
sveST.4$post.mat[sveST.4$vrij5a_R == "mat" & sveST.4$vrij5b_R == "mat"] <- "materijalist"
sveST.4$post.mat[sveST.4$vrij5a_R == "postmat" & sveST.4$vrij5b_R == "postmat"] <- "postmaterijalist"
sveST.4$post.mat <- factor(sveST.4$post.mat, levels = c("materijalist", "mješoviti tip", "postmaterijalist"))
var_label(sveST.4$post.mat) <- "Tip: mat-mješoviti-postmat"
# 

# table(sveST.4$post.mat)

sveST.4$post.mat.ingl.num <- recode(sveST.4$post.mat,
       "materijalist" = 0,
       "mješoviti tip" = 1,
       "postmaterijalist" = 2)

# aditivni inglehart # slijepa ulica, isto kao gore!
# suvisli inglehart će biti postmaterijalizam + povjerenje + tolerancija

sveST.4$gen.pov.ingl.num <- if_else(sveST.4$vrij6 == 1, true = 1, false = 0)

# CHECKS
# mean(sveST.4$gen.pov.ingl.num)
# xtabs(~ vrij6 + gen.pov.ingl.num, sveST.4)
# ffre(sveST.4$vrij6, levels = "prefixed")


# homotolerancija za postmat ----------------------------------------------

sveST.4$vrij7x03 %>%
  as.numeric %>%
  recode(`98` = NA_real_, `99` = NA_real_) %>% 
  table %>% cumsum()

sveST.4$homo.ingl.num <- if_else(sveST.4$vrij7x03 == 10, true = 2,
        false = if_else(sveST.4$vrij7x03 %in% c(7:9),
                        true = 1, false = 0)) # %>% table %>% prop.table()
  
# xtabs(~ homo.ingl.num + vrij7x03, data = sveST.4)


# susjedska tolernacija za postmat ----------------------------------------

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

sveST.5$indeks.tolerancije.ingl.num <- 
  if_else(sveST.5$indeks.tolerancije == "Niti jedna grupa nije nepoželjna",
          true = 2, false = if_else(sveST.5$indeks.tolerancije == "Ne zna je li koja grupa nepoželjna",
                                    true = 1, false = 0)
          )

# xtabs(~ indeks.tolerancije + indeks.tolerancije.ingl.num, sveST.5)

# select(sveST.5, ends_with(".ingl.num")) %>%
#   names
#   # lapply(table)
#   # cor(method = "kendall")



# KONAČNA SKALA #
df.ingl <- select(sveST.5, ends_with(".ingl.num"))
ingl.skala <- rowSums(df.ingl)
var_label(ingl.skala) <- "Inglehart skala"

ingl.complete <- add_column(df.ingl, ingl.skala) %>% as_data_frame()
rm(ingl.skala, df.ingl)

# ggplot(ingl.complete, aes(ingl.skala)) +
#   geom_bar()
