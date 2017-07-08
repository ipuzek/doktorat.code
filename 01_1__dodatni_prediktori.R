### SVEsplit doktorat ###

# CILJ: rekodiram i sređujem dodatne varijable #
# 1. rast i okoliš #



# OVA SKRIPTA JE DIO 01_recode
# ZOVEŠ JE ISKLJUČIVO IZ NJE!


# rast.okolis -------------------------------------------------------------

sveST$rast.okolis <- 
  sjmisc::rec(sveST$vrij10,
              
              rec = " 2=0 [Ekonomski rast i radna mjesta su važnija];
                1=1 [Zaštita okoliša je važnija];
                3=98 [NZBO] ",
              
              var.label = "Okoliš vs ekonomski rast")

class(sveST$rast.okolis) <- "labelled"
sveST$rast.okolis <- na_if(sveST$rast.okolis, 98)


# relig (ioznost) na 2 načina ---------------------------------------------

sveST$rel1_r <- sveST$rel1 %>% 
  to_factor() %>% 
  fct_collapse(NZBO = c("Ne zna", "Ne želi odgovoriti")) %>% 
  na_if("NZBO")  #  %>% fct_count()


sveST$rel2_r <- sveST$rel2 %>% 
  to_factor() %>% 
  fct_recode(`Gotovo nikad ili nikad` = "Rjeđe od toga - nikada",
             NZBO = "Ne zna - bez odgovora") %>% 
  na_if("NZBO")  #  %>% fct_count()


# sockap ------------------------------------------------------------------

tmp.sockap.df <- sveST %>% select(sk1_1 : sk1_7) %>% 
  mutate_all(isnt_na) %>% 
  transmute(sockap.veza = sk1_1 + sk1_2 + sk1_3 + sk1_4 + sk1_5 + sk1_6 + sk1_7)

sveST$sockap.veza <- factor(tmp.sockap.df$sockap.veza) %>% 
  fct_collapse(`Nema vezu nigdje` = "0",
               `Ima vezu` = c("1", "2", "3", "4", "5", "6", "7"))

rm(tmp.sockap.df)


# politika ----------------------------------------------------------------

tmp.politika.df <-
  sveST %>% 
  select(starts_with("pp1x"))

pol_recode <- function(x) {
  recode(x,
         `1` = 2, `2` = 1,
         `3` = 0, `98` = 0, `99` = 0)
}

tmp.politika.df.2 <- 
  map_df(tmp.politika.df, as.numeric) %>% 
  map_df(pol_recode)

sveST$nef.politika <- rowSums(tmp.politika.df.2)

rm(tmp.politika.df, tmp.politika.df.2)