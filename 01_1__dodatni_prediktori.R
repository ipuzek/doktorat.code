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

