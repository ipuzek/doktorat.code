### SVEsplit doktorat ###

# CILJ: rekodiram i sređujem dodatne varijable #
# 1. rast i okoliš #



# OVA SKRIPTA JE DIO 01_recode
# ZOVEŠ JE ISKLJUČIVO IZ NJE!



sveST$okolis.rast <- 
  sjmisc::rec(sveST$vrij10,
              
              rec = " 2=0 [Ekonomski rast i radna mjesta su važnija];
                1=1 [Zaštita okoliša je važnija];
                3=98 [NZBO] ",
              
              var.label = "Okoliš vs ekonomski rast")

class(sveST$okolis.rast) <- "labelled"
sveST$okolis.rast <- na_if(sveST$okolis.rast, 98)

