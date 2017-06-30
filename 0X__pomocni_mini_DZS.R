### SVEsplit doktorat ###

# CILJ: pomoćni uz prostorne jedinice #
# grad, naselje, što gdje spada? #

library(dplyr)

download.file("http://www.dzs.hr/Hrv/censuses/census2011/results/xls/Nas_01_HR.xls",
              "Nas_01_HR.xls")

nasST <- readxl::read_xls("Nas_01_HR.xls", skip = 1)
nasST <- as_data_frame(nasST)

ST <- nasST %>% 
  rename(ime.zupanije = `Ime županije`,
       grad.ili.opcina = `Grad ili općina`,
       ime.grada.opcine = `Ime grada/općine`,
       ime.naselja = `Ime naselja`,
       spol = Spol,
       ukupno = Ukupno) %>% 
  select(ime.zupanije : ukupno) %>% 
  slice(-1)

ST %>% filter(ime.grada.opcine == "Split") %>%
  xtabs(~ ime.naselja, data = .)

ST %>% filter(ime.naselja == "Split") %>% 
  

