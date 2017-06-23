### SVEsplit doktorat ###

# CILJ: bivarijatno, prati hipoteze kak tak #

# 1. osnovni model će bit stavovi vs inglehart...prihod...kontrolna demografija
# dobar dio cilja je vidjeti koliko varijance objašnjava inglehart kad se kontrolira prihod
# prihod imam samo kućanstva, ali OK, koristi po glavi

source("IvanP/!!!Doktorat/doktorat.code/04__inglehart_index.R")
source("IvanP/!!!Doktorat/doktorat.code/05__quality_index.R")

# sveST.5 %>% names %>% tail(30)


# NEP vs inglehart --------------------------------------------------------

sveST.6 <- sveST.5 %>%
  add_column(NEP.skala = NEP.complete$NEP.skala,
             ingl.skala = ingl.complete$ingl.skala)

select(sveST.6, NEP.skala, ingl.skala) %>% cor
  # lapply(table, useNA = "always")

ggplot(sveST.6, aes(x = ingl.skala, y = NEP.skala)) +
  geom_jitter() + geom_smooth()

group_by(sveST.6, ingl.skala) %>% 
  summarise(NEP.prosjek = mean(NEP.skala))
  # odnos je blago kurvilinearan - pad prema višim vrijednostima ingleharta
  # povezanost je TU, ali nelinearna!

# shared histogram in ggplot KILLAH!
prd <- group_by(sveST.6, ingl.skala) %>% summarise(NEP.prosjek = mean(NEP.skala))

ggplot(sveST.6, aes(x = NEP.skala)) +            # u ovom datasetu su obje varijable
  geom_bar(data = NEP.complete, fill = "grey") + # u ovom je samo zavisna
  geom_bar(aes(fill = ingl.skala)) +             # nezavisna
  geom_vline(data = prd, aes(xintercept = NEP.prosjek)) + # i prosjeci naravno (3. dataset, HA!)
  facet_wrap(~ ingl.skala, ncol = 1) +           # nezavisna
  guides(fill = FALSE)                           # remove the legend


# NEP vs prihod.PC --------------------------------------------------------
# obični prihod kućanstva je tugica, nemoj

select(sveST.6, NEP.skala, prihod.PC) %>% cor(use = "pairwise.complete.obs")
 # lapply(table, useNA = "always")

ggplot(sveST.6, aes(x = prihod.PC, y = NEP.skala)) +
  geom_point() + geom_smooth() +
  xlim(c(100, 10000))

# pokušaj logartimitranja
ggplot(sveST.6, aes(x = log(prihod.PC), y = NEP.skala)) +
         geom_point() + geom_smooth() +
  xlim(c(6,10))


# zamijenimo NEP oprekom "okoliš-rast" ------------------------------------


