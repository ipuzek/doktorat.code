### SVEsplit doktorat ###

# CILJ: 1. model kroz korake #
# ispod svakog modela ispiši kratki NALAZ

# 1. osnovni model će bit stavovi vs inglehart...prihod...kontrolna demografija
# prihod imam samo kućanstva, ali OK, koristi po glavi ### imam i društveni status

# rm(list = ls())
source("IvanP/!!!Doktorat/doktorat.code/01__recode.R")
source("IvanP/!!!Doktorat/doktorat.code/02__kotar_klaster.R")
source("IvanP/!!!Doktorat/doktorat.code/03__kriterij_construct.R")
source("IvanP/!!!Doktorat/doktorat.code/04__inglehart_index.R")
source("IvanP/!!!Doktorat/doktorat.code/05__quality_index.R")

library(intubate)
library(broom)
library(lme4)

sveST.6 <- sveST.5 %>%
  add_column(
    NEP.skala = NEP.complete$NEP.skala,
    ingl.skala = ingl.complete$ingl.skala,
    qual.skala = qual.complete$qual.skala
  )

# M1 - 1. korak: NEP ~ inglehart + socdem + prihod, u PUNO koraka, sve indivudalno ----
# NALAZ: NEP i prošireni indeks postmaterijalizma su slabo, ali značajno povezani
 # dok se ne dodaju drugi prediktori. Svaki od njih zasebno povećava udio objašnjene
 # varijance (ZNAČAJNO?)
 # istovremeno bitno smanjuje kfc. povezanosti ingl.indeksa i NEP-a.
 # prihod ne utječe ni na što, uključit ću ga ponovno tek na kontekstualnoj razini

sveST.6 %>%
  ntbt_lm(NEP.skala ~ ingl.skala) %>% 
  tidy() %>% filter(p.value < .01)

  arm::display()
  # HH::lmplot()

sveST.6 %>%
  # filter(NEP.skala > 8) %>% ## influential observation prema q-q plotu
  mutate(dob.sd = dmg2/(2*sd(dmg2)),
         obraz.rel__ = fct_relevel(obraz, "SŠ_4god")) %>% 
  ntbt_lm(NEP.skala ~ ingl.skala + spol + dob.sd + obraz.rel__) %>%
  # ntbt_lm(NEP.skala ~ ingl.skala + spol + obraz) %>%
  tidy %>% filter(p.value < .05)
  # arm::display()
  HH::lmplot()



# KOMENTAR: 
 # ovaj prvi model napravi natenane, ubacujući varijablu po varijablu
 # dijagnostika isto detaljno: q-q plot je OK (prikaži ga, još nešto zanimljivo?)
   # izbacivanjem jednog ispitanika bitno se mijenjaju svi koeficijenti


# M1 - 2. korak: NEP ~ inglehart + prihod + socdem, sve individualno ----
# NALAZ: ---
# KOMENTAR: prihod je toliko jadan da nije zaslužio ovakav tretman, ubacio sam ga u prvi m.


# M1 - alt - logistička: rast.okolis ~ inglehart + socdem, sve individualno ----

sveST.6 %>%
 ntbt_glm(
    rast.okolis ~ ingl.skala,
    family = binomial(link = logit), na.action = "na.exclude") %>%
  # arm::display() %>% 
  tidy() %>%filter(p.value < .01)

sveST.6 %>%
  # filter(NEP.skala > 8) %>% ## influential observation prema q-q plotu
  mutate(dob.sd = dmg2/(2*sd(dmg2)),
         obraz.rel__ = fct_relevel(obraz, "SŠ_4god")) %>% 
  ntbt_glm(
    rast.okolis ~ ingl.skala + spol + dob.sd + obraz.rel__ ,
    family = binomial(link = logit), na.action = "na.exclude") %>%
  # ntbt_lm(NEP.skala ~ ingl.skala + spol + obraz) %>%
  tidy %>%filter(p.value < .01)
  # binomTools::Rsq()
  # glance()
  # arm::display()
# plot()
sveST.6$rast.okolis


# NALAZ: 
# KOMENTAR: prihod ima vrlo nisku korelaciju sa zavisnim varijablama od interesa
 # a ima relativno velik broj missing vrijednosti, tako da njegovim uključivanjem
 # oslabljujemo model. Uključivanjem na razini zone riješit ćemo taj problem.

# M1 INTERMEZZO - zone

sveST.6 %>% 
  ntbt_lm(NEP.skala ~ zone) %>% 
  glance()

sveST.6 %>% 
  ntbt_lmer(NEP.skala ~ 1 + (1 | zone)) %>% 
  arm::display()


# M1 - 3. korak: NEP ~ inglehart + 1 | dio Splita ----

M1.ml.1 <- sveST.6 %>%
  ntbt_lmer(NEP.skala ~ ingl.skala + (1 | zone))
  
M1.gg.multi <- coef(M1.ml.1)$zone %>%
  rownames_to_column("zone.fct") %>% 
  as_tibble()

ggplot(data = sveST.6) +
  geom_jitter(aes(x = ingl.skala, y = NEP.skala),
              width = .1, height = .1, size = .5) +
  geom_abline(data = M1.gg.multi, aes(slope = ingl.skala, intercept = `(Intercept)`)) +
  facet_wrap(~ zone, nrow = 1) +
  theme_minimal() +
  labs(title = "Fig 12.4 - pg. 257",
       subtitle = "debela = no pooling, tanka = multilevel, crtkana = complete pooling",
       caption = "multilevel modelling is most important when it is close to complete pooling aka when the groups are smiliar to each other\n
                  when groups vary greatly, multilevel modelling is not much better than simple no-pooling")


# tidy()
  # arm::display()

# NALAZ: 
# KOMENTAR: 



# M1 - 4. korak: DESKRIPTIVNI model - sve individualno ----

sveST.6 %>%
  filter(NEP.skala > 8) %>% ## influential observation prema q-q plotu
  
  mutate(dob.sd = dmg2/(2*sd(dmg2)),
         obraz.rel__ = fct_relevel(obraz, "SŠ_4god")) %>% 
  
  ntbt_lm(NEP.skala ~ ingl.skala + spol + dob.sd + obraz.rel__ + rel2_r + sockap.veza + nef.politika + imovina.kucanstva) %>%
  # ntbt_lm(NEP.skala ~ ingl.skala + spol + obraz) %>%
  tidy %>% filter(p.value < .01)
  # arm::display()
HH::lmplot()



# NALAZ: 
# KOMENTAR:



# MOTIVACIJSKA MX: NEP ~ qual + 1 | dio Splita ----
# YEEEEEEEEEEEEEEEEESSSSSSSSSSSSSSSSSSSSSSSS #

MX <- sveST.6 %>%
  ntbt_lmer(NEP.skala ~ (qual.skala | zone))

MX.multi <- coef(MX)$zone %>%
  rownames_to_column("zone") %>%
  mutate(zone.fct = fct_inorder(zone)) %>%
  rename(intercept = `(Intercept)`) %>% 
  as_tibble()

filter(sveST.6, NEP.skala > 15) %>% 
  ggplot(aes(x = qual.skala, y = NEP.skala)) +
  geom_jitter(alpha = .8, stroke = 0.2) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(data = MX.multi, aes(slope = qual.skala, intercept = intercept)) +
  facet_wrap(~ zone, nrow = 1, strip.position = "left") +
  labs(x = "Percepcija problema urbanog okoliša",
       y = "NEP skala")
sveST.6$zone
ggsave("plot_to_rule_them_all.png",
       width = 30, height = 21, units = "cm", dpi = 600)
ggsejv()
# YEEEEEEEEEEEEEEEEESSSSSSSSSSSSSSSSSSSSSSSS #


# M2 - 1. korak NEP ~ percepcija + zona



sveST.6 %>%
  ntbt_lm(NEP.skala ~ qual.skala + zone) %>% 
  arm::display()
  # tidy() %>% filter(p.value < .01)
  HH::lmplot()

sveST.6 %>%
  ntbt_lmer(NEP.skala ~ qual.skala + (1 | zone)) %>% 
  # coef()
  # fixef()
  # ranef()
  arm::se.ranef()
  # arm::display()
  # tidy()

multi.prvi <- sveST.6 %>% 
  ntbt_lmer(NEP.skala ~ (qual.skala | zone))

multi.prvi %>% 
  # coef()
  # fixef()
  # ranef()  #  -> multi.prvi.ranefs
  # arm::se.ranef()  #  -> multi.prvi.se.ranefs
  arm::se.fixef()
  # arm::display()
  # tidy()
  
multi.final <- sveST.6 %>%
  ntbt_lmer(NEP.skala ~ (qual.skala + spol + dmg2) | zone)

multi.final %>% 
  # coef()
  # fixef()
  # ranef()  #  -> multi.final.ranefs
  # arm::se.ranef() ->  #  multi.final.se.ranefs
  arm::se.fixef()
  # arm::display()
  # tidy()

# HH::lmplot()