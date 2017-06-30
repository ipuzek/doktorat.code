### SVEsplit doktorat ###

# CILJ: bivarijatno, prati hipoteze kak tak #
# ispod svake analize ispiši kratki NALAZ

# 1. osnovni model će bit stavovi vs inglehart...prihod...kontrolna demografija
# dobar dio cilja je vidjeti koliko varijance objašnjava inglehart kad se kontrolira prihod
# prihod imam samo kućanstva, ali OK, koristi po glavi ### imam i društveni status

# rm(list = ls())
source("IvanP/!!!Doktorat/doktorat.code/01__recode.R")
source("IvanP/!!!Doktorat/doktorat.code/02__kotar_klaster.R")
source("IvanP/!!!Doktorat/doktorat.code/03__kriterij_construct.R")
source("IvanP/!!!Doktorat/doktorat.code/04__inglehart_index.R")
source("IvanP/!!!Doktorat/doktorat.code/05__quality_index.R")

sveST.6 <- sveST.5 %>%
  add_column(
    NEP.skala = NEP.complete$NEP.skala,
    ingl.skala = ingl.complete$ingl.skala,
    qual.skala = qual.complete$qual.skala
  )

# NEP vs inglehart --------------------------------------------------------

# NALAZ: odnos je blago kurvilinearan - pad prema višim vrijednostima ingleharta
 # povezanosti nešto ima, ali nelinearna!

sveST.6 <- sveST.5 %>%
  add_column(
    NEP.skala = NEP.complete$NEP.skala,
             ingl.skala = ingl.complete$ingl.skala,
             qual.skala = qual.complete$qual.skala
    )

select(sveST.6, NEP.skala, ingl.skala) %>% cor
  # lapply(table, useNA = "always")
cor.test(~ NEP.skala + ingl.skala, data = sveST.6)

ggplot(sveST.6, aes(x = ingl.skala, y = NEP.skala)) +
  geom_jitter() + geom_smooth(method = "lm") +
  labs(x = "Skala proširenog postmaterijalizma",
       y = "NEP-skala") +
  theme_minimal()

group_by(sveST.6, ingl.skala) %>% 
  summarise(NEP.prosjek = mean(NEP.skala))


# shared histogram in ggplot KILLAH!

ingl.plot <- sveST.6 %>% select(NEP.skala, ingl.skala) %>% 
  mutate(ingl.skala.fac = recode_factor(ingl.skala,
                                        `0` = "0 - Najniži (prošireni) postmaterijalizam",
                                        `1` = "1", `2` = "2", `3` = "3", `4` = "4", `5` = "5",
                                        `6` = "6 - Najviši (prošireni) postmaterijalizam"
  )
  )

prd <- group_by(ingl.plot, ingl.skala.fac) %>% summarise(NEP.prosjek = mean(NEP.skala))

ingl.plot %>% 
  ggplot(aes(x = NEP.skala)) +                        # u ovom datasetu su obje varijable
  geom_bar(data = NEP.complete, fill = "grey") +      # u ovom je samo zavisna
  geom_bar(aes(fill = ingl.skala.fac)) +              # nezavisna
  geom_vline(data = prd, aes(xintercept = NEP.prosjek),
             size = 1.3, alpha = .5) +                # i prosjeci naravno (3. dataset, HA!)
  facet_wrap(~ ingl.skala.fac, ncol = 1) +            # nezavisna
  guides(fill = FALSE) +                              # remove the legend
  labs(x = "Skala nove ekološke paradigme",
       y = "Broj ispitanika") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 700, name = "Udio ispitanika")) +
  theme_minimal()
  


# NEP vs prihod.PC --------------------------------------------------------
# obični prihod kućanstva je tugica, nemoj
# NALAZ: uglavnom ništa, osim malog uleknuća na srednjim vrijednostima,
 # ...ali daleko od značajnosti
 # NEP vs društvena pozicija ### NIŠTA!

select(sveST.6, NEP.skala, prihod.PC) %>% cor(use = "pairwise.complete.obs")
 # lapply(table, useNA = "always")
cor.test(~ NEP.skala + prihod.PC, data = sveST.6)

ggplot(sveST.6, aes(x = prihod.PC, y = NEP.skala)) +
  geom_point() + geom_smooth() +
  xlim(c(100, 10000))

# pokušaj logaritmiranja
ggplot(sveST.6, aes(x = log(prihod.PC), y = NEP.skala)) +
         geom_point() + geom_smooth() +
  xlim(c(6,10))

# NEP vs društvena pozicija ### NIŠTA!

sveST.6$dr.status <- na_if(sveST.6$dmg15, 8)

ltabs(~ dr.status, sveST.6,
      addNA = TRUE, drop.unused.levels = TRUE)

ggplot(sveST.6, aes(x = dr.status, y = NEP.skala)) +
  geom_jitter() + geom_smooth(method = "lm")

tapply(sveST.6$NEP.skala, to_factor(sveST.6$dr.status), mean) %>% round(1)

# NEP i opreka "okoliš-rast" ------------------------------------

# NEP vs okoliš-rast
# NALAZ: t = -5.9, df = 595.71, p-value > .01
 # Cohenov d iznosi 0.47, blizu srednje velikog (jipi?)
 # logistička regresija: Tjurov R2 je .049 #  SVE ISTO!

select(sveST.6, rast.okolis, NEP.skala) %>% cor(use = "pairwise.complete.obs")

m1 <- glm(rast.okolis ~ NEP.skala, family = binomial(link = "logit"), data = sveST.6)
# arm::display(mmm)
mm1 <- binomTools::Rsq(m1) # %>% plot
mm1$R2mod; sqrt(mm1$R2mod)
rm(m1, mm1)

t.test(NEP.skala ~ rast.okolis, data = sveST.6)
effsize::cohen.d(NEP.skala ~ rast.okolis, data = sveST.6)

# shared histogram in ggplot KILLAH!

rast.okolis.plot <- sveST.6 %>% select(NEP.skala, rast.okolis) %>% 
  mutate(rast.okolis.fac = to_fac_drop(rast.okolis))

prdd <- group_by(rast.okolis.plot, rast.okolis.fac) %>% summarise(NEP.prosjek = mean(NEP.skala))

rast.okolis.plot %>% na.omit %>% 
  ggplot(aes(x = NEP.skala)) +                        # u ovom datasetu su obje varijable
  geom_bar(data = na.omit(NEP.complete), fill = "grey") +      # u ovom je samo zavisna
  geom_bar(aes(fill = rast.okolis.fac)) +              # nezavisna
  geom_vline(data = na.omit(prdd), aes(xintercept = NEP.prosjek),
             size = 1.3, alpha = .5) +                # i prosjeci naravno (3. dataset, HA!)
  facet_wrap(~ rast.okolis.fac, ncol = 1) +            # nezavisna
  guides(fill = FALSE) +                              # remove the legend
  labs(x = "Skala nove ekološke paradigme",
       y = "Broj ispitanika") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 700, name = "Udio ispitanika")) +
  theme_minimal()


# NEP i obrazovanje -------------------------------------------------------
# NALAZ: najveća korelacija dosad (.14), jipi!
 # oblik povezanosti - opet lagani obrnuti U, ali pad je tek na Mag+
 # čini se da postoji jasan porast od OŠ prema SŠ, ali on se NE nastavlja

ltabs(~ obraz, sveST.6,
      addNA = TRUE, drop.unused.levels = TRUE)

sveST.6$obraz.num <- as.numeric(sveST.6$obraz)

# filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
sveST.6 %>% 
  select(qual.skala, dmg2) %>%
  cor(method = "pearson", use = "pairwise.complete.obs")

# filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
sveST.6 %>% 
  cor.test(~ ingl.skala + dmg2, data = .,
           method = "pearson", use = "pairwise.complete.obs")

filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
  ggplot(aes(x = obraz, y = NEP.skala)) +
  geom_boxplot() + 
  geom_smooth(aes(x = obraz.num), method = "lm", formula = y ~ splines::bs(x, degree = 2), se = FALSE) +
  geom_smooth(aes(x = obraz.num), method = "lm", colour = "red", se = FALSE)
  

# NEP i dob ---------------------------------------------------------------
# NALAZ: druga najveća korelacija dosad (-0.13), pa još negativna!

select(sveST.6, dmg2, NEP.skala) %>% cor(use = "pairwise.complete.obs")

filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
  ggplot(aes(x = dmg2, y = NEP.skala)) +
  geom_jitter(alpha = .8, stroke = 0.2) + 
  geom_smooth(method = "lm",
              formula = y ~ splines::bs(x, degree = 2), se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  facet_wrap(~ obraz, nrow = 1)

# NEP i spol ---------------------------------------------------------------
# NALAZ: korelacija je 0.14
 # t = -3.8, df = 590.58, p > .01 ### Cohenov d = 0.29 (mali, tek od 0.5 je srednji)

select(sveST.6, spol.num, NEP.skala) %>% cor(use = "pairwise.complete.obs")

# m1 <- glm(spol ~ NEP.skala, family = binomial(link = "logit"), data = sveST.6)
# # arm::display(m1)
# mm1 <- binomTools::Rsq(m1) # %>% plot
# mm1$R2mod; sqrt(mm1$R2mod)
# rm(m1, mm1)

t.test(ingl.skala ~ spol, data = sveST.6)
effsize::cohen.d(ingl.skala ~ spol, data = sveST.6)

# NEP i kvaliteta okoliša ---------------------------------------------------------------
# NALAZ: nikakva korelacija

select(sveST.6, qual.skala, NEP.skala) %>% cor(use = "pairwise.complete.obs")
cor.test(~ qual.skala + NEP.skala, data = sveST.6)

### problemi po zonama
group_by(sveST.6, zone) %>%
  summarise(qual.prosjek = mean(qual.skala)) %>% 
  arrange(qual.prosjek)

### korelacije po zonama
dplyr::select(sveST.6, qual.skala, NEP.skala, zone) %>% 
  split(.$zone) %>%
  map(dplyr::select, -zone) %>% 
  map_df(cor, use = "pairwise.complete.obs") %>% 
  t %>% as.data.frame.matrix() %>% 
  rownames_to_column("zone") %>% 
  dplyr::select(zone, V2) %>% 
  arrange(abs(V2)) # absolute je nice touch


### ovo sve služi samo za dobit statističku značajnost korelacija
### treba to linearnom regresijom

corrs <- select(sveST.6, qual.skala, NEP.skala, zone) %>% 
  split(.$zone) %>%
  map(select, -zone) %>% 
  # map_df(cor, use = "pairwise.complete.obs") %>%
  map(~ cor.test(~ qual.skala + NEP.skala, data = .x))

# pomoć za ekstrakciju podataka iz lista
un.corrs <- unlist(corrs)
to.extract <- names(un.corrs)[str_detect(names(un.corrs), "p.value")]
un.corrs[to.extract] %>% as.numeric()


### glorious plot - NEP vs qual.skala

filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
  ggplot(aes(x = qual.skala, y = NEP.skala)) +
  geom_jitter(alpha = .8, stroke = 0.2) + 
  # geom_smooth(method = "lm",
  #             formula = y ~ splines::bs(x, degree = 2), se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  facet_wrap(~ zone, nrow = 1)

# NALAZ (svih nalaza): u slučaju slabe kvalitete okoliša (), postoji pozitivna
 # korelacija i visok stupanj "utjecaja" procijenjen B regresijskim koeficijentom
 # između percipiranih ekoloških problema i NEP skale. Drugim riječima,
 # postojanje objektivnih problema i njihova percepcija se izravno vežu na rezultate
 # NEP skale, koja primarno mjeri zabrinutost za okoliš.
 # KLJUČNO je pri tome da rezultati NEP skale istovremeno NISU visoki u prosjeku
  # u odnosu na druge dijelove grada [ne radi se dakle o jednostavnom odnosu
  # - visoki problemi - visok NEP, nego baš o mehanizmu formiranja stava posredovanom
  # percepcijom problema ]

 # gradska zona čiji stanovnici okoliš ocjenjuju najbolje, ujedno je i zona gdje je
 # ova korelacija negativna.

 # DAKLE.
 # Ako je okoliš u relativno lošem stanju, porastom percpecije problema rast će i
 # proekološki stavovi.
 # Ako je okoliš u relativno dobrom stanju, porast percepcije problema neće imati
 # značajan učinak na ekološke stavove, ili će ta veza biti i negativna:
  # tendencija da lošija procjena okoliša rezultira nižom razinom proekoloških stavova,
  # što znači da se determinante odnosa kriju drugdje (kultura!)

### glorious plot - NEP vs ingl.skala
# NALAZ: ne funckionira toliko dobro

filter(sveST.6, NEP.skala > 10) %>% # izbacit outliera
  ggplot(aes(x = ingl.skala, y = NEP.skala)) +
  geom_jitter(alpha = .8, stroke = 0.2) + 
  # geom_smooth(method = "lm",
  #             formula = y ~ splines::bs(x, degree = 2), se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  facet_wrap(~ zone, nrow = 1)


# SEJV --------------------------------------------------------------------

source("IvanP/R/xx__ggsejv.R")

# ggsejv("IvanP/!!!Doktorat/doktorat.tekst/OUT/nep_by_inglehart.svg",
#        # plot = nep,
#        device = grDevices::svg,   # bolji za export u ODT
#        AA = "A5.p")

#  A5  # width = 148, height = 210, units = "mm")

ggsave("IvanP/!!!Doktorat/doktorat.tekst/OUT/nep_by_inglehart.svg",
       # plot = nep,
       device = grDevices::svg,   # bolji za export u ODT
       width = 180, height = 210, units = "mm")

ggsave("IvanP/!!!Doktorat/doktorat.tekst/OUT/nep_by_rast.okolis.svg",
       # plot = nep,
       device = grDevices::svg,   # bolji za export u ODT
       width = 170, height = 180/2, units = "mm")
