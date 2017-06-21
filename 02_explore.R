### SVEsplit doktorat ### EDA ###


# explore via vizz --------------------------------------------------------

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

# imovina vs prihod vs dob!


# explore via modelling -----------------------------------------------

library(broom)
library(arm)

tidy = koeficijenti
augment = vrijednosti (ala mutate)
glance = pojedina vrijednost (ala R2)


# prihod _ imovina --------------------------------------------------------

aov(prihod.PC ~ imovina.kucanstva, data = sveST) %>% tidy


# prihod _ dob _ spol -----------------------------------------------------

lm(prihod.PC ~ dmg1 + dmg2, data = sveST) %>% display()
lm(prihod.PC ~ spol + dmg2, data = sveST) %>% display()

lm(prihod.PC ~ dmg1 + dmg2, data = sveST, weights = rim_w_1) %>% display()
lm(prihod.PC ~ spol + dmg2, data = sveST, weights = rim_w_1) %>% display()

# imovina vs prihod vs dob! TODO