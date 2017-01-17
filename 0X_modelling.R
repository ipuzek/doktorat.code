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