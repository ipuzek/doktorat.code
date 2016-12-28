### SVEsplit doktorat ### EDA ###



# EXPLORE
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


