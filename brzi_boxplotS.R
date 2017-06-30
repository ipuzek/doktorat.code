# some vizz

library(tidyr)
library(ggplot2)

nmz <- sveST.6 %>% select(vrij14x01:vrij14x10) %>% var_label() %>% unlist() %>% unname()

jea <- sveST.6 %>%
  select(vrij14x01:vrij14x10) %>% 
  mutate_all(recode_98) %>% 
  gather(kljuc, velju, everything())

jea.minz <- jea %>% 
  group_by(kljuc) %>% 
  summarise(minz = mean(velju, na.rm = TRUE))

# ggplot(jea, aes(x = kljuc, y = velju)) +
#   geom_boxplot() +
#   geom_point(data = jea.minz, aes(y = minz), colour = "blue")

##

library(ggstance)
library(stringr)

ggplot(jea, aes(x = velju, y = kljuc)) +
  geom_boxploth() +
  geom_point(data = jea.minz, aes(x = minz), colour = "blue", size = 2) +
  scale_y_discrete(label = str_wrap(nmz, 50)) +
  labs(x = NULL, y = NULL)

# device = grDevices::svg je bolji ZA libreoffice
# svglite (defaultni) je bolji za edit
# ggsave_A6_l("unemployment__by_educ_SVG.svg", plot = grafovi.educ[[1]], device = grDevices::svg)

library(svglite)

ggsave_A4_p <- function(...) ggsave(..., width = 210.06, height = 296.93, units = "mm")
ggsave_A5_p <- function(...) ggsave(..., width = 148.08, height = 210.06, units = "mm")

ggsave_A5_p("boxplotz_svglite.svg")
# , device = grDevices::svg)
