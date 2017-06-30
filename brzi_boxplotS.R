### SVEsplit doktorat ###

# CILJ: vizz komponenata skala via boxplots #

# library(tidyr); library(ggplot2)

library(ggstance) # Horizontal 'ggplot2' Components
source("IvanP/R/xx__ggsejv.R")

# 1. izaberi dataset: NEP.complete // qual.complete

nmz <- qual.complete %>% select(-ends_with(".skala")) %>% var_label() %>% unlist() %>% unname()
jea <- qual.complete %>%
  select(-ends_with(".skala")) %>%
  mutate_all(na_if, 98) %>%
  gather(kljuc, velju, everything())

jea.minz <- jea %>%
  group_by(kljuc) %>%
  summarise(minz = mean(velju, na.rm = TRUE))

# ggplot(jea, aes(x = kljuc, y = velju)) +
#   geom_boxplot() +
#   geom_point(data = jea.minz, aes(y = minz), colour = "blue")

##


ggplot(jea, aes(x = velju, y = kljuc)) +
  geom_boxploth() +
  geom_point(data = jea.minz, aes(x = minz), shape = 0) +
  scale_y_discrete(label = str_wrap(nmz, 50)) +
  labs(x = NULL, y = NULL)

# device = grDevices::svg je bolji ZA libreoffice ### svglite (defaultni) je bolji za edit
# ggsave_A6_l("unemployment__by_educ_SVG.svg", plot = grafovi.educ[[1]], device = grDevices::svg)



# library(svglite)

ggsejv("IvanP/!!!Doktorat/doktorat.tekst/OUT/qual_box.svg",
       plot = last_plot(), device = grDevices::svg,   # bolji za export u ODT
       AA = "A6.l")

# , device = grDevices::svg)
#
# ggsave_A5_p("boxplotz_svglite.svg")
# , device = grDevices::svg)
