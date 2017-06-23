# GELMAN - 12. poglavlje --------------------------------------------------
library(lme4)
library(arm)

library(readr); library(magrittr); library(tibble); library(dplyr)
library(forcats)
# Set up the radon data

# read in and clean the data

srrs2 <- read_csv("IvanP/!!!Doktorat/Gelman/ARM_Data/radon/srrs2.dat", na = ".")

srrs2.radon <- srrs2 %>% mutate(
  radon = if_else(activity==0, .1, activity),
  log.radon = log(radon),
    county.fct = factor(county)
  )

srrs2.radon.filtered <- srrs2.radon %>% filter(state=="MN")

to.filter <- c("LAC QUI PARLE",
               "AITKIN",
               "KOOCHICHING",
               "DOUGLAS",
               "CLAY",
               "STEARNS",
               "RAMSEY",
               "ST LOUIS")


srrs2.radon.filtered.county <- srrs2.radon.filtered %>% 
  filter(county.fct %in% to.filter) %>% 
  mutate(county.fct = fct_relevel(county.fct, to.filter))


# n <- length(radon)   y <- log.radon    x <- floor

# get county index variable

county.name <- as.vector(srrs2.radon.filtered$county)
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}

# COMPLETE pooling
lm(log.radon ~ floor, data = srrs2.radon.filtered) %>% 
  display

# NO pooling
lm(log.radon ~ floor + county.fct - 1, data = srrs2.radon.filtered) %>% 
  display

# multilevel minimum
lmer(log.radon ~ 1 + (1 | county.fct), data = srrs2.radon.filtered) %>% 
  display()

# multilevel standard - varying intercept / fixed slope
M1 <- lmer(log.radon ~ floor + (1 | county.fct), data = srrs2.radon.filtered)
display(M1)

coef(M1)

fixef(M1); ranef(M1)

fixef(M1)["(Intercept)"] + ranef(M1)$county.fct["(Intercept)"] # iliti coef :D

se.fixef(M1); se.ranef(M1)

# SUMMARIZE and DISPLAY ---------------------------------------------------
fixef(M1)["floor"] + c(-2, 2)*se.fixef(M1)["floor"]

# standardne greške
coef(M1)$county.fct %>%
  rownames_to_column("county") %>% 
  mutate(se.intercept = se.ranef(M1)[["county.fct"]] %>% as.vector())

# plot 12.4 by ggplot

library(broom); library(ggplot2)
library(stringr)

# for geom_abline(slope = , intercept = ) (complete pooling)
M.complete <- lm(log.radon ~ floor, data = srrs2.radon.filtered) %>% 
  tidy()

# for no pooling (but fixed slopes)
M.no <- lm(log.radon ~ floor + county.fct - 1, data = srrs2.radon.filtered) %>% 
  tidy %>% 
  mutate(term = str_replace_all(term, "county.fct", ""),
         est.floor = estimate[term == "floor"]) %>%
  filter(term != "(Intercept)" & term != "floor") %>% 
  rename(county.fct = term) %>% 
  filter(county.fct %in% to.filter) %>% 
  mutate(county.fct = fct_relevel(county.fct, to.filter))

# for multilevel
M.gg.multi <- coef(M1)$county.fct %>%
  rownames_to_column("county.fct") %>%
  filter(county.fct %in% to.filter) %>%
  mutate(county.fct = fct_relevel(county.fct, to.filter))
  

ggplot(data = srrs2.radon.filtered.county) +
  geom_jitter(aes(x = floor, y = log.radon),
              width = .1, height = .1, size = .5) +
  geom_abline(slope = M.complete$estimate[2], intercept = M.complete$estimate[1],
              linetype = 3) +
  geom_abline(data = M.no, aes(slope = est.floor, intercept = estimate),
              size = 2) +
  geom_abline(data = M.gg.multi, aes(slope = floor, intercept = `(Intercept)`)) +
  facet_wrap(~ county.fct, nrow = 2) +
  theme_minimal() +
  labs(title = "Fig 12.4 - pg. 257",
      subtitle = "debela = no pooling, tanka = multilevel, crtkana = complete pooling",
       caption = "multilevel modelling is most important when it is close to complete pooling aka when the groups are smiliar to each other\n
                  when groups vary greatly, multilevel modelling is not much better than simple no-pooling")


### zanimljiv pokušaj, ali ne šljaka jer su VARYING SLOPES
# ggplot(data = M.no) +
#   geom_abline(slope = M.complete$estimate[2], intercept = M.complete$estimate[1]) +
#   geom_jitter(data = srrs2.radon.filtered, aes(x = floor, y = log.radon)) +
#   geom_smooth(data = srrs2.radon.filtered, aes(x = floor, y = log.radon),
#               method = "lm", se = FALSE) +
#   facet_wrap(~ county.fct)

# get the county-level predictor

srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips

# cty <- read.table ("cty.dat", header=T, sep=",")
cty <- read_csv("IvanP/!!!Doktorat/Gelman/ARM_Data/radon/cty.dat",
                na = ".",
                col_types = cols(
                  stfips = col_double(),
                  ctfips = col_double(),
                  st = col_character(),
                  cty = col_character(),
                  lon = col_double(),
                  lat = col_double(),
                  Uppm = col_double()
                ))


usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]

cty$Uppm %>% summary

mn <- srrs2$state=="MN"
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips$stfips)

uranium <- cty[usa.rows,"Uppm"]

u <- log (uranium)


u[county]
# DODAJ GRUPNI prediktor --------------------------------------------------


