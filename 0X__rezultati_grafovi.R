### SVEsplit doktorat ###

# CILJ: univarijatne skale, histogrami

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




# hist_with_normal --------------------------------------------------------

hist_with_normal <- function(x, title, bins) {
  
  parametri <- c(
    min = mean(sveST.6[[x]]),
    esde =  sd(sveST.6[[x]])
  )
  
  ggplot(sveST.6, aes_string(x = x)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = "gray80") +
    stat_function(fun = dnorm,
                  n = 700, args = list(mean = parametri["min"], sd = parametri["esde"])) +
    scale_y_continuous(sec.axis = sec_axis(~.*700, name = "Broj ispitanika"), name = "Udio ispitanika") +
    labs(x = "",
         title = title) +
    # subtitle = "",
    #caption = "sources: EVS/WVS (1994-2008), own survey (2015)",
    #caption = "")
    theme_minimal()
  
}

nep <- hist_with_normal("NEP.skala", "", bins = 39)
ingl <- hist_with_normal("ingl.skala", "", bins = 7)
qual <- hist_with_normal("qual.skala", "", bins = 42)



# SAVE_ME -----------------------------------------------------------------

source("IvanP/R/xx__ggsejv.R")

ggsejv("IvanP/!!!Doktorat/doktorat.tekst/OUT/nep_skala.svg",
       plot = nep, device = grDevices::svg,   # bolji za export u ODT
       AA = "A5.l")

ggsave("IvanP/!!!Doktorat/doktorat.tekst/OUT/qual_skala.svg", plot = qual,
       device = grDevices::svg,
       width = 170, height = 80, units = "mm")






# coreloplot za dodatak - netko je bio kreativan --------------------------
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

qual.2 <- select(qual, -qol1x03, -qol1x10, -qol1x11)

kratka.imena <- c("zrak","buka.promet","smrad","kriminal","smece","guzva","zp.nedostatak","zp.neodrzavanje","setnja","ulice.neodrzavanje","kom.infrastruktura")
# identical(length(names(qual.2)), length(kratka.imena))
names(qual.2) <- kratka.imena

ggcorr(qual.2, label = TRUE)

ggsave("IvanP/!!!Doktorat/doktorat.tekst/OUT/qual_correloplot.svg",
       width = 170, height = 170, units = "mm")

