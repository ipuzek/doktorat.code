### SVEsplit doktorat ###

# CILJ: prostorne jedinice #
# kotar; klaster; zone #

library(forcats)
# source("IvanP/!!!Doktorat/doktorat.code/01__recode.R")

# KOTAREVI ----------------------------------------------------------------

sifrarnik <- readxl::read_xlsx(
  "IvanP/!!!Doktorat/doktorat.code/data_pomocni/sifrarnik__kotarevi.xlsx"
  ) %>% select(ID, kotar.num)

kotar.char <- c(
  "Bačvice-Trstenik",
  "Blatine-Škrape",
  "Bol",
  "Brda-Neslanovac",
  "Gripe",
  "Kman",
  "Kocunar",
  "Lokve",
  "Lovret-Grad",
  "Lučac-Manuš",
  "Mejaši",
  "Meje",
  "Mertojak",
  "Plokite-Sućidar",
  "Pujanke",
  "Ravne njive",
  "Sirobuja",
  "Spinut",
  "Split 3",
  "Varoš",
  "Visoka",
  "Žnjan"
)


sveST$kotar <- "aaa"

for (i in 1:22) {
  sveST$kotar[sveST$id %in% sifrarnik$ID[sifrarnik$kotar.num == i]] <- kotar.char[i]
}

# count(sveST, kotar) %>% View


sveST$klaster <- "aaa"

sifrarnik.klaster <- readxl::read_xlsx(
  "IvanP/!!!Doktorat/doktorat.code/data_pomocni/sifrarnik__kotarevi_klasteri.xlsx"
  )

for (i in 1:22) {
  sveST$klaster[sifrarnik.klaster$kotar[i] == sveST$kotar] <- sifrarnik.klaster$klaster[i]
}

# count(sveST, klaster)
# xtabs(~ kotar + klaster, data = sveST)

# REDUCIRANI KOTAREVI -----------------------------------------------------

count(sveST, kotar) %>% as.data.frame()
  # openxlsx::write.xlsx("kotar.xlsx")

xtabs(~ kotar + klaster, sveST)
  # openxlsx::write.xlsx("kotar_klaster.xlsx")

sveST$zone <- fct_collapse(
  
  sveST$kotar,
  
  `Spinut-Varoš-Meje` = c("Spinut", "Varoš", "Meje"),
  `Lovret-Grad` = "Lovret-Grad",
  `Bačvice-Trstenik-Mertojak` = c("Bačvice-Trstenik", "Mertojak"),
  `Bol` = "Bol",
  `Lučac-Manuš-Gripe` = c("Lučac-Manuš", "Gripe"),
  `Lokve-Blatine-Škrape-Split_3` = c("Lokve", "Blatine-Škrape", "Split 3"),
  `Plokite-Sućidar` = "Plokite-Sućidar",
  `Ravne_njive-Brda-Neslanovac` = c("Ravne njive", "Brda-Neslanovac"),
  `Kman-Kocunar-Pujanke` = c("Kman", "Kocunar", "Pujanke"),
  `Mejaši-Sirobuja-Žnjan-Visoka` = c("Mejaši", "Sirobuja", "Žnjan", "Visoka")
  
  ) %>% 
  
  factor(levels = c("Spinut-Varoš-Meje",
                    "Lovret-Grad",
                    "Bačvice-Trstenik-Mertojak",
                    "Bol",
                    "Lučac-Manuš-Gripe",
                    "Lokve-Blatine-Škrape-Split_3",
                    "Plokite-Sućidar",
                    "Ravne_njive-Brda-Neslanovac",
                    "Kman-Kocunar-Pujanke",
                    "Mejaši-Sirobuja-Žnjan-Visoka"))


# xtabs(~ kotar + zone, sveST) %>% openxlsx::write.xlsx("kotar_zone.xlsx")
# 
# fct_count(sveST$zone)
# levels(sveST$zone)

# just CHECK
# šine <- c("0049301", "0113034", "0148652", "0113069")
# as.character(sveST$sk) %in% šine %>% sum
