
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


