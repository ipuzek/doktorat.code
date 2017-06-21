# skala
# 

library(psych)

nmz <- sveST.6 %>% select(vrij14x01:vrij14x10) %>% var_label() %>% unlist() %>% unname()

sveST.6 %>%
  select(vrij14x01:vrij14x09) %>% 
  mutate_all(recode_98) %>% 
  cor(use = "pairwise.complete.obs")

sveST.6 %>%
  select(vrij14x01:vrij14x09) %>% 
  mutate_all(recode_98) %>%
  psych::alpha(keys = c("vrij14x01", "vrij14x05", "vrij14x07", "vrij14x08"))




select(scopes.2015, starts_with("p37x"), -p37x2, -p37x7) %>% 
  var_label()

select(scopes.2015, starts_with("p37x"), -p37x2, -p37x7) %>% 
  map(frre)

skala <- scopes.2015 %>% 
  select(starts_with("p37x"), -p37x2, -p37x7) %>%
  mutate_all(as.numeric) %>% 
  mutate_all(na_iff, y = 6)

cor(skala, use = "pairwise.complete.obs")
alpha(as.data.frame(skala), check.keys = TRUE)

# očekivano, 4. čestica ne valja. izbacujem.
skala.fin <- skala %>% select(-p37x4)

# alpha = .71
alpha(as.data.frame(skala.fin), check.keys = TRUE)

# samo kompletni odgovori
scopes.2015$scale.gend <- rowSums(skala.fin, na.rm = FALSE)


# shared histogram in ggplot ----------------------------------------------
scopes.bg <- select(scopes.2015, - country)

scopes.2015 %>% 
  group_by(country) %>%
  mutate(gr_mean_scale.gend = mean(scale.gend, na.rm = TRUE)) %>%
  ggplot(aes(x = scale.gend)) +
  geom_bar(data = scopes.bg, fill = "grey") +
  geom_bar(aes(fill = country)) +
  geom_vline(aes(xintercept =  gr_mean_scale.gend), colour = "darkgrey") +
  facet_wrap(~ country) +
  guides(fill = FALSE)  # to remove the legend


mean_se.df <- scopes.2015 %>%
  split(.$country) %>%              # group_by(country) %>% 
  map("scale.gend") %>%             # do(mean_se(.$scale.gend))
  map_df(mean_se, .id = "country")  


scopes.2015 %>%
  ggplot(aes(x = scale.gend)) +
  geom_histogram(data = scopes.bg, fill = "grey", binwidth = 2) +
  geom_histogram(aes(fill = country), binwidth = 2) +
  lajna(mean_se.df, size = 2) +
  lajna_min(mean_se.df) + lajna_max(mean_se.df) +
  facet_grid(country ~ .) +
  guides(fill = FALSE)  # to remove the legend

lajna <- function(data, size) {
  geom_vline(data = data, aes_string(xintercept = "y"), 
             alpha = .7, colour = "darkgrey", size = size) }
lajna_min <- function(data) {
  geom_vline(data = data, aes_string(xintercept = "ymin"),
             colour = "red", size = .2) }
lajna_max <- function(data) {    
  geom_vline(data = data, aes_string(xintercept = "ymax"),
             colour = "red", size = .2) }


