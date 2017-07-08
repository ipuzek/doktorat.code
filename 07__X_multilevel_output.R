# PRVI --------------------------------------------------------------------

multi.prvi.ranefs$zone %>%
  mutate_if(is.numeric, round, 2)


# (Intercept) qual.skala
# 1         1.39      -0.05
# 2        -1.95       0.08
# 3         0.39      -0.01
# 4         1.03      -0.04
# 5        -0.74       0.03
# 6        -0.14       0.01
# 7        -1.89       0.07
# 8         0.94      -0.04
# 9         0.50      -0.02
# 10       -2.86       0.11


multi.prvi.se.ranefs$zone %>% round(2)

# (Intercept) qual.skala
# Spinut-Varoš-Meje                   1.43       0.06
# Lovret-Grad                         1.52       0.06
# Bačvice-Trstenik-Mertojak           1.39       0.05
# Bol                                 1.55       0.06
# Lučac-Manuš-Gripe                   1.50       0.06
# Lokve-Blatine-Škrape-Split_3        1.34       0.05
# Plokite-Sućidar                     1.28       0.05
# Ravne_njive-Brda-Neslanovac         1.33       0.05
# Kman-Kocunar-Pujanke                1.39       0.05
# Mejaši-Sirobuja-Žnjan-Visoka        1.30       0.05


# FINAL -------------------------------------------------------------------

multi.final.ranefs$zone %>%
  mutate_if(is.numeric, round, 2)

# > multi.final.ranefs$zone %>%
  # +   mutate_if(is.numeric, round, 2)
(Intercept) qual.skala spolŽena  dmg2
1            0      -0.01     0.48  0.00
2            0       0.09     2.10 -0.07
3            0       0.01     0.57 -0.01
4            0       0.05     1.95 -0.06
5            0       0.02     0.47 -0.01
6            0       0.05     1.48 -0.05
7            0       0.06     1.13 -0.04
8            0       0.02     1.52 -0.04
9            0       0.05     1.40 -0.04
10           0       0.06     1.95 -0.06

multi.final.se.ranefs$zone %>% round(2)

# > multi.final.se.ranefs$zone %>% round(2)
(Intercept) qual.skala spolŽena dmg2
Spinut-Varoš-Meje                      0       0.03     0.81 0.02
Lovret-Grad                            0       0.04     0.95 0.03
Bačvice-Trstenik-Mertojak              0       0.03     0.84 0.02
Bol                                    0       0.04     0.96 0.03
Lučac-Manuš-Gripe                      0       0.04     0.95 0.03
Lokve-Blatine-Škrape-Split_3           0       0.03     0.76 0.02
Plokite-Sućidar                        0       0.03     0.90 0.03
Ravne_njive-Brda-Neslanovac            0       0.03     0.84 0.03
Kman-Kocunar-Pujanke                   0       0.04     0.96 0.03
Mejaši-Sirobuja-Žnjan-Visoka           0       0.03     0.85 0.02