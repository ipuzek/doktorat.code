library(survey)

source("/IvanP/R/pocetni.R")

sveST <- haven::read_spss("/IvanP/!Istrazivanja/Split - kulturne potrebe/Podaci/web0903_13_lab_v1_encoding.sav")

http://stats.stackexchange.com/questions/7562/simple-post-stratification-weights-in-r?rq=1
## Fix id and weights for your data. 
sveST.Design <- svydesign(id = ~id, data = sveST , weights = ~rim_w_1)
str(sveST.Design)
