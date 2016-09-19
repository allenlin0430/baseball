library(dplyr)
library(xml2)

## 計算OPS, OPS+, 
# OPS = OBP + SLG
# OPS+ = (SLG/聯盟平均SLG)+(OBP/聯盟平均OBP)-1 百分比表示
dat.b = read.csv(paste(Sys.Date(),"BattersPlayers.csv", sep = ""))


# 取出PA/TG >= 3.1 的球員
dat = dat.b %>%
  mutate(`BB/K` = round(BB/SO, 2),
         `AB/HR` = round(AB/HR, 2),
         OPS = round(OBP + SLG, 3),
         `OPS+` = round(((OBP/(sum(H+BB+IBB+HBP)/sum(AB+BB+IBB+HBP+SF)))+(SLG/(sum(TB)/sum(AB)))-1)*100, 1),
         IsoP = SLG-AVG) %>%
  filter(PA/TG >= 3.1) %>% 
  select(NAME, AB, AVG, OBP, SLG, `BB/K`, `AB/HR`, OPS, `OPS+`, IsoP, team)


# 取出 2 <= PA/TG < 3 1
dat.1 = dat.b %>%
  mutate(`BB/K` = round(BB/SO, 2),
         `AB/HR` = round(AB/HR, 2),
         OPS = round(OBP + SLG, 3),
         `OPS+` = round(((OBP/(sum(H+BB+IBB+HBP)/sum(AB+BB+IBB+HBP+SF)))+(SLG/(sum(TB)/sum(AB)))-1)*100, 1),
         IsoP = SLG-AVG) %>%
  filter(2 <= PA/TG & PA/TG < 3.1) %>% 
  select(NAME, AB, AVG, OBP, SLG, `BB/K`, `AB/HR`, OPS, `OPS+`, IsoP, team)

write.csv(dat, paste(Sys.Date(),"OPS.csv"), row.names = F)
write.csv(dat.1, paste(Sys.Date(),"OPSlessPA.csv"), row.names = F)
