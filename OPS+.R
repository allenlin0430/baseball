library(dplyr)
library(xml2)

## 計算OPS, OPS+, 
# OPS = OBP + SLG
# OPS+ = (SLG/聯盟平均SLG)+(OBP/聯盟平均OBP)-1 百分比表示
dat.b = read.csv(paste(Sys.Date(),"BattersPlayers.csv", sep = ""))

# 球隊出賽場次
url = "http://www.cpbl.com.tw/standing/season/2016.html?&year=2016&season=0"
doc = read_html(url)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[2]"
team = xml_text(xml_find_all(doc, xpath),trim = T)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[3]"
games = xml_text(xml_find_all(doc, xpath),trim = T)
tmp = cbind(team,games)
# 新增 TG 變數
dat.b = cbind(dat.b,"TG" = rep(0,nrow(dat.b)))
dat.b[which(dat.b$team == "中信兄弟"),"TG"] = as.numeric(tmp[team == "中信兄弟", 2])
dat.b[which(dat.b$team == "統一7-ELEVEn"),"TG"] = as.numeric(tmp[team == "統一7-ELEVEn", 2])
dat.b[which(dat.b$team == "Lamigo"),"TG"] = as.numeric(tmp[team == "Lamigo", 2])
dat.b[which(dat.b$team == "義大"),"TG"] = as.numeric(tmp[team == "義大", 2])

# 取出PA/G >= 3.1 的球員
dat = dat.b %>%
  mutate(`BB/K` = round(BB/SO, 2),
         `AB/HR` = round(AB/HR, 2),
         OPS = round(OBP + SLG, 3),
         `OPS+` = round(((OBP/(sum(H+BB+IBB+HBP)/sum(AB+BB+IBB+HBP+SF)))+(SLG/(sum(TB)/sum(AB)))-1)*100, 1),
         IsoP = SLG-AVG) %>%
  filter(PA/TG >= 3.1) %>% 
  select(player, AB, AVG, OBP, SLG, `BB/K`, `AB/HR`, OPS, `OPS+`, IsoP, team)


# 取出 2 <= PA/G < 3 1
dat.1 = dat.b %>%
  mutate(`BB/K` = round(BB/SO, 2),
         `AB/HR` = round(AB/HR, 2),
         OPS = round(OBP + SLG, 3),
         `OPS+` = round(((OBP/(sum(H+BB+IBB+HBP)/sum(AB+BB+IBB+HBP+SF)))+(SLG/(sum(TB)/sum(AB)))-1)*100, 1),
         IsoP = SLG-AVG) %>%
  filter(2 <= PA/TG & PA/TG < 3.1) %>% 
  select(player, AB, AVG, OBP, SLG, `BB/K`, `AB/HR`, OPS, `OPS+`, IsoP, team)

write.csv(dat, paste(Sys.Date(),"OPS.csv"), row.names = F)
write.csv(dat.1, paste(Sys.Date(),"OPSlessPA.csv"), row.names = F)
