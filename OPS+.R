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
dat.b[which(dat.b$"球隊" == "中信兄弟"),"TG"] = as.numeric(tmp[team == "中信兄弟", 2])
dat.b[which(dat.b$"球隊" == "統一7-ELEVEn"),"TG"] = as.numeric(tmp[team == "統一7-ELEVEn", 2])
dat.b[which(dat.b$"球隊" == "Lamigo"),"TG"] = as.numeric(tmp[team == "Lamigo", 2])
dat.b[which(dat.b$"球隊" == "義大"),"TG"] = as.numeric(tmp[team == "義大", 2])

attach(dat.b)
meanobp = sum(H+BB+IBB+HBP)/sum(AB+BB+IBB+HBP+SF)
meanslg = sum(TB)/sum(AB)
ops = OBP + SLG
opsplus = ((OBP/meanobp)+(SLG/meanslg)-1)*100
isop = SLG-AVG
detach(dat.b)
dat.b = cbind(dat.b, "OPS" = ops, "OPS+"= opsplus, "IsoP" = isop)

# 取出PA/G >= 3.1 的球員
dat = filter(dat.b, dat.b$PA/dat.b$TG >= 3.1)
attach(dat)
dat = cbind("球員" = as.character(球員名稱),
            "球隊" = as.character(球隊),
            "打席" = PA,
            "打擊率" = AVG,
            "上壘率" = OBP,
            "長打率" = SLG,
            "AB/HR" = round(AB/HR, 2),
            "BB/k" = round(BB/SO, 2),
            "OPS" = round(OPS,3),
            "OPS+" = round(`OPS+`,1),
            "IsoP" = IsoP)
dat = data.frame(dat, stringsAsFactors = F)
for(i in 3:ncol(dat)){
  dat[,i] = as.numeric(dat[,i])
}
dat$'球隊' = factor(dat$'球隊')
detach(dat)

# 取出 2 <= PA/G < 3 1
dat.1 = filter(dat.b,2 <= dat.b$PA/dat.b$TG & dat.b$PA/dat.b$TG < 3.1)
attach(dat.1)
dat.1 = cbind("球員" = as.character(球員名稱),
            "球隊" = as.character(球隊),
            "打席" = PA,
            "打擊率" = AVG,
            "上壘率" = OBP,
            "長打率" = SLG,
            "AB/HR" = round(AB/HR, 2),
            "BB/k" = round(BB/SO, 2),
            "OPS" = round(OPS,3),
            "OPS+" = round(`OPS+`,1),
            "IsoP" = IsoP)
dat.1 = data.frame(dat.1, stringsAsFactors = F)
dat.1$'球隊' = factor(dat.1$'球隊')
for(i in 3:ncol(dat.1)){
  dat.1[,i] = as.numeric(dat.1[,i])
}
detach(dat.1)

write.csv(dat, paste(Sys.Date(),"OPS.csv"), row.names = F)
write.csv(dat.1, paste(Sys.Date(),"OPSlessPA.csv"), row.names = F)
