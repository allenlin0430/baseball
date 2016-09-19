library(xml2)
library(dplyr)
#library(xmlview)
#### batter stat data
### set your target url
#url <- "http://www.cpbl.com.tw/stats/all.html?year=2016&stat=pbat&online=0&sort=G&order=desc&per_page="
#doc   <- read_html(url)
### open the document to test your xpath 方便的檢視器
#xml_view(doc, add_filter = T)
### Set the xpath of info needed
#xpath <- "//*[@class='gap_b20']/table/tr/td"
#t <- xml_text(xml_find_all(doc, xpath))
#t = data.frame(matrix(t, ncol = 31, byrow = T), stringsAsFactors = F)

#### 自動收集每頁的資料，每天都可更新 ####
#### 打者 ####
p = 1
t = c()
u = c()
repeat{
  url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=pbat&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath <- "//*[@class='gap_b20']/table/tr/td"
  t1 <- xml_text(xml_find_all(doc, xpath))
  t1 = data.frame(matrix(t1, ncol = 31, byrow = T), stringsAsFactors = F)
  if(nrow(t1) == 0) break
  t = rbind(t,t1)
  xpath.player <- "//*[@class='gap_b20']/table//tr/td[2]/a"
  u1 = xml_attrs(xml_find_all(doc, xpath.player), "href")
  u1 = paste0("http://www.cpbl.com.tw",unlist(u1),sep = "")
  u = c(u, u1)
  p = p + 1
}

# title names
xpath <- "//*[@class='gap_b20']/table/tr[1]/th"  
title.name = xml_text(xml_find_all(doc, xpath))
colnames(t) = title.name

# 打者選手的個人資料連結 var.32
t = cbind(t, u)

# 轉成數值
for(i in 3:31){
  t[,i] <- as.numeric(t[,i])
}
t[,32] = as.character(t[,32])
batters = t[,-1]
head(batters)
dim(batters)


# 設定球隊
for(i in 1:nrow(batters)){
  url = batters[i,31]
  doc <- read_html(url)
  xpath = "/html/body/div[4]/div/div/h1/div/a[1]"
  team.name = substring(xml_text(xml_find_all(doc, xpath)), first = 1)
  batters[i,31] = team.name
}
colnames(batters)[31] = 'team'


batters = cbind(batters, level = rep(0,nrow(batters)))
# 藉由隊名裡的二軍，來分類球員目前狀態(一軍/二軍)
farm.index = grep("二軍",batters[, 'team'])
batters[farm.index, 32] = "二軍"
batters[-farm.index, 32] = "一軍"


# 清除整理隊名
batters[,"team"] = sub("統一二軍","統一7-ELEVEn",batters[,"team"])
batters[,"team"] = sub("二軍","",batters[,"team"])


# 球隊出賽場次
url = "http://www.cpbl.com.tw/standing/season/2016.html?&year=2016&season=0"
doc = read_html(url)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[2]"
team = xml_text(xml_find_all(doc, xpath),trim = T)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[3]"
games = xml_text(xml_find_all(doc, xpath),trim = T)
tmp = cbind(team,games)
# 新增 TG 變數
batters = cbind(batters,"TG" = rep(0,nrow(batters)))
batters[which(batters$team == "中信兄弟"),"TG"] = as.numeric(tmp[team == "中信兄弟", 2])
batters[which(batters$team == "統一7-ELEVEn"),"TG"] = as.numeric(tmp[team == "統一7-ELEVEn", 2])
batters[which(batters$team == "Lamigo"),"TG"] = as.numeric(tmp[team == "Lamigo", 2])
batters[which(batters$team == "義大"),"TG"] = as.numeric(tmp[team == "義大", 2])



# Save
write.csv(batters, paste(Sys.Date(),"BattersPlayers.csv", sep = ""),row.names = F)
  
rm(list = ls())

#### 投手 ####
# 自動收集每頁的資料
p = 1
t = c()
u = c()
repeat{
  url <- url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=ppit&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath <- "//*[@class='gap_b20']/table/tr/td"
  t1 <- xml_text(xml_find_all(doc, xpath))
  t1 = data.frame(matrix(t1, ncol = 31, byrow = T), stringsAsFactors = F)
  if(nrow(t1) == 0) break
  t = rbind(t,t1)
  xpath.player <- "//*[@class='gap_b20']/table//tr/td[2]/a"
  u1 = xml_attrs(xml_find_all(doc, xpath.player), "href")
  u1 = paste0("http://www.cpbl.com.tw",unlist(u1),sep = "")
  u = c(u, u1)
  p = p + 1
}
# title names
xpath <- "//*[@class='gap_b20']/table/tr[1]/th"  
title.name = xml_text(xml_find_all(doc, xpath))
colnames(t) = title.name

# 投手選手的個人資料連結 var.32
t = cbind(t, u)

# 轉成數值
for(i in 3:31){
  t[,i] <- as.numeric(t[,i])
}
t[,32] = as.character(t[,32])
pitchers = t[,-1]


# 球隊標示
for(i in 1:nrow(pitchers)){
  url = as.character(pitchers[i,31])
  doc <- read_html(url)
  xpath = "/html/body/div[4]/div/div/h1/div/a[1]"
  team.name = substring(xml_text(xml_find_all(doc, xpath)), first = 1)
  pitchers[i,31] = team.name
}
colnames(pitchers)[31] = 'team'
# 藉由隊名裡的二軍，來分類球員目前狀態(一軍/二軍)
pitchers = cbind(pitchers, level = rep(0, nrow(pitchers)))
farm.index = grep("二軍",pitchers[,"team"])
pitchers[farm.index, 32] = "二軍"
pitchers[-farm.index, 32] = "一軍"

# 清除整理隊名
pitchers[,"team"] = sub("統一二軍","統一7-ELEVEn",pitchers[,"team"])
pitchers[,"team"] = sub("二軍","",pitchers[,"team"])

# 球隊出賽場次
url = "http://www.cpbl.com.tw/standing/season/2016.html?&year=2016&season=0"
doc = read_html(url)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[2]"
team = xml_text(xml_find_all(doc, xpath),trim = T)
xpath = "//*[@class = 'gap_b20']/table[1]/tr/td[3]"
games = xml_text(xml_find_all(doc, xpath),trim = T)
tmp = cbind(team,games)
# 新增 TG 變數
pitchers = cbind(pitchers,"TG" = rep(0,nrow(pitchers)))
pitchers[which(pitchers$team == "中信兄弟"),"TG"] = as.numeric(tmp[team == "中信兄弟", 2])
pitchers[which(pitchers$team == "統一7-ELEVEn"),"TG"] = as.numeric(tmp[team == "統一7-ELEVEn", 2])
pitchers[which(pitchers$team == "Lamigo"),"TG"] = as.numeric(tmp[team == "Lamigo", 2])
pitchers[which(pitchers$team == "義大"),"TG"] = as.numeric(tmp[team == "義大", 2])

# Save
write.csv(pitchers, paste(Sys.Date(),"PitchersPlayers.csv", sep = ""), row.names = F )
rm(list = ls())

dat.p = read.csv( paste(Sys.Date(),"PitchersPlayers.csv", sep = ""))
dat.b = read.csv(paste(Sys.Date(),"BattersPlayers.csv", sep = ""))
