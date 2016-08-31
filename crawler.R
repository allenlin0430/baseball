library(xml2)
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
repeat{
  url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=pbat&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath <- "//*[@class='gap_b20']/table/tr/td"
  t1 <- xml_text(xml_find_all(doc, xpath))
  t1 = data.frame(matrix(t1, ncol = 31, byrow = T), stringsAsFactors = F)
  if(nrow(t1) == 0) break
  t = rbind(t,t1)
  p = p + 1
}

# title names
xpath <- "//*[@class='gap_b20']/table/tr[1]/th"  
title.name = xml_text(xml_find_all(doc, xpath))
colnames(t) = title.name

for(i in 3:31){
  t[,i] <- as.numeric(t[,i])
}
batters = t[,-1]
head(batters)
dim(batters)

# 打者選手的個人資料連結
p = 1
t1 = c()
name1 = c()
repeat{
  url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=pbat&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath.player <- "//*[@class='gap_b20']/table//tr/td[2]/a"
  name = xml_text(xml_find_all(doc, xpath.player))
  if(length(name) == 0) break
  t = xml_attrs(xml_find_all(doc, xpath.player), "href")
  t = paste0("http://www.cpbl.com.tw",unlist(t),sep = "")
  t1 = c(t1,t)
  name1 = c(name1, name)
  p = p+1
}
batterinfo = cbind("球員名稱" = name1,"網址" = t1)
head(batterinfo)
batterinfo = cbind(batterinfo, "球隊" = rep(0,nrow(batterinfo)))
# 設定球隊
for(i in 1:nrow(batterinfo)){
  url = as.character(batterinfo[i,2])
  doc <- read_html(url)
  xpath = "/html/body/div[4]/div/div/h1/div/a[1]"
  team.name = substring(xml_text(xml_find_all(doc, xpath)), first = 1)
  batterinfo[i,3] = team.name
}
batterinfo = cbind(batterinfo, "狀態" = rep(0,nrow(batterinfo)))
# 藉由隊名裡的二軍，來分類球員目前狀態(一軍/二軍)
farm.index = grep("二軍",batterinfo[,"球隊"])
batterinfo[farm.index,4] = "二軍"
batterinfo[-farm.index,4] = "一軍"
names(batterinfo)[4] = "狀態"

# 清除整理隊名
batterinfo[,"球隊"] = sub("統一二軍","統一7-ELEVEn",batterinfo[,"球隊"])
batterinfo[,"球隊"] = sub("二軍","",batterinfo[,"球隊"])
batterinfo = batterinfo[,c("球員名稱","球隊","狀態")]
dat.b = merge(batterinfo, batters , by.x = "球員名稱", by.y = "NAME")

# Save
write.csv(dat.b, paste(Sys.Date(),"BattersPlayers.csv", sep = ""),row.names = F)
  
rm(list = ls())

#### 投手 ####
# 自動收集每頁的資料
p = 1
t = c()
repeat{
  url <- url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=ppit&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath <- "//*[@class='gap_b20']/table/tr/td"
  t1 <- xml_text(xml_find_all(doc, xpath))
  t1 = data.frame(matrix(t1, ncol = 31, byrow = T), stringsAsFactors = F)
  if(nrow(t1) == 0) break
  t = rbind(t,t1)
  p = p + 1
}
# title names
xpath <- "//*[@class='gap_b20']/table/tr[1]/th"  
title.name = xml_text(xml_find_all(doc, xpath))
colnames(t) = title.name
for(i in 3:31){
  t[,i] <- as.numeric(t[,i])
}
pitchers = t[,-1]

# 投手選手的個人資料連結
p = 1
t1 = c()
name1 = c()
repeat{
  url <- paste("http://www.cpbl.com.tw/stats/all.html?year=2016&stat=ppit&online=0&sort=G&order=desc&per_page=", p, sep = "")
  doc <- read_html(url)
  xpath.player <- "//*[@class='gap_b20']/table//tr/td[2]/a"
  name = xml_text(xml_find_all(doc, xpath.player))
  if(length(name) == 0) break
  t = xml_attrs(xml_find_all(doc, xpath.player), "href")
  t = paste0("http://www.cpbl.com.tw",unlist(t),sep = "")
  t1 = c(t1,t)
  name1 = c(name1, name)
  p = p+1
}
pitchersinfo = cbind("球員名稱" = name1,"網址" = t1)
head(pitchersinfo)

# 球隊標示
pitchersinfo = cbind(pitchersinfo, "球隊" = rep(0, nrow(pitchersinfo)))
for(i in 1:nrow(pitchersinfo)){
  url = as.character(pitchersinfo[i,2])
  doc <- read_html(url)
  xpath = "/html/body/div[4]/div/div/h1/div/a[1]"
  team.name = substring(xml_text(xml_find_all(doc, xpath)), first = 1)
  pitchersinfo[i,3] = team.name
}

# 藉由隊名裡的二軍，來分類球員目前狀態(一軍/二軍)
pitchersinfo = cbind(pitchersinfo, "狀態" = rep(0, nrow(pitchersinfo)))
farm.index = grep("二軍",pitchersinfo[,"球隊"])
pitchersinfo[farm.index,4] = "二軍"
pitchersinfo[-farm.index,4] = "一軍"
names(pitchersinfo)[4] = "狀態"

# 清除整理隊名
pitchersinfo[,"球隊"] = sub("統一二軍","統一7-ELEVEn",pitchersinfo[,"球隊"])
pitchersinfo[,"球隊"] = sub("二軍","",pitchersinfo[,"球隊"])
pitchersinfo = pitchersinfo[,c("球員名稱","球隊","狀態")]
dat.p = merge(pitchersinfo, pitchers , by.x = "球員名稱", by.y = "NAME")

# Save
write.csv(dat.p, paste(Sys.Date(),"PitchersPlayers.csv", sep = ""), row.names = F )
rm(list = ls())

dat.p = read.csv( paste(Sys.Date(),"PitchersPlayers.csv", sep = ""))
dat.b = read.csv(paste(Sys.Date(),"BattersPlayers.csv", sep = ""))
