#1 druid
#2 hunter
#3 mage
#4 paladin
#5 priest
#6 rogue
#7 shaman
#8 warlock
#9 warrior


library(jsonlite)
data <- fromJSON("output_file1.json")
cards_data <-fromJSON("cards.json")
attach(data)
date <- as.Date(substr(date,1,10), "%Y-%m-%d")

deck1 = cards[[2]]
for (i in deck1){
  print(cards_data[which(cards_data$dbfId == i),]$name)
}

deck_archetype[which(deck_archetype == "Edit")] = "Unknown"
table(deck_type)
ladder = c(which(deck_type == "None"), which(deck_type == "Ranked Deck"), which(deck_type == "Theorycraft"), which(deck_type == "Tournament"))
dataT = data[ladder,]

dataS = dataT[which(deck_format == "S"),]

datelimit = as.Date("2017-06-09")
dataR = dataS[which(date >= datelimit),]

attach(dataR)
for (class in classes <- split(dataR, deck_class)){
  counts = data.frame()
  for (arc in archetypes <- split(class, class$deck_archetype)){
    counts = rbind(counts, nrow(arc))
  }
  plt1 = cbind(counts,names(archetypes))
  plt1 = plt1[which(plt1[,2] != "Unknown"),]
  plt2 = plt1[order(-plt1[,1],plt1[,2]),]
  barplot(plt2[1:5,1],names.arg=plt2[1:5,2], las=1)
  par(mar=c(10,4,2,2))
  print(sd(plt2[1:5,1]))
}

huntercards = dataR[which(deck_class == "Hunter"),]$cards
huntercards2 = unlist(huntercards)
huntercards3 = data.frame(table(huntercards2))
huntercards4 = huntercards3[order(-huntercards3[,2],huntercards3[,1]),]
for (i in seq(1,10,1)){
  if (i == 1){
    print("Most Used Hunter Cards:")
  }
  cat(cards_data[which(cards_data$dbfId == huntercards4[i,1]),]$name, ", cost: ", cards_data[which(cards_data$dbfId == huntercards4[i,1]),]$cost, "\n")
}

#reformat card column
card_col = ["card_"]

#regression
regs = c()
for (deck in dataR){
  
  model <- glm(included ~ data$cards ,family=binomial(link='logit'),data=data)
} 