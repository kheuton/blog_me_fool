library(shiny); library(gsheet); library(plyr)
setwd("~/Dropbox/sopt_it/compare_app/")

rm(list=ls())

link <- paste0("https://docs.google.com/spreadsheets/d/1XsWOxX099kS0ynf40s",
               "FTqaTrnfpxbe_N8_cRs5gOhzQ/edit?usp=sharing")
df <- as.data.frame(gsheet2tbl(link, sheetid=0) [,1:5])
df[df$Deck == "","Deck"] <- "Unknown"
df$Date_id <- as.Date(df$Date_id, format="%m/%d/%Y")

k=100
source("./functions.R")
Player <- unique(df$Player)
base <- create_elo_base(df)
df <- df[order(df$Date_id),]
for(i in unique(df$Game_ID)){
    new_game <- subset(df, Game_ID == i)
    base <- update_elo(new_game, base, k)
}
#standings <- base[!duplicated(base$Player, fromLast = TRUE),]
#standings <- standings[order(-standings$score),]
#rownames(standings) <- NULL
#standings$date <- NULL
#standings
#base
players <- c("Pat", "Neal", "Baby K", "Claire", "Mari")

plot (base$date, base$score, type = 'n')
for (i in 1:length (players)){
    temp <-  subset (base, Player == players [i])
    temp <- ddply(temp, ~date,summarise,score=mean(score))
    lines (temp$date, temp$score, col = i, 
           lty = 6, lwd = 1)
}

legend ("topleft", players, col = 1:5, lty = 6, lwd = 1, 
        cex=.7, pt.cex=3, text.font=2)
