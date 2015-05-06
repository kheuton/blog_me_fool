library(shiny); library(gsheet); library(plyr); library(ggplot2)
setwd("~/Dropbox/sopt_it/compare_app/")

rm(list=ls())

link <- paste0("https://docs.google.com/spreadsheets/d/1XsWOxX099kS0ynf40s",
               "FTqaTrnfpxbe_N8_cRs5gOhzQ/edit?usp=sharing")
df <- as.data.frame(gsheet2tbl(link, sheetid=0) [,1:5])
df[df$Deck == "","Deck"] <- "Unknown"
df$Date_id <- as.Date(df$Date_id, format="%m/%d/%Y")

k=100
source("./functions.R")

elo_graph <- function(df, k=25){
    Player <- unique(df$Player)
    base <- create_elo_base(df)
    df <- df[order(df$Date_id),]
    for(i in unique(df$Game_ID)){
        new_game <- subset(df, Game_ID == i)
        base <- update_elo(new_game, base, k)
    }
    standings <- base[!duplicated(base$Player, fromLast = TRUE),]
    standings <- standings[order(-standings$score),]
    best <- standings$Player[1:min(c(5, nrow(standings)))]
    rownames(base) <- NULL
    #standings$date <- NULL
    base <- subset(base, Player %in% best)
    base <- base[duplicated(base$Player),]
    base[!duplicated(base[,c("Player", "date")], fromLast=TRUE),]
}

elo2_graph <- function(df, k=25){
    Player <- unique(df$Player)
    base <- create_elo_base(df)
    df <- df[order(df$Date_id),]
    for(i in unique(df$Game_ID)){
        new_game <- subset(df, Game_ID == i)
        base <- update_elo2(new_game, base, k)
    }
    standings <- base[!duplicated(base$Player, fromLast = TRUE),]
    standings <- standings[order(-standings$score),]
    best <- standings$Player[1:min(c(5, nrow(standings)))]
    rownames(base) <- NULL
    #standings$date <- NULL
    base <- subset(base, Player %in% best)
    base <- base[duplicated(base$Player),]
    base[!duplicated(base[,c("Player", "date")], fromLast=TRUE),]
}

temp <- elo2_graph(df)

ggplot(data=temp, aes(x=date, y=score, group=Player, color=Player)) + geom_line()
