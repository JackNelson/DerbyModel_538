horse.num <- seq(from = 1, to = 20, by = 1)
horse.pace <- seq(from = 52, to = 90, by = 2)
horse.place <- seq(from = 1, to = 20, by = 1)
horse.time <- seq(from = 1, to = 20, by = 1)

DerbyResults <- data.frame(horse.num = integer(),
                           horse.pace = integer(),
                           race.num = integer(),
                           horse.time = integer(),
                           horse.place = integer())

for (i in 1:1000){
  race.num <- rep(i,20)
  Results <- data.frame(horse.num, horse.pace, race.num)
  for (horse in 1:20){
    dist = 0
    time = 0
    while (dist < 200){
      x <- sample(1:100, 1)
      time <- time + 1
      if (horse.pace[horse] < x) {
        dist <- dist - 1
      }
      else {
        dist <- dist + 1
      }
    }
    horse.time[horse] = time
  }
  Results$horse.time <- horse.time
  Results <- Results[order(horse.time),]
  Results$horse.place <- horse.place
  DerbyResults <- rbind(DerbyResults, Results)
}

DerbyResults.1st <- aggregate(horse.place ~ horse.num, sum, 
                              data = DerbyResults[which(DerbyResults$horse.place == 1), ])
names(DerbyResults.1st)[names(DerbyResults.1st) == 'horse.place'] <- '1st'

DerbyResults.2nd <- aggregate(horse.place ~ horse.num, sum, 
                              data = DerbyResults[which(DerbyResults$horse.place == 2), ])
DerbyResults.2nd$horse.place <- DerbyResults.2nd$horse.place / 2
names(DerbyResults.2nd)[names(DerbyResults.2nd) == 'horse.place'] <- '2nd'

DerbyResults.3rd <- aggregate(horse.place ~ horse.num, sum, 
                              data = DerbyResults[which(DerbyResults$horse.place == 3), ])
DerbyResults.3rd$horse.place <- DerbyResults.3rd$horse.place / 3
names(DerbyResults.3rd)[names(DerbyResults.3rd) == 'horse.place'] <- '3rd'

DerbyResults.Summary <- Reduce(function(x,y) merge(x, y, all=TRUE), 
                               list(DerbyResults.1st,DerbyResults.2nd,DerbyResults.3rd))
  
DerbyResults.Summary
