library(ggplot2)
library(gridExtra)
library(animation)

horse.num <- seq(from = 1, to = 20, by = 1)
horse.pace <- seq(from = 52, to = 90, by = 2)
horse.place <- rep(NA, 20)
horse.time <- rep(0, 20)
horse.dist <- rep(0, 20)

DerbyResults <- data.frame(horse.num = integer(),
                           horse.pace = integer(),
                           race.num = integer(),
                           horse.time = integer(),
                           horse.place = integer(),
                           horse.dist = integer())

saveVideo({
  i <- 1
    race.num <- rep(i, 20)
    
    Results <- data.frame(horse.num, horse.pace, race.num, 
                          horse.time, horse.place, horse.dist)
    clock <- 0
    rank <- 1
    
    while (rank < 6){
      clock <- clock + 1
      for (horse in 1:20){
        if (Results$horse.dist[horse] < 200){
          x <- sample(1:100, 1)
          if (Results$horse.pace[horse] < x){
            Results$horse.dist[horse] <- Results$horse.dist[horse] - 1
          }
          else {
            Results$horse.dist[horse] <- Results$horse.dist[horse] + 1
            if (Results$horse.dist[horse] == 200){
              Results$horse.time[horse] <- clock
              Results$horse.place[horse] <- rank
              rank <- rank + 1
            }
            else {
            }
          }
        }  
        else {
        }
      }
      #plotting for each time interval
      p1 <- ggplot(Results, aes(x = horse.dist, y = horse.num, colour = horse.num)) + 
        geom_point(size=2) +
        labs(x = "Distance (M)", y = "Horse") +
        ggtitle(paste("The Lucky Derby Race ", i)) + 
        scale_x_continuous(limits = c(-10, 200)) + 
        theme(plot.title = element_text(size=10, face='bold', hjust=0.5), 
              legend.title = element_blank())
      
      p2 <- tableGrob(head(Results[order(Results$horse.place, -Results$horse.dist), 
                                   c(1,6,5)], n = 5), 
                      cols = c("Horse", "Distance", "Place"))    
      
      grid.arrange(p1, p2, ncol = 1)            
      
    }
}, interval = 0.1, ani.width = 550, ani.height = 550)  