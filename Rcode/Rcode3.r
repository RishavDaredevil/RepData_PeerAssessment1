sum(is.na(actve$steps))
library(impute)
prob4 <- actve[-2]
prob4 <- impute.knn(as.matrix(prob4),k = 3)$data
prob4 <- tibble(steps = prob4[,1],
                interval = prob4[,2],date=actve$date)
prob5 <- prob4 |> group_by(date) |> 
    summarise(total_steps = sum(steps))
g <- prob5 |> ggplot(aes(total_steps))
g + geom_histogram(bins = 20,color = 'white')
prob6 <- prob4 |> group_by(date) |> 
    summarise(mean = mean(steps),
              median = median(steps))

