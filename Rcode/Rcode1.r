actve <- read_csv('activity.zip')
actve_no_na <- actve[which(!is.na(actve[1])),]
prob1 <- actve_no_na |> group_by(date) |> 
    summarise(total_steps = sum(steps))
g <- prob1 |> ggplot(aes(total_steps))
g + geom_histogram(bins = 20,color = 'white')
prob2 <- actve_no_na |> group_by(date) |> 
    summarise(mean = mean(steps),
              median = median(steps))