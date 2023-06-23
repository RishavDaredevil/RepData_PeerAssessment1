prob3 <- actve |> group_by(interval) |> 
    summarise(mean = mean(steps,na.rm = TRUE))
g <- prob3 |> ggplot(aes(interval,mean))
g + geom_line()
prob3[which.max(prob3$mean),1]
