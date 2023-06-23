prob7 <- prob4 %>% 
    mutate(diff = factor(ifelse(
        wday(prob4$date)>1&
            wday(prob4$date)<7,1,2),
        labels = c('weekday','weekend')))
prob7 <- prob7 |> group_by(interval,diff) |> 
    summarise(mean = mean(steps,na.rm = TRUE))
g <- prob7 |> ggplot(aes(interval,mean))
g + geom_line() + facet_grid(diff~.)
        
           