# India at the Olympics

This is a statistical analysis of India's performance at the Olympics over the years. This analysis tries to answer (or ask!) questions as to why, when and how India's performance has been impacted by various factors. It also tries to compare India's performance against the other countries which are either considered equivalent or better developed than India.

Note: This analysis is done in R using Rstudio. The code mentioned in this document can be used to recreate the analysis

## Initial analysis: India and the rest of the world at the olympics

Let's compare India's performance to other top performing nations at the Olympics over the entire span.


```r
# Load data
medalists <- read.csv('archive/athlete_events.csv', stringsAsFactors = FALSE) %>% 
  filter(Season == 'Summer', Medal %in% c('Gold','Silver','Bronze')) %>% 
  select(Sex, Age, Team, NOC, Year, City, Sport, Medal)

# India compared to the top 10 ----
tally <- medalists %>%  
  group_by(Team) %>% 
    summarise(Total = n()) %>% 
      arrange(desc(Total))

# Data pre-processing
tally$Team <- factor(tally$Team) %>% fct_reorder(tally$Total)
top10 <- tally %>% arrange(desc(Total)) %>% top_n(10)
top10_india <- top10 %>% add_row(tally %>% filter(Team == 'India'))
top10_india$Team <- factor(top10_india$Team) %>% 
                      fct_reorder(top10_india$Total)
if_india <- factor(c(0,0,0,0,0,0,0,0,0,0,1))
top10_india <- top10_india %>% add_column(if_india)

# Charting
top10_india %>% 
  ggplot(aes(x = Team, y =Total, fill = if_india)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = Team, y = Total, label = Total, vjust = -0.7)) +
  labs(x = 'Country', y = 'Total Medals', 
       title = 'India compared to other top 10 performers')
```


![india-compared-to-world](plot/india-world-comparison.png "india-compared-to-world")
