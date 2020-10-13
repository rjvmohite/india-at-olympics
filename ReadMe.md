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


![india-compared-to-world](plots/india-world-comparison.png "india-compared-to-world")

## India's performance over the years
This is how India has performed over the years at the Olympics. As we can see, it has been highly inconsistent throughout, with a peak of medals in the middle of the 20th century and then waning off to lower numbers.
A better insight can explain this phenomenon when we compare India's performance across various sports. It is seen that hockey has been predominant sport for India's performance in the Olympics and accounts for more almost 90% of the medals India has.
```r
# India's Performance over the years ----
df <- medalists %>% 
  filter(Team == 'India') %>% 
    group_by(Year,Medal) %>% 
      summarise(Total = n())
df
df %>% 
ggplot(aes(x = Year, y = Total, colour = Medal)) + theme_linedraw() + 
  geom_point() + 
  labs(y = "Medals", title = "India's performance over the years")

rm(df)


# India's sport-wise performance ----
medalists %>% 
  filter(Team == 'India') %>% 
    group_by(Sport,Medal) %>% 
      summarise(Total = n()) %>% 
ggplot(aes(x = Sport, y = Total, fill = Medal)) + theme_linedraw() + 
  geom_col(position = 'fill') + 
  # geom_text(aes(label = round(Total/sum(Total),2)), nudge_y = 0.0) +
  labs(y = "Total Medals", title = "India's performance by sport")

```
![india-over-the-years](plots/india-performance.png "india-performance") ![india-sportwise](plots/India-sportwise.png "india-performance")

### Sportwise analysis
Here we can see that Most of India's Gold medals are from Hockey only with 1 singular gold from shooting. Rest all sports have never earned a gold medal for India.

![india-sportwise](plots/India-sportwise-normalised.png "india-sportwise")

## Hockey at the Olympics
Seeing India's impressive performance in the sport of hockey one can expect India to be the best hockey team with most medals in the sport. However, a little analysis shows otherwise. India is fourth on the all time tally of medals in the sport of hockey.
```r
# Hockey medals by country ----
tally <- medalists %>% 
  filter(Sport == 'Hockey') %>% 
    group_by(Team) %>% 
      summarise(Total = n()) %>% 
        arrange(desc(Total)) %>% 
          top_n(10)

# Data Processing
tally$Team <- factor(tally$Team) %>% fct_reorder(tally$Total)
tally
if_india <- factor(c(0,0,0,1,0,0,0,0,0,0))
tally <- tally %>% add_column(if_india)

# Charting
tally %>% 
ggplot(aes(x = Team, y = Total, fill = if_india )) + theme_linedraw() + 
  geom_col(show.legend = FALSE) + 
  geom_text(aes(x = Team, y = Total, label = Total, vjust = -0.4)) +
  labs(y = "Total Medals", title = "Hockey medals by country")
```
![hockey-olympics](plots/hockey-by-country.png "hockey-Olympics")

## India: Before and After Independence
In general, countries should perform better after achieving independence (if they were under foreign rule). Let's see how India's performance has aged after it became independent from the United Kingdom.
```r
India_before <- medalists %>% 
                  filter(Team == 'India', Year < 1947) %>% 
                    summarise(Total = n())
India_after <- medalists %>% 
                filter(Team == 'India', Year > 1947) %>% 
                  summarise(Total = n())
df <- data.frame(Time = c('Before Independence','After Independence'),
                 Medals = c(India_before[1,1], India_after[1,1]))
#Charting
ggplot(df, aes(x = Time, y = Medals, fill = Time)) + 
  geom_col(show.legend = F) +
  coord_cartesian(ylim = c(0,175)) +
  geom_text(aes(label = Medals),vjust = -0.4)
  labs(x = '', title = 'India: Before and After Independence')

rm(df, India_after, India_before)
```
![india-independence](plots/india-independence.png "india-independence")
