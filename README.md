# Covid19
death rate visualization of Covid19

library(COVID19)
library(Hmisc)
attach(covid19)
library(tidyverse) 
library(lubridate) #for date
library(knitr)
library(printr)
library(ggtext)
library(plotly) # for ggplotly 
library(ggplot2)
library(ggthemes) # for scale_color_tableau()

head(covid19)
covid19$id <- toupper(substring(covid19$id,1,3))
str(covid19)


#Capitalize the first letter of both words in a two word string

Caps <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

result<- sapply(name, Caps)

#write out result to working directory(desktop)
write.csv(result,file = "result.csv")

## replce NA
sum(is.na(covid19))

na<- function(x){
  sum(is.na(x))
}
#checking na in each columns
sort(apply(covid19, 2, na))


mean(covid19$deaths)

covid_deaths <- covid19(verbose = FALSE) %>%
  ungroup() %>% 
  mutate(Week = week(date)) %>% 
  select(Country = id, Date = date, Week, Deaths = deaths, Population = population) %>% 
  filter(Date <  today() & Date > today()-90) %>% 
          #add(days(-2))) %>% 
  mutate(Deaths_by_1Mpop = round(Deaths/Population*1e6)) 



######### ######### ######### ######### ######### ######### ######### #########
covid_deaths_total<- covid19 %>%
  mutate(month= month(date)) %>%
  mutate(week= week(date))  %>%
  filter(deaths== max(deaths)) %>%
  top_n(10,deaths) %>%
  select(month,week, deaths,population,id) %>%
  group_by(month,deaths,id,week) %>%
  summarise(sum_of_deaths=sum(deaths),avg=mean(deaths)) %>%
  arrange(desc(sum_of_deaths))

 

top_ten<- covid_deaths_total[1:10,] 


ggplotly(ggplot(top_ten,aes(id,sum_of_deaths,color=id)) + 
  geom_bar(stat="identity", position=position_dodge()))
 




#Function to get the top countries
######### ######### ######### ######### ######### ######### ######### ######### #########
get_top_countries_df <- function(covid_deaths, top_by, top_n, since){
  covid_deaths %>% 
    group_by(Date) %>% 
    top_n(100, Population) %>% 
    group_by(Country) %>% 
    filter(Date == max(Date)) %>% 
    ungroup() %>% 
    top_n(top_n, {{top_by}}) %>% 
    select(Country) %>% 
    inner_join(covid_deaths, ., by = "Country") %>% 
    filter(Date >= ymd(since))
}


top_countries<- covid_deaths %>%
  mutate(Month=month(Date)) %>%
  filter(Month>=2) %>%
  top_n(1000,Deaths) 

ggplotly(top_countries %>% 
  
           ggplot(aes(Date, Deaths, col =Country)) + 
           geom_line(size = 1, show.legend = F) +
           labs(title = "Total deaths due to COVID-19", 
                caption = "Source: covid19datahub.io") + 
           theme_minimal() + 
           #theme_custom() +
           scale_color_tableau(direction = -1) +
           
           NULL
) 
######### ######### ######### ######### ######### ######### ######### ######### #########

ggplotly(covid_deaths %>% 
    get_top_countries_df(top_by = Deaths, top_n = 10, since = 20200101) %>% 
    ggplot(aes(Date, Deaths, col =Country)) + 
    geom_line(size = 1, show.legend = F) +
    labs(title = "Total deaths due to COVID-19", 
         caption = "Source: covid19datahub.io") + 
    theme_minimal() + 
    #theme_custom() +
    scale_color_tableau(direction = -1) +
    #facet_wrap(~ Country) +
    NULL
) %>%
  
  layout(legend = list(orientation = "h", y = 0),
         annotations = list(
           x = 1, y = 1.05, text = "Source: covid19datahub.io",
           showarrow = F, xref = 'paper', yref = 'paper', font = list(size = 10)
         ) 
      )
 

