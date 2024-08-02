install.packages("devtools")
install.packages("Rcpp")
library(devtools)
devtools::install_github("thiyangt/sta3262")
library(sta3262)

get_individual_project_country("AS2021490")
library(coronavirus)
data("coronavirus")
library(tidyverse)

#------------------------------------------------------------------------------
phlippines <- coronavirus %>%
  filter(country == "Philippines")
library(skimr)
skim(phlippines)


#-------------------------------------------------------------------------------
# filtered dataset

phl <- coronavirus %>%
      filter(country == "Philippines") %>%
       select(date, type, cases)
#-------------------------------------------------------------------------------
#structure of the data set 
library(skimr)
skim(phl)
#-------------------------------------------------------------------------------

# time series plot 
confirmed <- phl %>%
             filter(type == "confirmed")
deaths <- phl %>%
          filter(type == "death")
recoveries <- phl %>%
              filter(type == "recovery")
confirmed %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(colour= "blue") + 
  labs(x= "Dates",
     y= "Confirmed cases",
     title = "Confirmed cases in Philippines")

deaths %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(colour="green") +  labs(x= "Dates",
                      y= "Death cases",
                      title = "Death cases in Philippines")
recoveries %>%
  ggplot(aes(x = date, y = cases)) +
  geom_path(colour="red") + labs(x= "Dates",
                                 y= "Recovery cases",
                                 title = "Recovery cases in Philippines")

#-------------------------------------------------------------------------------
# active cases - have to rewrite
actives <- phl %>%
          group_by( date,type) %>%
          summarise(total = sum(cases))
a <- actives %>%
  pivot_wider(names_from = type, values_from = total) %>%
  arrange(date) %>%
  mutate(totalactive = cumsum(active),
         totalrecovery = cumsum(recovery),
         totaldeath = cumsum(death)) %>%
  mutate(active = confirmed - death - recovery)
#-------------------------------------------------------------------------------
# comparison
neigbour <- coronavirus %>%
            filter(country == c("Vietnam", "Malaysia", "Philippines", "Japan")) %>%
            select(date, country, type, cases)
neigh_confirm <- neigbour %>%
                  filter(type == "confirmed")

neigh_confirm %>%
  ggplot(aes(x = date, y = cases, colour = country)) +
  facet_grid(rows = vars(country)) +
  geom_line()  + labs(x= "Dates",
                     y= "confirmed cases",
                     title = "comparison of confirmed cases with neighbouring countries")


neigh_deaths <- neigbour %>%
               filter(type == "death")

neigh_deaths %>%
  ggplot(aes(x = date, y = cases, colour = country)) + 
  facet_grid(rows = vars(country)) +
  geom_line()  + labs(x= "Dates",
                      y= "death cases",
                      title = "comparison of death cases with neighbouring countries")



neigh_recoveries  <- neigbour %>%
  filter(type == "recovery")

neigh_recoveries %>%
  ggplot(aes(x = date, y = cases, colour = country)) +
  facet_grid(rows = vars(country)) +
  geom_line()  + labs(x= "Dates",
                      y= "recovery cases",
                      title = "comparison of recovery cases with neighbouring countries")



#-------------------------------------------------------------------------------
phl %>%
  ggplot(aes(x = date, y = cases, colour = cases)) +
  facet_grid(rows = vars(type)) +
  geom_line()
#-------------------------------------------------------------------------------
#population wise comparison
# population of phl = 109581085

popdata <- coronavirus %>%
          filter(population > 100000000 & population < 120000000 & country != "China") %>%
          select(country, date, type, cases)
popdata2 <- popdata %>%
            group_by(country, type)

popconfirmed <- popdata2 %>%
  filter(type == "confirmed")
popdeath <- popdata2 %>%
  filter(type == "death")
poprecovery <- popdata2 %>%
  filter(type == "recovery")

popconfirmed %>%
  ggplot(aes(x = date, y = cases, colour = country)) + 
  geom_line() + labs(x= "Dates",
                   y= "confirmed cases",
                   title = "confirmed cases in countries with same population range ")


popdeath %>%
  ggplot(aes(x = date, y = cases, colour = country)) + 
  geom_line() + labs(x= "Dates",
                     y= "death cases",
                     title = "death cases in countries with same population range")

poprecovery %>%
  ggplot(aes(x = date, y = cases, colour = country)) + 
  geom_line() + labs(x= "Dates",
                     y= "recovery cases",
                     title = "recovery cases in countries with same population range")


#-----------------------------------------------------------------------------

