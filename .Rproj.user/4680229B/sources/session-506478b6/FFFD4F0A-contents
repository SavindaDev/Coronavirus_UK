#Author- Savinda Dilahra

install.packages("devtools")


library(devtools)
devtools::install_local("G:/Research/R project/sta3262-main")

library(sta3262)

get_individual_project_country("AS2021381")

install.packages("coronavirus")
library(coronavirus)
data("coronavirus")

View(coronavirus)

library(tidyverse)
uk_dataset <- filter(coronavirus,country=="United Kingdom")
view(uk_dataset)
uk_covid <- as_tibble(select(uk_dataset,date,type,cases))

uk_covid_type <- group_by(uk_covid,type)
view(uk_covid_type)
case_count_type <- summarise(uk_covid_type,case_type=sum(cases))

ggplot(uk_covid, aes(x=type,y=cases, fill=as.factor(type)), NA==FALSE) + 
  geom_boxplot( ) 

skim(uk_covid_type)  

library(skimr)
skim(uk_covid)

library(ggplot2)
ggplot(case_count_type, aes(x=type, fill=as.factor(type),
                     label = scales::percent(case_type) ), NA==FALSE) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

data <- data.frame(
  type = c("Active", "death", "recovery"),
  case_count = c(24658705, 220721, 24693)
)

# Calculate percentages
data$percentage <- (data$case_count / sum(data$case_count)) * 100

# Plot the bar chart
ggplot(data, aes(x = type, y = percentage, fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "COVID-19 Case Types as Percentages",
       x = "Case Type",
       y = "Percentage") +
  theme_minimal()
confirmed_corona <- uk_covid %>% filter(type=="confirmed")
summary(confirmed_corona)


boxplot(confirmed_corona$cases)
outliers <- boxplot(confirmed_corona$cases, plot=FALSE)$out
x <- confirmed_corona
confirmed_corona<- x[-which(confirmed_corona$cases %in% outliers),]


confirmed_corona <- uk_covid_dataset %>% filter(type=="confirmed")
summary(confirmed_algeria_corona)

confirmed_UK_corona <- confirmed_corona %>% mutate(cases = replace(cases, which(cases < 0 && boxplot(uk_covid_dataset$cases, plot=FALSE)$out), NA))
summary(confirmed_UK_corona)

uk_covid
uk_cases <- 

skim(gapminder)  
glimpse(coronavirus) 
as_tibble(coronavirus)
view(uk_covid)
str(uk_dataset)

uk_summary <- uk_covid %>%
  group_by(type, date) %>%
  summarize(total = sum(cases))

# Plotting the data
ggplot(uk_summary, aes(x = date, y = total, color = type)) +
  geom_line() +
  labs(title = "COVID-19 Cases in the United Kingdom",
       x = "Date", y = "Number of Cases") +
  theme_minimal()

## Effect of vaccination

united_kingdom_before <- united_kingdom %>% 
  filter(date < "2022-01-01")

united_kingdom_after <- united_kingdom %>% 
  filter(date >= "2022-01-01")

## divided into before and after 2022

united_kingdom_before2022 <- united_kingdom_before %>%
  filter(type %in% c("confirmed", "death")) %>%
  group_by(type) %>%
  summarise(total_cases = sum(cases))

united_kingdom_after2022 <- united_kingdom_after %>%
  filter(type %in% c("confirmed", "death")) %>%
  group_by(type) %>%
  summarise(total_cases = sum(cases))


united_kingdom_vaccination <- bind_rows(
  united_kingdom_before2022 %>% 
    mutate(period = "Before 2022"),
  united_kingdom_after2022 %>% 
    mutate(period = "After 2022")
  
)

