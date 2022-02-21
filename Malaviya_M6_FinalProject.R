install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("magrittr")
library(magrittr)
install.packages('tidyr')
library(tidyr)
install.packages('plyr')
library(plyr)
install.packages('dplyr')
library(dplyr)

cyber_data = read_csv("/Users/pratik_4511/Desktop/Northeastern University /Quarter_1A/M6/Cyber Security Breaches.csv")
print(summary(cyber_data))
complete.cases(cyber_data)
View(cyber_data)

#filtered with CA data 
california = filter(cyber_data, cyber_data$State == 'CA')
print(california)
View(california)
breach_count = count(california)
print(breach_count)
summary(california)
data.frame(california)
print(mean(california$Number))


abs = (cyber_data$Business_Associate_Involved)
count(abs)



#filtered with NY data -------------------------------------------
newyork = filter(cyber_data, cyber_data$State == 'NY')
print(newyork)
View(newyork)
breach_count = count(newyork)
print(breach_count)
summary(newyork)
data.frame(newyork)

 llb = ggplot(newyork,
       aes(year,
           Individuals_Affected))+
  geom_point(stat = 'identity', colour = 'red',size = 3,pchs=2)+
  theme(plot.background = element_rect(colour = 'black',size = 2))+
  labs(title = 'Affected people of NewYork')+
   
 llb
 llb+
   ylim(c(0,60000))
 #-------------------------------------------------------------------
 
 # Filtering the specific breach ---------------------------------
 
 theft = filter(cyber_data, cyber_data$Type_of_Breach == 'Theft')
 print(theft)
 View(theft)
 print(count(theft))
 
 
 #-----------------------------------------------------------------
 #filtered with TX data -------------------------------------------
 Texas = filter(cyber_data, cyber_data$State == 'TX')
 print(Texas)
 View(Texas)
 breach_count = count(Texas)
 print(breach_count)
 summary(Texas)
 data.frame(Texas)
 
 ppl = ggplot(Texas,
        aes(year,
            Individuals_Affected))+
   geom_point(stat = 'identity', colour = 'red',size = 3,pchs=2)+
   theme(plot.background = element_rect(colour = 'black',size = 2))+
   labs(title = 'Affected people of Texas')
 ppl
 ppl+
   ylim(c(0,60000))
 #-------------------------------------------------------------------

#plot-2 Type of breach in CA 
pfg = ggplot(data = california,
       aes(x = Type_of_Breach,
           y = Individuals_Affected))+
  geom_bar(fill = 'cadetblue',
           stat = 'identity',
           position = 'stack',)+
  xlab(" Type of Breach (CA)")+
  coord_flip()+
  theme_bw()+
  ylab("Individual Affected")+
  labs(title = 'Types of Cyber Breaches in California')
pfg
pfg +
  ylim(0,100000)

#3 point graph Business Involved in CALIFORNIA ------------
ggplot(data = california,
       aes(x = Location_of_Breached_Information,
           y = year))+
  geom_line(colour = 'darkgreen',
             size = 3, 
             stat = 'identity',
             alpha = 0.5)+
  geom_smooth()+
  coord_flip()+
  theme_bw()+
  labs(title = 'Data Breached from Specific Location in California',
       x = 'Data Breached Loaction',
       y = 'Year')


#--------------------------------------------------


# number of mediators involved --------------------- 
names(california)
ppl = california %>%
  drop_na(Business_Associate_Involved) %>%
  ggplot(aes(x = Business_Associate_Involved))+
  geom_bar(fill = "#97B3C6")+
  coord_flip()+
  theme_bw()+
  labs(x = 'Involvement of Mediators',
       y = NULL,
       title = 'Bussinesses who passed the Data')
ppl
ppl+
  ylim(0,3)
#---------------------------------------------------




 # PLOT - 01 Cyber Breaches IN USA -----------------
ggplot(data = cyber_data,
       aes(State,year))+
  geom_bar(fill = 'orange',
           stat = 'identity',
           position = 'stack',)+
  xlab("States")+
  ylab("Cyber Breaches")+
  labs(title = 'Cyber Breaches in USA')
#----------------------------------------------------


# Pi Donut ----------------------------------
install.packages("webr")
library(webr)
partition = cyber_data %>%
  group_by(State,Individuals_Affected,X) %>%
  summarise(n = sum(X))
print(partition)
View(partition)
summary(partition)

PieDonut(partition,
         aes(State,Individuals_Affected),
         ratioByGroup = FALSE,
         title = 'Donut')
#----------------------------------------------

# barplot of INDIVIDUAL AFFECTED IN USA--------
 ggp =  ggplot(data = partition,
         aes(State,
             Individuals_Affected))+
  geom_bar(fill = 'skyblue',
           stat = 'identity',
           position = 'stack',)+
    theme_bw()+
  labs(title = 'People Affected by Breahes in USA')+
  xlab('States')+
  ylab('Individual Affected')
 ggp
 
 ggp + 
   ylim(0,20000)  # expanding the y limit 
#------------------------------------------------------
 
 #-------------------------------------------------
function_one =  california %>% 
   group_by(Individuals_Affected,State) %>% # Variable to be transformed
   count() %>% 
   ungroup() %>% 
   mutate(Individuals_Affected = `n` / sum(`n`)) %>% 
   arrange(Individuals_Affected) %>%
   mutate(State = scales::percent(Individuals_Affected))
 
 ggplot(function_one, aes(x = "", y = Individuals_Affected, fill = State)) +
   geom_col() +
   coord_polar(theta = "y")
 
#-------------------------------------------------
 
 # parliament chart -----------------------------
 install.packages("ggparliament")
 library(ggparliament)
 
 rublo  =  cyber_data %>%
   filter(State == 'CA' & year == 2014,2009)
 View(rublo)
 
 ru_semi = parliament_data(rublo,
                           type = 'semicircle',
                           parl_rows = 10,
                           party_seats = rublo$Number)
 ggplot(ru_semi,
        aes(x = x,
            y = y,
            colour = Name_of_Covered_Entity))+
   geom_parliament_seats() + 
   theme_ggparliament() +
   labs(title = "California State, 2014") +
   scale_colour_manual(values = ru_semi$colour, 
                       limits = ru_semi$Name_of_Covered_Entity) 
 
 #---------------------------------------------------------
 
 
 






