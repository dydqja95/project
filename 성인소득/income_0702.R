library(readxl)
library(tidyverse)
library(DT)
library(writexl)
library(RColorBrewer)
library(epiDisplay)
library(janitor)
library(gridExtra)

data <- readxl::read_excel(path = "d:/FastCampus/EDA프로젝트/데이터/adult.xlsx",)
data


colnames(data)


data <- data.frame(data)



rm(list = ls())

data1 <- data %>% 
  dplyr::mutate(age.group = cut(age,
                                breaks = seq(from = 10, to = 100, by = 10),
                                right  = FALSE, 
                                labels = c("10대", "20대", "30대", "40대", "50대", "60대", "70대","80대","90대"))) %>% 
  dplyr::mutate(education.group = ifelse(education == "Bachelors", "학사", ifelse( education ==  "Some-college", "Some-college", ifelse( education == "11th" | education == "HS-grad" | education == "9th" | education == "7th-8th" | education == "12th" | education == "1st-4th" | education == "10th" | education == "5th-6th" | education == "Preschool", "HS-grad", ifelse(education == "Prof-school", "Prof-school", ifelse(education == "Assoc-acdm","Assoc-acdm", ifelse(education == "Assoc-voc", "Assoc-voc", "Doctorate")))))))%>% 
  dplyr::mutate(hour.group = cut(hours_per_week,
                                 breaks = seq(from = 0, to =100, by = 10),
                                 right  = FALSE,
                                 labels = c("0-10","10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80","80-90", "90-100")))


View(data1)



# 나이 막대
data1 %>% 
  ggplot2::ggplot(mapping = aes(x = age.group))+
  ggplot2::geom_bar(fill = "skyblue")+
  ggplot2::theme_classic()+
  ggplot2::labs(title = "Number of Age",
                x     = "Age",
                y     = "Count")


# 교육 수 막대
data1 %>% 
  dplyr::group_by(education.group) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(education.group, -n), y = n))+
  ggplot2::geom_bar(fill = "skyblue",stat = "identity") +
  ggplot2::theme_classic()+
  ggplot2::labs(title = "Number of Education",
                x     = "Education",
                y     = "Number")


# 교육수준 수 파이
data1 %>% 
  dplyr::group_by(education.group) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n)) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = education.group)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0, direction = 1)


# workclass
data1 %>% 
  dplyr::group_by(workclass) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(workclass, -n), y = n))+
  ggplot2::geom_bar(fill = "skyblue",stat = "identity") +
  ggplot2::theme_classic()+
  ggplot2::labs(title = "Number of Age",
                x     = "Education",
                y     = "Number")

# workclass pie
data1 %>% 
  dplyr::group_by(workclass) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = workclass)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0, direction = 1)



# occupation
data1 %>% 
  dplyr::group_by(occupation) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(occupation, -n), y = n))+
  ggplot2::geom_bar(fill = "skyblue",stat = "identity") +
  ggplot2::theme_classic()+
  ggplot2::labs(title = "Number of Age",
                x     = "Education",
                y     = "Number")

# occupation
data1 %>% 
  dplyr::group_by(occupation) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = occupation)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0, direction = 1)






# hour_per_week 
hour.group1 <- data1 %>% 
  dplyr::group_by(hour.group) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(hour.group,-n), y = n)) +
  ggplot2::geom_bar(stat = "Identity", fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(title    = "주단위 일하는 시간과 빈도수 그래프",
                x        = "Work_per_week",
                y        = "Frequency") +
  ggplot2::theme(plot.title = element_text(size  = 15,
                                           face  = "bold",
                                           hjust = 0.5),
                 axis.title.x = element_text(size = 12,
                                             face = "bold"),
                 axis.title.y = element_text(size  = 12,
                                             face  = "bold",
                                             angle = 0,
                                             vjust = 0.5))








# gender
gender.1 <- data1 %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(gender,-n), y = n)) +
  ggplot2::geom_bar(stat = "Identity", fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(title    = "성병",
                x        = "성별",
                y        = "Frequency") +
  ggplot2::theme(plot.title = element_text(size  = 15,
                                           face  = "bold",
                                           hjust = 0.5),
                 axis.title.x = element_text(size = 12,
                                             face = "bold"),
                 axis.title.y = element_text(size  = 12,
                                             face  = "bold",
                                             angle = 0,
                                             vjust = 0.5))

# gender pie
gender.2 <-  data1 %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = gender)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "주단위 일하는 시간과 빈도수 그래프") +
  ggplot2::theme(plot.title = element_text(size = 10, 
                                           hjust = .5))

grid.arrange(gender.1, gender.2, nrow=1, ncol= 2)




# income
income.1 <- data1 %>% 
  dplyr::group_by(income) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(income,-n), y = n)) +
  ggplot2::geom_bar(stat = "Identity", fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(title    = "사람별 수입",
                x        = "수입",
                y        = "Frequency") +
  ggplot2::theme(plot.title = element_text(size  = 15,
                                           face  = "bold",
                                           hjust = 0.5),
                 axis.title.x = element_text(size = 12,
                                             face = "bold"),
                 axis.title.y = element_text(size  = 12,
                                             face  = "bold",
                                             angle = 0,
                                             vjust = 0.5))

# income pie
income.2 <- data1 %>% 
  dplyr::group_by(income) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = income)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "주단위 일하는 시간과 빈도수 그래프") +
  ggplot2::theme(plot.title = element_text(size = 10, 
                                           hjust = .5))


grid.arrange(income.1, income.2, nrow=1, ncol= 2)




# race
race.one <- data1 %>% 
  dplyr::group_by(race) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(race,-n), y = n)) +
  ggplot2::geom_bar(stat = "Identity", fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(title    = "Race",
                x        = "Race",
                y        = "Frequency") +
  ggplot2::theme(plot.title = element_text(size  = 15,
                                           face  = "bold",
                                           hjust = 0.5),
                 axis.title.x = element_text(size = 12,
                                             face = "bold"),
                 axis.title.y = element_text(size  = 12,
                                             face  = "bold",
                                             angle = 0,
                                             vjust = 0.5))


# race pie
race.two <- data1 %>% 
  dplyr::group_by(race) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot2::ggplot(mapping = aes(x = "", y = reorder(n,n), fill = race)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_polar(theta = "y", start = 0) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "주단위 일하는 시간과 빈도수 그래프") +
  ggplot2::theme(plot.title = element_text(size = 10, 
                                           hjust = .5))


grid.arrange(race.one, race.two, nrow=1, ncol= 2)



# age.group vs income

label <-  c("<=50K",">50K")
par(mfrow = c(3,3))
for(i in 1:9){
  pie(c(table(data1$age.group, data1$income)[i,1] / (table(data1$age.group, data1$income)[i,1] + table(data1$age.group, data1$income)[i,2]), table(data1$age.group, data1$income)[i,2] / (table(data1$age.group, data1$income)[i,1] + table(data1$age.group, data1$income)[i,2])),
      radius    = 1,
      clockwise = TRUE,
      init.angle = 90,
      col = c("skyblue","gray"),
      main = paste("Pie of" , rownames(table(data1$age.group, data1$income))[i]),
      labels = label)
}