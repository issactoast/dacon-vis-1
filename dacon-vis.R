library(showtext)
library(ggthemes)
library(tidyverse)
library(tibbletime)
library(magrittr)

options(scipen = 999, warn=-1)
font_add_google('Noto Sans KR', 'notosanskr')
font_add_google('Nanum Pen Script', 'nanumpen')
showtext_auto()

people_data <- read.csv("data/new_people.csv")
process_data <- read.csv("data/process.csv")

head(people_data)


people_info <- people_data %>%
    select(DAESU:BIRTH) %>%
    filter(DAESU >= 15)
head(people_info)

options(repr.plot.width=5, repr.plot.height=3)

custom_theme <- function(base_size = 12, base_family = "notosanskr"){
    theme_fivethirtyeight() %+replace%
        theme(legend.position = "none",
              text = element_text(family = "notosanskr"),
              panel.grid.major.x = element_blank(),
              title = element_text(size = rel(2)),
              axis.text = element_text(size = rel(1.2)),
              axis.title = element_text(size = rel(0.7)), 
              axis.line = element_line(size = rel(0.1)))
}

people_info %>%
    mutate(DAESU = paste(DAESU, "대")) %>%
    group_by(DAESU) %>%
    summarize(n = n()) %>%
    ggplot(., aes(x = as.factor(DAESU), n)) +
    geom_bar(stat = "identity", col = "gray10", width = 0.75, fill = "orangered3") +
    geom_text(aes(y = n + 15, label = n), size = 5, family = "notosanskr") +
    ylim(NA, 400) + 
    custom_theme() +
    labs(title = "국회 대수별 국회의원 수",
         x = "", y = "인원 수 (명)")

suggest_data <- read.csv("data/suggest.csv")
suggest_data %>% dim()

# 15대 국회 1996년 5월 30일 개원
suggest_data <- suggest_data %>%
    mutate(Year = format(as.Date(PROPOSE_DT), "%Y") %>% as.integer(),
           Month = format(as.Date(PROPOSE_DT), "%m") %>% as.integer()/12,
           datenum = Year + Month) %>% 
    filter(datenum > 1996.5 & datenum <= 2020.5)

suggest_data$PROPOSE_DT %>% as.Date() %>% format("%y") %>% as.integer() %>% hist()
suggest_data$PROPOSE_DT %>% as.Date() %>% format("%m") %>% as.integer() %>% hist()

suggest_data %>% 
    mutate(Month = format(as.Date(PROPOSE_DT), "%m")) %>%
    group_by(Month) %>% 
    summarise(n = n()) %>% 
    ggplot(., aes(x = factor(Month, labels = paste0(1:12, "월")),
                  y = n)) +
    geom_bar(stat = "identity", col = "gray10", width = 0.75, fill = "orangered3") +
    geom_text(aes(y = n + 200, label = n), size = 5, family = "notosanskr") +
    custom_theme() +
    labs(title = "월별 국회 법안 발의건수 (15대~20대)",
        x = "", y = "발의건수")

suggest_data %>% 
    mutate(Year = format(as.Date(PROPOSE_DT), "%y")) %>%
    mutate(Year = factor(Year, levels = sprintf("%02d", c(96:99, 0:20)))) %>% 
    ggplot(., aes(x = Year)) +
        geom_bar(col = "gray10", width = 0.75, fill = "orangered3") +
        custom_theme() +
        labs(x = "년도", y = "발의건수")

suggest_data %>% 
    filter_time(time_formula = '2013' ~ '2020') %>% 
    select(BILL_NAME, PROPOSE_DT) %>% head()
    
suggest_data$PROPOSE_DT %>% as.Date() %>% format("%Y")
suggest_data$PROPOSE_DT %>% as.Date() %>% format("%m") %>% as.integer() %>% hist()

suggest_data$PROC_RESULT %>% as.factor() %>% table() %>% knitr::kable()

suggest_data %>%
    select(BILL_ID, BILL_NAME, COMMITTEE, PROPOSER, PROC_RESULT) %>% 
    head()

process_data[1,]
names(suggest_data)

# First plot code
# people_info %>%
#     mutate(DAESU = paste(DAESU, "대")) %>%
#     group_by(DAESU) %>%
#     summarize(n = n()) %>%
#     ggplot(., aes(x = as.factor(DAESU), n)) +
#     geom_bar(stat = "identity", col = "gray10", width = 0.75, fill = "orangered3") +
#     geom_text(aes(y = n + 15, label = n), size = 5, family = "notosanskr")+
#     ylim(NA, 400) + 
#     theme_fivethirtyeight() +
#     theme(legend.position = "none",
#           text = element_text(family = "notosanskr"),
#           panel.grid.major.x=element_blank(),
#           title = element_text(size = 25),
#           axis.title = element_text(size = 15), 
#           axis.text = element_text(size = 15), 
#           axis.line = element_line(size = 1)) +
#     labs(title = "국회 대수별 국회의원 수",
#          x = "", y = "인원 수 (명)")
