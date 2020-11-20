# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("scales")
library(tidyverse)
library(readxl)
library(reshape2)
library(scales)

# CHANGE THIS FIRST LINE TO BE NEW FILE LOCATION (flip the slahes to match below)
#
# data <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/Traffic, Guestcard data.xlsx") # change this <-
# data2 <- data %>% mutate(date = as.Date(as.POSIXct(...1,tz="Los Angeles"))) %>% 
#   select(date, 'Website Traffic', Guestcards)
# data2melt <- melt(data2, id = "date")
# chart <- ggplot(data2melt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d-%Y")) +
#   scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
#   ylab("% Change") + xlab("Date")
# pdf("C:/Users/jonah/Desktop/R Scripting/Tiff/chart_5_14_2020.pdf") # change output location <- 
# print(chart)
# dev.off()

########################################################
# 
# d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/traffic.xlsx") # change this <-
# d2 <- d %>% filter(name == "111 Kent")
# d3 <- d2 %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% 
#   select(date, '2019', '2020')
# d3melt <- melt(d3, id = "date")
# chart <- ggplot(d3melt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
#   scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
#   ylab("Website Traffic (#)") + xlab("Date")
#  
#   
# pdf("C:/Users/jonah/Desktop/R Scripting/Tiff/traffic_111kent.pdf") # change output location <- 
# print(chart + ggtitle("111 Kent"))
# dev.off()

############### fxn traffic #############################
d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/traffic.xlsx") # change this <-
d[is.na(d)] <- 0
d <- d %>% mutate(name = ifelse(as.character(name) == "Northshore Austin - MF", "Northshore Austin", as.character(name)))

traffic <- function(d, name, i, date, value, variable, scales, pretty_breaks) {
  d2 <- d %>% filter(name == name[i])
  d3 <- d2 %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% 
    select(date, '2019', '2020')
  d3melt <- melt(d3, id = "date")
  chart <- ggplot(d3melt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
    scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
    ylab("Website Traffic (#)") + xlab("Date")
  
  
  pdf(paste0("C:/Users/jonah/Desktop/R Scripting/Tiff/", "traffic_", d$name[i], ".pdf"), width = 11, height = 8.5) # change output location <- 
  print(chart + ggtitle(paste0(d$name[i])))
  dev.off()
  
}

traffic(d = d, name = d$name, i = 1)
traffic(d = d, name = d$name, i = 2)
traffic(d = d, name = d$name, i = 3)
traffic(d = d, name = d$name, i = 4)
traffic(d = d, name = d$name, i = 5)
traffic(d = d, name = d$name, i = 6)
traffic(d = d, name = d$name, i = 7)
traffic(d = d, name = d$name, i = 8)
traffic(d = d, name = d$name, i = 9)
traffic(d = d, name = d$name, i = 10)
traffic(d = d, name = d$name, i = 11)

############### fxn guest cards #############################
d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/guestcards.xlsx") # change this <-
d[is.na(d)] <- 0
d <- d %>% mutate(name = ifelse(as.character(name) == "Northshore Austin - MF", "Northshore Austin", as.character(name)))

guestcards <- function(d, name, i, date, value, variable, scales, pretty_breaks) {
  d2 <- d %>% filter(name == name[i])
  d3 <- d2 %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% 
    select(date, '2019', '2020')
  d3melt <- melt(d3, id = "date")
  chart <- ggplot(d3melt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
    scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
    ylab("Guest Cards (#)") + xlab("Date")
  
  
  pdf(paste0("C:/Users/jonah/Desktop/R Scripting/Tiff/", "guestcards_", d$name[i], ".pdf"), width = 11, height = 8.5) # change output location <- 
  print(chart + ggtitle(paste0(d$name[i])))
  dev.off()
  
}

guestcards(d = d, name = d$name, i = 1)
guestcards(d = d, name = d$name, i = 2)
guestcards(d = d, name = d$name, i = 3)
guestcards(d = d, name = d$name, i = 4)
guestcards(d = d, name = d$name, i = 5)
guestcards(d = d, name = d$name, i = 6)
guestcards(d = d, name = d$name, i = 7)
guestcards(d = d, name = d$name, i = 8)
guestcards(d = d, name = d$name, i = 9)
guestcards(d = d, name = d$name, i = 10)
guestcards(d = d, name = d$name, i = 11)

############### fxn net leases #############################

d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/netleases.xlsx") # change this <-
d[is.na(d)] <- 0
d <- d %>% mutate(name = ifelse(as.character(name) == "Northshore Austin - MF", "Northshore Austin", as.character(name)))

netleases <- function(d, name, i, date, value, variable, scales, pretty_breaks) {
  d2 <- d %>% filter(name == name[i])
  d3 <- d2 %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% 
    select(date, '2019', '2020')
  d3melt <- melt(d3, id = "date")
  chart <- ggplot(d3melt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
    scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
    ylab("Net Leases (#)") + xlab("Date")
  
  
  pdf(paste0("C:/Users/jonah/Desktop/R Scripting/Tiff/", "netleases_", d$name[i], ".pdf"), width = 11, height = 8.5) # change output location <- 
  print(chart + ggtitle(paste0(d$name[i])))
  dev.off()
  
}

netleases(d = d, name = d$name, i = 1)
netleases(d = d, name = d$name, i = 2)
netleases(d = d, name = d$name, i = 3)
netleases(d = d, name = d$name, i = 4)
netleases(d = d, name = d$name, i = 5)
netleases(d = d, name = d$name, i = 6)
netleases(d = d, name = d$name, i = 7)
netleases(d = d, name = d$name, i = 8)
netleases(d = d, name = d$name, i = 9)
netleases(d = d, name = d$name, i = 10)
netleases(d = d, name = d$name, i = 11)

##### total ####
### traffic ###
d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/traffic.xlsx") # change this <-
d[is.na(d)] <- 0

total <- d %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% group_by(date) %>% mutate(sum2019 = sum(`2019`), sum2020 = sum(`2020`)) %>% filter(name == name[1]) %>% select(date, sum2019, sum2020)

totalmelt <- melt(total, id = "date")
chart <- ggplot(totalmelt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
  scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
  ylab("Website Traffic (#)") + xlab("Date")


pdf("C:/Users/jonah/Desktop/R Scripting/Tiff/traffic_allproperties.pdf", width = 11, height = 8.5) # change output location <- 
print(chart + ggtitle("All Properties"))
dev.off()

### guest cards ###
d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/guestcards.xlsx") # change this <-
d[is.na(d)] <- 0

total <- d %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% group_by(date) %>% mutate(sum2019 = sum(`2019`), sum2020 = sum(`2020`)) %>% filter(name == name[1]) %>% select(date, sum2019, sum2020)

totalmelt <- melt(total, id = "date")
chart <- ggplot(totalmelt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
  scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
  ylab("Guest Cards (#)") + xlab("Date")


pdf("C:/Users/jonah/Desktop/R Scripting/Tiff/guestcards_allproperties.pdf", width = 11, height = 8.5) # change output location <- 
print(chart + ggtitle("All Properties"))
dev.off()

### net leases ###
d <- read_excel("C:/Users/jonah/Desktop/R Scripting/Tiff/netleases.xlsx") # change this <-
d[is.na(d)] <- 0

total <- d %>% mutate(date = as.Date(as.POSIXct(date,tz="Los Angeles"))) %>% group_by(date) %>% mutate(sum2019 = sum(`2019`), sum2020 = sum(`2020`)) %>% filter(name == name[1]) %>% select(date, sum2019, sum2020)

totalmelt <- melt(total, id = "date")
chart <- ggplot(totalmelt, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(breaks = scales::pretty_breaks(n=17), labels = date_format("%m-%d")) +
  scale_y_continuous(breaks = scales:: pretty_breaks(n=10)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.title = element_blank()) +
  ylab("Net Leases (#)") + xlab("Date")


pdf("C:/Users/jonah/Desktop/R Scripting/Tiff/netleases_allproperties.pdf", width = 11, height = 8.5) # change output location <- 
print(chart + ggtitle("All Properties"))
dev.off()



