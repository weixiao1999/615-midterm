myName <- "Weixiao Li"
library(tidyverse)
library(magrittr)
library(readxl)
strawberries_2022oct30_a_1_ <- read_excel("C:/Users/16932/Desktop/strawberries-2022oct30-a (1).xlsx")
strawb <- strawberries_2022oct30_a_1_

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

unique(strawb[1])

unique(strawb[2])

unique(strawb[3])

T <- NULL

for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

drop_cols <- cnames[which(T == 1)]

strawb %<>% select(!all_of(drop_cols))

strawb %<>% arrange(Year, State)

colnames(strawb)

temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()
strawb2 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "items", "units"),
                               sep = ",",
                               fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")
#2
n <- 6
alpha <- 0.05
df <- 5
mean_2 <-mean(c(231304956,1446458,211553703, 1221571,19751253,224886))
sd_2 <- sd(c(231304956,1446458,211553703, 1221571,19751253,224886))
se_2<- sd_2/sqrt(n)
t_score <- qt(p=alpha/2,df,lower.tail = F)
low <- mean_2-se_2*t_score
upper <- mean_2+se_2*t_score
#(-39779720,194947329)

#3
n <- 169
alpha <- 0.05
df <- 168
strawb_3 <- read_excel("C:/Users/16932/Desktop/strawberry.xlsx")
mean_3 <- mean(strawb_3$x)
sd_3 <- sd(strawb_3$x)
se_3<- sd_3/sqrt(n)
t_score <- qt(p=alpha/2,df,lower.tail = F)
low <- mean_3-se_3*t_score
upper <- mean_3+se_3*t_score
#(-10378.79,199946.1)


#4
d4_1 <- filter(strawb, Domain != 'ORGANIC STATUS' & Domain != 'TOTAL')
d4_2 <- grep('TOTAL',d4_1$`Domain Category`,ignore.case = T)
length(d4_2)
unique(d4_1[11])
#There are 175-36=139 chemicals listed.
#5
d5_1 <- filter(strawb, State == 'FLORIDA' & Domain != 'ORGANIC STATUS' & Domain != 'TOTAL')
d5_2 <- filter(strawb, State == 'CALIFORNIA' & Domain != 'ORGANIC STATUS' & Domain != 'TOTAL')
unique(d5_1[11])
unique(d5_2[11])
# There are 23 chemicals have been used in California than in Florida
