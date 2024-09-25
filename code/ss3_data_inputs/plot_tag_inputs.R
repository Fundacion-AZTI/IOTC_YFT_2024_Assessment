
# tagging input data comparison 
#
#
#......................................................


library(ggplot2)
library(here)

# Set working directiry using here():
proj_dir = here()

source(here('code', 'auxiliary_functions.R'))

# Read path and parameters for plots:
source('sharepoint_path.R')
setwd(shrpoint_path)

relFon <- read.csv("data/ss3_inputs/tag/Realease_tag_2021.csv") 
recFon <- read.csv("data/ss3_inputs/tag/Recovered_tag_2021.csv")[,-1]

relFair <- read.csv("data/ss3_inputs/tag/Realease_tag_GrowthFarley.csv") 
recFair <- read.csv("data/ss3_inputs/tag/Recovered_tag_GrowthFarley.csv")


head(relFon)
head(relFair)

relFon$Growth <- "Fonteneau"
relFair$Growth <- "Fairley"
df <- rbind(relFon,relFair)
df2 <- aggregate(number_prime~rel_age+Growth,data=df,sum)
ggplot()+geom_bar(data=df2,aes(x=rel_age,y=number_prime,fill=Growth),stat="identity",position = 'dodge')+theme_fun()+
  xlab("Age")+ylab("Number realease")

dir_plot <- "output/figures/"
SavePlot('Releases_Tag_ALK_Fonteneau_Farley',15,10)

recFon$Growth <- "Fonteneau"
recFair$Growth <- "Fairley"
df <- rbind(recFon,recFair)
df$yr <- qtr2yearqtr(df$rec_yr,1950,13)
df$Growth <- as.factor(df$Growth)
df$rec_yr <- as.character(df$rec_yr)
df2 <- aggregate(number_prime~yr+Growth,data=df,sum)
ggplot()+geom_bar(data=df2,aes(x=yr,y=number_prime,fill=Growth),stat="identity",position = 'dodge')+theme_fun()+
  xlab("Age")+ylab("Recovered year")

dir_plot <- "output/figures/"
SavePlot('Recovered_Tag_ALK_Fonteneau_Farley',15,10)
