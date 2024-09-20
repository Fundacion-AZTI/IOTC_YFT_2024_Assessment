
# tagging input data comparison 
#
#......................................................



source(here('code', 'auxiliary_functions.R'))

# Read path and parameters for plots:
source('sharepoint_path.R')
setwd(shrpoint_path)

relFon <- read.csv("data/ss3_inputs/tag/Realease_tag_2021.csv") 
recFon <- read.csv("data/ss3_inputs/tag/Recovered_tag_2021.csv")[,-1]

relFair <- read.csv("data/ss3_inputs/tag/Realease_tag_GrowthFairly.csv") 
recFair <- read.csv("data/ss3_inputs/tag/Recovered_tag_GrowthFairly.csv")


head(relFon)
head(relFair)

relFon$Growth <- "Fonteneau"
relFair$Growth <- "Fairly"
df <- rbind(relFon,relFair)
ggplot()+geom_bar(data=df,aes(x=rel_age,y=number_prime,fill=Growth),stat="identity",position = 'dodge')+theme_fun()+
  xlab("Age")+ylab("Number realease")

dir_plot <- "output/figures/"
SavePlot('Releases_Tag_ALK_Fonteneau_Fairly',15,10)

recFon$Growth <- "Fonteneau"
recFair$Growth <- "Fairly"
df <- rbind(recFon,recFair)
df$yr <- qtr2yearqtr(df$rec_yr,1950,13)
ggplot()+geom_bar(data=df,aes(x=yr,y=number_prime,fill=Growth),stat="identity",position = 'dodge')+theme_fun()+
  xlab("Age")+ylab("Recovered year")

dir_plot <- "output/figures/"
SavePlot('Recovered_Tag_ALK_Fonteneau_Fairly',15,10)
