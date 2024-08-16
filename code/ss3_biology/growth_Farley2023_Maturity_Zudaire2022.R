#
#       ESTIMATE INPUT PARAMETERS FOR SS3 ASSUMING 
#   FARLEY'S GROWTH
#       COMPARE OUTPUT SS3 AND FARLEY'S GROWTH
#............................................................

library(dplyr)
library(r4ss)
library(here)

source("code/auxiliary_functions.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

#CALCULATE AND PLOT LENGTH AT AGE

#initial values
age <- seq(0.25,15,0.25)
FL <- c(10:167)
param <- c(167.47, 3.11, 0.39, 0.82, 0.34, -0.01)

#calculate length at age and age at length
FLatAge <-  growth2stage.f(age, param)
AgeatFL <- calc.age.2stage(FL, param=param, min.age=0, max.age=15)

df <- NULL
df$FL <- FL
df$AgeatFL <- AgeatFL
df <- as.data.frame(df)

#Plot Farleys growth
p<- ggplot(df)+geom_line(aes(AgeatFL, FL))+
xlab("Age")+ylab("Length (cm)")+theme_fun()


#Estimate k parameter of VB for each quarter for Farleys growth
k = vector('numeric',length=15)
k[1:length(age)] = 0.12  # the best fit to Farleys data
temp <- NULL
temp$mean <- growth2stage.f(age, yft.param)
temp$age <- age
temp <- as.data.frame(temp)
Linf = param[1] 
for (i in 2:length(age)) k[i] = log((temp[temp$age==age[i],'mean']-Linf)/(temp[temp$age==age[i+1],'mean']-Linf))
	
VB=NA
VB[1] =temp$mean[1]
k[1]<- k[1]
for (i in 2:length(age)) VB[i] = Linf+(VB[i-1]-Linf)*(exp(-k[i-1]))


df2 <- NULL
df2$age<-age
df2$VB<- VB
df2 <- as.data.frame(df2)
p+ geom_point(df2, inherit.aes = FALSE,mapping=aes(x=age,y=VB),colors="red")
points(age[1:28],VB,col=5,pch=19)
relK <- c(k[1]*4,k[-1]/k[-length(k)])[1:15]
names(relK) <- c("k1",paste0("relk",2:15))
write.csv(relK[1:15],file=file.path("data","ss3_inputs","FarleyGrowth_Inputss3.csv"),row.names=FALSE) 
  
#The estimated relative K values
# 0.4673156 1.3759791 1.2635756 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000



#Linear until Amin=1 

#_growth_parms														
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn	
# 0.1	     0.6	    0.462	    0.462	  0	   0	 -2	0	0	  0	  0	0.5	0	0	#_NatM_p_1_Fem_GP_1                 
# 1	      45	       29.94634	       22	 10	   6	 -2	0	0	  0	  0	0.5	0	0	#_L_at_Amin_Fem_GP_1                
# 120	     170	      167.47	      145	 10	   6	 -4	0	0	  0	  0	0.5	0	0	#_L_at_Amax_Fem_GP_1                
# 0.05	     0.5	    0.12	    0.455	0.8	   6	 -4	0	0	  0	  0	0.5	0	0	#_VonBert_K_Fem_GP_1  
# -5	       5	      0.467	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_2_Fem_GP_1                  
# -15	       5	     1.263	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_3_Fem_GP_1                  
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_4_Fem_GP_1                  
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_5_Fem_GP_1                  
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_6_Fem_GP_1                  
# -15	       5	      1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_7_Fem_GP_1                  
# -15	       5	      1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_8_Fem_GP_1                  
# -15	       5	      1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_9_Fem_GP_1                  
# -15	       5	      1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_10_Fem_GP_1                 
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_11_Fem_GP_1                 
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_12_Fem_GP_1                 
# -15	       5	        1	        1	  1	   0	 -1	0	0	  0	  0	  0	0	0	#_Age_K_13_Fem_GP_1                 


# 
sub_scs <- c("06_update_Growth")
scs_wd <-paste0("models/update/",sub_scs)

sc_ss3 <- SS_output(dir=scs_wd,  repfile = "Report.sso",covar=F)

#Plot Farleys growth
cols <- c("varA" = "black", "varB" = "red")
df2 <-sc_ss3$endgrowth

 ggplot()+geom_line(df,mapping=aes(x=AgeatFL, y=FL,color="varA"),linewidth=1.5)+
  xlab("Age")+ylab("Length (cm)")+
  geom_point(df2,mapping=aes(x=Age_Beg*0.25,y=Len_Beg,color="varB"),size=2.5,show.legend=T)+
     scale_color_manual(name=NULL, values=cols,
                        labels=c("Farley et al. 2023",c("ss3 Growth")) )+
   theme_fun()


SavePlot('GrowthFarley',15,10)

#
#       MATURITY
#............................................................

# zudaire et al. 2022

FL <- c(50:160)

alfa <- -9.25
beta <- 0.091
L50<- 101.7
Pmat=exp(alfa+beta*FL)/(1+exp(alfa+beta*FL))
plot(FL,Pmat)
df <- NULL
df$FL <- FL
df$Pmat <- Pmat
df <- as.data.frame(df)

slope <-nls(Pmat ~ 1/(1+exp(alfa2*(FL-L50))), data=df, start=list(alfa2=-0.01))
    # Nonlinear regression model
    # model: Pmat ~ 1/(1 + exp(alfa2 * (FL - L50)))
    # data: df
    # alfa2 
    # -0.091 
    # residual sum-of-squares: 4.045e-05
    # 
    # Number of iterations to convergence: 6 
    # Achieved convergence tolerance: 1.912e-09
alfa2 <- summary(slope)[[10]][1]
pmat_ss3 <- 1/(1+exp(alfa2*(FL-L50)))
df$pmat_ss3 <- pmat_ss3
df2 <- NULL
df2$L50 <- L50
df2$slope <- alfa2
df2 <- as.data.frame(df2)
write.csv(df2, file=file.path("data","ss3_inputs","maturityInput_ss3.csv"),row.names=FALSE)

ggplot()+geom_line(df,mapping=aes(x=FL, y=Pmat,color="varA"),linewidth=5)+
  xlab("Age")+ylab("Length (cm)")+
  geom_point(df,mapping=aes(x=FL,y=pmat_ss3,color="varB"),size=2.5,show.legend=T)+
  scale_color_manual(name=NULL, values=cols,
                     labels=c("Zudaire et al. 2022",c("Pmat ss3")) )+
  theme_fun()
SavePlot('Maturity_Zudaire',15,10)
