library(MatchIt)
library(data.table)
library(dplyr)
library(tidyr)
library(readxl)
library(varhandle)
library(stringr)
library(writexl)



Rocket<-read_excel('C:\\Users\\Tao\\Desktop\\jhu课程\\2021-spring\\Marketing Management X1\\Rock\\input\\dataset_Rocket_forR.xlsx')
set.seed(345)
match_results_v <- matchit(group ~ mode_impr_hour
                           ,
                           data = Rocket,
                           method = "nearest",
                           distance = "logit", # propensity score matching
                           ratio = 1, 
                           exact = c("mode_impr_day"
                           )
                           #m.order="random",
                           #replace = FALSE,
                           #caliper = 0.2,
                           #calclosest = TRUE,

)
summ<-summary(match_results_v)
mathingratio<-paste0(round((summ$nn[2,2]/ summ$nn[1,2])*100,1),"%")
df.match<-match.data(match_results_v)[1:ncol(Rocket)]
write_xlsx(df.match,"C:\\Users\\Tao\\Desktop\\jhu课程\\2021-spring\\Marketing Management X1\\Rock\\output\\dataset_Rocket_forR_result.xlsx")