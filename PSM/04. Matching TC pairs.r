# Code 4
################################################################################
# China M3 TC Matching
# Matching Test and Control Pairs using Matchi It
# Last modified 11-22-2019
# Modified by -Tao Xing
################################################################################

# Function to evaluate the matching model performace
PrintEval<-function(match_results_v){
  summ<-summary(match_results_v)
  # Extracting total mean difference in test and cotrol groups after matching
  meandiff<-sum(abs(summ$sum.matched$`Mean Diff`))
  # EXtracting ratio of test hospitals paired with control to total test hospitals
  mathingratio<-paste0(round((summ$nn[2,2]/ summ$nn[1,2])*100,1),"%")
  
  # To obtain Lift
  out_matrix<-setNames(data.frame(match_results_v$match.matrix),c("control_rowno"))
  out_match<-match.data(match_results_v)
  group1<-merge(out_matrix,out_match,by=0,all.x=T )
  group2<-merge(group1 , out_match , by.x="control_rowno" , by.y=0 , all.x=T , suffixes=c("_test","_control"))
  group3<-na.omit(group2)
  L1_test_growth<- (sum(group3$PostSales_test)  - sum(group3$PreSales_test))/ sum(group3$PreSales_test)
  L1_Control_growth <- (sum(group3$PostSales_control)  - sum(group3$PreSales_control))/ sum(group3$PreSales_control)
  
  #L1_test_growth2<-(group3$PostSales_test-group3$PreSales_test)/group3$PreSales_test
  #L1_test_growth2<- L1_test_growth2[is.nan( L1_test_growth2)==FALSE]
  #L1_test_growth2<- L1_test_growth2[which(L1_test_growth2!=Inf)]
  #L1_test_growth2<-mean(L1_test_growth2)
  #L1_Control_growth2<-(group3$PostSales_control-group3$PreSales_control)/group3$PreSales_control
  #L1_Control_growth2<- L1_Control_growth2[is.nan( L1_Control_growth2)==FALSE]
  #L1_Control_growth2<- L1_Control_growth2[which(L1_Control_growth2!=Inf)]
  #L1_Control_growth2<-mean(L1_Control_growth2)
  # ABsolute Lift
  Lift_1<- L1_test_growth- L1_Control_growth
  #Lift_11<- L1_test_growth2- L1_Control_growth2
  print(paste0("meandiff -> ",round(meandiff,0),"   Lift -> ",round(Lift_1,2)," Matching % -> ",mathingratio))
  
}

PrintEval2<-function(match_results_v){
  summ<-summary(match_results_v)
  # Extracting total mean difference in test and cotrol groups after matching
  meandiff<-sum(abs(summ$sum.matched$`Mean Diff`))
  # EXtracting ratio of test hospitals paired with control to total test hospitals
  mathingratio<-paste0(round((summ$nn[2,2]/ summ$nn[1,2])*100,1),"%")
  
  # To obtain Lift
  out_matrix<-setNames(data.frame(match_results_v$match.matrix),c("control_rowno"))
  out_match<-match.data(match_results_v)
  group1<-merge(out_matrix,out_match,by=0,all.x=T )
  group2<-merge(group1 , out_match , by.x="control_rowno" , by.y=0 , all.x=T , suffixes=c("_test","_control"))
  group3<-na.omit(group2)
  #L1_test_growth<- (sum(group3$PostSales_test)  - sum(group3$PreSales_test))/ sum(group3$PreSales_test)
  #L1_Control_growth <- (sum(group3$PostSales_control)  - sum(group3$PreSales_control))/ sum(group3$PreSales_control)
  
  L1_test_growth2<-(group3$PostSales_test-group3$PreSales_test)/group3$PreSales_test
  L1_test_growth2<- L1_test_growth2[is.nan( L1_test_growth2)==FALSE]
  L1_test_growth2<- L1_test_growth2[which(L1_test_growth2!=Inf)]
  L1_test_growth2<-mean(L1_test_growth2)
  L1_Control_growth2<-(group3$PostSales_control-group3$PreSales_control)/group3$PreSales_control
  L1_Control_growth2<- L1_Control_growth2[is.nan( L1_Control_growth2)==FALSE]
  L1_Control_growth2<- L1_Control_growth2[which(L1_Control_growth2!=Inf)]
  L1_Control_growth2<-mean(L1_Control_growth2)
  # ABsolute Lift
  #Lift_1<- L1_test_growth- L1_Control_growth
  Lift_1<- L1_test_growth2- L1_Control_growth2
  print(paste0("meandiff -> ",round(meandiff,0),"   Lift -> ",round(Lift_1,2)," Matching % -> ",mathingratio))
  
}

# Matching Test and Control Pairs using Match It
library(MatchIt)

# Case 1 - including pre post sales,calls,ecalls,events and hospital basic info along with super4tag
# set.seed(300)
#数据标准化
# Test_Control_All_PrePost[,15:26]<-scale(Test_Control_All_PrePost[,15:26],scale=TRUE)

# match_results_v <- matchit(TC_Flag ~ PreSales+
#                              PreEvents + PostEvents +
#                              # PreEventsWithSpeaker + PostEventsWithSpeaker +
#                              # PreEventsWithoutSpeaker + PostEventsWithoutSpeaker +
#                              # PreEventsWithSpeakerFee + PostEventsWithSpeakerFee +
#                              PreCalls + PostCalls +
#                              PostECalls + PreECalls ,
#                            data = Test_Control_All_PrePost,
#                            method = "nearest",
#                            # distance = "logit",
#                            ratio = 1,
#                            exact = c("Tier","Hospital_Super4Tag","Duration","Hospital Level","Market"),
#                            replace = TRUE,
#                            caliper = 0.1,
#                            calclosest = T
#                            # ,
#                            # mahvars = c("PreSales", "PreEvents")
#                            )
# PrintEval(match_results_v)
# # PrintEval2(match_results_v)
# summ <- summary(match_results_v)
# summ$nn



#-- must do the rownames step when using the mahavrs options
#-- modified by Xin Huang here
rownames(Test_Control_All_PrePost) <-
  seq(1, nrow(Test_Control_All_PrePost))

set.seed(345)
match_results_v <- matchit(TC_Flag ~ PreSales+
                             PreEvents + PostEvents +
                             PreEventsWithSpeaker + PostEventsWithSpeaker +
                             # PreEventsWithoutSpeaker + PostEventsWithoutSpeaker +
                             PreEventsWithSpeakerFee + PostEventsWithSpeakerFee
                             + PreCalls + PostCalls 
                           # + PostECalls + PreECalls
                           ,
                           data = Test_Control_All_PrePost,
                           method = "nearest",
                           distance = "logit", # propensity score matching
                           ratio = 1, 
                           exact = c("Tier", "Hospital_Super4Tag", 
                                     "Duration",
                                     "Hospital Level", "Market"
                                     #,"Top 200"
                                     ),#modify province og market
                           #m.order="random",
                           replace = FALSE,
                           caliper = 0.2,
                           #calclosest = TRUE,
                           #nearst= c(#"PreCalls","PostCalls"
                            # "PreEvents","PostEvents","PreEventsWithSpeaker","PostEventsWithSpeaker",
                             #          "PreEventsWithSpeakerFee","PostEventsWithSpeakerFee","PreSales"
                             #),
                           mahvars = c("PreSales" # here is the mahalanobis distance
                                       #, "PreEvents",
                                        # "PostEvents"
                                        #,
                                        #,"PreEventsWithSpeaker",
                                        #"PostEventsWithSpeaker"
                                        #,
                                        #"PreEventsWithoutSpeaker" ,
                                        #"PostEventsWithoutSpeaker" ,
                                        #,"PreEventsWithSpeakerFee" ,
                                        #"PostEventsWithSpeakerFee"
                                        #"PreCalls" ,
                                        #"PostCalls"
                                       # "PostECalls" ,
                                       # "PreECalls"
                           )
)

PrintEval(match_results_v)
# PrintEval2(match_results_v)
summ <- summary(match_results_v)
summ$nn
# "meandiff -> 171   Lift -> 0.34 Matching % -> 99.7%"
summary(match_results_v)


###################################Obtaining results########
out_matrix <- 
  setNames(data.frame(match_results_v$match.matrix),c("control_rowno"))
out_match <- match.data(match_results_v)
group1 <- merge(out_matrix, out_match, by=0, all.x=T)
group2 <- merge(group1, out_match, by.x="control_rowno",
                by.y=0 , all.x=T , suffixes=c("_test","_control"))
group3 <- na.omit(group2)
# write_xlsx(group3,"group3.xlsx")
# write_xlsx(Master_HCP_Table,"C:/Users/honey.gupta/Desktop/Master_HCP_Table.xlsx")
L1_test_growth <- (sum(group3$PostSales_test) - sum(group3$PreSales_test))/ sum(group3$PreSales_test)
L1_Control_growth <- (sum(group3$PostSales_control)  - sum(group3$PreSales_control))/ sum(group3$PreSales_control)
# ABsolute Lift
Lift_1<- (L1_test_growth- L1_Control_growth)*100

# getting city wise test and control matching
group3$Control_Unique<-paste0(group3$`Hospital ID_control`,"-",group3$Duration_control)

tierwisenum<- group3 %>% dplyr::distinct(Control_Unique,Tier_control) %>% dplyr::group_by(Tier_control) %>% 
  dplyr::summarise(cnt_c = n())
tierwisenum2<- group3  %>% dplyr::group_by(Tier_test) %>% 
  dplyr::summarise(cnt_t = n())
tierwisenum3<-cbind(tierwisenum,tierwisenum2)
tierwisenum3<-tierwisenum3[,c(-3)]

#############################Significance testing###############
# Checking if pre sales of test and control groups is significatly similar

t.test(group3$PreSales_test,group3$PreSales_control,paired = T)
t.test(group3$PreEvents_test,group3$PreEvents_control,paired = T)
t.test(group3$PostEvents_test,group3$PostEvents_control,paired = T)
t.test(group3$PreCalls_test,group3$PreCalls_control,paired = T)
t.test(group3$PostCalls_test,group3$PostCalls_control,paired = T)
t.test(group3$PreECalls_test,group3$PreECalls_control,paired = T)
t.test(group3$PostECalls_test,group3$PostECalls_control,paired = T)


###
t.test(group3$PreSales_test,group3$PreSales_control)
t.test(group3$PreEvents_test,group3$PreEvents_control)
t.test(group3$PostEvents_test,group3$PostEvents_control)
t.test(group3$PreCalls_test,group3$PreCalls_control)
t.test(group3$PostCalls_test,group3$PostCalls_control)
t.test(group3$PreECalls_test,group3$PreECalls_control)
t.test(group3$PostECalls_test,group3$PostECalls_control)

write_xlsx(group3,"C:/Users/GLJLI/OneDrive - Bayer/Desktop/emr-tao/2019-10/new code and data/output data/group3_1108.xlsx")


#########################################################################################
# Deep dives-

# Getting Monthly Test and Control Sales,events,calls
Test_monthly <- 
  merge(Test_Control_All,group3[,c("Hospital ID_test","Duration_test")],by.x = c("Hospital ID","Duration"),
                    by.y = c("Hospital ID_test","Duration_test"))
nrow(group3)
length(unique(Test_monthly$`Hospital ID`))

Control_monthly<-merge(Test_Control_All,group3[,c("Hospital ID_control","Duration_control")],by.x = c("Hospital ID","Duration"),
                    by.y = c("Hospital ID_control","Duration_control"),
                    all.y = T)
length(unique(Control_monthly$`Hospital ID`))
length(unique(group3$`Hospital ID_control`))
 
nrow(Control_monthly)
nrow(Test_monthly)

unique(Control_monthly$TC_Flag)

test_control_matched_monthly<-rbind(Test_monthly,Control_monthly)

test_control_matched_monthly$month_req <-ifelse(test_control_matched_monthly$Month >= test_control_matched_monthly$Pre_L &
                                                  test_control_matched_monthly$Month<= test_control_matched_monthly$Pre_U,1,
                                                ifelse(test_control_matched_monthly$Month >= test_control_matched_monthly$Post_L &
                                                         test_control_matched_monthly$Month<= test_control_matched_monthly$Post_U,1,0))

write_xlsx(test_control_matched_monthly,"C:/Users/GLJLI/OneDrive - Bayer/Desktop/emr-tao/2019-10/new code and data/output data/test_control_matched_monthly.xlsx")
prepostchart_sales <- test_control_matched_monthly %>% dplyr::filter(month_req==1) %>% 
  dplyr::group_by(Month,TC_Flag) %>% dplyr::summarise(sales_sum = sum(TotSales)) %>% 
  tidyr::spread(key = TC_Flag, value = sales_sum)

prepostchart_events <- test_control_matched_monthly %>% dplyr::filter(month_req==1) %>% 
  dplyr::group_by(Month,TC_Flag) %>% dplyr::summarise(Event_sum = sum(SmallEvent_Cnt)) %>% 
  tidyr::spread(key = TC_Flag, value = Event_sum)

prepostchart_calls <- test_control_matched_monthly %>% dplyr::filter(month_req==1) %>% 
  dplyr::group_by(Month,TC_Flag) %>% dplyr::summarise(Call_sum = sum(Calls)) %>% 
  tidyr::spread(key = TC_Flag, value = Call_sum)

prepostchart_ecalls <- test_control_matched_monthly %>% dplyr::filter(month_req==1) %>% 
  dplyr::group_by(Month,TC_Flag) %>% dplyr::summarise(eCall_sum = sum(eCalls)) %>% 
  tidyr::spread(key = TC_Flag, value = eCall_sum)


# Sales data for eda chart
# Overall_SAles<-Hosp_Sales_Final %>% group_by(Month) %>% dplyr::summarise(sum_sales = sum(TotSales))
Test_Sales_before_Matching <- China_TC_ADS %>% group_by(Month,TC_Flag) %>% dplyr::summarise(sum_sales = sum(TotSales)) %>% 
  tidyr::spread(value = sum_sales, key = TC_Flag)

write_xlsx(Hosp_Sales_Final,"C:/Users/GLJLI/OneDrive - Bayer/Desktop/emr-tao/2019-10/new code and data/output data/Hosp_Sales_Final.xlsx")
# 
write_xlsx(China_TC_ADS,"C:/Users/GLJLI/OneDrive - Bayer/Desktop/emr-tao/2019-10/new code and data/output data/China_TC_ADS.xlsx")

# First response graph chart data
FResp_Data <- China_TC_ADS %>% distinct(`Hospital ID`,TC_Flag,First_Response) %>% 
  mutate(MonthofResp =  format(as.Date(First_Response), "%B-%y")) %>% filter(TC_Flag==1) %>% 
  group_by(MonthofResp) %>% 
  dplyr::summarise(Cnt = n()) %>% 
  dplyr::mutate(Pct = Cnt/sum(Cnt))

##############################################################
# checking significance region wise
group3_north<-group3 %>% filter(Market_test=="North")
t.test(group3_north$PreSales_test,group3_north$PreSales_control)

group3_south<-group3 %>% filter(Market_test=="South")
t.test(group3_south$PreSales_test,group3_south$PreSales_control)

group3_west<-group3 %>% filter(Market_test=="West")
t.test(group3_west$PreSales_test,group3_west$PreSales_control)



open_no_hcp <- China_TC_ADS %>% distinct(`Hospital ID`,TC_Flag,Open_No) %>% dplyr::group_by(TC_Flag) %>% dplyr::summarise(sum=sum(Open_No))

# write_xlsx(open_no_hcp,"open_no_hcp.xlsx")
write_xlsx(Master_HCP_Table_Cost_Filter,"C:/Users/GLJLI/OneDrive - Bayer/Desktop/emr-tao/2019-10/new code and data/output data/Master_HCP_Table_Cost_Filter.xlsx")
