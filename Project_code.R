# Final Project 

rm(list=ls())
install.packages("dplyr")
library(dplyr)

#---------------------------------------------------------------------
# I. Import/export data 
#---------------------------------------------------------------------
PPP0<-read.csv("Data/public_150k_plus_240930.csv")
PPP1<-read.csv("Data/public_up_to_150k_1_240930.csv")
PPP2<-read.csv("Data/public_up_to_150k_2_240930.csv")
PPP3<-read.csv("Data/public_up_to_150k_3_240930.csv")
PPP4<-read.csv("Data/public_up_to_150k_4_240930.csv")
PPP5<-read.csv("Data/public_up_to_150k_5_240930.csv")
PPP6<-read.csv("Data/public_up_to_150k_6_240930.csv")
PPP7<-read.csv("Data/public_up_to_150k_7_240930.csv")
PPP8<-read.csv("Data/public_up_to_150k_8_240930.csv")
PPP9<-read.csv("Data/public_up_to_150k_9_240930.csv")
PPP10<-read.csv("Data/public_up_to_150k_10_240930.csv")
PPP11<-read.csv("Data/public_up_to_150k_11_240930.csv")
PPP12<-read.csv("Data/public_up_to_150k_12_240930.csv")

SUSB<-read.csv("Data/SUSB_5states.csv")
#---------------------------------------------------------------------
# II. Data processing / cleaning
#---------------------------------------------------------------------
#Filter out business based and active in CA, FL, IL, NY, TX
states <- c("CA", "FL", "IL", "NY", "TX")

PPP0 <- PPP0[PPP0$BorrowerState %in% states & PPP0$ProjectState %in% states, ]
PPP1 <- PPP1[PPP1$BorrowerState %in% states & PPP1$ProjectState %in% states, ]
PPP2 <- PPP2[PPP2$BorrowerState %in% states & PPP2$ProjectState %in% states, ]
PPP3 <- PPP3[PPP3$BorrowerState %in% states & PPP3$ProjectState %in% states, ]
PPP4 <- PPP4[PPP4$BorrowerState %in% states & PPP4$ProjectState %in% states, ]
PPP5 <- PPP5[PPP5$BorrowerState %in% states & PPP5$ProjectState %in% states, ]
PPP6 <- PPP6[PPP6$BorrowerState %in% states & PPP6$ProjectState %in% states, ]
PPP7 <- PPP7[PPP7$BorrowerState %in% states & PPP7$ProjectState %in% states, ]
PPP8 <- PPP8[PPP8$BorrowerState %in% states & PPP8$ProjectState %in% states, ]
PPP9 <- PPP9[PPP9$BorrowerState %in% states & PPP9$ProjectState %in% states, ]
PPP10 <- PPP10[PPP10$BorrowerState %in% states & PPP10$ProjectState %in% states, ]
PPP11 <- PPP11[PPP11$BorrowerState %in% states & PPP11$ProjectState %in% states, ]
PPP12 <- PPP12[PPP12$BorrowerState %in% states & PPP12$ProjectState %in% states, ]

#Create Consolidated PPP Dataset
LN = c(
  PPP0$LoanNumber, PPP1$LoanNumber, PPP2$LoanNumber,
  PPP3$LoanNumber, PPP4$LoanNumber, PPP5$LoanNumber,
  PPP6$LoanNumber, PPP7$LoanNumber, PPP8$LoanNumber,
  PPP9$LoanNumber, PPP10$LoanNumber, PPP11$LoanNumber
)

D1 = c(
  rep(0, nrow(PPP0)), rep(1, nrow(PPP1)), rep(2, nrow(PPP2)),
  rep(3, nrow(PPP3)), rep(4, nrow(PPP4)), rep(5, nrow(PPP5)),
  rep(6, nrow(PPP6)), rep(7, nrow(PPP7)), rep(8, nrow(PPP8)),
  rep(9, nrow(PPP9)), rep(10, nrow(PPP10)), rep(11, nrow(PPP11))
)

PPP=t(rbind(LN,D1))
PPP=as.data.frame(PPP)

#Year of Approval
PPP$DateApproved = c(
  PPP0$DateApproved, PPP1$DateApproved, PPP2$DateApproved,
  PPP3$DateApproved, PPP4$DateApproved, PPP5$DateApproved,
  PPP6$DateApproved, PPP7$DateApproved, PPP8$DateApproved,
  PPP9$DateApproved, PPP10$DateApproved, PPP11$DateApproved
)
PPP$YrApproved = substr((PPP$DateApproved), nchar(PPP$DateApproved) - 3, nchar(PPP$DateApproved))
unique(PPP$YrApproved)

# Other variables
PPP$CurrentApprovalAmount = c(
  PPP0$CurrentApprovalAmount, PPP1$CurrentApprovalAmount, PPP2$CurrentApprovalAmount,
  PPP3$CurrentApprovalAmount, PPP4$CurrentApprovalAmount, PPP5$CurrentApprovalAmount,
  PPP6$CurrentApprovalAmount, PPP7$CurrentApprovalAmount, PPP8$CurrentApprovalAmount,
  PPP9$CurrentApprovalAmount, PPP10$CurrentApprovalAmount, PPP11$CurrentApprovalAmount
)

PPP$BorrowerName = c(
  PPP0$BorrowerName, PPP1$BorrowerName, PPP2$BorrowerName,
  PPP3$BorrowerName, PPP4$BorrowerName, PPP5$BorrowerName,
  PPP6$BorrowerName, PPP7$BorrowerName, PPP8$BorrowerName,
  PPP9$BorrowerName, PPP10$BorrowerName, PPP11$BorrowerName
)

PPP$BorrowerState = c(
  PPP0$BorrowerState, PPP1$BorrowerState, PPP2$BorrowerState,
  PPP3$BorrowerState, PPP4$BorrowerState, PPP5$BorrowerState,
  PPP6$BorrowerState, PPP7$BorrowerState, PPP8$BorrowerState,
  PPP9$BorrowerState, PPP10$BorrowerState, PPP11$BorrowerState
)

# NATCS
PPP$NAICSCode = c(
  PPP0$NAICSCode, PPP1$NAICSCode, PPP2$NAICSCode,
  PPP3$NAICSCode, PPP4$NAICSCode, PPP5$NAICSCode,
  PPP6$NAICSCode, PPP7$NAICSCode, PPP8$NAICSCode,
  PPP9$NAICSCode, PPP10$NAICSCode, PPP11$NAICSCode
)

#Check NAs
summary(PPP$NAICSCode)
#Remove NAs
library(dplyr)
PPP <- PPP %>%
  filter(!is.na(NAICSCode))
#Remove rows not included in SUSB calculation
PPP <- PPP %>%
  filter(!(NAICSCode %in% c(525110, 525120, 525190, 525920, 541120))) 

#New column getting first three digit of NAICSCode for mapping with SUSB
PPP$NAICS_3dig=substr(as.character(PPP$NAICSCode), 1, 3)
PPP <- PPP %>%
  filter(!(NAICS_3dig %in% c(111,112,482, 491, 814)))

#New column getting first two digit of NAICSCode for mapping with SUSB
PPP$NAICS_2dig=substr(as.character(PPP$NAICSCode), 1, 2)
PPP <- PPP %>%
  filter(!(NAICS_2dig %in% c(92)))

unique(PPP$NAICS_2dig)
unique(SUSB$NAICS)
PPP$NAICS_2dig[PPP$NAICS_2dig == "31"] <- "31-33"
PPP$NAICS_2dig[PPP$NAICS_2dig == "32"] <- "31-33"
PPP$NAICS_2dig[PPP$NAICS_2dig == "33"] <- "31-33"
PPP$NAICS_2dig[PPP$NAICS_2dig == "44"] <- "44-45"
PPP$NAICS_2dig[PPP$NAICS_2dig == "45"] <- "44-45"
PPP$NAICS_2dig[PPP$NAICS_2dig == "48"] <- "48-49"
PPP$NAICS_2dig[PPP$NAICS_2dig == "49"] <- "48-49"
PPP$NAICS_2dig=as.character(PPP$NAICS_2dig)
#double check by unique values
length(unique(PPP$NAICS_2dig))
length(unique(SUSB$NAICS))

summary(PPP)
summary(SUSB)

#Avoid double count in the same year
PPP_aggregated_20 <- PPP %>%
  filter(YrApproved == "2020") %>%
  group_by(BorrowerName) %>%
  summarize(
    YrApproved = "2020",
    Loan_Approval_Amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    State = first(BorrowerState),
    NAICS = first(NAICS_2dig)
  )


PPP_aggregated_21 <- PPP %>%
  filter(YrApproved == "2021") %>%
  group_by(BorrowerName) %>%
  summarize(
    YrApproved = "2021",
    Loan_Approval_Amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    State = BorrowerState,
    NAICS = first(NAICS_2dig)
  )

summary(PPP_aggregated_20)
summary(PPP_aggregated_21)

write.csv(PPP_aggregated_20, file="Data/PPP_20.csv")
write.csv(PPP_aggregated_21, file="Data/PPP_21.csv")


#---------------------------------------------------------------------
# III. Defining Key values
#---------------------------------------------------------------------
rm(list=ls())

PPP_20<-read.csv("Data/PPP_20.csv")
PPP_21<-read.csv("Data/PPP_21.csv")
library(dplyr)
library(tidyr) 

# Aggregate loan values by State and Sector
Total_loans_20 <- PPP_20 %>%
  group_by(State, NAICS) %>%
  summarize(TotalLoanValue = sum(Loan_Approval_Amount, na.rm = TRUE), .groups = 'drop')

Total_loans_21 <- PPP_21 %>%
  group_by(State, NAICS) %>%
  summarize(TotalLoanValue = sum(Loan_Approval_Amount, na.rm = TRUE), .groups = 'drop')


# SUSB and PPP loan dataset
SUSBPPP<-read.csv("Data/SUSB_PPP_5.csv")
SUSBPPP <- subset(SUSBPPP, select = -c(X))

#Finding treatment intensity 
#Use Total amount of PPP loan issued 
SUSBPPP$Treatment_intensity = SUSBPPP$PPP_Loan/SUSBPPP$Establishments
summary(SUSBPPP$Treatment_intensity)

#Normalize using z-score
SUSBPPP$Norm_TI = (SUSBPPP$Treatment_intensity - mean(SUSBPPP$Treatment_intensity))/sd(SUSBPPP$Treatment_intensity)
summary(SUSBPPP$Norm_TI)
hist(SUSBPPP$Norm_TI)

#Define Control and Treatment group (2020)
summary(SUSBPPP$Norm_TI[SUSBPPP$Year == 2020])
hist(SUSBPPP$Norm_TI[SUSBPPP$Year == 2020])
#Taking median: 0.19827 as threshold
#Control group(0) when Norm_TI <=0.2
#Treatment group(1) when Norm_TI >0.2
threshold_20 = 0.2
SUSBPPP$T_C_20=0  
SUSBPPP$T_C_20[SUSBPPP$Year == 2020] <- ifelse(SUSBPPP$Norm_TI[SUSBPPP$Year == 2020] > threshold_20, 1, 0)
T19 = SUSBPPP$NAICS[SUSBPPP$T_C_20 == 1]
SUSBPPP$T_C_20[SUSBPPP$Year == 2019 & SUSBPPP$NAICS %in% T19] = 1

#Define Control and Treatment group (2021)
summary(SUSBPPP$Norm_TI)
summary(SUSBPPP$Norm_TI[SUSBPPP$Year == 2021])
hist(SUSBPPP$Norm_TI[SUSBPPP$Year == 2021])
#Taking median: -0.05165 as threshold
#Control group(0) when Norm_TI <=-0.05
#Treatment group(1) when Norm_TI >-0.05
threshold_21=-0.05
SUSBPPP$T_C_21=0  
SUSBPPP$T_C_21[SUSBPPP$Year == 2021] <- ifelse(SUSBPPP$Norm_TI[SUSBPPP$Year == 2021] > threshold_21, 1, 0)
T20 = SUSBPPP$NAICS[SUSBPPP$T_C_21 == 1]
SUSBPPP$T_C_21[SUSBPPP$Year == 2020 & SUSBPPP$NAICS %in% T20] = 1

#Adding t: Post2020 & Post 2021
SUSBPPP$Post2020 <- ifelse(SUSBPPP$Year == 2020, 1, 0)
SUSBPPP$Post2021 <- ifelse(SUSBPPP$Year == 2021, 1, 0)

summary(SUSBPPP)
write.csv(SUSBPPP, file="Data/PPP_DID.csv")

install.packages("stargazer")
library(stargazer)
stargazer(SUSBPPP, type="html", out="SUSBPPP.html", summary.stat=c("n", "mean", "sd", "min", 
                                                                    "p25", "median", "p75", "max"))

#---------------------------------------------------------------------
# IV. DID (for first draw of PPP: 2019 vs 2020)-> Significant
#---------------------------------------------------------------------
rm(list=ls())
DID<-read.csv("Data/PPP_DID.csv")
library(dplyr)
install.packages("stargazer")
library(stargazer)

DID$AnnualPayroll=as.numeric(DID$Annual.Payroll...1.000.)
names(DID)[names(DID) == "Firm.Survival_19"] <- "Firm.Survival"
names(DID)[names(DID) == "Annual.Payroll...1.000."] <- "Annual_Payroll"

# Subsampling based on time and group
# Pre-treatment (t = 0) and post-treatment (t = 1)
# Treatment group is defined by T_C_20 or T_C_21, indicating treated sectors
# Post-treatment indicator is Post2020 or Post2021

#For 2020: 
# Control group in t=0 (before 2020 treatment)
p00 <- DID$Firm.Survival[DID$T_C_20 == 0 & DID$Post2020 == 0 & DID$Year == 2019]
# Control group in t=1 (after 2020 treatment)
p01 <- DID$Firm.Survival[DID$T_C_20 == 0 & DID$Post2020 == 1]
# Treatment group in t=0 (before 2020 treatment)
p10 <- DID$Firm.Survival[DID$T_C_20 == 1 & DID$Post2020 == 0 & DID$Year == 2019]
# Treatment group in t=1 (after 2020 treatment)
p11 <- DID$Firm.Survival[DID$T_C_20 == 1 & DID$Post2020 == 1]

# Compare control and treatment group survival rates before treatment
mean(p00); mean(p10)
t.test(p00, p10) # Check if there's a difference in survival rates before treatment
# p-value = 0.04362
# Reject the null hypothesis,there is enough statistical evidence 
# showing that the control and treatment groups differ significantly in their mean survival rates before treatment.

# Compare survival rates over time
mean(p00); mean(p01) # Control group trend
mean(p10); mean(p11) # Treatment group trend
#Result: Both increase over time but treatment group showed a larger increase. 

#DID of survival rates
# Survival rate difference before and after in treatment group
diff_treatment <- mean(p11) - mean(p10)
# Survival rate difference before and after in control group
diff_control <- mean(p01) - mean(p00)
# Treatment effect (DID estimate)
DID_effect <- diff_treatment - diff_control
DID_effect
#Result:0.02656372, a positive DID estimate suggests that the treatment (PPP loans) 
#had a beneficial impact on firm survival rates.
#i.e. if there were 1,000 firms in 2019, receiving PPP loan will allow 26.56≈27 more firms surviving in 2020.

#Use regression with two way fixed effect(TWFE) - 2020

# DID regression for 2020 treatment effect
DID_19_20 <- DID %>%
  filter(Year %in% c(2019, 2020))  # Makes sure to include only specified years
m1 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020, data = DID_19_20)
summary(m1)
plot(DID_19_20$T_C_20,DID_19_20$Firm.Survival,pch=20)
points(DID_19_20$T_C_20,m1$fitted.values,pch=15,col=2)

#F-test:p-value of 0.006243
#<0.05 suggests that the overall model is significant

# Add control variables for robustness
m2 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) , data = DID_19_20)
m3 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(NAICS.Description), data = DID_19_20)
library(stargazer)
stargazer(m1, m2, m3, type = "html", out = "DID_20.html")
#T-test: 
#m2 & m3 : Some of the coefficient is significant and 
#greatly affect the coefficient of survival rate and R^2
#Keeping both model 

m4 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) + as.factor(NAICS.Description), data = DID_19_20)
m5 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) + as.factor(NAICS.Description) + Employment, data = DID_19_20)
m6 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) + as.factor(NAICS.Description) + AnnualPayroll, data = DID_19_20)
anova(m4,m5) 
#F-test: 
#F test p value is 0.7858-> fail to reject null hypothesis as is larger than 0.05
#keeping m5 is unnecessary
anova(m4,m6) 
#F-test: 
#F test p value is 0.7244-> fail to reject null hypothesis as is larger than 0.05
#keeping m6 is unnecessary

#DID for 2020 
m1 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020, data = DID_19_20)
m2 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) , data = DID_19_20)
m3 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(NAICS.Description), data = DID_19_20)
m4 <- lm(Firm.Survival ~ T_C_20 + Post2020 + T_C_20 * Post2020 + as.factor(State.Name) + as.factor(NAICS.Description), data = DID_19_20)

library(stargazer)
stargazer(m1, m2, m3, m4, type = "html", out = "DID_20.html")

#---------------------------------------------------------------------
# V. DID (for second draw of PPP: 2020 vs 2021) -> violated DID rule 
#---------------------------------------------------------------------

#For 2021: 
# Control group in t=0 (before 2021 treatment)
p00 <- DID$Firm.Survival[DID$T_C_21 == 0 & DID$Post2021 == 0 & DID$Year == 2020]
# Control group in t=1 (after 2021 treatment)
p01 <- DID$Firm.Survival[DID$T_C_21 == 0 & DID$Post2021 == 1]
# Treatment group in t=0 (before 2021 treatment)
p10 <- DID$Firm.Survival[DID$T_C_21 == 1 & DID$Post2021 == 0 & DID$Year == 2020]
# Treatment group in t=1 (after 2021 treatment)
p11 <- DID$Firm.Survival[DID$T_C_21 == 1 & DID$Post2021 == 1]

# Compare control and treatment group survival rates before treatment
mean(p00); mean(p10)
t.test(p00, p10) # Check if there's a difference in survival rates before treatment
# p-value = 0.0482
# Reject the null hypothesis,there is enough statistical evidence 
# showing that the control and treatment groups differ significantly in their mean survival rates before treatment.

# Compare survival rates over time
mean(p00); mean(p01) # Control group trend
mean(p10); mean(p11) # Treatment group trend
#Result: Control group and Treatment group has a different trend, violating the DID rule

#---------------------------------------------------------------------
# VI. DID (for full PPP: 2019 vs 2021) -> violated DID rule 
#---------------------------------------------------------------------
#Try to compare 2019 vs 2021 for the full period of PPP (who have received two draws of PPP loan)
#Treatment group: received PPP loan in both 2020 & 2021
T20=unique(DID$NAICS[DID$T_C_20 == 1])
T21=unique(DID$NAICS[DID$T_C_21 == 1])
Tfull <- intersect(T20, T21)
DID$T_C_Full <- ifelse(DID$NAICS %in% Tfull, 1, 0)

# Control group in t=0 (before treatment)
p00 <- DID$Firm.Survival[DID$T_C_Full == 0 & DID$Year == 2019]
# Control group in t=1 (after full 2021 treatment)
p01 <- DID$Firm.Survival[DID$T_C_Full == 0 & DID$Year == 2021]
# Treatment group in t=0 (before treatment)
p10 <- DID$Firm.Survival[DID$T_C_Full == 1 & DID$Year == 2019]
# Treatment group in t=1 (after full 2021 treatment)
p11 <- DID$Firm.Survival[DID$T_C_Full == 1 & DID$Year == 2021]

# Compare control and treatment group survival rates before treatment
mean(p00); mean(p10)
t.test(p00, p10) # Check if there's a difference in survival rates before treatment
# p-value = 0.7385
# Fail to reject the null hypothesis,there is not enough evidence 
# to conclude that the control and treatment groups differ significantly in their mean survival rates before treatment.

# Compare survival rates over time
mean(p00); mean(p01) # Control group trend
mean(p10); mean(p11) # Treatment group trend
#Result: Control group and Treatment group has a different trend, violating the DID rule

#---------------------------------------------------------------------
# VII. Graphics
#---------------------------------------------------------------------
#. Barplot on Loan distribution across states
library(ggplot2)
ggplot(DID, aes(x = State.Name, y = AnnualPayroll)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Loan Amount Across States", x = "State", y = "Loan Amount")

#. Barplot on Loan distribution across sectors
library(ggplot2)
ggplot(DID, aes(x = NAICS.Description, y = AnnualPayroll)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Loan Amount Across Sectors", x = "Sectors", y = "Loan Amount")

