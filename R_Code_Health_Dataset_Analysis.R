setwd("C:\\Users\\sanda\\OneDrive\\Desktop\\ST3010_CaseStudy")
#install.packages("readxl")
library(readxl)
Health_Data=read_xlsx("CaseStudy.xlsx")
getwd()

#attach(CaseStudy)
summary(CaseStudy)

#Explore the Data
head(Health_Data)
summary(Health_Data)
str(Health_Data)
colnames(Health_Data)

#Change Pre-Post-Hypertension column name to Pre-Hypertension
colnames(Health_Data)[colnames(Health_Data) == "Pre-Weight"] <- "Pre_Weight"
colnames(Health_Data)[colnames(Health_Data) == "Pre-Height"] <- "Pre_Height"
colnames(Health_Data)[colnames(Health_Data) == "Pre-BMI"] <- "Pre_BMI"
colnames(Health_Data)[colnames(Health_Data) == "Pre-Post-Hypertension"] <- "Pre_Hypertension"
colnames(Health_Data)[colnames(Health_Data) == "Pre-Obesity"] <- "Pre_Obesity"
colnames(Health_Data)[colnames(Health_Data) == "Post weight"] <- "Post_Weight"
colnames(Health_Data)[colnames(Health_Data) == "Post- height"] <- "Post_Height"
colnames(Health_Data)[colnames(Health_Data) == "Post BMI"] <- "Post_BMI"
colnames(Health_Data)[colnames(Health_Data) == "Post-Hypertension"] <- "Post_Hypertension"
colnames(Health_Data)[colnames(Health_Data) == "Post-Obesity"] <- "Post_Obesity"
colnames(Health_Data)

#Categorical Variable
Health_Data$Gender=as.factor(Health_Data$Gender)
Health_Data$Ethnicity=as.factor(Health_Data$Ethnicity)
Health_Data$Pre_Hypertension=as.factor(Health_Data$Pre_Hypertension)
Health_Data$Pre_Obesity=as.factor(Health_Data$Pre_Obesity)
Health_Data$Post_Hypertension=as.factor(Health_Data$Post_Hypertension)

Health_Data$Post_Obesity=as.factor(Health_Data$Post_Obesity)
summary(Health_Data)

#Check for missing values
sum(is.na(Health_Data))

#Check Duplicates
sum(duplicated(Health_Data))

#Unique Values
unique(Health_Data$Gender)
unique(Health_Data$Ethnicity)
unique(Health_Data$Pre_Hypertension)
unique(Health_Data$Pre_Obesity)
unique(Health_Data$Post_Hypertension)
unique(Health_Data$Post_Obesity)

#____________________________________________Object 1________________________________________________________________________
#Assess Changes in Health Risk Factors (Obesity and Hypertension) Before and After the Program:
#~~~~~~~~~~~~~~~~~~~~~~~~ Find Using Calculating Hypothesis Testing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#@@@@@@@@@@@@@@@@@@@@@@@ Find Parametric assumptions are valid or invalid for Variables
qqnorm(Health_Data$Pre_BMI)
qqline(Health_Data$Pre_BMI)
shapiro.test(Health_Data$Pre_BMI)
#p-value<0.05 we reject Ho -> Pre_BMI variable is not normally distributed

qqnorm(Health_Data$Post_BMI)
qqline(Health_Data$Post_BMI)
shapiro.test(Health_Data$Post_BMI)
#p-value<0.05 we reject Ho -> Post_BMI variable is not normally distributed

qqnorm(Health_Data$Pre_Height)
qqline(Health_Data$Pre_Height)
shapiro.test(Health_Data$Pre_Height)
#p-value<0.05 we reject Ho -> Pre_Height variable is not normally distributed

qqnorm(Health_Data$Post_Height)
qqline(Health_Data$Post_Height)
shapiro.test(Health_Data$Post_Height)
#p-value<0.05 we reject Ho -> Post_Height variable is not normally distributed

qqnorm(Health_Data$Pre_Weight)
qqline(Health_Data$Pre_Weight)
shapiro.test(Health_Data$Pre_Weight)
#p-value<0.05 we reject Ho -> Pre_Weight variable is not normally distributed

qqnorm(Health_Data$Post_Weight)
qqline(Health_Data$Post_Weight)
shapiro.test(Health_Data$Post_Weight)
#p-value<0.05 we reject Ho -> Post_Weight variable is not normally distributed

qqnorm(Health_Data$Age)
qqline(Health_Data$Age)
shapiro.test(Health_Data$Age)
#p-value<0.05 we reject Ho -> Post_Weight variable is not normally distributed

##### Then normality assumptions are violated in here. So, we use Non Parametric test
#@@@@@@@@@@@@@@@@@@@@@@@@@@ Health Risk Changes Calculating By Pre, Post Weight
#=============== Wilcox's Signed Rank Test for Matched Pairs for Pre and Post Weight

#...........Ho:M(Pre-Post)=0 H1:M(Pre-Post) =/ 0
wilcox.test(Health_Data$Pre_Weight,Health_Data$Post_Weight,mu=0, alternative ="two.sided",paired = T,exact=F,conf.int = T,conf.level = 0.95)
#Reject Ho

#............Ho:M(Pre-Post)=<0 H1:M(Pre-Post) > 0
wilcox.test(Health_Data$Pre_Weight,Health_Data$Post_Weight,mu=0, alternative ="greater",paired = T,exact=F,conf.int = T,conf.level = 0.95)
#Reject Ho
##### Therefore Pre Weight is higher than Post we can determine Obesity Risk is reduced then can assume Hypertension is also reduced.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Find Using Pre and Post Change Count Considering in Health Risk Factors
#install.packages("dplyr")
library(dplyr)

# Assuming you have a data frame named 'health_data' with 'Pre_Obesity' and 'Post_Obesity' columns
# Create a new variable 'Change_in_Obesity' with custom labels
# @@@@@@@@@@@@@@@@@@@@@ For Obesity Find Count Number of Changes
Change_Obesity = ifelse(Health_Data$Pre_Obesity == "No" & Health_Data$Post_Obesity == "Yes", "Increased",
                        ifelse(Health_Data$Pre_Obesity == "Yes" & Health_Data$Post_Obesity == "No", "Decreased", "No Change"))
Change_Obesity
Count_Increased_Obe = sum(Change_Obesity == "Increased")
Count_Increased_Obe
Count_Decreased_Obe = sum(Change_Obesity == "Decreased")
Count_Decreased_Obe
Count_No_Change_Obe= sum(Change_Obesity == "No Change")
Count_No_Change_Obe

# Create the bar chart for Change in Obesity
Count_Data_Obe=c(0,4,286)
Label_Count_Data_Obe=c("Increased","Decreased","No Change")
Lable_Count_Data_Value_Obe=c("0","4","286")
bar_heights=barplot(Count_Data_Obe,
                    names.arg = Label_Count_Data_Obe, # Add labels to the bars
                    col = "skyblue", # Customize the bar color
                    main = "Bar Chart for Obesity Changes", # Add a title
                    xlab = "Categories", # Label for the x-axis
                    ylab = "Values" # Label for the y-axis
)

#@@@@@@@@@@@@@@@@@@@ For Hypertension Find Number of Changes
Change_Hypertension = ifelse(Health_Data$Pre_Hypertension == "No" & Health_Data$Post_Hypertension == "Yes", "Increased",
                             ifelse(Health_Data$Pre_Hypertension == "Yes" & Health_Data$Post_Hypertension == "No", "Decreased", "No Change"))
Change_Hypertension
Count_Increased_Hyp = sum(Change_Hypertension == "Increased")
Count_Increased_Hyp
Count_Decreased_Hyp = sum(Change_Hypertension == "Decreased")

Count_Decreased_Hyp
Count_No_Change_Hyp = sum(Change_Hypertension == "No Change")
Count_No_Change_Hyp

# Create the bar chart for Change in Hypertension
Count_Data_Obe=c(0,41,249)
Label_Count_Data_Obe=c("Increased","Decreased","No Change")
Lable_Count_Data_Value_Obe=c("0","41","249")
bar_heights=barplot(Count_Data_Obe,
                    names.arg = Label_Count_Data_Obe, # Add labels to the bars
                    col = "purple", # Customize the bar color
                    main = "Bar Chart for Hypertension Changes", # Add a title
                    xlab = "Categories", # Label for the x-axis
                    ylab = "Values" # Label for the y-axis
)

# Display the updated data frame
head(health_data)

#________________________________________________________ Object 2 ______________________________________________________________
#Determine if there are any significant differences between various demographic groups in terms of health outcomes:
unique(Health_Data$Ethnicity)

#*******************************Perform Kruskal Wallis Test for Demography groups in health outcomes:
#@@@@@@@@@@@@@@@@@@@@@@ Create a data frame for Understanding testing significance
African_Data <- subset(Health_Data,Health_Data$Ethnicity == "African")
Asian_Data <- subset(Health_Data,Health_Data$Ethnicity == "Asian")
Caucasian_Data <- subset(Health_Data,Health_Data$Ethnicity == "Caucasian")
Hispanic_Data <- subset(Health_Data,Health_Data$Ethnicity == "Hispanic")

# Data frame Pre_Height data for Ethnicity
Pre_Height_African <- African_Data$Pre_Height
Pre_Height_African
Pre_Height_Asian <- Asian_Data$Pre_Height
Pre_Height_Asian

Pre_Height_Caucasian <- Caucasian_Data$Pre_Height
Pre_Height_Caucasian
Pre_Height_Hispanic <- Hispanic_Data$Pre_Height
Pre_Height_Hispanic
max_length=max(c(length(Pre_Height_African),length(Pre_Height_Asian),length(Pre_Height_Caucasian),length(Pre_Height_Hispanic)))
max_length
Pre_Height_DataFrame=data.frame(African=c(Pre_Height_African,rep(NA,max_length-length(Pre_Height_African))),Asian=c(Pre_Height_Asian,rep(NA,max_length-length(Pre_Height_Asian))),Caucasian=c(Pre_Height_Caucasian,rep(NA,max_length-length(Pre_Height_Caucasian))),Hispanic=c(Pre_Height_Hispanic,rep(NA,max_length-length(Pre_Height_Hispanic))))
Pre_Height_DataFrame

# Data frame Pre_Weight data for Ethnicity levels
Pre_Weight_African <- African_Data$Pre_Weight
Pre_Weight_African
Pre_Weight_Asian <- Asian_Data$Pre_Weight
Pre_Weight_Asian
Pre_Weight_Caucasian <- Caucasian_Data$Pre_Weight
Pre_Weight_Caucasian
Pre_Weight_Hispanic <- Hispanic_Data$Pre_Weight
Pre_Weight_Hispanic
max_length1=max(c(length(Pre_Weight_African),length(Pre_Weight_Asian),length(Pre_Weight_Caucasian),length(Pre_Weight_Hispanic)))
max_length1
Pre_Weight_DataFrame=data.frame(African=c(Pre_Weight_African,rep(NA,max_length1-length(Pre_Weight_African))),Asian=c(Pre_Weight_Asian,rep(NA,max_length1-length(Pre_Weight_Asian))),Caucasian=c(Pre_Weight_Caucasian,rep(NA,max_length1-length(Pre_Weight_Caucasian))),Hispanic=c(Pre_Height_Hispanic,rep(NA,max_length1-length(Pre_Weight_Hispanic))))
Pre_Weight_DataFrame

# Data frame Pre_BMI data for Ethnicity
Pre_BMI_African <- African_Data$Pre_BMI
Pre_BMI_African
Pre_BMI_Asian <- Asian_Data$Pre_BMI
Pre_BMI_Asian

Pre_BMI_Caucasian <- Caucasian_Data$Pre_BMI
Pre_BMI_Caucasian
Pre_BMI_Hispanic <- Hispanic_Data$Pre_BMI
Pre_BMI_Hispanic
max_length2=max(c(length(Pre_BMI_African),length(Pre_BMI_Asian),length(Pre_BMI_Caucasian),length(Pre_BMI_Hispanic)))
max_length2
Pre_BMI_DataFrame=data.frame(African=c(Pre_BMI_African,rep(NA,max_length2-length(Pre_BMI_African))),Asian=c(Pre_BMI_Asian,rep(NA,max_length2-length(Pre_BMI_Asian))),Caucasian=c(Pre_BMI_Caucasian,rep(NA,max_length2-length(Pre_BMI_Caucasian))),Hispanic=c(Pre_BMI_Hispanic,rep(NA,max_length2-length(Pre_BMI_Hispanic))))
Pre_BMI_DataFrame

# Data frame Pre_BMI data for Gender
Female_Data <- subset(Health_Data, Health_Data$Gender == "Female")
Male_Data <- subset(Health_Data, Health_Data$Gender == "Male")
Other_Data <- subset(Health_Data, Health_Data$Gender == "Other")
Pre_BMI_Female <- Female_Data$Pre_BMI
Pre_BMI_Female
Pre_BMI_Male <- Male_Data$Pre_BMI
Pre_BMI_Male
Pre_BMI_Other <- Other_Data$Pre_BMI
Pre_BMI_Other
max_length_a=max(c(length(Pre_BMI_Female),length(Pre_BMI_Male),length(Pre_BMI_Other)))
max_length_a
Pre_BMI_DataFrame_a=data.frame(Female=c(Pre_BMI_Female,rep(NA,max_length_a-length(Pre_BMI_Female))),Male=c(Pre_BMI_Male,rep(NA,max_length_a-length(Pre_BMI_Male))),Other=c(Pre_BMI_Other,rep(NA,max_length_a-length(Pre_BMI_Other))))
Pre_BMI_DataFrame_a

# Data frame Pre_Weight data for Gender
Pre_Weight_Female <- Female_Data$Pre_Weight
Pre_Weight_Female
Pre_Weight_Male <- Male_Data$Pre_Weight
Pre_Weight_Male
Pre_Weight_Other <- Other_Data$Pre_Weight
Pre_Weight_Other
max_length_b=max(c(length(Pre_Weight_Female),length(Pre_Weight_Male),length(Pre_Weight_Other)))
max_length_b
Pre_Weight_DataFrame_b=data.frame(Female=c(Pre_Weight_Female,rep(NA,max_length_b-length(Pre_Weight_Female))),Male=c(Pre_Weight_Male,rep(NA,max_length_b-length(Pre_Weight_Male))),Other=c(Pre_Weight_Other,rep(NA,max_length_b-length(Pre_Weight_Other))))
Pre_Weight_DataFrame_b

# Data frame Pre_Height data for Gender
Pre_Height_Female <- Female_Data$Pre_Height
Pre_Height_Female
Pre_Height_Male <- Male_Data$Pre_Height
Pre_Height_Male
Pre_Height_Other <- Other_Data$Pre_Height
Pre_Height_Other
max_length_c=max(c(length(Pre_Height_Female),length(Pre_Height_Male),length(Pre_Height_Other)))
max_length_c
Pre_Height_DataFrame_c=data.frame(Female=c(Pre_Height_Female,rep(NA,max_length_c-length(Pre_Height_Female))),Male=c(Pre_Height_Male,rep(NA,max_length_c-length(Pre_Height_Male))),Other=c(Pre_Height_Other,rep(NA,max_length_c-length(Pre_Height_Other))))
Pre_Height_DataFrame_c

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Testing Significance Using Non Parametric Tests
#@@@@@@@@@@@@@@@@@@@@@ Using Kruskal Wallis Test Ethnicity Vs Health Outcomes

# Testing significance difference between Ethnicity groups in Pre_BMI
Kruskal_Ethnicity_Pre_BMI=kruskal.test(Health_Data$Pre_BMI~Health_Data$Ethnicity)
Kruskal_Ethnicity_Pre_BMI

# Testing significance difference between Ethnicity groups in Post_BMI
Kruskal_Ethnicity_Post_BMI=kruskal.test(Health_Data$Post_BMI~Health_Data$Ethnicity)
Kruskal_Ethnicity_Post_BMI

# Testing significance difference between Ethnicity groups in Pre_Weight
Kruskal_Ethnicity_Pre_Weight=kruskal.test(Health_Data$Pre_Weight~Health_Data$Ethnicity)
Kruskal_Ethnicity_Pre_Weight

# Testing significance difference between Ethnicity groups in Post_Weight
Kruskal_Ethnicity_Post_Weight=kruskal.test(Health_Data$Post_Weight~Health_Data$Ethnicity)
Kruskal_Ethnicity_Post_Weight

# Testing significance difference between Ethnicity groups in Pre_Height
Kruskal_Ethnicity_Pre_Height=kruskal.test(Health_Data$Pre_Height~Health_Data$Ethnicity)
Kruskal_Ethnicity_Pre_Height

# Testing significance difference between Ethnicity groups in Post_Height
Kruskal_Ethnicity_Post_Height=kruskal.test(Health_Data$Post_Height~Health_Data$Ethnicity)
Kruskal_Ethnicity_Post_Height

#@@@@@@@@@@@@@@@@@@@@@ Using Kruskal Wallis Test Gender Vs Health Outcomes

# Testing significance difference between Gender groups in Pre_BMI
Kruskal_Gender_Pre_BMI=kruskal.test(Health_Data$Pre_BMI~Health_Data$Gender)
Kruskal_Gender_Pre_BMI

# Testing significance difference between Gender groups in Post_BMI
Kruskal_Gender_Post_BMI=kruskal.test(Health_Data$Post_BMI~Health_Data$Gender)
Kruskal_Gender_Post_BMI

# Testing significance difference between Gender group in Pre_Weight
Kruskal_Gender_Pre_Weight=kruskal.test(Health_Data$Pre_Weight~Health_Data$Gender)
Kruskal_Gender_Pre_Weight

# Testing significance difference between Gender group in Post_Weight
Kruskal_Gender_Post_Weight=kruskal.test(Health_Data$Post_Weight~Health_Data$Gender)
Kruskal_Gender_Post_Weight

# Testing significance difference between Gender groups in Pre_Height
Kruskal_Gender_Pre_Height=kruskal.test(Health_Data$Pre_Height~Health_Data$Gender)
Kruskal_Gender_Pre_Height

# Testing significance difference between Gender groups in Post_Height
Kruskal_Gender_Post_Height=kruskal.test(Health_Data$Post_Height~Health_Data$Gender)
Kruskal_Gender_Post_Height

#@@@@@@@@@@@@@@@@@@@@@ Using Kruskal Wallis Test Age Vs Health Outcomes
unique(Health_Data$Age)

# Testing significance difference between Age in Pre_Weight
Kruskal_Age_Pre_Weight=kruskal.test(Health_Data$Pre_Weight~Health_Data$Age)
Kruskal_Age_Pre_Weight

# Testing significance difference between Age in Post_Weight
Kruskal_Age_Post_Weight=kruskal.test(Health_Data$Post_Weight~Health_Data$Age)
Kruskal_Age_Post_Weight

# Testing significance difference between Age in Pre_BMI
Kruskal_Age_Pre_BMI=kruskal.test(Health_Data$Pre_BMI~Health_Data$Age)
Kruskal_Age_Pre_BMI

# Testing significance difference between Age in Post_BMI
Kruskal_Age_Post_BMI=kruskal.test(Health_Data$Post_BMI~Health_Data$Age)
Kruskal_Age_Post_BMI

# Testing significance difference between Age in Pre_Height
Kruskal_Age_Pre_Height=kruskal.test(Health_Data$Pre_Height~Health_Data$Age)
Kruskal_Age_Pre_Height

# Testing significance difference between Age in Post_Height
Kruskal_Age_Post_Height=kruskal.test(Health_Data$Post_Height~Health_Data$Age)
Kruskal_Age_Post_Height

#________________________________________________ Object 3___________________________________________________________________
#@@@@@@@@@@@@@@@@@@ Assign Categorical variables to Dummy variable @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Health_Data$Pre_Obesity=ifelse(Health_Data$Pre_Obesity == "Yes", 1, 0)
Health_Data$Post_Obesity <- ifelse(Health_Data$Post_Obesity == "Yes", 1, 0)
Health_Data$Pre_Hypertension <- ifelse(Health_Data$Pre_Hypertension == "Yes", 1, 0)
Health_Data$Post_Hypertension <- ifelse(Health_Data$Post_Hypertension == "Yes", 1, 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Pearson Correlation Test for Pre_Obesity and Health outcomes~~~~~~~~~~~~~~~~~~~~~~~
cor.test(Health_Data$Pre_Obesity,Health_Data$Pre_Weight,method="pearson")
cor.test(Health_Data$Pre_Obesity,Health_Data$Pre_Height,method="pearson")
cor.test(Health_Data$Pre_Obesity,Health_Data$Pre_BMI,method="pearson")
cor.test(Health_Data$Pre_Obesity,Health_Data$Post_Weight,method="pearson")
cor.test(Health_Data$Pre_Obesity,Health_Data$Post_Height,method="pearson")
cor.test(Health_Data$Pre_Obesity,Health_Data$Post_BMI,method="pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Pearson Correlation Test for Post_Obesity and Health outcomes~~~~~~~~~~~~~~~~~~~~~~~
cor.test(Health_Data$Post_Obesity,Health_Data$Pre_Weight,method="pearson")
cor.test(Health_Data$Post_Obesity,Health_Data$Pre_Height,method="pearson")
cor.test(Health_Data$Post_Obesity,Health_Data$Pre_BMI,method="pearson")
cor.test(Health_Data$Post_Obesity,Health_Data$Post_Weight,method="pearson")
cor.test(Health_Data$Post_Obesity,Health_Data$Post_Height,method="pearson")
cor.test(Health_Data$Post_Obesity,Health_Data$Post_BMI,method="pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Pearson Correlation Test for Pre_Hypertension and Health outcomes~~~~~~~~~~~~~~~~~~~~~~~
cor.test(Health_Data$Pre_Hypertension,Health_Data$Pre_Weight,method="pearson")
cor.test(Health_Data$Pre_Hypertension,Health_Data$Pre_Height,method="pearson")
cor.test(Health_Data$Pre_Hypertension,Health_Data$Pre_BMI,method="pearson")
cor.test(Health_Data$Pre_Hypertension,Health_Data$Post_Weight,method="pearson")
cor.test(Health_Data$Pre_Hypertension,Health_Data$Post_Height,method="pearson")
cor.test(Health_Data$Pre_Hypertension,Health_Data$Post_BMI,method="pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~ Pearson Correlation Test for Post_Hypertension and Health outcomes~~~~~~~~~~~~~~~~~~~~~~~
cor.test(Health_Data$Post_Hypertension,Health_Data$Pre_Weight,method="pearson")
cor.test(Health_Data$Post_Hypertension,Health_Data$Pre_Height,method="pearson")
cor.test(Health_Data$Post_Hypertension,Health_Data$Pre_BMI,method="pearson")
cor.test(Health_Data$Post_Hypertension,Health_Data$Post_Weight,method="pearson")
cor.test(Health_Data$Post_Hypertension,Health_Data$Post_Height,method="pearson")
cor.test(Health_Data$Post_Hypertension,Health_Data$Post_BMI,method="pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Create a correlation matrix for data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Person_DataFrame=data.frame(Health_Data$Pre_Hypertension,Health_Data$Post_Hypertension,Health_Data$Pre_Obesity,Health_Data$Post_Obesity,
                            Health_Data$Pre_Weight,Health_Data$Pre_Height,Health_Data$Pre_BMI,
                            Health_Data$Post_Weight,Health_Data$Post_Height,Health_Data$Post_BMI)
Person_DataFrame
correlation_matrix=cor(Person_DataFrame)
correlation_matrix
round(correlation_matrix,2)

#~~~~~~~~~~~~~~~~~~~~~~~~~ Create Heat map to the data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(reshape2)
heatmap_data <- melt(correlation_matrix)

# Create a heatmap plot with correlation values
ggplot(data = heatmap_data, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low = "white", high = "orange", limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Correlation Heatmap",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))