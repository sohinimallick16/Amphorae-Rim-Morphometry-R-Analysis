# ====================================================
#               Amphorae Data Analysis
# ====================================================
# Author: Sohini Mallick
# Description: Comprehensive data cleaning, exploratory
#              analysis, and hypothesis testing on
#              Amphorae datasets.
# ====================================================


#### ABOUT THE DATASET ####
# Source: https://github.com/Mcotsar/LearningBaetica
# Associated Paper: 
# Coto-Sarmiento, M., X. Rubio-Campillo, and J. Remesal.,2018.Identifying Social Learning between Roman Amphorae Workshops through Morphometric Similarity. Journal of Archaeological Science 96 (August), 117-23. 
# https://doi.org/10.1016/j.jas.2018.06.002.
# The dataset consists of morphometric data of Dressel 20 amphorae rim sherds, dating to the Roman empire from the Baetica region (modern day Spain)
# There are three categorical values -type, excavation sites and chronology
# There are 8 quantiative values constituting various morphometric dimesnsions of the rims

rm(list = ls()) 

#### Packages ####
install.packages(c("rstatix", "car"))
library(rstatix)
library(car)

#### READING THE DATA####
data_path <- "./data/dataDressel.csv"
Data <<- read.csv(data_path, header = TRUE, stringsAsFactors = TRUE)

head(Data)
# Checking the levels in the data
levels(Data$excavation) #Consists of the name of 5 site within the region from which amphorae data has been collected
levels(Data$type) #consists of the different sub-categories of Dressel 20 amphorae found
levels(Data$chronology) #consists of 4 chronologies, representing three centuries and one intermediate phase


#### Preliminary data preparation step ####
# Since in this analysis, we are only considering the Dressel 20 type amphoare
# we remove the sherds of 'Dressel 23' amphorae present in the dataset
Dressel_drop<<-subset(Data, Data$type!="Dressel 23"& Data$type!="Dressel B" & Data$type!="Dressel G" )
head(Dressel_drop)
Data<<-droplevels(Dressel_drop)
levels(Data$type)

#Values in the original dataset not being used for analysis are removed
Data<- Data[,-c(7:13)];Data

#### FUNCTIONS ####
outlierCheck<- function(dataset, col_name){
        W<- dataset %>% 
                group_by(excavation) %>%
                identify_outliers(col_name)
        print(W)
        extreme_val<<- which(W$is.extreme==TRUE);print(extreme_val)
        all_outliers<<- which(W$is.outlier==TRUE)
        outliers_list <- list(e=extreme_val,a=all_outliers)
        return(outliers_list)

}
anova_assumption<-function(dataset,col_name,row_name){
        #checking normality assumptions
        #1. Analysing ANOVA model residuals
        #Model<<-lm(formula=col_name ~ excavation, data=dataset)
        Model<<-lm(paste(col_name, "~", row_name), data = dataset)
        res <- resid(Model);res
        
        qqnorm(res) #plotting QQ plot of residuals 
        qqline(res) 
        
        #2.Shapiro-Wilk Test
        #Ho: The data is normally distributed
        sw<-shapiro.test(res);sw$p.value
        if(sw$p.value>0.05){
                print("Data is normally distributed")
                
        }
        else{
                print("Data is not normally distributed")
                
        }
}
anova_parametric<-function(dataset,formula){
        #checking homogenity of variance assumption (Ho: There is no significant difference among variances)
        l<-leveneTest(formula, data = dataset)#levene test
        if(l$`Pr(>F)`>0.05){
                print("There is no significant difference amongst variances")
          
        } else{
                print("There is a significant difference amongst variances")
        }
        
        print("Anova F-Test")
        #Null Hyothesis (ANOVA)
        #Ho: There is a similarity in mean (Belen=Delicias=Malpica=Parlamento=Villaseca)
        #Ha: There is no similarity in mean
        fit<<-aov(formula=formula,data=dataset)
        
        FCalc<-anova(fit)$"F value";print(FCalc)
        df<-anova(fit)$"Df"
        
        Fcritical<<-qf(1-0.05, df[1], df[2]);print(Fcritical)
        if (Fcritical>FCalc) {
          print ("We fail to reject the Null Hypothesis")
          print(summary(fit))
        } else {
          print ("We reject the Null Hypothesis")
          print(summary(fit))
        }
        
}
anova_nonparametric<-function(dataset,formula){
  print("Kruskal-Willis Test")
        k<-kruskal.test(formula, data = dataset)
        print(k$p.value) 
        if(k$p.value >0.05){
                print("We fail to reject the Null Hypothesis")
         
        } else{
                print("We reject the Null Hypothesis")
                
        }
}
skewCal <<- function(x,df) {
        m<-mean(x)
        s<-sd(x)
        num_1=x[1:df]-m
        numerator=sum(num_1^3)
        denominator=((df-1)*(s^3))
        skew_val=numerator/denominator;#formula for calculating skewness
        print(skew_val)
        
        if(skew_val>-0.5 & skew_val<0.5){
                print("Data is symmetrical")
        } else if ((skew_val>-1 & skew_val< -0.5) | (skew_val>0.5 & skew_val<1)) {
                print("Data is moderately skewed")
        } else if(skew_val<-1 | skew_val>1) {
                print("Data is highly skewed")
        }
}
####EXPLORATORY DATA ANALYSIS####

### Bar graphs ###

counts <- table(Data1$type, Data1$chronology);counts
barplot(counts, main="Amphorae types found in each chronology",
        xlab="Chronological Time Periods", col=c("darkblue","red","green"),
        legend = rownames(counts), beside=TRUE)

counts1 <- table(Data$type, Data$excavation); counts1
barplot(counts1, main="Amphorae types found in each site",
       xlab="Excavation sites", col=c("darkblue","red","green"),
       legend = rownames(counts1), beside=TRUE)

write.table(counts1,file = "type_excavation_table", sep = ",", quote = FALSE, row.names = T)

####HYPOTHESIS TESTING (Chi-Square)####

### Chi-Square Analysis ###

# 1. We check for a statistical association between type and chronology,since the visualisation shows an almost direct association with the use of a 
#certain type of amphorae within a particular time period

#Ho: There is no association between Type & Chronology
#Ha: There is an association between Type & Chronology

##Contingency Table
tblTC<<-table(Data$type, Data$chronology) 
print(tblTC)

##Data Preparation
#Under chronology there is one sherd for which the chronology has not been entered into the dataset. We drop that
chronology_unknown<<-subset(Data, Data$chronology!="")
head(chronology_unknown)
Data1<<-Data
Data1<<-droplevels(chronology_unknown);Data1
levels(Data1$chronology)

newtblTC<<-table(Data1$type, Data1$chronology) #new contingency table after moving level
print(newtblTC)

#We collapse 'A' & 'A-B' into one column
beforeB<<-apply(newtblTC[,1:2], 1, FUN=sum) #collapsing categories 'A' & 'A-B' levels
print(beforeB)

slicedTableTC<<-table(Data1$type, Data1$chronology, exclude = c("A","A-B")); slicedTableTC #slicing original table
bindcolumnB<<-cbind(beforeB,slicedTableTC); bindcolumnB #binding the new column 
class(bindcolumnB) #to check if the new binded table is till of data type 'table' -> It is no longer a table 

tblTCBinded<<-as.table(bindcolumnB); tblTCBinded #coercing the two classes into a table
class(tblTCBinded)

##Calculations
summary(tblTCBinded)
ChiCalculatedTC<- chisq.test(tblTCBinded, correct = FALSE); ChiCalculatedTC$statistic
ChiCalculatedTC$observed
ChiCalculatedTC$expected #expected frequency all>1, 80%>5

ChiCriticalTC<<-qchisq(0.95, df=8)
print(ChiCriticalTC)


if (ChiCriticalTC>ChiCalculatedTC$statistic) {
        print ("We fail to reject the Null Hypothesis")
        print("There is no statistical association between the amphorae type and chronological time period")
} else {
        print ("We reject the Null Hypothesis")
        print("There is a statistical association between the amphorae type and chronological time period")
}


# 2. We check for a statistical association between excavation and type

#Ho: There is no association between Type & Excavation
#Ha: There is an association between Type & Excavation
tblET<<-table(Data$excavation, Data$type) 
print(tblET)

##Calculations
summary(tblET)
ChiCalculatedET<- chisq.test(tblET, correct = FALSE);ChiCalculatedET$statistic 
ChiCalculatedET$observed
ChiCalculatedET$expected #expected frequency all>1

#barplot(ChiCalculatedET$observed, ChiCalculatedET$expected, legend=rownames(tblETBinded), beside = TRUE)

ChiCriticalET<<-qchisq(0.95, df=12)
print(ChiCriticalET)


if (ChiCriticalET>ChiCalculatedET$statistic) {
        print ("We fail to reject the Null Hypothesis")
        print("There is no statistical association between the amphorae type and site")
} else {
        print ("We reject the Null Hypothesis")
        print("There is a statistical association between the amphorae type and site")
}




####ANOVA - DRESSEL C####
DresselC<<-subset(Data, Data$type=="Dressel C") 
head(DresselC)
levels(DresselC$type)
DresselC<<-droplevels(DresselC)
levels(DresselC$type) #creating Dressel C dataset

##1. Exterior Diameter

#Descriptive statistics
sd(DresselC$exterior_diam)
summary(DresselC$exterior_diam)
summary(DresselC, digits = 1) #summary of statistics for all Dressel C data

#Visualisation (boxplot)
boxplot(DresselC$exterior_diam~DresselC$excavation,
        main="Boxplots of external diameter for each site (Dressel C)",
        xlab="Site Names",
        ylab="Rim External Diameters",
        col="orange",
        border="brown"
)

#check for normality 
anova_assumption(DresselC,"exterior_diam","excavation")

#Anova parametric test
f<-DresselC$exterior_diam~DresselC$excavation
anova_parametric(DresselC,f)

##2. Inside Diameter

#Descriptive statistics
sd(DresselC$inside_diam)

#Visualisation (boxplot)
boxplot(DresselC$inside_diam~DresselC$excavation,
        main="Boxplots of internal diameter for each site (Dressel C)",
        xlab="Site Names",
        ylab="Rim internal Diameters",
        col="orange",
        border="brown"
)

#check for normality 
anova_assumption(DresselC,"inside_diam","excavation") #Data is not normal
#check for outliers
outlierCheck(DresselC,"inside_diam") #outliers not removed
#Anova non parametric test
f1<-DresselC$inside_diam~DresselC$excavation
anova_nonparametric(DresselC,f1)

####ANOVA - DRESSEL D####
DresselD<<-subset(Data, Data$type=="Dressel D");DresselD
head(DresselD)

levels(DresselD$type)
DresselD<<-droplevels(DresselD)
levels(DresselD$type)

 ##1. Exterior_diam

#Descriptive statistics
summary(DresselD, digits = 1) #summary of statistics for all Dressel C data
sd(DresselD$exterior_diam)

#Visualisation (boxplot)
boxplot(DresselD$exterior_diam~DresselD$excavation,
        main="Boxplots of external diameter for each site (Dressel D)",
        xlab="Site Names",
        ylab="Rim External Diameters",
        col="orange",
        border="brown"
)


#check for normality 
anova_assumption(DresselD,"exterior_diam","excavation") #Data is not normal
#checking outliers
o<-outlierCheck(DresselD,"exterior_diam")
o$e
o$a
#removing all outliers (Parlamento very skewed, delicias has enough data)
D_ext<-DresselD[-o$a,]

#Visualisation of cleaned data
boxplot(D_ext$exterior_diam~D_ext$excavation,
        main="Boxplots of external diameter for each site(Dressel D)",
        xlab="Site Names",
        ylab="Rim External Diameters(cleaned)",
        col="orange",
        border="brown"
)
#checking normality assumption a second time with 'cleaned' data
anova_assumption(D_ext,"exterior_diam","excavation") #Data still not normal
#check level of skewness
skewCal(D_ext$exterior_diam,nrow(D_ext)) 
skewCal(DresselD$exterior_diam,nrow(DresselD))
#ANOVA non parametric test (on original data)
fde<-D_ext$exterior_diam~D_ext$excavation
anova_nonparametric(D_ext,fde)

  ##2. Inside_diam

#Descriptive statistics
sd(DresselD$inside_diam)

#Visualisation (boxplot)
boxplot(DresselD$inside_diam~DresselD$excavation,
        main="Boxplots of internal diameter for each site (Dressel D)",
        xlab="Site Names",
        ylab="Rim internal Diameters",
        col="orange",
        border="brown"
)

#check for normality 
anova_assumption(DresselD,"inside_diam","excavation") #not normal
#check for outliers
o2<-outlierCheck(DresselD,"inside_diam") #no extreme values present. Outliers will not be deleted due to the small amount of observations
#removing outliers
D_ins<-DresselD[-o2$a,]
#checking normality assumption a second time with 'cleaned' data
anova_assumption(D_ins,"inside_diam","excavation") #still not normal
#checking level of skewness
nrow(D_ins)
skewCal(DresselD$inside_diam,nrow(DresselD)) 
skewCal(D_ins$inside_diam,nrow(D_ins)) 
  ## Value of skewness is all, and qq plot shows almost normal deviation. Parametric test conducted
  ## Non significant difference between cleaned and original data
#Anova parametric test
f1<-DresselD$inside_diam~DresselD$excavation
anova_nonparametric(DresselD,f1)

####ANOVA - DRESSEL E####
DresselE<<-subset(Data, Data$type=="Dressel E");DresselE
head(DresselE)

levels(DresselE$type)
DresselE<<-droplevels(DresselE)
levels(DresselE$type)

##1. Exterior_diam

#Descriptive statistics
summary(DresselE, digits = 1) #summary of statistics for all Dressel E data
sd(DresselE$exterior_diam)

#Visualisation (boxplot)
boxplot(DresselE$exterior_diam~DresselE$excavation,
        main="Boxplots of external diameter for each site (Dressel E)",
        xlab="Site Names",
        ylab="Rim External Diameters",
        col="orange",
        border="brown"
)

#check for normality 
anova_assumption(DresselE,"exterior_diam","excavation") #Data is not normal (SW)
#checking outliers (e-> extreme outlier values, a-> all outliers)
o3<-outlierCheck(DresselE,"exterior_diam")
o3$e
o3$a
#removing extreme values 
E_ext<-DresselE[-o3$a,]

#Visualisation of cleaned data
boxplot(E_ext$exterior_diam~E_ext$excavation,
        main="Boxplots of external diameter for each site(Dressel E)",
        xlab="Site Names",
        ylab="Rim External Diameters(cleaned)",
        col="orange",
        border="brown"
)
#checking normality assumption a second time with 'cleaned' data
anova_assumption(E_ext,"exterior_diam","excavation") #SW: Not normal #qq plot heavy tailed but not much deviation
#check level of skewness
skewCal(DresselE$exterior_diam,nrow(DresselE))
skewCal(E_ext$exterior_diam,nrow(E_ext)) 
#ANOVA non-parametric test (on original data)
fee<-E_ext$exterior_diam~E_ext$excavation
anova_nonparametric(DresselE,fee)

##2. Inside_diam

#Descriptive statistics
sd(DresselE$inside_diam)

#Visualisation (boxplot)
boxplot(DresselE$inside_diam ~ DresselE$excavation,
        main="Boxplots of internal diameter for each site (Dressel E)",
        xlab="Site Names",
        ylab="Rim internal Diameters",
        col="orange",
        border="brown"
)

#check for normality 
anova_assumption(DresselE,"inside_diam","excavation") #not normal
#check for outliers
o4<-outlierCheck(DresselE,"inside_diam") 
#removing outliers
E_ins<-DresselE[-o4$e,] #only extreme values removed
#Visualisation of cleaned data
boxplot(E_ins$exterior_diam~E_ins$excavation,
        main="Boxplots of external diameter for each site(Dressel E)",
        xlab="Site Names",
        ylab="Rim External Diameters(cleaned)",
        col="orange",
        border="brown"
)
#checking normality assumption a second time with 'cleaned' data
anova_assumption(E_ins,"inside_diam","excavation") #still not normal
#checking level of skewness
nrow(D_ins)
skewCal(DresselE$inside_diam,nrow(DresselE)) 
skewCal(E_ins$inside_diam,nrow(E_ins)) 
#ANOVA non-parametric test (on cleaned data)
fei<-DresselE$inside_diam~DresselE$excavation
anova_nonparametric(DresselE,fei)
