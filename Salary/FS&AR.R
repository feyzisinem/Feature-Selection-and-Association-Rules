
#Feature Selection (Ozellik Secimi)

boruta.salarysurvey <- Boruta(salary_survey$Salary~., data = salary_survey, doTrace = 2)  #running for about 6 minutes
boruta.salary_train<-cbind(salary_survey$Salary,df)
boruta.salary <- TentativeRoughFix(boruta.salarysurvey)
print(boruta.salarysurvey)

plot(boruta.salary, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.salary$ImpHistory),function(i)
    boruta.salary$ImpHistory[is.finite(boruta.salary$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.salary$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.salary$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(boruta.salary, withTentative = F)

salary_df <- attStats(boruta.salary)
print(salary_df)   #according to this result "Flutter, IOS, Abap, Scala, Diger" skill variables are no effect on salary

#Association Rules Analysis (Birliktelik Kurallari Analizi) 
##Skills

library(arules)
library(arulesViz)
library(htmlwidgets)
library(here)


data1 = read.transactions(file = "C:/Users/sinem.feyzi/Desktop/maas_skills_2021.txt",
                          format = c("basket"),
                          sep= ",",
                          cols = NULL,
                          rm.duplicates = TRUE,
                          encoding = "UTF-8")


dim(data1)
length(data1)
str(data1)
inspect(data1)
inspect(data1[1:10])
ItemSetList <- data1@itemInfo
ItemSetList

s=summary(data1)
s@itemSummary
s@lengths
itemFrequency(data1, type="relative")
itemFrequency(data1, type="absolute")

###Export The Rules

Rules1 <- apriori(data1 , parameter=list(supp=min_supp, 
                                         conf=min_conf , 
                                         minlen=min_lenght,
                                         target='rules'))
summary(Rules1)
inspect(Rules1[1:25])

Rules1_DF <- as(Rules1 , "data.frame")
write.csv2(Rules1_DF , file="C:/Users/sinem.feyzi/Desktop/Rules1.csv" , row.names=FALSE)


###Export Interactive HTML The Rules

html_page <- inspectDT(Rules1)

inspectDT(Rules1)     
browseURL(paste0("C:/Users/sinem.feyzi/Desktop" , "/Output/" , 
                 "Rules1.html") , browser = getOption("browser") , 
          encodeIfNeeded = FALSE)

##Benefits

data2 = read.transactions(file = "C:/Users/sinem.feyzi/Desktop/maas_benefits_2021.txt",
                          format = c("basket"),
                          sep= ",",
                          cols = NULL,
                          rm.duplicates = TRUE,
                          encoding = "UTF-8")


dim(data2)
length(data2)
str(data2)
inspect(data2)
inspect(data2[1:10])
ItemSetList2 <- data2@itemInfo
ItemSetList2

s2=summary(data2)
s2@itemSummary
s2@lengths
itemFrequency(data2, type="relative")
itemFrequency(data2, type="absolute")

###Export The Rules

Rules2 <- apriori(data2 , parameter=list(supp=min_supp, 
                                         conf=min_conf , 
                                         minlen=min_lenght,
                                         target='rules'))
summary(Rules2)
inspect(Rules2[1:25])

Rules2_DF <- as(Rules2 , "data.frame")
write.csv2(Rules2_DF , file="C:/Users/sinem.feyzi/Desktop/Rules2.csv" , row.names=FALSE)

###Export Interactive HTML The Rules

html_page2 <- inspectDT(Rules2)

inspectDT(Rules2)     
browseURL(paste0("C:/Users/sinem.feyzi/Desktop" , "/Output/" , 
                 "Rules2.html") , browser = getOption("browser") , 
          encodeIfNeeded = FALSE)












