library(gdata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
#library(taRifx)

#Ingesting file
setwd("/xxxxxxxxxx/R Scripts")
df <- read.csv(file = "./Data/Missouri_Department_of_Agriculture_-_feed_sample_testing_results.csv", header = TRUE, na.strings = c("NA", "NaN", ""))

#rename a column header
names(df)[3] <- "Type"

#make a table
FeedTbl <- tbl_df(df)

#deal with strange factor issues
FeedTbl$RetailerBusinessName <- as.factor(FeedTbl$RetailerBusinessName)
FeedTbl$BrandName <- as.factor(FeedTbl$BrandName)
FeedTbl$SampleID <- as.factor(FeedTbl$SampleID)

#pull out relevant columns
FeedTbl2 <- FeedTbl %>% 
  filter(Type == "Pet Foods") %>%
  select(TakenDate, BrandName, RetailerBusinessName, PROT_GUAR, PROT_RSLT, FAT.AH_GUAR, FAT.AH_RSLT)

FeedTbl2$PROT_GUAR <- as.numeric(as.character(FeedTbl2$PROT_GUAR))
FeedTbl2$PROT_RSLT <- as.numeric(as.character(FeedTbl2$PROT_RSLT))
FeedTbl2$ProtDiff <- FeedTbl2$PROT_GUAR - FeedTbl2$PROT_RSLT

FeedTbl2$FAT.AH_GUAR <- as.numeric(as.character(FeedTbl2$FAT.AH_GUAR))
FeedTbl2$FAT.AH_RSLT <- as.numeric(as.character(FeedTbl2$FAT.AH_RSLT))
FeedTbl2$FatDiff <- FeedTbl2$FAT.AH_GUAR - FeedTbl2$FAT.AH_RSLT

prot <- FeedTbl2 %>% 
  group_by(BrandName)


prottopten <- prot[order(abs(prot$ProtDiff),decreasing=T)[1:10],]
prottopten <- prottopten[1:10,] 

fattopten <- FeedTbl2[order(abs(FeedTbl2$FatDiff),decreasing=T)[1:10],]
fattopten <- fattopten[1:10,]
#head(prottopten)
#botfive <- prot[order(abs(prot$ProtDiff),decreasing=F)[1:5],]
#botfive <- botfive[1:4,] 
#head(botfive)
#glimpse(topfive$BrandName)

#wrap the name
prottopten$BrandName = str_wrap(prottopten$BrandName, width = 8)
fattopten$BrandName = str_wrap(fattopten$BrandName, width = 8)

#first row has out of bounds value
fattopten <- fattopten[-1,]

f <- ggplot(prottopten, aes(x = BrandName, y = ProtDiff))
f <- f + geom_bar(stat = "identity", aes(colour = BrandName)) 
f <- f + theme(legend.position="none")
f

h <- ggplot(fattopten, aes(x = BrandName, y = FatDiff))
h <- h + geom_bar(stat = "identity", aes(colour = BrandName)) 
h <- h + theme(legend.position="none")
h

protshort <- prot[1:500,]
protsum <- prot %>% 
  group_by(BrandName) %>% 
  summarise(AvgProtRslt = mean(PROT_RSLT, na.rm = TRUE), AvgProtG = mean(PROT_GUAR, na.rm=TRUE), ProtSD = sd(PROT_RSLT, na.rm = TRUE), FatSD = sd(FAT.AH_RSLT))

#plots
a <- ggplot(protsum, aes(AvgProtG, AvgProtRslt))
a + geom_point()
a + geom_text(aes(label=BrandName),hjust=0, vjust=1, nudge_x = 1,
nudge_y = 1, check_overlap = TRUE, size = 2)

#shorten the brand name and then plot FatStandardDev against ProteinStandardDev
protsum$BrandNameShort <- strtrim(protsum$BrandName, 10)
b <- ggplot(protsum, aes(FatSD, ProtSD))
b + geom_point()
b + geom_text(aes(label=BrandNameShort),hjust=0, vjust=0, nudge_x = 0,
              nudge_y = 0, check_overlap = FALSE, size = 2)

#Quick plot of number of each class of feed
FeedTbl %>% summarise(FT_classes = n_distinct(class_))
glimpse(FeedTbl$class_)
y = FeedTbl %>% count(class_)
#y
qplot(x = class_, y = n, data = y)
