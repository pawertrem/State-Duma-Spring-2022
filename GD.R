library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(stringr)
library(ggplot2)
library(lubridate)

#Preliminary count of the total number of votes

df <- read_excel('C:/Users/User/Desktop/GosDuma/GD.xlsx')
df15july <- read_excel('C:/Users/User/Desktop/GosDuma/GD.xlsx', sheet=2)
df<- df%>%select(absent, abstain, against, 'for')
df15july <- df15july%>%select(absent, abstain, against, 'for')
df <- df[2:1877, ]
df15july <- df15july[2:37,]
df <- rbind(df, df15july)

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.integer)
sums <- as.data.frame(colSums(df))
colnames(sums)<-'sum'
sums$names <- c('Не голосовали', 'Воздержались', 'Против', 'За')
sums<-sums[,c('names', "sum")]
write_xlsx(sums,'C:/Users/User/Desktop/GosDuma/sums.xlsx')

#Data import

df <- read_excel('C:/Users/User/Desktop/GosDuma/GD.xlsx')
deps <- read_excel('C:/Users/User/Desktop/GosDuma/deps4.xlsx', sheet = 3)
#deps <- read_excel('C:/Users/User/Desktop/GosDuma/GD.xlsx', sheet = 3)
df <- df[2:1877,2:453]
df <- as.data.frame(t(df))
df$ID = rownames(df)
dfnames <- df$ID

df15july <- read.csv('C:/Users/User/Desktop/GosDuma/list3.csv', sep = ';')
df15july <- df15july[2:37, 2:451]
df15july <- as.data.frame(t(df15july))
df15july$ID <- rownames(df15july)
df15july$ID <- gsub('X', '', df15july$ID)
typeof(df15july$ID)
#df15july$ID <- gsub('E7', '', df15july$ID) 9911280
df15names <- df15july$ID

df <- df%>%full_join(df15july, by='ID')
rownames(df) <- df$ID
df <- df%>%select(-ID)
df<-as.data.frame(t(df))
write_xlsx(df,'C:/Users/User/Desktop/GosDuma/dftotal.xlsx')

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)
df_res = data.frame()

for (i in 1:ncol(df)){
  
  df_temp = data.frame(t(table(df[,i])))
  df_temp$Var1 = names(df)[i]
  
  df_res = rbind(df_res, df_temp)
  
}

colnames(df_res)<-c('ID', 'Vote', 'Freq')
deps$ID=as.integer(deps$ID)
df_res$ID=as.integer(df_res$ID)
df<- df_res%>%left_join(deps, by='ID')
df$Фракция <- as.factor(df$Фракция)

#Timeline

dfTime <- df
dfTime$month <- month(dfTime$date)

dfTime <- dfTime%>%select(month)%>%group_by(month)%>%count()
dfTime$month <- c('Январь', 'Февраль', 'Март', 'Апрель', 'Май', 'Июнь', 'Июль')
write_xlsx(dfTime,'C:/Users/User/Desktop/GosDuma/month2.xlsx')

#Parties

parties<-df%>%select('Фракция', Vote, Freq)
colnames(parties) <- c('Party', 'Vote', 'Freq')
parties<-parties%>%group_by(Party, Vote)%>%summarise(Freq=sum(Freq))
parties <- parties%>%ungroup()%>%group_by(Party)%>%mutate(all = sum(Freq))
parties <- parties%>%mutate(Perc=round(Freq/all*100, 2))%>%select(-all, -Freq)

ER <- parties%>%filter(Party=='ЕР')%>%ungroup()%>%select(-Party, -Vote)

NOTINC <- parties%>%filter(Party=='Депутаты, не входящие во фракции')%>%ungroup()%>%select(-Party, -Vote)

KPRF <- parties%>%filter(Party=='КПРФ')%>%ungroup()%>%select(-Party, -Vote)

LDPR <- parties%>%filter(Party=='ЛДПР')%>%ungroup()%>%select(-Party, -Vote)

NovLud <- parties%>%filter(Party=='Новые люди')%>%ungroup()%>%select(-Party, -Vote)

SRZP <- parties%>%filter(Party=='СРЗП')%>%ungroup()%>%select(-Party, -Vote)

partiesfreq <- cbind(ER, KPRF, LDPR, NovLud, SRZP)
rownames(partiesfreq) <- c('Не голосовали', 'Воздержались', 'Против', 'За')
partiesfreq$Vote = rownames(partiesfreq)
colnames(partiesfreq) <- c('ЕР', 'КПРФ', 'ЛДПР', 'НЛ', 'СРЗП')

partiesnoperc <- cbind(ER, KPRF, LDPR, NovLud, SRZP)
rownames(partiesnoperc) <- c('Не голосовали', 'Воздержались', 'Против', 'За')
colnames(partiesnoperc) <- c('ЕР', 'КПРФ', 'ЛДПР', 'НЛ', 'СРЗП')

write_xlsx(partiesnoperc, 'C:/Users/User/Desktop/GosDuma/partiesperc.xlsx')
write_xlsx(partiesfreq, 'C:/Users/User/Desktop/GosDuma/partiesfreq.xlsx')

#Candidates

cand<-df%>%select('Депутат', 'Фракция', Vote, Freq)
colnames(cand) <- c('Deputy', 'Party', 'Vote', 'Freq')
cand <- cand%>%group_by(Deputy)%>%mutate(All=sum(Freq))
cand <- cand%>%mutate(Perc=round(Freq/All*100, 2))%>%select(-All)

candfor<-cand%>%filter(Vote=='for')%>%arrange(-Perc)
rangs <- c(1:nrow(candfor))
candfor<-cbind(candfor, rangs)
candfor$Deputy <- str_c(candfor$...6, candfor$Deputy, sep = '. ')
write_xlsx(candfor,'C:/Users/User/Desktop/GosDuma/candfor.xlsx')

candag<-cand%>%filter(Vote=='against')%>%arrange(-Perc)
rangs <- c(1:nrow(candag))
candag<-cbind(candag, rangs)
candag$Deputy <- str_c(candag$...6, candag$Deputy, sep = '. ')
write_xlsx(candag,'C:/Users/User/Desktop/GosDuma/candag.xlsx')

candabsent<-cand%>%filter(Vote=='absent')%>%arrange(-Perc)
candabsent <- candabsent[-1:-2,]
rangs <- c(1:nrow(candabsent))
candabsent<-cbind(candabsent, rangs)
candabsent$Deputy <- str_c(candabsent$...6, candabsent$Deputy, sep = '. ')
write_xlsx(candabsent,'C:/Users/User/Desktop/GosDuma/candabsent.xlsx')

candabstain<-cand%>%filter(Vote=='abstain')%>%arrange(-Perc)
rangs <- c(1:nrow(candabstain))
candabstain<-cbind(candabstain, rangs)
candabstain$Deputy <- str_c(candabstain$...6, candabstain$Deputy, sep = '. ')
write_xlsx(candabstain,'C:/Users/User/Desktop/GosDuma/candabstain.xlsx')

#Region (for maps)

region <- df%>%select(Vote, Freq, 'Регион')
colnames(region) <- c('Vote', 'Freq', 'Region') 
region$Region=as.factor(region$Region)
region <- region%>%filter(!str_detect(Region, ',')&!str_detect(Region, 'Все регионы Российской Федерации')&!str_detect(Region, 'Ленинградская область – Всеволожский одномандатный избирательный округ № 111')&!str_detect(Region, 'Ханты-Мансийский автономный округ - Югра (Тюменская область)'))
region <-region%>%filter(Region!='-')
region$Region <- gsub("\r", "", region$Region)
region$Region <- gsub("Чувашская Республика - Чувашия", "Чувашская Республика", region$Region)
region = region%>%group_by(Region, Vote)%>%summarise(Freq=sum(Freq))
region = region%>%ungroup()%>%group_by(Region)%>%mutate(all = sum(Freq))
region <- region%>%mutate(Perc=round(Freq/all*100, 2))%>%select(-all, -Freq)
region[276:278,1] <- "Удмуртская Республика"
region[345:350, 1] <- 'Удмуртская Республика'
region <- region[-360:-362,]
region$Region <- gsub('Удмуртская Республика (Удмуртия)', 'Удмуртская Республика', region$Region)
region$Perc=str_c(region$Perc, '%')
regionfor=region%>%filter(Vote=='for')%>%select(-Vote)
regionag=region%>%filter(Vote=='against')%>%select(-Vote)
regionabs=region%>%filter(Vote=='absent')%>%select(-Vote)
write_xlsx(regionfor,'C:/Users/User/Desktop/GosDuma/regionfor.xlsx')
write_xlsx(regionag,'C:/Users/User/Desktop/GosDuma/regionag.xlsx')
write_xlsx(regionabs,'C:/Users/User/Desktop/GosDuma/regionabs.xlsx')


#CandidatesByRegion

cand<-df%>%select('Депутат', 'Регион', Vote, Freq)
colnames(cand) <- c('Deputy', 'Region', 'Vote', 'Freq')
cand <- cand%>%group_by(Deputy)%>%mutate(All=sum(Freq))
cand <- cand%>%mutate(Perc=round(Freq/All*100, 2))%>%select(-All)
cand <- cand%>%filter(!str_detect(Region, ',')&!str_detect(Region, 'Все регионы Российской Федерации')&!str_detect(Region, 'Ленинградская область – Всеволожский одномандатный избирательный округ № 111')&!str_detect(Region, 'Ханты-Мансийский автономный округ - Югра (Тюменская область)'))
cand <-cand%>%filter(Region!='-')
cand$Region <- gsub("\r", "", cand$Region)
cand$Region <- gsub("Чувашская Республика - Чувашия", "Чувашская Республика", cand$Region)
write_xlsx(cand,'C:/Users/User/Desktop/GosDuma/candreg.xlsx')
regions <- cand%>%select(Deputy, Region)

candname <- unique(cand$Deputy)
candfor <- as.data.frame(cbind(cand$Deputy[cand$Vote=='for'], cand$Perc[cand$Vote=='for']))
candag <- as.data.frame(cbind(cand$Deputy[cand$Vote=='against'], cand$Perc[cand$Vote=='against']))
candabsent <- as.data.frame(cbind(cand$Deputy[cand$Vote=='absent'], cand$Perc[cand$Vote=='absent']))
candabstain <- as.data.frame(cbind(cand$Deputy[cand$Vote=='abstain'], cand$Perc[cand$Vote=='abstain']))

cand2 <- as.data.frame(candname)
colnames(cand2) <- 'Deputy'
cand2 <- cand2%>%left_join(candfor, by = c('Deputy' = 'V1'))
cand2 <- cand2%>%left_join(candag, by = c('Deputy' = 'V1'))
cand2 <- cand2%>%left_join(candabsent, by = c('Deputy' = 'V1'))
cand2 <- cand2%>%left_join(candabstain, by = c('Deputy' = 'V1'))
colnames(cand2) <- c('Deputy', 'За', 'Против', 'Не голосовал', 'Воздержался')
cand2 <- cand2%>%left_join(regions, by='Deputy')
cand2 <- unique(cand2)
cand2$За <- as.numeric(cand2$За)
cand2$Против <- as.numeric(cand2$Против)
cand2$`Не голосовал` <- as.numeric(cand2$`Не голосовал`)
cand2$Воздержался <- as.numeric(cand2$Воздержался)
cand2[is.na(cand2)] <- 0
cand3 <- as.data.frame(t(cand2))
write_xlsx(cand2,'C:/Users/User/Desktop/GosDuma/candreg2.xlsx')
