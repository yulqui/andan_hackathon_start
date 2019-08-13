library(data.table)
library(ggplot2)


hkt_codebook <- fread('хакатон/codebook.csv')
hkt_data <- fread('хакатон/hacathon.csv')
hkt_data[,.N, by='q216']
hkt_data[,.N, by='d_age']
barplot(V1 ~ ATTEND, data = sex_and_religion)
??barplot

hkt_data[,.N, by='lf_max_age']
hkt_data[,.N, by='lf_min_age']

maxmin_match_by_age <- hkt_data[,.SD,.SDcol=c('lf_max_age', 'lf_min_age'), by='d_age']

correl_max<-hkt_data[,cor.test(d_age,lf_max_age)]
correl_min<-hkt_data[,cor.test(d_age,lf_min_age)]
correl_max
correl_min

#hkt_data_age_clean <- hkt_data[,,na.rm=T]


ggplot(data=hkt_data,aes(x=d_age,y=lf_max_age)) + geom_point(size=1)+ stat_smooth(method = "lm", col = "blue") + annotate("text",x = 50,y=50,label=correl_max$estimate) + ggtitle('Max age of P by R age')  +theme(plot.title = element_text(color = 'black', size=10, hjust = 0.5)) 


ggplot(data=maxmin_match_by_age,aes(x=d_age,y=lf_max_age)) + geom_point(size=1, color="red") + stat_smooth(method = "lm", col = "black") + theme_minimal() + ggtitle("Корреляция возраста респондента с максимальным возрастом партнера") + theme(plot.title = element_text(size = 15, face = "bold")) + xlab("Возраст") + ylab("Максимальный возраст партнера")+annotate("text",x = 90,y=75,label=round(correl_max$estimate, 4)) +annotate('text', x=90, y=50, label=round(correl_max$p.value, 18)) + theme(panel.grid = element_blank())

ggplot(data=maxmin_match_by_age,aes(x=d_age,y=lf_min_age)) + geom_point(size=1, color="blue") + stat_smooth(method = "lm", col = "black") + theme_minimal() + ggtitle("Корреляция возраста респондента с минимальным возрастом партнера") + theme(plot.title = element_text(size = 15, face = "bold")) + xlab("Возраст") + ylab("Минимальный возраст партнера")+annotate("text",x = 90,y=40,label=round(correl_min$estimate, 4)) +annotate('text', x=90, y=50, label=round(correl_min$p.value, 18)) + theme(panel.grid = element_blank())

options(scipen = 999)

ggplot(data=maxmin_match_by_age, aes(d_age)) + geom_density()
ggplot(data=maxmin_match_by_age, aes(lf_max_age)) + geom_density()
ggplot(data=maxmin_match_by_age, aes(lf_min_age)) + geom_density()

qqnorm(maxmin_match_by_age$d_age)
qqnorm(maxmin_match_by_age$lf_max_age)
qqnorm(maxmin_match_by_age$lf_min_age)

maxmin_match_by_age <- maxmin_match_by_age[d_age<75]

library(nortest)
lillie.test(maxmin_match_by_age$lf_max_age)

#ggplot(data=hkt_data,aes(x=d_age,y=lf_max_age)) + geom_boxplot(outlier.color = 'blue') 
#ggplot(data=hkt_data,aes(x=d_age,y=lf_min_age), add) + geom_boxplot(outlier.color = 'red')



#3  Выберите другую бинарную переменную. Сравните, как отличается возраст респондентов в зависимости от их ответа на выбранный вами вопрос. Значимое ли это различие? Визуализируйте результат. 

age_and_arrogance <-  data.table(hkt_data$V1, hkt_data$q1099, hkt_data$d_age)
setnames(age_and_arrogance, old='V2', new='smarter_than_most')
setnames(age_and_arrogance, old='V3', new='age_distribution')
age_and_arrogance <- age_and_arrogance[!(is.na(smarter_than_most))]
age_and_arrogance <- age_and_arrogance[!(is.na(age_distribution))]
a_and_a <- age_and_arrogance[, mean(age_distribution), by=smarter_than_most]
a_and_a <- a_and_a[smarter_than_most=='Yes', smater_rus:='Да']
a_and_a <- a_and_a[smarter_than_most=='No', smater_rus:='Нет']
#a_and_a <- 
setnames(a_and_a, old='V1', new='mean')
#age_and_arrogance[, age_distribution:=factor(age_distribution)]
boxplot(age_and_arrogance$age_distribution ~ age_and_arrogance$smarter_than_most)
t.test(age_and_arrogance$age_distribution ~ age_and_arrogance$smarter_than_most)
ggplot(data=a_and_a, aes(x=smater_rus, y=mean)) + geom_bar(stat = 'identity') + coord_flip() +ggtitle("Разница между средними возрастами заносчивых мудил и скромников") + labs(x='Думают, что они масые умные', y='Средний возраст') +theme(axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank()) + geom_text(aes(label = round(a_and_a$mean,2), hjust=-0.3), fontface="bold", colour="black",size = 4, position=position_dodge(width = 1))


age_communists <- data.table(hkt_data$V1, hkt_data$d_age, hkt_data$q174)
setnames(age_communists, old=с('V2', 'V3'), new=c('age', 'commies'))
setnames(age_communists, old='V2', new='age')
setnames(age_communists, old='V3', new='commies')
age_communists <- age_communists[commies=='Good', attitude_to_communism:='positive']
age_communists <- age_communists[commies=='Bad', attitude_to_communism:='negative']
age_communists <- age_communists[!(is.na(attitude_to_communism))]
age_communists <- age_communists[attitude_to_communism=='negative', attitude_rus:='зашквар']
age_communists <- age_communists[attitude_to_communism=='positive', attitude_rus:='красавчики']
age_communists <- age_communists[!(is.na(age))]
age_com <- age_communists[, mean(age_communists$age), by=attitude_rus]
ggplot(data=age_com, aes(x=attitude_rus, y=V1)) + geom_bar(stat = 'identity') + coord_flip() +ggtitle("Соотношение возраста и любви к коммунизму") + labs(x='Эти проклятые коммуняки', y='Средний возраст') +theme(axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank()) + geom_text(aes(label = round(age_com$V1,2), hjust=-0.3), fontface="bold", colour="black",size = 4, position=position_dodge(width = 1))


age_marriage <- data.table(hkt_data$V1, hkt_data$d_age, hkt_data$q71)
#setnames(age_communists, old=с('V2', 'V3'), new=c('age', 'commies'))
setnames(age_marriage, old='V2', new='age')
setnames(age_marriage, old='V3', new='marriage')
age_marriage <- age_marriage[marriage=='Yes', marriage_att:='positive']
age_marriage <- age_marriage[marriage=='No', marriage_att:='negative']
age_marriage <- age_marriage[!(is.na(marriage_att))]
age_marriage <- age_marriage[!(is.na(age))]
# age_communists <- age_communists[attitude_to_communism=='negative', attitude_rus:='зашквар']
# age_communists <- age_communists[attitude_to_communism=='positive', attitude_rus:='красавчики']
age_marriage <- age_marriage[!(is.na(age))]
age_marriage2 <- age_marriage[, mean(age_marriage$age), by=marriage_att]
ggplot(data=age_marriage2, aes(x=marriage_att, y=V1)) + geom_bar(stat = 'identity') + coord_flip() +ggtitle("Соотношение возраста и отношения к interracial marriage") + labs(x='Эти кровосмесители', y='Средний возраст') +theme(axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank()) + geom_text(aes(label = round(age_com$V1,2), hjust=-0.3), fontface="bold", colour="black",size = 4, position=position_dodge(width = 1))
