

###### Conspiracy narratives and debunking about COVID-19 origination on Chinese social media: #####
                      ########### how it started and who is to blame #############

## Author: Kaiping Chen, Anfan Chen, Jingwen Zhang, Jingbo Meng, Cuihua Shen

## Date: August 11, 2020

##install packages
library(data.table)
library(lubridate)
library(ggplot2)
library(MASS)
library(grid)
library(gridExtra)
library(stargazer)


setwd("~/OneDrive - UW-Madison/WeiboCOVID19")
d <- fread("Conspiracy_COVID_20200807.csv")

################### Figure 1 ###################
conspiracy <- d[信息类型!=2] #n=1516
consp.dt <- cbind(conspiracy[, 1], conspiracy[, 43], conspiracy[, 53:60], conspiracy[, 45:51])
setnames(consp.dt, c("信息类型","中国","美国", "自然形成","人工合成","基因改造","生物武器","五G","电子烟","转基因作物"), 
         c("debunk_or_not", "China","US","Nature/unknown origin","Human-synthesis","Lab-edited","Bio weapon","5G","ECigarette","GMO"))

consp.dt[,debunk_or_not:=as.character(debunk_or_not)]
consp.dt[debunk_or_not==0,debunk_or_not:="conspiracy"]
consp.dt[debunk_or_not==1,debunk_or_not:="debunk"]

consp.dt[,debunk_or_not:=factor(debunk_or_not)]


consp.dt <- melt(consp.dt, id.vars = 1:10, variable.name = "conspiracy_type")[value > 0]
consp.dt[, value := NULL]
consp.dt[, ID := NULL]
att.dt <- consp.dt[, lapply(.SD, sum), by = c("conspiracy_type","debunk_or_not")]
# totals
tot.dt <- consp.dt[, .N, by = c("conspiracy_type","debunk_or_not")]
# merge together
att.dt <- merge(att.dt, tot.dt, by = c("conspiracy_type","debunk_or_not"))

att.dt[, Others:=塞尔维亚 + 日本 +欧洲国家 + Bill_Gates + 其他国家或主体 + 无明确]


plot_dt <- melt(att.dt, id.vars = c("conspiracy_type","debunk_or_not"), measure.vars=c("China","US","Others"), variable.name = "Responsibility", value.name ="count")

nature <- subset(plot_dt, conspiracy_type=="Nature/unknown origin")
nature[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","Nature/unknown origin","debunk")]
nature[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"Nature/unknown origin")]


Humansynthesis <- subset(plot_dt, conspiracy_type=="Human-synthesis")
Humansynthesis[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","Human-synthesis","debunk")]
Humansynthesis[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"Human-synthesis")]

Labedited <- subset(plot_dt, conspiracy_type=="Lab-edited")
Labedited[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","Lab-edited","debunk")]
Labedited[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"Lab-edited")]

Bioweapon <- subset(plot_dt, conspiracy_type=="Bio weapon")
Bioweapon[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","Bioweapon","debunk")]
Bioweapon[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"Bioweapon")]

fiveG <- subset(plot_dt, conspiracy_type=="5G")
fiveG[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","5G","debunk")]
fiveG[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"5G")]

GMO <- subset(plot_dt, conspiracy_type=="GMO")
GMO[,debunk_or_not:=ifelse(debunk_or_not=="conspiracy","GMO","debunk")]
GMO[, debunk_or_not:=as.factor(debunk_or_not)][, debunk_or_not:=relevel(debunk_or_not,"GMO")]

p1<- ggplot(nature, aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13),axis.title.y=element_text(size=13), legend.position = "none") +
  ylab("Number of Conspiracy and Debunking Posts on Weibo") +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)
p2<- ggplot(Humansynthesis, aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13),legend.position = "none", axis.title.y=element_blank()) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)
p3<- ggplot(Labedited, aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13), legend.position = "none", axis.title.y=element_blank()) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)
p4<- ggplot(Bioweapon, aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13), legend.position = "none", axis.title.y=element_blank()) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)
p5<- ggplot(fiveG , aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13), legend.position = "none", axis.title.y=element_blank()) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)
p6<- ggplot(GMO, aes(debunk_or_not, count, fill=Responsibility)) +
  geom_col() +  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=13),axis.title.y=element_blank()) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)


grid.arrange(p1, p2,p3,p4,p5,p6, ncol=6)

################### Figure 2 #####################
# text labels were added later on manually using PPT

conspiracy[, dates :=as.Date(parse_date_time(created_at,"ymd_HM"))]

ChinaCOVID19 <- fread("Newly Comfirmed Case Num.csv")
ChinaCOVID19[, dates :=as.Date(parse_date_time(Date,"m/d/y"))]
setnames(ChinaCOVID19, "Newly Confirmed Case Number", "China_Cases")

  #Evolution of Conspiracy and Debunking Posts
d2 <- conspiracy[, c("dates", "信息类型")]
setnames(d2,"信息类型", "debunk_or_not")

d2[,debunk_or_not:=as.character(debunk_or_not)]
d2[debunk_or_not==0,debunk_or_not:="Conspiracy posts"]
d2[debunk_or_not==1,debunk_or_not:="Debunking posts"]

tmp <- d2[, .(total_posts=.N), .(dates,debunk_or_not)][order(dates)]

a <- merge(tmp, ChinaCOVID19, by="dates",all.x=TRUE)

  #Evolution of Responsibility Attribution

d3 <- conspiracy[, c("ID","dates", "中国", "美国")]
setnames(d3,c("中国", "美国"), c("China","US"))

d3 <- melt(d3, id.vars = c("ID","dates"), variable.name = "responsibility_attribution")[value > 0]

d3[, value := NULL]
d3[, ID := NULL]

tmp <- d3[, .(total_posts=.N), .(dates,responsibility_attribution)][order(dates)]

b <- merge(tmp, ChinaCOVID19, by="dates",all.x=TRUE)

  # Putting a and b in one plot

setnames(a,"debunk_or_not","countryOrattitude_value")
setnames(b,"responsibility_attribution","countryOrattitude_value")

a[, country_attitude:="conspiracy vs debunking"]
b[, country_attitude:="responsbility_attribution"]

d4 <- rbind(a,b)

coeff=300
figure4 <- ggplot(d4, aes(x=dates)) +
  geom_col( aes(y=China_Cases / coeff),fill="lightgrey") + # Divide by 100 to get the same range than the total posts
  geom_line( aes(y=total_posts,col = countryOrattitude_value)) + 
  scale_y_continuous(
    name = "Total Posts",
    sec.axis = sec_axis(~.*coeff, name="Total China COVID19 Cases")
  )

figure4 + facet_grid(vars(country_attitude)) +
  #labs(colour = "countryOrattitude_value") +
  scale_color_discrete(l=50) +
  #ggtitle(label = "Evolution of Responsibilty, Conspiracy and Counteractive narrative", subtitle="with China COVID19 Cases at Background") +
  #geom_text(x=as.numeric(ymd("2020-02-02")), y=20, label="Trump Closed U.S Border") +
  #geom_text(x=as.numeric(ymd("2020-02-15")), y=100, label="Hubei Changed Counting of Diagonses\nCases Surged") +
  #geom_text(x=as.numeric(ymd("2020-03-17")), y=70, label="Trump Called China Virus\nChinese Top Officials Blamed US Soldiers for Virus\nChina Expelled US Journalists") +
  #geom_text(x=as.numeric(ymd("2020-03-24")), y=50, label="Trump Tweeted \nHe Would Stop Use Term China Virus") +
  #geom_text(x=as.numeric(ymd("2020-04-15")), y=40, label="Trump Green Card Ban") +
  #geom_text(x=as.numeric(ymd("2020-04-28")), y=20, label="US 5G Clean\nPath on Huawei") +
  geom_vline(xintercept=as.numeric(c(ymd("2020-02-15"), ymd("2020-03-15"), ymd("2020-03-24"), ymd("2020-04-15"),ymd("2020-04-29"))),
             linetype=4, colour="black") + 
  theme(panel.grid=element_blank(),legend.title=element_blank(), 
        axis.title.y=element_text(size=13), axis.text.x=element_text(size=13), axis.title.x=element_blank(),
        strip.text.y=element_text(size=13)) +
  theme(panel.background = element_rect(fill="aliceblue"))

################### Figure 4 #####################

d <- fread("Conspiracy_COVID_20200807.csv")
conspiracy <- d[信息类型!=2] #n=1516

  ## Preparation: merge variables/create new dummies

# merge several types under 阴谋论类型
conspiracy[, conspiracy_type_others:=ifelse(五G == 1|电子烟==1|转基因作物==1, 1, 0)]
# create a new variable: deliberately-made or not under 阴谋论类型. Deliberately-made means as long as any of the non自然形成 type is mentioned in a post, we coded that this post contains deliberately-made conspiracy
conspiracy[, deliberately_made:=ifelse(人工合成 == 1|基因改造==1|生物武器==1|五G==1| 转基因作物==1, 1, 0)]

# merge several types under 归责对象
conspiracy[, responsibility_other_entities:=ifelse(塞尔维亚 == 1|日本==1|欧洲国家==1|Bill_Gates==1|其他国家或主体==1, 1, 0)]

# create several new variables for 归责对象
conspiracy[, has_responsibility:=ifelse(无明确== 1, 0, 1)]

conspiracy[, china_responsibility_only:=ifelse(responsibility_other_entities == 1|美国==1|无明确==1, 0, 1)]
conspiracy[, US_responsibility_only:=ifelse(responsibility_other_entities == 1|中国==1|无明确==1, 0, 1)]

conspiracy[, responsibility_ordinal:=ifelse(china_responsibility_only == 1, "China_responsbile_only", 
                                            ifelse(US_responsibility_only==1,"US_responsbile_only", "AllOtherSituations_responsible"))]

conspiracy[, responsibility_ordinal:=as.factor(responsibility_ordinal)]
conspiracy[, responsibility_ordinal:=relevel(responsibility_ordinal,"AllOtherSituations_responsible")]

# create a new variable for 信源. "cited_source" is an ordinal variable: has source and mention scientist, has source but not mention scientist, no source
conspiracy[, cited_source:=ifelse(科学研究人员或学者==1, "cite_scientists",
                                           ifelse(无信源==1,"no_source_cite","only_cite_nonscientist_source"))]
conspiracy[,cited_source:=as.factor(cited_source)]
conspiracy[, cited_source:=relevel(cited_source,"no_source_cite")]

conspiracy[, cited_scientist:=ifelse(科学研究人员或学者==1, 1,0)]

conspiracy[, has_source:=ifelse(无信源== 1, 0, 1)]

#manually checked whether above codings are correct: write.xlsx(conspiracy, "test.xlsx")

# create participation, mobilization as dependent variables (in addition to info cascade)
conspiracy[,participation := rowSums(.SD), .SDcols=c("like_num", "repost_num","comment_num")]
conspiracy[,participation := log(participation+1)]
conspiracy[,mobilization := rowSums(.SD), .SDcols=c("@ count","# count")]
conspiracy[,mobilization :=log(mobilization+1)]

# create the time variable. Time means the difference between when the data is collected - the post created date
conspiracy[, dates :=as.Date(parse_date_time(created_at,"ymd_HM"))]
conspiracy[, collection_date:=as.Date(parse_date_time("2020-04-30","ymd"))]
conspiracy[, time_since_posted:=collection_date-dates]

# create province as a dummy for user: from Hubei or not
conspiracy[, hubei:=ifelse(province=="湖北",1,0)]

# rename some variables
setnames(conspiracy, "Emotion Polarity(0=negative,1=postive)", "emotion_polarity")
setnames(conspiracy, "emotion score", "emotion_score")
setnames(conspiracy, "Length of Text", "post_length")
setnames(conspiracy, "number of posts", "user_total_posts")
setnames(conspiracy, "Verification Status(0=ordinary user, 1= influencer, 2= organizational user)", "verification_status")

# turn some variables into factor variable and assign baseline
cols <- c("自然形成", "人工合成", "基因改造", "生物武器", "conspiracy_type_others","verification_status","vip_level")
conspiracy[,(cols):= lapply(.SD, as.factor), .SDcols = cols]

conspiracy[, vip_level:=relevel(vip_level,"未开通")] #baseline 未开通

# check the distribution of post length and fans number
#hist(conspiracy$post_length)
#hist(conspiracy$fans_num)
conspiracy[,post_length:=log(post_length)]
conspiracy[,fans_num:=log(fans_num)]


f1 <- lm (participation ~ 信息类型*cited_source + 
            信息类型 *自然形成 + 信息类型 *人工合成 + 信息类型 *基因改造 + 信息类型 *生物武器 + 信息类型 *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            信息类型 *gender + verification_status + 信息类型*fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + time_since_posted, data=conspiracy)

f2 <- lm (mobilization ~ 信息类型*cited_source + 
            信息类型 *自然形成 + 信息类型 *人工合成 + 信息类型 *基因改造 + 信息类型 *生物武器 + 信息类型 *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            信息类型*gender + verification_status + fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + time_since_posted, data=conspiracy)

f1.dt <- data.table(variable=make.names(names(f1$coefficients)), coeff=f1$coefficients,
                    low=confint(f1)[, 1], up=confint(f1)[, 2])

f1.dt <- f1.dt[variable %in% c("信息类型", "gender男", "信息类型.gender男", "信息类型.fans_num")]

f1.dt[, variable_rename := c("Debunking","Male","Debunking*Male", "Debunking*Number of Follower")]

f1.dt <- transform(f1.dt,  variable_rename = reorder(variable_rename, order(variable_rename,
                                                                            decreasing = TRUE)))

f2.dt <- data.table(variable=make.names(names(f2$coefficients)), coeff=f2$coefficients,
                    low=confint(f2)[, 1], up=confint(f2)[, 2])
f2.dt <- f2.dt[variable %in% c("信息类型", "cited_sourcecite_scientists",  "信息类型.cited_sourcecite_scientists")]

f2.dt[, variable_rename := c("Debunking","Citing Scientists","Debunking*Cite Scientists")]

f2.dt <- transform(f2.dt,  variable_rename = reorder(variable_rename, order(variable_rename,
                                                                            decreasing = TRUE)))

plot1<- ggplot(f1.dt, aes(x=variable_rename, y = coeff, ymin=low, ymax=up)) +  ylim(-1, 1) + geom_line() + geom_point(color="red") +
  geom_errorbar(width=0) + geom_hline(yintercept=0, linetype=2) + coord_flip() + theme_bw() + 
  theme(panel.grid=element_blank(), 
        axis.title.y = element_text(size = 15), axis.text.y=element_text(size=15),
        axis.title.x = element_text(size = 15), axis.text.x=element_text(size=15), plot.title = element_text(size=15)) + labs(x = NULL) + ylab("Coefficient Value") +
  ggtitle("Interaction Effects on Participation: Debunk and User Attributes")

plot2<- ggplot(f2.dt, aes(x=variable_rename, y = coeff, ymin=low, ymax=up)) + ylim(-1, 1) + geom_line() + geom_point(color="red") +
  geom_errorbar(width=0) + geom_hline(yintercept=0, linetype=2) + coord_flip() + theme_bw() + 
  theme(panel.grid=element_blank(), 
        axis.title.y = element_text(size = 15), axis.text.y=element_text(size=15),
        axis.title.x = element_text(size = 15),axis.text.x=element_text(size=15), plot.title = element_text(size=15)) + labs(x = NULL) + ylab("Coefficient Value") +
  ggtitle("Interaction Effects on Mobilization: Debunk and Cite Scientists")

grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size="first"))

#################### Appendix D. Regression Table Output for Figure 4 ###########################

m0_0 <- lm(participation ~ 信息类型 + 
             gender + verification_status + fans_num + hubei + user_total_posts +
             emotion_score + emotion_polarity + Anger + Anx + Sad +
             post_length + time_since_posted, data=conspiracy)
exp(-0.106)

m0_1 <- lm(mobilization ~ 信息类型 + 
             gender + verification_status + fans_num + hubei + user_total_posts +
             emotion_score + emotion_polarity + Anger + Anx + Sad +
             post_length + time_since_posted, data=conspiracy)

exp(0.107)

stargazer(m0_0,m0_1, style = "apsr")


#################### Appendix E. Regression Table Output for Figure 5 ############################

f1 <- lm (participation ~ 信息类型*cited_source + 
            信息类型 *自然形成 + 信息类型 *人工合成 + 信息类型 *基因改造 + 信息类型 *生物武器 + 信息类型 *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            信息类型 *gender + verification_status + 信息类型*fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + time_since_posted, data=conspiracy)

f2 <- lm (mobilization ~ 信息类型*cited_source + 
            信息类型 *自然形成 + 信息类型 *人工合成 + 信息类型 *基因改造 + 信息类型 *生物武器 + 信息类型 *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            信息类型*gender + verification_status + fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + time_since_posted, data=conspiracy)

stargazer(f1,f2, stype="apsr")

