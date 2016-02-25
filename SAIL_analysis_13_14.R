#Data Exploration / Analysis
#SAIL / Math 104 Data
#Michael Chirico

# Package Setup and Convenient Functions ####
rm(list=ls(all=T))
gc()
setwd("C:\\Users\\Mike\\Desktop\\SAIL\\")
logwd<-c(".\\logs\\")
library(data.table)
library(xtable)
library(texreg)
write.packages(logwd%+%"data_exploration_13_14.txt")

pretty_chisq_p<-function(x,y,digits=2){
  round(chisq.test(table(x,y))$p.value,digits)
}

insert_nas<-function(x,pos=NULL){
  res<-numeric(length(x)+length(pos))
  res[setdiff(1:length(res),pos)][1:length(x)]<-x
  res
}

# Data Import and Setup ####
full_data<-setnames(
  fread("sail_data_math_104.csv",
        colClasses=abbr_to_colClass("ifcfcincfcfnfnfnfnifcfcfcfcfcfff",
                                    "13121411123424242412121212121996")),
  fread("sail_data_math_104_col_names.csv",header=F)$V1)

#create some new, derived variables

##Year of birth
full_data[,birth_date:=as.Date(birth_date,format="%d-%b-%y")]
full_data[,birth_year:=format(birth_date,"%Y")]

##Demographic categories
full_data[,`:=`(male=gender=="Male",white=race=="White",
                black=race=="Black",asian=race=="Asian",
                hispanic=race=="Hispanic",born93=birth_year=="1993",
                born94=birth_year=="1994",born95=birth_year=="1995",
                citizen_us=us_citizen=="US Citizen",
                citizen_perm=us_citizen=="US Permanent Resident",
                citizen_alien=us_citizen=="Non-Resident Alien")]

##Grade categories
full_data[,`:=`(grade_a_2013c=grade_2013c %in% c("A","A-","A+"),
                grade_b_2013c=grade_2013c %in% c("B","B-","B+"),
                grade_c_2013c=grade_2013c %in% c("C","C-","C+"),
                grade_d_2013c=grade_2013c %in% c("D","D+"),
                grade_f_2013c=grade_2013c=="F",
                grade_a_2014a=grade_2014a %in% c("A","A-","A+"),
                grade_b_2014a=grade_2014a %in% c("B","B-","B+"),
                grade_c_2014a=grade_2014a %in% c("C","C-","C+"),
                grade_d_2014a=grade_2014a %in% c("D","D+"),
                grade_f_2014a=grade_2014a=="F")]

##SAT total score
full_data[,sat_total:=sat_verbal+sat_math+sat_writing]
###To compare ACT & SAT, will need old 1600-based SAT score
full_data[,sat_old_total:=sat_verbal+sat_math]

##SAT<->ACT Mapping
full_data[,paste0(c("sat_","act_"),"approx_flag"):=
            list(is.na(sat_old_total),is.na(act_score))]
setkey(full_data,act_score
       )[fread("act_sat_mapping.csv"),
         sat_old_total:=ifelse(is.na(sat_old_total),i.sat_v_m,sat_old_total)]
setkey(full_data,sat_old_total
       )[setkey(fread("act_sat_mapping.csv"),sat_v_m),
         act_score:=ifelse(is.na(act_score),
                                   i.act_composite,
                                   act_score)]

## In case we ever need a plot to validate the SAT<->ACT mapping we use, which is >10 years old
# full_data[!is.na(sat_old_total)&!is.na(act_score),plot(sat_old_total,act_score)]
# fread("act_sat_mapping.csv")[,lines(sat_v_m,act_composite)]
# full_data[!is.na(sat_old_total)&!is.na(act_score),
#           median(1.*act_score),by=sat_old_total][,abline(lm(V1~sat_old_total),col="red")]

##Re-cast gross need as a string by first removing the pesky comma
full_data[,gross_need_yr_1:=ifelse(gross_need_yr_1=="",0,
                                   as.integer(gsub(",|\\$|\\s","",gross_need_yr_1)))]

##Convert grades to GPAs

setkey(full_data,grade_2014a)[fread("grade_gpa_mapping.csv"),gpa_2014a:=i.gpa]
setkey(full_data,grade_2013c)[fread("grade_gpa_mapping.csv"),gpa_2013c:=i.gpa]
setkey(full_data,grade_2013a)[fread("grade_gpa_mapping.csv"),gpa_2013a:=i.gpa]
setkey(full_data,grade_2012c)[fread("grade_gpa_mapping.csv"),gpa_2012c:=i.gpa]
setkey(full_data,recent_grade)[fread("grade_gpa_mapping.csv"),recent_gpa:=i.gpa]

##Lecture number (see "Math 104 Lectures and Recitations Spring 2014 Schedule.docx")
full_data[!section_2014a %in% c("  ","601"),
          lecture_2014a:=paste0("00",1+as.integer(substr(section_2014a,2,2)))]
full_data[!section_2013c %in% c("  ","601"),
          lecture_2013c:=paste0("00",1+as.integer(substr(section_2013c,2,2)))]
full_data[!section_2013a %in% c("  ","601"),
          lecture_2013a:=paste0("00",1+as.integer(substr(section_2013a,2,2)))]
full_data[!section_2012c %in% c("  ","601"),
          lecture_2012c:=paste0("00",1+as.integer(substr(section_2012c,2,2)))]

##SAIL lectures
####***********NEEDS TO BE CONFIRMED THAT THIS IS THE ACTUAL BREAKDOWN********************
#### (for now, we have a good idea given the section sizes)
full_data[!is.na(lecture_2013c),
          c("sail","sail_2013c"):=ifelse(lecture_2013c %in% c("002","003","006","007"),1,0)]
full_data[!is.na(lecture_2014a),
          c("sail","sail_2014a"):=ifelse(lecture_2014a %in% c("001","002"),1,0)]

##create country and then regional identifiers
###See data glossary for notes on definitions

###country identifiers
full_data[,perm_city:=gsub("\\s\\w*\\d\\w*.*","",perm_city)]
setkey(full_data,perm_city)[fread("city_country_mapping.csv"),country:=country][perm_state!="",country:="USA"]

###US Census Region vs. Rest of World (ROW)
setkey(full_data,perm_state)[fread("state_us_region_mapping.csv"),us_region:=us_region]
full_data[,`:=`(region_mid_atlantic=us_region=="Mid-Atlantic",
                region_new_england=us_region=="New England",
                region_midwest=us_region=="Midwest",
                region_south=us_region=="South",
                region_west=us_region=="West",
                region_row=us_region=="ROW")]

###By continent
setkey(full_data,country)[fread("country_continent_mapping.csv"),`:=`(continent=continent,world_region=world_region)]

# Analysis of First-Year Data ####

#Table of descriptive statistics for SAIL vs. non-SAIL classrooms
round_vars1<-c("male","white","black","asian","hispanic",
               paste0("born",93:95),
               paste0("citizen_",c("us","perm","alien")))
round_vars2<-paste0("region_",c("mid_atlantic","new_england","midwest",
                                "south","west","row"))
mean_vars<-c(paste0("sat_",c("math","verbal","writing")),"act_score","hs_gpa","enrollments")
t_vars<-setdiff(mean_vars,"enrollments")

setkey(full_data,sail_2013c,birth_year)
cat(capture.output(print(xtable(matrix(
  cbind(c(full_data[.(1),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars1],
          full_data[.(1),lapply(.SD,function(x){round(mean(x,na.rm=T),1)}),.SDcols=mean_vars],
          full_data[.(1),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars2],
          full_data[.(1),.N]),
        c(full_data[.(0),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars1],
          full_data[.(0),lapply(.SD,function(x){round(mean(x,na.rm=T),1)}),.SDcols=mean_vars],
          full_data[.(0),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars2],
          full_data[.(0),.N]),
        c(full_data[.(c(0,1)),pretty_chisq_p(gender,sail_2013c)],
          full_data[.(c(0,1)),pretty_chisq_p(race[grepl("Na",race)==F],
                                             sail_2013c[grepl("Na",race)==F])],NA,NA,NA,
          full_data[.(c(0,1),paste0(1993:1996)),pretty_chisq_p(birth_year,sail_2013c)],NA,NA,
          full_data[.(c(0,1)),pretty_chisq_p(us_citizen,sail_2013c)],NA,NA,
          full_data[,lapply(.SD[,t_vars,with=F],
                            function(x){round(t.test(x[sail_2013c==0],x[sail_2013c==1])$p.value,2)})],
          full_data[.(c(0,1)),pretty_chisq_p(enrollments,sail_2013c)],
          full_data[.(c(0,1)),pretty_chisq_p(us_region,sail_2013c)],rep(NA,6)),
        c(setkey(full_data,sail_2014a
                 )[.(1),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars1],
          full_data[.(1),lapply(.SD,function(x){round(mean(x,na.rm=T),1)}),.SDcols=mean_vars],
          full_data[.(1),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars2],
          full_data[.(1),.N]),
        c(full_data[.(0),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars1],
          full_data[.(0),lapply(.SD,function(x){round(mean(x,na.rm=T),1)}),.SDcols=mean_vars],
          full_data[.(0),lapply(.SD,function(x){pct(mean(x),1)}),.SDcols=round_vars2],
          full_data[.(0),.N]),
        c(full_data[.(c(0,1)),pretty_chisq_p(gender,sail_2014a)],
          full_data[.(c(0,1)),pretty_chisq_p(race,sail_2014a)],NA,NA,NA,
          full_data[.(c(0,1),paste0(1993:1996)),pretty_chisq_p(birth_year,sail_2014a)],NA,NA,
          full_data[.(c(0,1)),pretty_chisq_p(us_citizen,sail_2014a)],NA,NA,
          full_data[,lapply(.SD[,t_vars,with=F],
                            function(x){round(t.test(x[sail_2014a==0],x[sail_2014a==1])$p.value,2)})],
          full_data[.(c(0,1)),pretty_chisq_p(enrollments,sail_2014a)],
          full_data[.(c(0,1)),pretty_chisq_p(us_region,sail_2014a)],rep(NA,6))),
  ncol=6,dimnames=list(c("% Male","% White","% Black","% Asian","% Hispanic","% Born 1993",
                         "% Born 1994","% Born 1995","% US Citizen","% Perm. Res.",
                         "% Non-Res. Alien","SAT Math","SAT Verbal","SAT Writing","ACT",
                         "HS GPA","# Enrollments","% Mid-Atlantic","% New England","% Midwest",
                         "% South","% West","% Rest of World","# Students"),
                       c("13C SAIL","13C Non-SAIL","p diff.","14A SAIL","14A Non-SAIL","p diff"))),
  align=c("|r|rrr|rrr|"),caption=c("Background Data, SAIL vs. Non-SAIL")),
  hline.after=c(-1,0,1,5,8,11,17,23,24))),sep="\n\n")

#Table of outcome descriptives for SAIL vs. Non-SAIL classes
setkey(full_data,sail_2013c)
cat(capture.output(print(xtable(matrix(
  t(rbind(cbind(full_data[.(c(1,0)),lapply(.SD[,paste0("grade_",letters[c(1:4,6)],"_2013c"),with=F],
                                           function(x){pct(mean(x),1)}),by=sail_2013c][,!"sail_2013c",with=F],
                full_data[.(c(1,0)),.N,by=sail_2013c][,"N",with=F]),
          full_data[.(c(1,0)),.(pretty_chisq_p(grade_2013c,sail_2013c),NA,NA,NA,NA,NA)],
          cbind(setkey(full_data,sail_2014a
          )[.(c(1,0)),lapply(.SD[,paste0("grade_",letters[c(1:4,6)],"_2014a"),with=F],
                             function(x){pct(mean(x),1)}),by=sail_2014a][,!"sail_2014a",with=F],
          full_data[.(c(1,0)),.N,by=sail_2014a][,"N",with=F]),
          full_data[.(c(1,0)),.(pretty_chisq_p(grade_2013c,sail_2014a),NA,NA,NA,NA,NA)],
          use.names=F)),ncol=6,
  dimnames=list(c("Final Grade A","Final Grade B","Final Grade C",
                  "Final Grade D","Final Grade F","# Students"),
                c("13C SAIL","13C Non-SAIL","p diff","14A SAIL","14A Non-SAIL","p diff"))),
  align=c("|r|rrr|rrr|"),caption=c("Background Data, SAIL vs. Non-SAIL")),
  hline.after=c(-1,0,5,6))),sep="\n\n")

#Table of descriptives emphasizing Fall vs. Spring comparison
table1<-matrix(
  cbind(c(100*mean(full_data[sail==1,]$gender=="Male"),
          100*mean(full_data[sail==1,]$race=="White"),
          100*mean(full_data[sail==1,]$race=="Black"),
          100*mean(full_data[sail==1,]$race=="Asian"),
          100*mean(full_data[sail==1,]$race=="Hispanic"),
          100*mean(full_data[sail==1,]$birth_year=="93"),
          100*mean(full_data[sail==1,]$birth_year=="94"),
          100*mean(full_data[sail==1,]$birth_year=="95"),
          100*mean(full_data[sail==1,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail==1,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail==1,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail==1,]$sat_math,na.rm=T),
          mean(full_data[sail==1,]$sat_verbal,na.rm=T),
          mean(full_data[sail==1,]$sat_writing,na.rm=T),
          mean(full_data[sail==1,]$act_score,na.rm=T),
          mean(full_data[sail==1,]$hs_gpa,na.rm=T),
          mean(full_data[sail==1,]$enrollments),
          100*mean(full_data[sail==1,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail==1,]$us_region=="New England"),
          100*mean(full_data[sail==1,]$us_region=="Midwest"),
          100*mean(full_data[sail==1,]$us_region=="South"),
          100*mean(full_data[sail==1,]$us_region=="West"),
          100*mean(full_data[sail==1,]$us_region=="ROW"),
          nrow(full_data[sail==1,])),
        c(100*mean(full_data[sail==0,]$gender=="Male"),
          100*mean(full_data[sail==0,]$race=="White"),
          100*mean(full_data[sail==0,]$race=="Black"),
          100*mean(full_data[sail==0,]$race=="Asian"),
          100*mean(full_data[sail==0,]$race=="Hispanic"),
          100*mean(full_data[sail==0,]$birth_year=="93"),
          100*mean(full_data[sail==0,]$birth_year=="94"),
          100*mean(full_data[sail==0,]$birth_year=="95"),
          100*mean(full_data[sail==0,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail==0,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail==0,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail==0,]$sat_math,na.rm=T),
          mean(full_data[sail==0,]$sat_verbal,na.rm=T),
          mean(full_data[sail==0,]$sat_writing,na.rm=T),
          mean(full_data[sail==0,]$act_score,na.rm=T),
          mean(full_data[sail==0,]$hs_gpa,na.rm=T),
          mean(full_data[sail==0,]$enrollments),
          100*mean(full_data[sail==0,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail==0,]$us_region=="New England"),
          100*mean(full_data[sail==0,]$us_region=="Midwest"),
          100*mean(full_data[sail==0,]$us_region=="South"),
          100*mean(full_data[sail==0,]$us_region=="West"),
          100*mean(full_data[sail==0,]$us_region=="ROW"),
          nrow(full_data[sail==0,])),
        c(chisq.test(table(full_data[!is.na(sail),]$gender,
                           full_data[!is.na(sail),]$sail))$p.value,
          chisq.test(table(full_data[!is.na(sail),]$race,
                           full_data[!is.na(sail),]$sail))$p.value,NA,NA,NA,
          chisq.test(table(full_data[!is.na(sail)&birth_year>92,]$birth_year,
                           full_data[!is.na(sail)&birth_year>92,]$sail))$p.value,NA,NA,
          chisq.test(table(full_data[!is.na(sail),]$us_citizen,
                           full_data[!is.na(sail),]$sail))$p.value,NA,NA,
          t.test(full_data[sail==1,]$sat_math,full_data[sail==0,]$sat_math)$p.value,
          t.test(full_data[sail==1,]$sat_verbal,full_data[sail==0,]$sat_verbal)$p.value,
          t.test(full_data[sail==1,]$sat_writing,full_data[sail==0,]$sat_writing)$p.value,
          t.test(full_data[sail==1,]$act_score,full_data[sail==0,]$act_score)$p.value,
          t.test(full_data[sail==1,]$hs_gpa,full_data[sail==0,]$hs_gpa)$p.value,
          chisq.test(table(full_data[!is.na(sail),]$enrollments,full_data[!is.na(sail),]$sail))$p.value,
          chisq.test(table(full_data[!is.na(sail),]$us_region,full_data[!is.na(sail),]$sail))$p.value,NA,NA,NA,NA,NA,NA),
        c(100*mean(full_data[sail_2013c==1,]$gender=="Male"),100*mean(full_data[sail_2013c==1,]$race=="White"),
          100*mean(full_data[sail_2013c==1,]$race=="Black"),100*mean(full_data[sail_2013c==1,]$race=="Asian"),
          100*mean(full_data[sail_2013c==1,]$race=="Hispanic"),100*mean(full_data[sail_2013c==1,]$birth_year=="93"),
          100*mean(full_data[sail_2013c==1,]$birth_year=="94"),100*mean(full_data[sail_2013c==1,]$birth_year=="95"),
          100*mean(full_data[sail_2013c==1,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail_2013c==1,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail_2013c==1,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail_2013c==1,]$sat_math,na.rm=T),
          mean(full_data[sail_2013c==1,]$sat_verbal,na.rm=T),
          mean(full_data[sail_2013c==1,]$sat_writing,na.rm=T),
          mean(full_data[sail_2013c==1,]$act_score,na.rm=T),
          mean(full_data[sail_2013c==1,]$hs_gpa,na.rm=T),
          mean(full_data[sail_2013c==1,]$enrollments),
          100*mean(full_data[sail_2013c==1,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail_2013c==1,]$us_region=="New England"),
          100*mean(full_data[sail_2013c==1,]$us_region=="Midwest"),
          100*mean(full_data[sail_2013c==1,]$us_region=="South"),
          100*mean(full_data[sail_2013c==1,]$us_region=="West"),
          100*mean(full_data[sail_2013c==1,]$us_region=="ROW"),
          nrow(full_data[sail_2013c==1,])),
        c(100*mean(full_data[sail_2014a==1,]$gender=="Male"),100*mean(full_data[sail_2014a==1,]$race=="White"),
          100*mean(full_data[sail_2014a==1,]$race=="Black"),100*mean(full_data[sail_2014a==1,]$race=="Asian"),
          100*mean(full_data[sail_2014a==1,]$race=="Hispanic"),100*mean(full_data[sail_2014a==1,]$birth_year=="93"),
          100*mean(full_data[sail_2014a==1,]$birth_year=="94"),100*mean(full_data[sail_2014a==1,]$birth_year=="95"),
          100*mean(full_data[sail_2014a==1,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail_2014a==1,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail_2014a==1,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail_2014a==1,]$sat_math,na.rm=T),
          mean(full_data[sail_2014a==1,]$sat_verbal,na.rm=T),
          mean(full_data[sail_2014a==1,]$sat_writing,na.rm=T),
          mean(full_data[sail_2014a==1,]$act_score,na.rm=T),
          mean(full_data[sail_2014a==1,]$hs_gpa,na.rm=T),
          mean(full_data[sail_2014a==1,]$enrollments),
          100*mean(full_data[sail_2014a==1,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail_2014a==1,]$us_region=="New England"),
          100*mean(full_data[sail_2014a==1,]$us_region=="Midwest"),
          100*mean(full_data[sail_2014a==1,]$us_region=="South"),
          100*mean(full_data[sail_2014a==1,]$us_region=="West"),
          100*mean(full_data[sail_2014a==1,]$us_region=="ROW"),
          nrow(full_data[sail_2014a==1,])),
        c(chisq.test(table(full_data[sail==1,]$gender,is.na(full_data[sail==1,]$sail_2013c)))$p.value,
          chisq.test(table(full_data[sail==1,]$race,is.na(full_data[sail==1,]$sail_2013c)))$p.value,NA,NA,NA,
          chisq.test(table(full_data[sail==1&birth_year>92,]$birth_year,
                           is.na(full_data[sail==1&birth_year>92,]$sail_2013c)))$p.value,NA,NA,
          chisq.test(table(full_data[sail==1,]$us_citizen,is.na(full_data[sail==1,]$sail_2013c)))$p.value,NA,NA,
          t.test(full_data[sail_2013c==1,]$sat_math,full_data[sail_2014a==1,]$sat_math)$p.value,
          t.test(full_data[sail_2013c==1,]$sat_verbal,full_data[sail_2014a==1,]$sat_verbal)$p.value,
          t.test(full_data[sail_2013c==1,]$sat_writing,full_data[sail_2014a==1,]$sat_writing)$p.value,
          t.test(full_data[sail_2013c==1,]$act_score,full_data[sail_2014a==1,]$act_score)$p.value,
          t.test(full_data[sail_2013c==1,]$hs_gpa,full_data[sail_2014a==1,]$hs_gpa)$p.value,
          chisq.test(table(full_data[sail==1,]$enrollments,is.na(full_data[sail==1,]$sail_2013c)))$p.value,
          chisq.test(table(full_data[sail==1,]$us_region,is.na(full_data[sail==1,]$sail_2013c)))$p.value,NA,NA,NA,NA,NA,NA),
        c(100*mean(full_data[sail_2013c==0,]$gender=="Male"),100*mean(full_data[sail_2013c==0,]$race=="White"),
          100*mean(full_data[sail_2013c==0,]$race=="Black"),100*mean(full_data[sail_2013c==0,]$race=="Asian"),
          100*mean(full_data[sail_2013c==0,]$race=="Hispanic"),100*mean(full_data[sail_2013c==0,]$birth_year=="93"),
          100*mean(full_data[sail_2013c==0,]$birth_year=="94"),100*mean(full_data[sail_2013c==0,]$birth_year=="95"),
          100*mean(full_data[sail_2013c==0,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail_2013c==0,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail_2013c==0,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail_2013c==0,]$sat_math,na.rm=T),
          mean(full_data[sail_2013c==0,]$sat_verbal,na.rm=T),
          mean(full_data[sail_2013c==0,]$sat_writing,na.rm=T),
          mean(full_data[sail_2013c==0,]$act_score,na.rm=T),
          mean(full_data[sail_2013c==0,]$hs_gpa,na.rm=T),
          mean(full_data[sail_2013c==0,]$enrollments),
          100*mean(full_data[sail_2013c==0,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail_2013c==0,]$us_region=="New England"),
          100*mean(full_data[sail_2013c==0,]$us_region=="Midwest"),
          100*mean(full_data[sail_2013c==0,]$us_region=="South"),
          100*mean(full_data[sail_2013c==0,]$us_region=="West"),
          100*mean(full_data[sail_2013c==0,]$us_region=="ROW"),
          nrow(full_data[sail_2013c==0,])),
        c(100*mean(full_data[sail_2014a==0,]$gender=="Male"),100*mean(full_data[sail_2014a==0,]$race=="White"),
          100*mean(full_data[sail_2014a==0,]$race=="Black"),100*mean(full_data[sail_2014a==0,]$race=="Asian"),
          100*mean(full_data[sail_2014a==0,]$race=="Hispanic"),100*mean(full_data[sail_2014a==0,]$birth_year=="93"),
          100*mean(full_data[sail_2014a==0,]$birth_year=="94"),100*mean(full_data[sail_2014a==0,]$birth_year=="95"),
          100*mean(full_data[sail_2014a==0,]$us_citizen=="US Citizen"),
          100*mean(full_data[sail_2014a==0,]$us_citizen=="US Permanent Resident"),
          100*mean(full_data[sail_2014a==0,]$us_citizen=="Non-Resident Alien"),
          mean(full_data[sail_2014a==0,]$sat_math,na.rm=T),
          mean(full_data[sail_2014a==0,]$sat_verbal,na.rm=T),
          mean(full_data[sail_2014a==0,]$sat_writing,na.rm=T),
          mean(full_data[sail_2014a==0,]$act_score,na.rm=T),
          mean(full_data[sail_2014a==0,]$hs_gpa,na.rm=T),
          mean(full_data[sail_2014a==0,]$enrollments),
          100*mean(full_data[sail_2014a==0,]$us_region=="Mid-Atlantic"),
          100*mean(full_data[sail_2014a==0,]$us_region=="New England"),
          100*mean(full_data[sail_2014a==0,]$us_region=="Midwest"),
          100*mean(full_data[sail_2014a==0,]$us_region=="South"),
          100*mean(full_data[sail_2014a==0,]$us_region=="West"),
          100*mean(full_data[sail_2014a==0,]$us_region=="ROW"),
          nrow(full_data[sail_2014a==0,])),
        c(chisq.test(table(full_data[sail==0,]$gender,is.na(full_data[sail==0,]$sail_2013c)))$p.value,
          chisq.test(table(full_data[sail==0,]$race,is.na(full_data[sail==0,]$sail_2013c)))$p.value,NA,NA,NA,
          chisq.test(table(full_data[sail==0&birth_year>92,]$birth_year,
                           is.na(full_data[sail==0&birth_year>92,]$sail_2013c)))$p.value,NA,NA,
          chisq.test(table(full_data[sail==0,]$us_citizen,is.na(full_data[sail==0,]$sail_2013c)))$p.value,NA,NA,
          t.test(full_data[sail_2013c==0,]$sat_math,full_data[sail_2014a==0,]$sat_math)$p.value,
          t.test(full_data[sail_2013c==0,]$sat_verbal,full_data[sail_2014a==0,]$sat_verbal)$p.value,
          t.test(full_data[sail_2013c==0,]$sat_writing,full_data[sail_2014a==0,]$sat_writing)$p.value,
          t.test(full_data[sail_2013c==0,]$act_score,full_data[sail_2014a==0,]$act_score)$p.value,
          t.test(full_data[sail_2013c==0,]$hs_gpa,full_data[sail_2014a==0,]$hs_gpa)$p.value,
          chisq.test(table(full_data[sail==0,]$enrollments,is.na(full_data[sail==0,]$sail_2013c)))$p.value,
          chisq.test(table(full_data[sail==0,]$us_region,is.na(full_data[sail==0,]$sail_2013c)))$p.value,NA,NA,NA,NA,NA,NA)),
  ncol=9,dimnames=list(c("% Male","% White","% Black","% Asian","% Hispanic","% Born 1993",
                          "% Born 1994","% Born 1995","% US Citizen","% Perm. Res.",
                          "% Non-Res. Alien","SAT Math","SAT Verbal","SAT Writing","ACT",
                          "HS GPA","# Enrollments","% Mid-Atlantic","% New England","% Midwest",
                          "% South","% West","% Rest of World","# Students"),
                        c("SAIL","Non-SAIL","p diff","13C SAIL","14A SAIL","p diff.","13C Non-SAIL","14A Non-SAIL","p diff")))

print(xtable(table1,ndigits=1,align=c("|r|rrr|rrr|rrr|"),caption=c("Background Data, SAIL vs. Non-SAIL")),
      hline.after=c(-1,0,1,5,8,11,17,23,24))

table2<-matrix(
  cbind(c(100*mean(full_data[sail==1,]$recent_grade %in% c("A","A-","A+")),
          100*mean(full_data[sail==1,]$recent_grade %in% c("B","B-","B+")),
          100*mean(full_data[sail==1,]$recent_grade %in% c("C","C-","C+")),
          100*mean(full_data[sail==1,]$recent_grade %in% c("D","D+")),
          100*mean(full_data[sail==1,]$recent_grade %in% c("F")),
          nrow(full_data[sail==1,])),
        c(100*mean(full_data[sail==0,]$recent_grade %in% c("A","A-","A+")),
          100*mean(full_data[sail==0,]$recent_grade %in% c("B","B-","B+")),
          100*mean(full_data[sail==0,]$recent_grade %in% c("C","C-","C+")),
          100*mean(full_data[sail==0,]$recent_grade %in% c("D","D+")),
          100*mean(full_data[sail==0,]$recent_grade %in% c("F")),
          nrow(full_data[sail==0,])),
        c(chisq.test(table(full_data[!is.na(sail),]$recent_grade,
                           full_data[!is.na(sail),]$recent_term))$p.value,NA,NA,NA,NA,NA),
        c(100*mean(full_data[sail_2013c==1,]$grade_2013c %in% c("A","A-","A+")),
          100*mean(full_data[sail_2013c==1,]$grade_2013c %in% c("B","B-","B+")),
          100*mean(full_data[sail_2013c==1,]$grade_2013c %in% c("C","C-","C+")),
          100*mean(full_data[sail_2013c==1,]$grade_2013c %in% c("D","D+")),
          100*mean(full_data[sail_2013c==1,]$grade_2013c %in% c("F")),
          nrow(full_data[sail_2013c==1,])),
        c(100*mean(full_data[sail_2014a==1,]$grade_2014a %in% c("A","A-","A+")),
          100*mean(full_data[sail_2014a==1,]$grade_2014a %in% c("B","B-","B+")),
          100*mean(full_data[sail_2014a==1,]$grade_2014a %in% c("C","C-","C+")),
          100*mean(full_data[sail_2014a==1,]$grade_2014a %in% c("D","D+")),
          100*mean(full_data[sail_2014a==1,]$grade_2014a %in% c("F")),
          nrow(full_data[sail_2014a==1,])),
        c(chisq.test(table(full_data[sail==1,]$recent_grade,
                           full_data[sail==1,]$recent_term))$p.value,NA,NA,NA,NA,NA),
        c(100*mean(full_data[sail_2013c==0,]$grade_2013c %in% c("A","A-","A+")),
          100*mean(full_data[sail_2013c==0,]$grade_2013c %in% c("B","B-","B+")),
          100*mean(full_data[sail_2013c==0,]$grade_2013c %in% c("C","C-","C+")),
          100*mean(full_data[sail_2013c==0,]$grade_2013c %in% c("D","D+")),
          100*mean(full_data[sail_2013c==0,]$grade_2013c %in% c("F")),
          nrow(full_data[sail_2013c==0,])),
        c(100*mean(full_data[sail_2014a==0,]$grade_2014a %in% c("A","A-","A+")),
          100*mean(full_data[sail_2014a==0,]$grade_2014a %in% c("B","B-","B+")),
          100*mean(full_data[sail_2014a==0,]$grade_2014a %in% c("C","C-","C+")),
          100*mean(full_data[sail_2014a==0,]$grade_2014a %in% c("D","D+")),
          100*mean(full_data[sail_2014a==0,]$grade_2014a %in% c("F")),
          nrow(full_data[sail_2014a==0,])),
        c(chisq.test(table(full_data[sail==0,]$recent_grade,
                           full_data[sail==0,]$recent_term))$p.value,NA,NA,NA,NA,NA))
  ,ncol=9,dimnames=list(c("Final Grade A","Final Grade B","Final Grade C",
                          "Final Grade D","Final Grade F","# Students"),
                        c("SAIL","Non-SAIL","p diff","13C SAIL","14A SAIL","p diff","13C Non-SAIL","14A Non-SAIL","p diff")))

print(xtable(table2,ndigits=1,align=c("|r|rrr|rrr|rrr|"),caption=c("Background Data, SAIL vs. Non-SAIL")),
      hline.after=c(-1,0,5,6))

write.csv(rbind(table1,rep(NA,times=9),table2),file="descriptive_stats_table3-4_141012.csv",quote=F,na="")
rm(table1,table2)

#Regressions
##First: GPA vs. covariates
reg_14a<-lm(gpa_2014a~sail+gender+relevel(factor(race),ref="White")+hs_gpa+sat_math,
            data=full_data[!is.na(gpa_2014a)&!is.na(hs_gpa)
                           &!is.na(sat_math)&!is.na(sail)&grade_2014a!="",])
reg_13c<-lm(gpa_2013c~sail+gender+relevel(factor(race),ref="White")+hs_gpa+sat_math,
            data=full_data[!is.na(gpa_2013c)&!is.na(hs_gpa)
                           &!is.na(sat_math)&!is.na(sail)&grade_2013c!="",])
reg_14a_log<-glm(I(grade_2014a %in% c("A+","A","A-"))~
                   sail+gender+relevel(factor(race),ref="White")+hs_gpa+sat_math,
                 data=full_data[!is.na(gpa_2014a)&!is.na(hs_gpa)
                                &!is.na(sat_math)&!is.na(sail)&grade_2014a!="",],
                 family=binomial(link="logit"))
reg_13c_log<-glm(I(grade_2013c %in% c("A+","A","A-"))~
                   sail+gender+relevel(factor(race),ref="White")+hs_gpa+sat_math,
                 data=full_data[!is.na(gpa_2013c)&!is.na(hs_gpa)
                                &!is.na(sat_math)&!is.na(sail)&grade_2013c!="",],
                 family=binomial(link="logit"))

texreg(list(reg_13c,reg_14a,reg_13c_log,reg_14a_log),
       custom.model.names=c("GPA, 2013F","GPA, 2014S","A Grade, 2013F","A Grade, 2014S"),
       custom.coef.names=c("Intercept","SAIL","Male","Asian","Black","Hispanic","Non-Resident Alien",
                           "Race Unknown","Multiple Races","HS GPA","SAT Math"),
       include.aic=F,include.bic=F,include.loglik=F,include.deviance=F,
       groups=list("Race (vs. White)"=4:9))

#EXPLORE INTERACTION EFFECTS FURTHER
summary(lm(gpa_2014a~sail*gender+race+hs_gpa+sat_math,data=full_data[!is.na(gpa_2014a)&!is.na(hs_gpa)
                                                                      &!is.na(sat_math)&!is.na(sail)&grade_2014a!="",]))

##Second: Grade=A vs. covariates