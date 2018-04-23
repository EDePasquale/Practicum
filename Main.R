library(dplyr)
library(Matrix) #need for the sparse matrix

#read in all data tables
location="/Users/car6wn/Documents/Classes/Spring_2018/Practicum/FromDave/"
cohort_all_diagnosis=read.table(paste0(location, "cohort_all_diagnosis.txt"), sep="\t", header=T, quote="", colClasses=c(RANDOM_PAT_ID="character", RANDOM_PAT_ENC_CSN_ID="character", AGE="numeric", START_DATE="factor", NAME="character", CODE="character", DX_TYPE="character"))
#cohort_all_encounters=read.table(paste0(location, "cohort_all_encounters.rpt"), sep="\t", header=T, nrows=100)
#cohort_all_lab_results=read.table(paste0(location, "cohort_all_lab_results.rpt"), sep="\t", fill=T, header=T, nrows=10)
cohort_all_social=read.table(paste0(location, "cohort_all_social.rpt"), sep="\t", quote="", header=T, colClasses=c(RANDOM_PAT_ID="character", RANDOM_PAT_ENC_CSN_ID="character", AGE="numeric", TOBACCO_USER="character", TOBACCO_PAK_PER_DY="numeric", TOBACCO_USED_YEARS="numeric", SMOKING_QUIT_DATE="factor", CIGARETTES_YN="factor", PIPES_YN="factor", CIGARS_YN="factor", SNUFF_YN="factor", CHEW_YN="factor", SMOKELESS_TOB_USE="character", SMOKING_TOB_USE="character", SMOKELESS_QUIT_DATE="factor"))
cohort_diagnosis=read.table(paste0(location, "cohort_diagnosis.txt"), sep="\t", quote="", header=T, colClasses=c(RANDOM_PAT_ID="character", RANDOM_PAT_ENC_CSN_ID="character", AGE="numeric", START_DATE="factor", NAME="character", CODE="character", DX_TYPE="character"))
#patient_encounters=read.table(paste0(location, "patient_encounters.rpt"), sep="\t", fill=T, header=T)
patient_demos=read.table(paste0(location, "Ericas_patients_table.txt"), sep="\t", quote="", header=T, colClasses=c(RANDOM_PAT_ID="character", GENDER="character", ETHNIC_GROUP="character", LINE="numeric", PATIENT_RACE="character"))

#save list of all patients
#write.table(as.data.frame(unique(cohort_diagnosis$RANDOM_PAT_ID)), paste0(location, "Ericas_patients.csv"), sep=",", row.names = F, col.names=F)

#cut -f 5 cohort_all_lab_results.txt | sort -u  > unique_labs.txt
cohort_eosinophil=read.table(paste0(location, "cohort_eosinophil.txt"), sep="\t", quote="", header=T, colClasses=c(RANDOM_PAT_ID="character", RANDOM_PAT_ENC_CSN_ID="character", RANDOM_ORDER_PROC_ID="character", AGE="numeric", PROC_NAME="character", PROC_ID="numeric", PROC_CODE="character", ORDERING_DATE="character", RESULT_TIME="character", ORDER_DESCRIPTION="character", RESULT_LINE_NUMBER="numeric", COMPONENT_ID="numeric", COMPONENT_NAME="character", COMPONENT_ABBREV="factor", RESULT_VALUE="numeric", LAB_STATUS="character", RESULT_STATUS="character"))

#####################
#                   #
# Cohort Definition #
#                   #
#####################

#exclusion criteria (criteria from Miravitlles et al.)
unique_codes=as.data.frame(unique(cohort_all_diagnosis$CODE))
I21_410_patients=unique(cohort_all_diagnosis[grep("I21|410", cohort_all_diagnosis$CODE),1]) #Acute myocardial infarction (4740)
I20_413_patients=unique(cohort_all_diagnosis[grep("I20|413", cohort_all_diagnosis$CODE),1]) #Angina pectoris (2139)
I50_428_patients=unique(cohort_all_diagnosis[grep("I50|428", cohort_all_diagnosis$CODE),1]) #Heart failure (10002)
CXX_10_patients=unique(cohort_all_diagnosis[grep(paste0("C", 00:80,collapse="|"), cohort_all_diagnosis$CODE),1]) #malignant neoplasms C00-C80 (5564)
CXX_9_patients=unique(cohort_all_diagnosis[grep(paste0(140:239,collapse="|"), cohort_all_diagnosis$CODE),1]) #malignant neoplasms 140-239 (28979)
K74_571_patients=unique(cohort_all_diagnosis[grep("K74|571", cohort_all_diagnosis$CODE),1]) #Fibrosis and cirrhosis of liver (3913)
N18_585_patients=unique(cohort_all_diagnosis[grep("N18|585", cohort_all_diagnosis$CODE),1]) #Chronic kidney disease (7734)
M0X_714_patients=unique(cohort_all_diagnosis[grep("M05|M06|714", cohort_all_diagnosis$CODE),1]) #Rheumatoid Arthritis (2018)
#"any systemic inflammatory disease" - please help me define 
#connective tissue disorder, infection, vasculitis, pneumonia

eosinophil_patients=unique(cohort_eosinophil$RANDOM_PAT_ID)
non_eosinophil_patients=setdiff(cohort_diagnosis$RANDOM_PAT_ID, eosinophil_patients)
excluded_patients=Reduce(union, list(I21_410_patients, I20_413_patients, I50_428_patients, CXX_10_patients, CXX_9_patients, K74_571_patients, N18_585_patients, M0X_714_patients, non_eosinophil_patients)) #29415 patients
cohort_diagnosis=cohort_diagnosis[!(cohort_diagnosis$RANDOM_PAT_ID %in% excluded_patients),]

#define the asthma cohort based on ICD codes
asthma_cohort1=cohort_diagnosis[grep("J45",cohort_diagnosis[,6]),] #19389 rows
asthma_cohort2=cohort_diagnosis[grep("493",cohort_diagnosis[,6]),] #27353 rows
length(intersect(asthma_cohort1$RANDOM_PAT_ID, asthma_cohort2$RANDOM_PAT_ID)) #2705 overlapping patients between the diagnosis criteria
length(union(asthma_cohort1$RANDOM_PAT_ID, asthma_cohort2$RANDOM_PAT_ID)) #10387 union
asthma_cohort_all=rbind(asthma_cohort1, asthma_cohort2)
asthma_patients_list=union(asthma_cohort1$RANDOM_PAT_ID, asthma_cohort2$RANDOM_PAT_ID)

#define the prelimenary copd cohort based on containing FEV1/FVC
test=cohort_diagnosis[which(cohort_diagnosis$VALUE != ""),] #3503 have a FEV1/FVC value

#examine the FEV1/FVC values for pre/post tests
single_encounters=as.data.frame(matrix(nrow=0, ncol=ncol(test))) #3142 encounters
colnames(single_encounters)=colnames(test)
multiple_encounters=as.data.frame(matrix(nrow=0, ncol=ncol(test))) #361 encounters
colnames(multiple_encounters)=colnames(test)
unique_pats=unique(test$RANDOM_PAT_ID)
for(pat in 1:length(unique_pats)){
  encounters=test[test$RANDOM_PAT_ID == unique_pats[pat],]
  num_encounters=nrow(encounters) #if encounters more than 1 then it MAY be double test
  dates=length(unique(encounters$START_DATE))
  if(num_encounters == dates){
    single_encounters=rbind(single_encounters, encounters)
  }else{
    multiple_encounters=rbind(multiple_encounters, encounters)
  }
}
length(unique(multiple_encounters$RANDOM_PAT_ID))# 60 (number of patients that have had a pre/post test)
test$VALUE=as.numeric(as.character(test$VALUE))
test2=test[which(test$VALUE<70),] #1340 have a value less than 70
#copd_patients_list=unique(test2$RANDOM_PAT_ID)
alt_copd=cohort_diagnosis[grep("J44",cohort_diagnosis[,6]),] #4309 rows
alt_copd2=cohort_diagnosis[grep("496",cohort_diagnosis[,6]),] #6370 rows
alt_copd3=cohort_diagnosis[grep("491",cohort_diagnosis[,6]),] #1650 rows
all3=Reduce(union, list(alt_copd$RANDOM_PAT_ID, alt_copd2$RANDOM_PAT_ID, alt_copd3$RANDOM_PAT_ID)) #3018 with all 3 diagnostic criteria
copd_patients_list=all3

#get the list of patients for each of the 3 groups
aco_patients_list=intersect(asthma_patients_list, copd_patients_list) #789 patients in overlap
ex_asthma_patients_list=setdiff(asthma_patients_list, aco_patients_list) #9598 patients in exlusively asthma
ex_copd_patients_list=setdiff(copd_patients_list, aco_patients_list) #2229 patients in exclusively copd

#exclude patients with COPD diagnosis from asthma only group (explore alternate COPD diagnosis (ICD codes))
alt_diagnosis_overlap=intersect(all3, copd_patients_list) #3018 overlapping patients with FEV1 criteria defined patients
ex_asthma_patients_list2=setdiff(ex_asthma_patients_list, all3) #9598 patients in exlusively asthma

#save as data frames
aco_patients_list_df=as.data.frame(aco_patients_list)
ex_asthma_patients_list_df=as.data.frame(ex_asthma_patients_list2)
ex_copd_patients_list_df=as.data.frame(ex_copd_patients_list)

#write files
#write.table(aco_patients_list_df, paste0(location, "aco_patients_list_df2.txt"), sep="\t", col.names=F, row.names = F)
#write.table(ex_asthma_patients_list_df, paste0(location, "ex_asthma_patients_list_df2.txt"), sep="\t", col.names=F, row.names = F)
#write.table(ex_copd_patients_list_df, paste0(location, "ex_copd_patients_list_df2.txt"), sep="\t", col.names=F, row.names = F)

#make data frames for later use
asthma_patients=cohort_diagnosis[cohort_diagnosis$RANDOM_PAT_ID %in% ex_asthma_patients_list2,] #9598 unique pats
copd_patients=cohort_diagnosis[cohort_diagnosis$RANDOM_PAT_ID %in% ex_copd_patients_list,] #2229 unique pats
aco_patients=cohort_diagnosis[cohort_diagnosis$RANDOM_PAT_ID %in% aco_patients_list,] #789 unique pats

################
#              #
#    Tables    #
#              #
################

#
#table 1
#

table_1=as.data.frame(matrix(nrow=12, ncol=5))
colnames(table_1)=c("Category","Asthma", "ACO", "COPD", "P-value")

#age
table_1[1,1]="Age (years)"

#asthma
unique_pats=unique(asthma_patients$RANDOM_PAT_ID)
asthma_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=asthma_patients[asthma_patients$RANDOM_PAT_ID == unique_pats[pat],]
  asthma_med_age_list[pat,1]=median(encounters$AGE)
}
table_1[1,2]=paste0(median(asthma_med_age_list[,1]), " (", round(sd(asthma_med_age_list[,1]),1), ")")

#aco
unique_pats=unique(aco_patients$RANDOM_PAT_ID)
aco_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=aco_patients[aco_patients$RANDOM_PAT_ID == unique_pats[pat],]
  aco_med_age_list[pat,1]=median(encounters$AGE)
}
table_1[1,3]=paste0(median(aco_med_age_list[,1]), " (", round(sd(aco_med_age_list[,1]),1), ")")

#copd
unique_pats=unique(copd_patients$RANDOM_PAT_ID)
copd_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=copd_patients[copd_patients$RANDOM_PAT_ID == unique_pats[pat],]
  copd_med_age_list[pat,1]=median(encounters$AGE)
}
table_1[1,4]=paste0(median(copd_med_age_list[,1]), " (", round(sd(copd_med_age_list[,1]),1), ")")

#anova
a=cbind(asthma_med_age_list, rep(1, nrow(asthma_med_age_list)))
colnames(a)=c("ages", "groups")
b=cbind(aco_med_age_list, rep(2, nrow(aco_med_age_list)))
colnames(b)=c("ages", "groups")
c=cbind(copd_med_age_list, rep(3, nrow(copd_med_age_list)))
colnames(c)=c("ages", "groups")
age_anova_table=rbind(a,b,c)
age_anova=aov(ages ~ groups, age_anova_table)
k=summary(age_anova) #significantly different ages
if(k[[1]][["Pr(>F)"]][[1]] < 0.05){
  table_1[1,5]="p<0.05"
}else{
  table_1[1,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#sex
patient_demos=patient_demos[!(patient_demos$RANDOM_PAT_ID %in% excluded_patients),]
table_1[2,1]="Sex (% Male)"

#asthma
unique_pats=unique(asthma_patients$RANDOM_PAT_ID)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% ex_asthma_patients_list2,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_1[2,2]=round((length(which(asthma_patients_demos$GENDER == "Male"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_patients$RANDOM_PAT_ID)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_patients_list,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_1[2,3]=round((length(which(aco_patients_demos$GENDER == "Male"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_patients$RANDOM_PAT_ID)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_patients_list,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_1[2,4]=round((length(which(copd_patients_demos$GENDER == "Male"))/nrow(copd_patients_demos))*100,0)

#chi-sq
a=cbind(asthma_patients_demos$GENDER, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("gender", "groups")
b=cbind(aco_patients_demos$GENDER, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("gender", "groups")
c=cbind(copd_patients_demos$GENDER, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("gender", "groups")
gender_chi_table=rbind(a,b,c)
gender_chi_table=as.data.frame(gender_chi_table)
#table(gender_chi_table$gender, gender_chi_table$groups)
gender_chi=chisq.test(gender_chi_table$gender, gender_chi_table$groups)
if(gender_chi$p.value < 0.05){
  table_1[2,5]="p<0.05"
}else{
  table_1[2,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#race
table_1[3,1]="Race (%)"
table_1[4,1]="White or Caucasian"
table_1[5,1]="Black or African American"
table_1[6,1]="Other"


#asthma
unique_pats=unique(asthma_patients$RANDOM_PAT_ID)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% ex_asthma_patients_list2,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_1[4,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(asthma_patients_demos))*100,0)
table_1[5,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(asthma_patients_demos))*100,0)
table_1[6,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Other" | 
                                   asthma_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   asthma_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   asthma_patients_demos$PATIENT_RACE == "Asian" | 
                                   asthma_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   asthma_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_patients$RANDOM_PAT_ID)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_patients_list,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_1[4,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(aco_patients_demos))*100,0)
table_1[5,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(aco_patients_demos))*100,0)
table_1[6,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Other" | 
                                   aco_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   aco_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   aco_patients_demos$PATIENT_RACE == "Asian" | 
                                   aco_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   aco_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_patients$RANDOM_PAT_ID)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_patients_list,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_1[4,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(copd_patients_demos))*100,0)
table_1[5,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(copd_patients_demos))*100,0)
table_1[6,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Other" | 
                                   copd_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   copd_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   copd_patients_demos$PATIENT_RACE == "Asian" | 
                                   copd_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   copd_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(copd_patients_demos))*100,0)

#chi-sq
asthma_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", asthma_patients_demos$PATIENT_RACE)
aco_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", aco_patients_demos$PATIENT_RACE)
copd_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", copd_patients_demos$PATIENT_RACE)
a=cbind(asthma_patients_demos$PATIENT_RACE, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("race", "groups")
b=cbind(aco_patients_demos$PATIENT_RACE, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("race", "groups")
c=cbind(copd_patients_demos$PATIENT_RACE, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("race", "groups")
race_chi_table=rbind(a,b,c)
race_chi_table=as.data.frame(race_chi_table)
#table(race_chi_table$race, race_chi_table$groups)
race_chi=chisq.test(race_chi_table$race, race_chi_table$groups)
if(race_chi$p.value < 0.05){
  table_1[3,5]="p<0.05"
}else{
  table_1[3,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}  

#smoking
table_1[7,1]="Smoking (%)"
table_1[8,1]="Never"
table_1[9,1]="Yes"
table_1[10,1]="Quit"

#asthma
asthma_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% ex_asthma_patients_list2,]
asthma_social=asthma_social[!(is.na(asthma_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(asthma_social$RANDOM_PAT_ID)
asthma_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=asthma_social[asthma_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    asthma_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    asthma_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_1[8,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_1[9,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_1[10,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)

#aco
aco_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% aco_patients_list,]
aco_social=aco_social[!(is.na(aco_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(aco_social$RANDOM_PAT_ID)
aco_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=aco_social[aco_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    aco_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    aco_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_1[8,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_1[9,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_1[10,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)

#copd
copd_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% ex_copd_patients_list,]
copd_social=copd_social[!(is.na(copd_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(copd_social$RANDOM_PAT_ID)
copd_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=copd_social[copd_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    copd_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    copd_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_1[8,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_1[9,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_1[10,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)

#chi-sq
a=cbind(asthma_mode_TOBACCO_USER_list, rep(1, nrow(asthma_mode_TOBACCO_USER_list)))
colnames(a)=c("tobacco_user", "groups")
b=cbind(aco_mode_TOBACCO_USER_list, rep(2, nrow(aco_mode_TOBACCO_USER_list)))
colnames(b)=c("tobacco_user", "groups")
c=cbind(copd_mode_TOBACCO_USER_list, rep(3, nrow(copd_mode_TOBACCO_USER_list)))
colnames(c)=c("tobacco_user", "groups")
tobacco_user_chi_table=rbind(a,b,c)
tobacco_user_chi_table=as.data.frame(tobacco_user_chi_table)
#table(tobacco_user_chi_table$race, tobacco_user_chi_table$groups)
tobacco_user_chi=chisq.test(tobacco_user_chi_table$tobacco_user, tobacco_user_chi_table$groups)
if(tobacco_user_chi$p.value < 0.05){
  table_1[7,5]="p<0.05"
}else{
  table_1[7,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#write.table(table_1, paste0(location, "table_1_ICDdef.txt"), sep="\t", row.names = F)
##################


#################
#               #
#    Figures    #
#               #
#################

#
#Figure 1 : Barplots by eopsinophil level by condition
#

#The serum reference ranges of eosinophils are as follows: [1]

#Eosinophils blood (%): 0.0-6.0 (This range may vary slightly in different laboratories.)
#Eosinophil blood count (absolute): 30-350. 

#The percentage of eosinophils is multiplied by the white blood cell count to give the absolute eosinophil count. 
#This range may vary slightly in different laboratories.


asthma_eos=cohort_eosinophil[cohort_eosinophil$RANDOM_PAT_ID %in% ex_asthma_patients_list2,]
aco_eos=cohort_eosinophil[cohort_eosinophil$RANDOM_PAT_ID %in% aco_patients_list,]
copd_eos=cohort_eosinophil[cohort_eosinophil$RANDOM_PAT_ID %in% ex_copd_patients_list,]

asthma_eos_PCT=asthma_eos[which(asthma_eos$COMPONENT_ABBREV == "EOS PCT"),]
asthma_eos_ABS=asthma_eos[which(asthma_eos$COMPONENT_ABBREV == "EOS ABS"),]
aco_eos_PCT=aco_eos[which(aco_eos$COMPONENT_ABBREV == "EOS PCT"),]
aco_eos_ABS=aco_eos[which(aco_eos$COMPONENT_ABBREV == "EOS ABS"),]
copd_eos_PCT=copd_eos[which(copd_eos$COMPONENT_ABBREV == "EOS PCT"),]
copd_eos_ABS=copd_eos[which(copd_eos$COMPONENT_ABBREV == "EOS ABS"),]


unique_pats=unique(asthma_eos_ABS$RANDOM_PAT_ID)
asthma_eos_ABS_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  labs=asthma_eos_ABS[asthma_eos_ABS$RANDOM_PAT_ID == unique_pats[pat],]
  row.names(asthma_eos_ABS_list)[pat]=unique_pats[pat]
  asthma_eos_ABS_list[pat,1]=mean(labs$RESULT_VALUE) #TODO: try this with max
}
asthma_eos_ABS_list=subset(asthma_eos_ABS_list, V1 <= 2000)
#max average value is 1954

unique_pats=unique(aco_eos_ABS$RANDOM_PAT_ID)
aco_eos_ABS_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  labs=aco_eos_ABS[aco_eos_ABS$RANDOM_PAT_ID == unique_pats[pat],]
  row.names(aco_eos_ABS_list)[pat]=unique_pats[pat]
  aco_eos_ABS_list[pat,1]=mean(labs$RESULT_VALUE)
}
aco_eos_ABS_list=subset(aco_eos_ABS_list, V1 <= 2000)
#max average value is 1777.188

unique_pats=unique(copd_eos_ABS$RANDOM_PAT_ID)
copd_eos_ABS_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  labs=copd_eos_ABS[copd_eos_ABS$RANDOM_PAT_ID == unique_pats[pat],]
  row.names(copd_eos_ABS_list)[pat]=unique_pats[pat]
  copd_eos_ABS_list[pat,1]=mean(labs$RESULT_VALUE)
}
copd_eos_ABS_list=subset(copd_eos_ABS_list, V1 <= 2000)
#max average value is 1797

boxplot(asthma_eos_ABS_list[,1], aco_eos_ABS_list[,1], copd_eos_ABS_list[,1], names=c("Asthma", "ACO", "COPD"), ylab="Absolute Eosinophil Count", col=c("Red", "Cyan", "Green"))

asthma_eos_ABS_L150=row.names(subset(asthma_eos_ABS_list, V1 < 150)) #5169
asthma_eos_ABS_150_199=row.names(subset(asthma_eos_ABS_list, (V1 >= 150 & V1 <=199))) #1269
asthma_eos_ABS_200_299=row.names(subset(asthma_eos_ABS_list, (V1 >= 200 & V1 <=299))) #1550
asthma_eos_ABS_300_399=row.names(subset(asthma_eos_ABS_list, (V1 >= 300 & V1 <=399))) #705
asthma_eos_ABS_G400=row.names(subset(asthma_eos_ABS_list, V1 >= 400)) #889

aco_eos_ABS_L150=row.names(subset(aco_eos_ABS_list, V1 < 150)) #400
aco_eos_ABS_150_199=row.names(subset(aco_eos_ABS_list, (V1 >= 150 & V1 <=199))) #112
aco_eos_ABS_200_299=row.names(subset(aco_eos_ABS_list, (V1 >= 200 & V1 <=299))) #130
aco_eos_ABS_300_399=row.names(subset(aco_eos_ABS_list, (V1 >= 300 & V1 <=399))) #59
aco_eos_ABS_G400=row.names(subset(aco_eos_ABS_list, V1 >= 400)) #83

copd_eos_ABS_L150=row.names(subset(copd_eos_ABS_list, V1 < 150)) #1238
copd_eos_ABS_150_199=row.names(subset(copd_eos_ABS_list, (V1 >= 150 & V1 <=199))) #280
copd_eos_ABS_200_299=row.names(subset(copd_eos_ABS_list, (V1 >= 200 & V1 <=299))) #370
copd_eos_ABS_300_399=row.names(subset(copd_eos_ABS_list, (V1 >= 300 & V1 <=399))) #159
copd_eos_ABS_G400=row.names(subset(copd_eos_ABS_list, V1 >= 400)) #178

par(mfrow=c(1,3))
par(mar=c(5,8,5,2))
barplot(c(length(asthma_eos_ABS_G400), length(asthma_eos_ABS_300_399), length(asthma_eos_ABS_200_299), length(asthma_eos_ABS_150_199), length(asthma_eos_ABS_L150)), names.arg = c(">=400 cells/uL", "300-399 cells/uL", "200-299 cells/uL", "150-199 cells/uL", "<150 cells/uL"), xlab="Number of Patients", main = "Asthma", col="Red", horiz=T, las=2)
barplot(c(length(aco_eos_ABS_G400), length(aco_eos_ABS_300_399), length(aco_eos_ABS_200_299), length(aco_eos_ABS_150_199), length(aco_eos_ABS_L150)), names.arg = c(">=400 cells/uL", "300-399 cells/uL", "200-299 cells/uL", "150-199 cells/uL", "<150 cells/uL"),  xlab="Number of Patients",main = "ACO", col="Cyan", horiz=T, las=2)
barplot(c(length(copd_eos_ABS_G400), length(copd_eos_ABS_300_399), length(copd_eos_ABS_200_299), length(copd_eos_ABS_150_199), length(copd_eos_ABS_L150)), names.arg = c(">=400 cells/uL", "300-399 cells/uL", "200-299 cells/uL", "150-199 cells/uL", "<150 cells/uL"),  xlab="Number of Patients",main = "COPD", col="Green", horiz=T, las=2)


#
#Figure 2 : Comorbidities
#

#save new reduced patient IDs
new_asthma_patients=row.names(asthma_eos_ABS_list)
new_aco_patients=row.names(aco_eos_ABS_list)
new_copd_patients=row.names(copd_eos_ABS_list)
new_all_patients=c(new_asthma_patients, new_aco_patients, new_copd_patients)
males=patient_demos[which(patient_demos$GENDER == "Male"),1]
females=patient_demos[which(patient_demos$GENDER == "Female"),1]

new_cohort_all_diagnosis=cohort_all_diagnosis[cohort_all_diagnosis$RANDOM_PAT_ID %in% new_all_patients,]
new_cohort_all_diagnosis_ICD9=new_cohort_all_diagnosis[grep("ICD9", new_cohort_all_diagnosis[,7]),]
new_cohort_all_diagnosis_ICD10=new_cohort_all_diagnosis[grep("ICD10", new_cohort_all_diagnosis[,7]),]
#intersect(new_cohort_all_diagnosis_ICD9$RANDOM_PAT_ID,males)
new_cohort_all_diagnosis_ICD9_M=new_cohort_all_diagnosis_ICD9[new_cohort_all_diagnosis_ICD9$RANDOM_PAT_ID %in% males,]
new_cohort_all_diagnosis_ICD9_F=new_cohort_all_diagnosis_ICD9[new_cohort_all_diagnosis_ICD9$RANDOM_PAT_ID %in% females,]
new_cohort_all_diagnosis_ICD10_M=new_cohort_all_diagnosis_ICD10[new_cohort_all_diagnosis_ICD10$RANDOM_PAT_ID %in% males,]
new_cohort_all_diagnosis_ICD10_F=new_cohort_all_diagnosis_ICD10[new_cohort_all_diagnosis_ICD10$RANDOM_PAT_ID %in% females,]


#######
#     #
# ALL #
#     #
#######

#Define populations
asthma_low=c(asthma_eos_ABS_L150, asthma_eos_ABS_150_199, asthma_eos_ABS_200_299, asthma_eos_ABS_300_399)
aco_low=c(aco_eos_ABS_L150, aco_eos_ABS_150_199, aco_eos_ABS_200_299, aco_eos_ABS_300_399)
copd_low=c(copd_eos_ABS_L150, copd_eos_ABS_150_199, copd_eos_ABS_200_299, copd_eos_ABS_300_399)
asthma_high=asthma_eos_ABS_G400
aco_high=aco_eos_ABS_G400
copd_high=copd_eos_ABS_G400

#List of populations to loop over
list_of_names=c("asthma_low", 
                "asthma_high", 
                "aco_low",
                "aco_high", 
                "copd_low", 
                "copd_high") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sds_ICD9=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))
table_of_sds_ICD10=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9[i,]=apply(kitty,2,sum)
  table_of_sds_ICD9[i,]=apply(kitty, 2, sd)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10[i,]=apply(kitty,2,sum)
  table_of_sds_ICD10[i,]=apply(kitty, 2, sd)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9)=list_of_names
colnames(table_of_sums_ICD9)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sds_ICD9)=list_of_names
colnames(table_of_sds_ICD9)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10)=list_of_names
colnames(table_of_sums_ICD10)=unique(new_cohort_all_diagnosis_ICD10$NAME)
row.names(table_of_sds_ICD10)=list_of_names
colnames(table_of_sds_ICD10)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#save them
write.table(data.frame("Groups"=rownames(table_of_sums_ICD9),table_of_sums_ICD9), paste0(location, "table_of_sums_ICD9.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD9),table_of_sds_ICD9), paste0(location, "table_of_sds_ICD9.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sums_ICD10),table_of_sums_ICD10), paste0(location, "table_of_sums_ICD10.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD10),table_of_sds_ICD10), paste0(location, "table_of_sds_ICD10.txt"), sep="\t", row.names=F)

#get N for each group
asthma_high_n=length(asthma_high)
asthma_low_n=length(asthma_low)

aco_high_n=length(aco_high)
aco_low_n=length(aco_low)

copd_high_n=length(copd_high)
copd_low_n=length(copd_low)

#get sums and sds for each group
asthma_low_sum_ICD9 <- as.numeric(table_of_sums_ICD9[1,1:ncol(table_of_sums_ICD9)])
asthma_low_sd_ICD9 <- as.numeric(table_of_sds_ICD9[1,1:ncol(table_of_sds_ICD9)])
aco_low_sum_ICD9 <- as.numeric(table_of_sums_ICD9[3,1:ncol(table_of_sums_ICD9)])
aco_low_sd_ICD9 <- as.numeric(table_of_sds_ICD9[3,1:ncol(table_of_sds_ICD9)])
copd_low_sum_ICD9 <- as.numeric(table_of_sums_ICD9[5,1:ncol(table_of_sums_ICD9)])
copd_low_sd_ICD9 <- as.numeric(table_of_sds_ICD9[5,1:ncol(table_of_sds_ICD9)])
asthma_high_sum_ICD9=as.numeric(table_of_sums_ICD9[2,1:ncol(table_of_sums_ICD9)])
asthma_high_sd_ICD9=as.numeric(table_of_sds_ICD9[2,1:ncol(table_of_sds_ICD9)])
aco_high_sum_ICD9=as.numeric(table_of_sums_ICD9[4,1:ncol(table_of_sums_ICD9)])
aco_high_sd_ICD9=as.numeric(table_of_sds_ICD9[4,1:ncol(table_of_sds_ICD9)])
copd_high_sum_ICD9=as.numeric(table_of_sums_ICD9[6,1:ncol(table_of_sums_ICD9)])
copd_high_sd_ICD9=as.numeric(table_of_sds_ICD9[6,1:ncol(table_of_sds_ICD9)])

asthma_low_sum_ICD10 <- as.numeric(table_of_sums_ICD10[1,1:ncol(table_of_sums_ICD10)])
asthma_low_sd_ICD10 <- as.numeric(table_of_sds_ICD10[1,1:ncol(table_of_sds_ICD10)])
aco_low_sum_ICD10 <- as.numeric(table_of_sums_ICD10[3,1:ncol(table_of_sums_ICD10)])
aco_low_sd_ICD10 <- as.numeric(table_of_sds_ICD10[3,1:ncol(table_of_sds_ICD10)])
copd_low_sum_ICD10 <- as.numeric(table_of_sums_ICD10[5,1:ncol(table_of_sums_ICD10)])
copd_low_sd_ICD10 <- as.numeric(table_of_sds_ICD10[5,1:ncol(table_of_sds_ICD10)])
asthma_high_sum_ICD10=as.numeric(table_of_sums_ICD10[2,1:ncol(table_of_sums_ICD10)])
asthma_high_sd_ICD10=as.numeric(table_of_sds_ICD10[2,1:ncol(table_of_sds_ICD10)])
aco_high_sum_ICD10=as.numeric(table_of_sums_ICD10[4,1:ncol(table_of_sums_ICD10)])
aco_high_sd_ICD10=as.numeric(table_of_sds_ICD10[4,1:ncol(table_of_sds_ICD10)])
copd_high_sum_ICD10=as.numeric(table_of_sums_ICD10[6,1:ncol(table_of_sums_ICD10)])
copd_high_sd_ICD10=as.numeric(table_of_sds_ICD10[6,1:ncol(table_of_sds_ICD10)])

asthma_proportion_table_ICD9=cbind(asthma_low_sum_ICD9, asthma_low_n-asthma_low_sum_ICD9, asthma_high_sum_ICD9, asthma_high_n-asthma_high_sum_ICD9, rep(NA, length(asthma_high_sum_ICD9)))
for(i in 1:nrow(asthma_proportion_table_ICD9)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD9[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD9[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD9=cbind(aco_low_sum_ICD9, aco_low_n-aco_low_sum_ICD9, aco_high_sum_ICD9, aco_high_n-aco_high_sum_ICD9, rep(NA, length(aco_high_sum_ICD9)))
for(i in 1:nrow(aco_proportion_table_ICD9)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD9[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD9[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD9=cbind(copd_low_sum_ICD9, copd_low_n-copd_low_sum_ICD9, copd_high_sum_ICD9, copd_high_n-copd_high_sum_ICD9, rep(NA, length(copd_high_sum_ICD9)))
for(i in 1:nrow(copd_proportion_table_ICD9)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD9[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD9[i,5]=prop.test(temp2)$p.value
}
p_values_ICD9=as.data.frame(cbind(asthma_proportion_table_ICD9[,5], aco_proportion_table_ICD9[,5], copd_proportion_table_ICD9[,5]))
row.names(p_values_ICD9)=colnames(table_of_sums_ICD9)
colnames(p_values_ICD9)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD9),p_values_ICD9), paste0(location, "p_values_ICD9.txt"), sep="\t", row.names=F)

asthma_proportion_table_ICD10=cbind(asthma_low_sum_ICD10, asthma_low_n-asthma_low_sum_ICD10, asthma_high_sum_ICD10, asthma_high_n-asthma_high_sum_ICD10, rep(NA, length(asthma_high_sum_ICD10)))
for(i in 1:nrow(asthma_proportion_table_ICD10)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD10[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD10[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD10=cbind(aco_low_sum_ICD10, aco_low_n-aco_low_sum_ICD10, aco_high_sum_ICD10, aco_high_n-aco_high_sum_ICD10, rep(NA, length(aco_high_sum_ICD10)))
for(i in 1:nrow(aco_proportion_table_ICD10)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD10[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD10[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD10=cbind(copd_low_sum_ICD10, copd_low_n-copd_low_sum_ICD10, copd_high_sum_ICD10, copd_high_n-copd_high_sum_ICD10, rep(NA, length(copd_high_sum_ICD10)))
for(i in 1:nrow(copd_proportion_table_ICD10)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD10[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD10[i,5]=prop.test(temp2)$p.value
}
p_values_ICD10=as.data.frame(cbind(asthma_proportion_table_ICD10[,5], aco_proportion_table_ICD10[,5], copd_proportion_table_ICD10[,5]))
row.names(p_values_ICD10)=colnames(table_of_sums_ICD10)
colnames(p_values_ICD10)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD10),p_values_ICD10), paste0(location, "p_values_ICD10.txt"), sep="\t", row.names=F)


#calculate and save significantly different diagnoses (Bonferoni correction)
p_values_ICD9_bonferoni=p_values_ICD9*nrow(p_values_ICD9)
significant_asthma_ICD9=p_values_ICD9_bonferoni[which(p_values_ICD9_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9),significant_asthma_ICD9), paste0(location, "significant_asthma_ICD9_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9=p_values_ICD9_bonferoni[which(p_values_ICD9_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9),significant_aco_ICD9), paste0(location, "significant_aco_ICD9_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9=p_values_ICD9_bonferoni[which(p_values_ICD9_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9),significant_copd_ICD9), paste0(location, "significant_copd_ICD9_B.txt"), sep="\t", row.names=F)

p_values_ICD10_bonferoni=p_values_ICD10*nrow(p_values_ICD10)
significant_asthma_ICD10=p_values_ICD10_bonferoni[which(p_values_ICD10_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10),significant_asthma_ICD10), paste0(location, "significant_asthma_ICD10_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10=p_values_ICD10_bonferoni[which(p_values_ICD10_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10),significant_aco_ICD10), paste0(location, "significant_aco_ICD10_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10=p_values_ICD10_bonferoni[which(p_values_ICD10_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10),significant_copd_ICD10), paste0(location, "significant_copd_ICD10_B.txt"), sep="\t", row.names=F)

#calculate and save significantly different diagnoses (no correction)
significant_asthma_ICD9=p_values_ICD9[which(p_values_ICD9$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9),significant_asthma_ICD9), paste0(location, "significant_asthma_ICD9_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9=p_values_ICD9[which(p_values_ICD9$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9),significant_aco_ICD9), paste0(location, "significant_aco_ICD9_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9=p_values_ICD9[which(p_values_ICD9$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9),significant_copd_ICD9), paste0(location, "significant_copd_ICD9_B.txt"), sep="\t", row.names=F)

significant_asthma_ICD10=p_values_ICD10[which(p_values_ICD10$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10),significant_asthma_ICD10), paste0(location, "significant_asthma_ICD10_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10=p_values_ICD10[which(p_values_ICD10$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10),significant_aco_ICD10), paste0(location, "significant_aco_ICD10_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10=p_values_ICD10[which(p_values_ICD10$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10),significant_copd_ICD10), paste0(location, "significant_copd_ICD10_B.txt"), sep="\t", row.names=F)

########
#      #
# MALE #
#      #
########

#Define populations
asthma_low_M=c(intersect(asthma_eos_ABS_L150, males), intersect(asthma_eos_ABS_150_199, males), intersect(asthma_eos_ABS_200_299, males), intersect(asthma_eos_ABS_300_399, males))
aco_low_M=c(intersect(aco_eos_ABS_L150, males), intersect(aco_eos_ABS_150_199, males), intersect(aco_eos_ABS_200_299, males), intersect(aco_eos_ABS_300_399, males))
copd_low_M=c(intersect(copd_eos_ABS_L150, males), intersect(copd_eos_ABS_150_199, males), intersect(copd_eos_ABS_200_299, males), intersect(copd_eos_ABS_300_399,males))
asthma_high_M=intersect(asthma_eos_ABS_G400, males)
aco_high_M=intersect(aco_eos_ABS_G400, males)
copd_high_M=intersect(copd_eos_ABS_G400, males)

#List of populations to loop over
list_of_names=c("asthma_low_M", 
                "asthma_high_M", 
                "aco_low_M",
                "aco_high_M", 
                "copd_low_M", 
                "copd_high_M") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sds_ICD9_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))
table_of_sds_ICD10_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9_M[i,]=apply(kitty,2,sum)
  table_of_sds_ICD9_M[i,]=apply(kitty, 2, sd)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10_M[i,]=apply(kitty,2,sum)
  table_of_sds_ICD10_M[i,]=apply(kitty, 2, sd)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9_M)=list_of_names
colnames(table_of_sums_ICD9_M)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sds_ICD9_M)=list_of_names
colnames(table_of_sds_ICD9_M)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10_M)=list_of_names
colnames(table_of_sums_ICD10_M)=unique(new_cohort_all_diagnosis_ICD10$NAME)
row.names(table_of_sds_ICD10_M)=list_of_names
colnames(table_of_sds_ICD10_M)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#save them
write.table(data.frame("Groups"=rownames(table_of_sums_ICD9_M),table_of_sums_ICD9_M), paste0(location, "table_of_sums_ICD9_M.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD9_M),table_of_sds_ICD9_M), paste0(location, "table_of_sds_ICD9_M.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sums_ICD10_M),table_of_sums_ICD10_M), paste0(location, "table_of_sums_ICD10_M.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD10_M),table_of_sds_ICD10_M), paste0(location, "table_of_sds_ICD10_M.txt"), sep="\t", row.names=F)

#get N for each group
asthma_high_n_M=length(asthma_high_M)
asthma_low_n_M=length(asthma_low_M)

aco_high_n_M=length(aco_high_M)
aco_low_n_M=length(aco_low_M)

copd_high_n_M=length(copd_high_M)
copd_low_n_M=length(copd_low_M)

#get sums and sds for each group
asthma_low_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[1,1:ncol(table_of_sums_ICD9_M)])
asthma_low_sd_ICD9_M <- as.numeric(table_of_sds_ICD9_M[1,1:ncol(table_of_sds_ICD9_M)])
aco_low_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[3,1:ncol(table_of_sums_ICD9_M)])
aco_low_sd_ICD9_M <- as.numeric(table_of_sds_ICD9_M[3,1:ncol(table_of_sds_ICD9_M)])
copd_low_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[5,1:ncol(table_of_sums_ICD9_M)])
copd_low_sd_ICD9_M <- as.numeric(table_of_sds_ICD9_M[5,1:ncol(table_of_sds_ICD9_M)])
asthma_high_sum_ICD9_M=as.numeric(table_of_sums_ICD9_M[2,1:ncol(table_of_sums_ICD9_M)])
asthma_high_sd_ICD9_M=as.numeric(table_of_sds_ICD9_M[2,1:ncol(table_of_sds_ICD9_M)])
aco_high_sum_ICD9_M=as.numeric(table_of_sums_ICD9_M[4,1:ncol(table_of_sums_ICD9_M)])
aco_high_sd_ICD9_M=as.numeric(table_of_sds_ICD9_M[4,1:ncol(table_of_sds_ICD9_M)])
copd_high_sum_ICD9_M=as.numeric(table_of_sums_ICD9_M[6,1:ncol(table_of_sums_ICD9_M)])
copd_high_sd_ICD9_M=as.numeric(table_of_sds_ICD9_M[6,1:ncol(table_of_sds_ICD9_M)])

asthma_low_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[1,1:ncol(table_of_sums_ICD10_M)])
asthma_low_sd_ICD10_M <- as.numeric(table_of_sds_ICD10_M[1,1:ncol(table_of_sds_ICD10_M)])
aco_low_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[3,1:ncol(table_of_sums_ICD10_M)])
aco_low_sd_ICD10_M <- as.numeric(table_of_sds_ICD10_M[3,1:ncol(table_of_sds_ICD10_M)])
copd_low_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[5,1:ncol(table_of_sums_ICD10_M)])
copd_low_sd_ICD10_M <- as.numeric(table_of_sds_ICD10_M[5,1:ncol(table_of_sds_ICD10_M)])
asthma_high_sum_ICD10_M=as.numeric(table_of_sums_ICD10_M[2,1:ncol(table_of_sums_ICD10_M)])
asthma_high_sd_ICD10_M=as.numeric(table_of_sds_ICD10_M[2,1:ncol(table_of_sds_ICD10_M)])
aco_high_sum_ICD10_M=as.numeric(table_of_sums_ICD10_M[4,1:ncol(table_of_sums_ICD10_M)])
aco_high_sd_ICD10_M=as.numeric(table_of_sds_ICD10_M[4,1:ncol(table_of_sds_ICD10_M)])
copd_high_sum_ICD10_M=as.numeric(table_of_sums_ICD10_M[6,1:ncol(table_of_sums_ICD10_M)])
copd_high_sd_ICD10_M=as.numeric(table_of_sds_ICD10_M[6,1:ncol(table_of_sds_ICD10_M)])

asthma_proportion_table_ICD9_M=cbind(asthma_low_sum_ICD9_M, asthma_low_n_M-asthma_low_sum_ICD9_M, asthma_high_sum_ICD9_M, asthma_high_n_M-asthma_high_sum_ICD9_M, rep(NA, length(asthma_high_sum_ICD9_M)))
for(i in 1:nrow(asthma_proportion_table_ICD9_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD9_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD9_M[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD9_M=cbind(aco_low_sum_ICD9_M, aco_low_n_M-aco_low_sum_ICD9_M, aco_high_sum_ICD9_M, aco_high_n_M-aco_high_sum_ICD9_M, rep(NA, length(aco_high_sum_ICD9_M)))
for(i in 1:nrow(aco_proportion_table_ICD9_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD9_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD9_M[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD9_M=cbind(copd_low_sum_ICD9_M, copd_low_n_M-copd_low_sum_ICD9_M, copd_high_sum_ICD9_M, copd_high_n_M-copd_high_sum_ICD9_M, rep(NA, length(copd_high_sum_ICD9_M)))
for(i in 1:nrow(copd_proportion_table_ICD9_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD9_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD9_M[i,5]=prop.test(temp2)$p.value
}
p_values_ICD9_M=as.data.frame(cbind(asthma_proportion_table_ICD9_M[,5], aco_proportion_table_ICD9_M[,5], copd_proportion_table_ICD9_M[,5]))
row.names(p_values_ICD9_M)=colnames(table_of_sums_ICD9_M)
colnames(p_values_ICD9_M)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD9_M),p_values_ICD9_M), paste0(location, "p_values_ICD9_M.txt"), sep="\t", row.names=F)

asthma_proportion_table_ICD10_M=cbind(asthma_low_sum_ICD10_M, asthma_low_n_M-asthma_low_sum_ICD10_M, asthma_high_sum_ICD10_M, asthma_high_n_M-asthma_high_sum_ICD10_M, rep(NA, length(asthma_high_sum_ICD10_M)))
for(i in 1:nrow(asthma_proportion_table_ICD10_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD10_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD10_M[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD10_M=cbind(aco_low_sum_ICD10_M, aco_low_n_M-aco_low_sum_ICD10_M, aco_high_sum_ICD10_M, aco_high_n_M-aco_high_sum_ICD10_M, rep(NA, length(aco_high_sum_ICD10_M)))
for(i in 1:nrow(aco_proportion_table_ICD10_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD10_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD10_M[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD10_M=cbind(copd_low_sum_ICD10_M, copd_low_n_M-copd_low_sum_ICD10_M, copd_high_sum_ICD10_M, copd_high_n_M-copd_high_sum_ICD10_M, rep(NA, length(copd_high_sum_ICD10_M)))
for(i in 1:nrow(copd_proportion_table_ICD10_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD10_M[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD10_M[i,5]=prop.test(temp2)$p.value
}
p_values_ICD10_M=as.data.frame(cbind(asthma_proportion_table_ICD10_M[,5], aco_proportion_table_ICD10_M[,5], copd_proportion_table_ICD10_M[,5]))
row.names(p_values_ICD10_M)=colnames(table_of_sums_ICD10_M)
colnames(p_values_ICD10_M)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD10_M),p_values_ICD10_M), paste0(location, "p_values_ICD10_M.txt"), sep="\t", row.names=F)

#calculate and save significantly different diagnoses (Bonferoni correction)
p_values_ICD9_M_bonferoni=p_values_ICD9_M*nrow(p_values_ICD9_M)
significant_asthma_ICD9_M=p_values_ICD9_M_bonferoni[which(p_values_ICD9_M_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9_M),significant_asthma_ICD9_M), paste0(location, "significant_asthma_ICD9_M_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9_M=p_values_ICD9_M_bonferoni[which(p_values_ICD9_M_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9_M),significant_aco_ICD9_M), paste0(location, "significant_aco_ICD9_M_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9_M=p_values_ICD9_M_bonferoni[which(p_values_ICD9_M_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9_M),significant_copd_ICD9_M), paste0(location, "significant_copd_ICD9_M_B.txt"), sep="\t", row.names=F)

p_values_ICD10_M_bonferoni=p_values_ICD10_M*nrow(p_values_ICD10_M)
significant_asthma_ICD10_M=p_values_ICD10_M_bonferoni[which(p_values_ICD10_M_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10_M),significant_asthma_ICD10_M), paste0(location, "significant_asthma_ICD10_M_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10_M=p_values_ICD10_M_bonferoni[which(p_values_ICD10_M_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10_M),significant_aco_ICD10_M), paste0(location, "significant_aco_ICD10_M_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10_M=p_values_ICD10_M_bonferoni[which(p_values_ICD10_M_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10_M),significant_copd_ICD10_M), paste0(location, "significant_copd_ICD10_M_B.txt"), sep="\t", row.names=F)

#calculate and save significantly different diagnoses (no correction)
significant_asthma_ICD9_M=p_values_ICD9_M[which(p_values_ICD9_M$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9_M),significant_asthma_ICD9_M), paste0(location, "significant_asthma_ICD9_M_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9_M=p_values_ICD9_M[which(p_values_ICD9_M$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9_M),significant_aco_ICD9_M), paste0(location, "significant_aco_ICD9_M_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9_M=p_values_ICD9_M[which(p_values_ICD9_M$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9_M),significant_copd_ICD9_M), paste0(location, "significant_copd_ICD9_M_B.txt"), sep="\t", row.names=F)

significant_asthma_ICD10_M=p_values_ICD10_M[which(p_values_ICD10_M$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10_M),significant_asthma_ICD10_M), paste0(location, "significant_asthma_ICD10_M_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10_M=p_values_ICD10_M[which(p_values_ICD10_M$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10_M),significant_aco_ICD10_M), paste0(location, "significant_aco_ICD10_M_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10_M=p_values_ICD10_M[which(p_values_ICD10_M$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10_M),significant_copd_ICD10_M), paste0(location, "significant_copd_ICD10_M_B.txt"), sep="\t", row.names=F)

##########
#        #
# FEMALE #
#        #
##########

#Define populations
asthma_low_F=c(intersect(asthma_eos_ABS_L150, females), intersect(asthma_eos_ABS_150_199, females), intersect(asthma_eos_ABS_200_299, females), intersect(asthma_eos_ABS_300_399, females))
aco_low_F=c(intersect(aco_eos_ABS_L150, females), intersect(aco_eos_ABS_150_199, females), intersect(aco_eos_ABS_200_299, females), intersect(aco_eos_ABS_300_399, females))
copd_low_F=c(intersect(copd_eos_ABS_L150, females), intersect(copd_eos_ABS_150_199, females), intersect(copd_eos_ABS_200_299, females), intersect(copd_eos_ABS_300_399,females))
asthma_high_F=intersect(asthma_eos_ABS_G400, females)
aco_high_F=intersect(aco_eos_ABS_G400, females)
copd_high_F=intersect(copd_eos_ABS_G400, females)

#List of populations to loop over
list_of_names=c("asthma_low_F", 
                "asthma_high_F", 
                "aco_low_F",
                "aco_high_F", 
                "copd_low_F", 
                "copd_high_F") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sds_ICD9_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))
table_of_sds_ICD10_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9_F[i,]=apply(kitty,2,sum)
  table_of_sds_ICD9_F[i,]=apply(kitty, 2, sd)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10_F[i,]=apply(kitty,2,sum)
  table_of_sds_ICD10_F[i,]=apply(kitty, 2, sd)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9_F)=list_of_names
colnames(table_of_sums_ICD9_F)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sds_ICD9_F)=list_of_names
colnames(table_of_sds_ICD9_F)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10_F)=list_of_names
colnames(table_of_sums_ICD10_F)=unique(new_cohort_all_diagnosis_ICD10$NAME)
row.names(table_of_sds_ICD10_F)=list_of_names
colnames(table_of_sds_ICD10_F)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#save them
write.table(data.frame("Groups"=rownames(table_of_sums_ICD9_F),table_of_sums_ICD9_F), paste0(location, "table_of_sums_ICD9_F.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD9_F),table_of_sds_ICD9_F), paste0(location, "table_of_sds_ICD9_F.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sums_ICD10_F),table_of_sums_ICD10_F), paste0(location, "table_of_sums_ICD10_F.txt"), sep="\t", row.names=F)
write.table(data.frame("Groups"=rownames(table_of_sds_ICD10_F),table_of_sds_ICD10_F), paste0(location, "table_of_sds_ICD10_F.txt"), sep="\t", row.names=F)

#get N for each group
asthma_high_n_F=length(asthma_high_F)
asthma_low_n_F=length(asthma_low_F)

aco_high_n_F=length(aco_high_F)
aco_low_n_F=length(aco_low_F)

copd_high_n_F=length(copd_high_F)
copd_low_n_F=length(copd_low_F)

#get sums and sds for each group
asthma_low_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[1,1:ncol(table_of_sums_ICD9_F)])
asthma_low_sd_ICD9_F <- as.numeric(table_of_sds_ICD9_F[1,1:ncol(table_of_sds_ICD9_F)])
aco_low_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[3,1:ncol(table_of_sums_ICD9_F)])
aco_low_sd_ICD9_F <- as.numeric(table_of_sds_ICD9_F[3,1:ncol(table_of_sds_ICD9_F)])
copd_low_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[5,1:ncol(table_of_sums_ICD9_F)])
copd_low_sd_ICD9_F <- as.numeric(table_of_sds_ICD9_F[5,1:ncol(table_of_sds_ICD9_F)])
asthma_high_sum_ICD9_F=as.numeric(table_of_sums_ICD9_F[2,1:ncol(table_of_sums_ICD9_F)])
asthma_high_sd_ICD9_F=as.numeric(table_of_sds_ICD9_F[2,1:ncol(table_of_sds_ICD9_F)])
aco_high_sum_ICD9_F=as.numeric(table_of_sums_ICD9_F[4,1:ncol(table_of_sums_ICD9_F)])
aco_high_sd_ICD9_F=as.numeric(table_of_sds_ICD9_F[4,1:ncol(table_of_sds_ICD9_F)])
copd_high_sum_ICD9_F=as.numeric(table_of_sums_ICD9_F[6,1:ncol(table_of_sums_ICD9_F)])
copd_high_sd_ICD9_F=as.numeric(table_of_sds_ICD9_F[6,1:ncol(table_of_sds_ICD9_F)])

asthma_low_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[1,1:ncol(table_of_sums_ICD10_F)])
asthma_low_sd_ICD10_F <- as.numeric(table_of_sds_ICD10_F[1,1:ncol(table_of_sds_ICD10_F)])
aco_low_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[3,1:ncol(table_of_sums_ICD10_F)])
aco_low_sd_ICD10_F <- as.numeric(table_of_sds_ICD10_F[3,1:ncol(table_of_sds_ICD10_F)])
copd_low_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[5,1:ncol(table_of_sums_ICD10_F)])
copd_low_sd_ICD10_F <- as.numeric(table_of_sds_ICD10_F[5,1:ncol(table_of_sds_ICD10_F)])
asthma_high_sum_ICD10_F=as.numeric(table_of_sums_ICD10_F[2,1:ncol(table_of_sums_ICD10_F)])
asthma_high_sd_ICD10_F=as.numeric(table_of_sds_ICD10_F[2,1:ncol(table_of_sds_ICD10_F)])
aco_high_sum_ICD10_F=as.numeric(table_of_sums_ICD10_F[4,1:ncol(table_of_sums_ICD10_F)])
aco_high_sd_ICD10_F=as.numeric(table_of_sds_ICD10_F[4,1:ncol(table_of_sds_ICD10_F)])
copd_high_sum_ICD10_F=as.numeric(table_of_sums_ICD10_F[6,1:ncol(table_of_sums_ICD10_F)])
copd_high_sd_ICD10_F=as.numeric(table_of_sds_ICD10_F[6,1:ncol(table_of_sds_ICD10_F)])

asthma_proportion_table_ICD9_F=cbind(asthma_low_sum_ICD9_F, asthma_low_n_F-asthma_low_sum_ICD9_F, asthma_high_sum_ICD9_F, asthma_high_n_F-asthma_high_sum_ICD9_F, rep(NA, length(asthma_high_sum_ICD9_F)))
for(i in 1:nrow(asthma_proportion_table_ICD9_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD9_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD9_F[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD9_F=cbind(aco_low_sum_ICD9_F, aco_low_n_F-aco_low_sum_ICD9_F, aco_high_sum_ICD9_F, aco_high_n_F-aco_high_sum_ICD9_F, rep(NA, length(aco_high_sum_ICD9_F)))
for(i in 1:nrow(aco_proportion_table_ICD9_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD9_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD9_F[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD9_F=cbind(copd_low_sum_ICD9_F, copd_low_n_F-copd_low_sum_ICD9_F, copd_high_sum_ICD9_F, copd_high_n_F-copd_high_sum_ICD9_F, rep(NA, length(copd_high_sum_ICD9_F)))
for(i in 1:nrow(copd_proportion_table_ICD9_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD9_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD9_F[i,5]=prop.test(temp2)$p.value
}
p_values_ICD9_F=as.data.frame(cbind(asthma_proportion_table_ICD9_F[,5], aco_proportion_table_ICD9_F[,5], copd_proportion_table_ICD9_F[,5]))
row.names(p_values_ICD9_F)=colnames(table_of_sums_ICD9_F)
colnames(p_values_ICD9_F)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD9_F),p_values_ICD9_F), paste0(location, "p_values_ICD9_F.txt"), sep="\t", row.names=F)

asthma_proportion_table_ICD10_F=cbind(asthma_low_sum_ICD10_F, asthma_low_n_F-asthma_low_sum_ICD10_F, asthma_high_sum_ICD10_F, asthma_high_n_F-asthma_high_sum_ICD10_F, rep(NA, length(asthma_high_sum_ICD10_F)))
for(i in 1:nrow(asthma_proportion_table_ICD10_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(asthma_proportion_table_ICD10_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  asthma_proportion_table_ICD10_F[i,5]=prop.test(temp2)$p.value
}
aco_proportion_table_ICD10_F=cbind(aco_low_sum_ICD10_F, aco_low_n_F-aco_low_sum_ICD10_F, aco_high_sum_ICD10_F, aco_high_n_F-aco_high_sum_ICD10_F, rep(NA, length(aco_high_sum_ICD10_F)))
for(i in 1:nrow(aco_proportion_table_ICD10_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(aco_proportion_table_ICD10_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  aco_proportion_table_ICD10_F[i,5]=prop.test(temp2)$p.value
}
copd_proportion_table_ICD10_F=cbind(copd_low_sum_ICD10_F, copd_low_n_F-copd_low_sum_ICD10_F, copd_high_sum_ICD10_F, copd_high_n_F-copd_high_sum_ICD10_F, rep(NA, length(copd_high_sum_ICD10_F)))
for(i in 1:nrow(copd_proportion_table_ICD10_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(copd_proportion_table_ICD10_F[i,1:4], nrow=2, ncol=2, byrow=T)
  temp2=as.table(temp)
  copd_proportion_table_ICD10_F[i,5]=prop.test(temp2)$p.value
}
p_values_ICD10_F=as.data.frame(cbind(asthma_proportion_table_ICD10_F[,5], aco_proportion_table_ICD10_F[,5], copd_proportion_table_ICD10_F[,5]))
row.names(p_values_ICD10_F)=colnames(table_of_sums_ICD10_F)
colnames(p_values_ICD10_M)=c("asthma", "aco", "copd")
write.table(data.frame("Groups"=rownames(p_values_ICD10_F),p_values_ICD10_F), paste0(location, "p_values_ICD10_F.txt"), sep="\t", row.names=F)

#calculate and save significantly different diagnoses (Bonferoni correction)
p_values_ICD9_F_bonferoni=p_values_ICD9_F*nrow(p_values_ICD9_F)
significant_asthma_ICD9_F=p_values_ICD9_F_bonferoni[which(p_values_ICD9_F_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9_F),significant_asthma_ICD9_F), paste0(location, "significant_asthma_ICD9_F_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9_F=p_values_ICD9_F_bonferoni[which(p_values_ICD9_F_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9_F),significant_aco_ICD9_F), paste0(location, "significant_aco_ICD9_F_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9_F=p_values_ICD9_F_bonferoni[which(p_values_ICD9_F_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9_F),significant_copd_ICD9_F), paste0(location, "significant_copd_ICD9_F_B.txt"), sep="\t", row.names=F)

p_values_ICD10_F_bonferoni=p_values_ICD10_F*nrow(p_values_ICD10_F)
significant_asthma_ICD10_F=p_values_ICD10_F_bonferoni[which(p_values_ICD10_F_bonferoni$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10_F),significant_asthma_ICD10_F), paste0(location, "significant_asthma_ICD10_F_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10_F=p_values_ICD10_F_bonferoni[which(p_values_ICD10_F_bonferoni$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10_F),significant_aco_ICD10_F), paste0(location, "significant_aco_ICD10_F_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10_F=p_values_ICD10_F_bonferoni[which(p_values_ICD10_F_bonferoni$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10_F),significant_copd_ICD10_F), paste0(location, "significant_copd_ICD10_F_B.txt"), sep="\t", row.names=F)

#calculate and save significantly different diagnoses (no correction)
significant_asthma_ICD9_F=p_values_ICD9_F[which(p_values_ICD9_F$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD9_F),significant_asthma_ICD9_F), paste0(location, "significant_asthma_ICD9_F_B.txt"), sep="\t", row.names=F)
significant_aco_ICD9_F=p_values_ICD9_F[which(p_values_ICD9_F$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD9_F),significant_aco_ICD9_F), paste0(location, "significant_aco_ICD9_F_B.txt"), sep="\t", row.names=F)
significant_copd_ICD9_F=p_values_ICD9_F[which(p_values_ICD9_F$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD9_F),significant_copd_ICD9_F), paste0(location, "significant_copd_ICD9_F_B.txt"), sep="\t", row.names=F)

significant_asthma_ICD10_F=p_values_ICD10_F[which(p_values_ICD10_F$asthma<0.05),]
write.table(data.frame("DX"=rownames(significant_asthma_ICD10_F),significant_asthma_ICD10_F), paste0(location, "significant_asthma_ICD10_F_B.txt"), sep="\t", row.names=F)
significant_aco_ICD10_F=p_values_ICD10_F[which(p_values_ICD10_F$aco<0.05),]
write.table(data.frame("DX"=rownames(significant_aco_ICD10_F),significant_aco_ICD10_F), paste0(location, "significant_aco_ICD10_F_B.txt"), sep="\t", row.names=F)
significant_copd_ICD10_F=p_values_ICD10_F[which(p_values_ICD10_F$copd<0.05),]
write.table(data.frame("DX"=rownames(significant_copd_ICD10_F),significant_copd_ICD10_F), paste0(location, "significant_copd_ICD10_F_B.txt"), sep="\t", row.names=F)


##########################

asthma_differences_ICD9=intersect(row.names(significant_asthma_ICD9_F), row.names(significant_asthma_ICD9_M))
aco_differences_ICD9=intersect(row.names(significant_aco_ICD9_F), row.names(significant_aco_ICD9_M))
copd_differences_ICD9=intersect(row.names(significant_copd_ICD9_F), row.names(significant_copd_ICD9_M))
# 
asthma_differences_ICD10=intersect(row.names(significant_asthma_ICD10_F), row.names(significant_asthma_ICD10_M))
aco_differences_ICD10=intersect(row.names(significant_aco_ICD10_F), row.names(significant_aco_ICD10_M))
copd_differences_ICD10=intersect(row.names(significant_copd_ICD10_F), row.names(significant_copd_ICD10_M))
# 

#
#table 2
#


table_2=as.data.frame(matrix(nrow=12, ncol=5))
colnames(table_2)=c("Category","Asthma", "ACO", "COPD", "P-value")

#age
table_2[1,1]="Age (years)"

#asthma
unique_pats=unique(asthma_low)
asthma_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=asthma_patients[asthma_patients$RANDOM_PAT_ID == unique_pats[pat],]
  asthma_med_age_list[pat,1]=median(encounters$AGE)
}
table_2[1,2]=paste0(median(asthma_med_age_list[,1]), " (", round(sd(asthma_med_age_list[,1]),1), ")")

#aco
unique_pats=unique(aco_low)
aco_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=aco_patients[aco_patients$RANDOM_PAT_ID == unique_pats[pat],]
  aco_med_age_list[pat,1]=median(encounters$AGE)
}
table_2[1,3]=paste0(median(aco_med_age_list[,1]), " (", round(sd(aco_med_age_list[,1]),1), ")")

#copd
unique_pats=unique(copd_low)
copd_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=copd_patients[copd_patients$RANDOM_PAT_ID == unique_pats[pat],]
  copd_med_age_list[pat,1]=median(encounters$AGE)
}
table_2[1,4]=paste0(median(copd_med_age_list[,1]), " (", round(sd(copd_med_age_list[,1]),1), ")")

#anova
a=cbind(asthma_med_age_list, rep(1, nrow(asthma_med_age_list)))
colnames(a)=c("ages", "groups")
b=cbind(aco_med_age_list, rep(2, nrow(aco_med_age_list)))
colnames(b)=c("ages", "groups")
c=cbind(copd_med_age_list, rep(3, nrow(copd_med_age_list)))
colnames(c)=c("ages", "groups")
age_anova_table=rbind(a,b,c)
age_anova=aov(ages ~ groups, age_anova_table)
k=summary(age_anova) #significantly different ages
if(k[[1]][["Pr(>F)"]][[1]] < 0.05){
  table_2[1,5]="p<0.05"
}else{
  table_2[1,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#sex
patient_demos=patient_demos[!(patient_demos$RANDOM_PAT_ID %in% excluded_patients),]
table_2[2,1]="Sex (% Male)"

#asthma
unique_pats=unique(asthma_low)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% asthma_low,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_2[2,2]=round((length(which(asthma_patients_demos$GENDER == "Male"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_low)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_low,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_2[2,3]=round((length(which(aco_patients_demos$GENDER == "Male"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_low)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_low,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_2[2,4]=round((length(which(copd_patients_demos$GENDER == "Male"))/nrow(copd_patients_demos))*100,0)

#chi-sq
a=cbind(asthma_patients_demos$GENDER, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("gender", "groups")
b=cbind(aco_patients_demos$GENDER, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("gender", "groups")
c=cbind(copd_patients_demos$GENDER, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("gender", "groups")
gender_chi_table=rbind(a,b,c)
gender_chi_table=as.data.frame(gender_chi_table)
#table(gender_chi_table$gender, gender_chi_table$groups)
gender_chi=chisq.test(gender_chi_table$gender, gender_chi_table$groups)
if(gender_chi$p.value < 0.05){
  table_2[2,5]="p<0.05"
}else{
  table_2[2,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#race
table_2[3,1]="Race (%)"
table_2[4,1]="White or Caucasian"
table_2[5,1]="Black or African American"
table_2[6,1]="Other"


#asthma
unique_pats=unique(asthma_low)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% asthma_low,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_2[4,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(asthma_patients_demos))*100,0)
table_2[5,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(asthma_patients_demos))*100,0)
table_2[6,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Other" | 
                                   asthma_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   asthma_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   asthma_patients_demos$PATIENT_RACE == "Asian" | 
                                   asthma_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   asthma_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_low)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_low,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_2[4,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(aco_patients_demos))*100,0)
table_2[5,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(aco_patients_demos))*100,0)
table_2[6,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Other" | 
                                   aco_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   aco_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   aco_patients_demos$PATIENT_RACE == "Asian" | 
                                   aco_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   aco_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_low)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_low,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_2[4,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(copd_patients_demos))*100,0)
table_2[5,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(copd_patients_demos))*100,0)
table_2[6,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Other" | 
                                   copd_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   copd_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   copd_patients_demos$PATIENT_RACE == "Asian" | 
                                   copd_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   copd_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(copd_patients_demos))*100,0)

#chi-sq
asthma_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", asthma_patients_demos$PATIENT_RACE)
aco_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", aco_patients_demos$PATIENT_RACE)
copd_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", copd_patients_demos$PATIENT_RACE)
a=cbind(asthma_patients_demos$PATIENT_RACE, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("race", "groups")
b=cbind(aco_patients_demos$PATIENT_RACE, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("race", "groups")
c=cbind(copd_patients_demos$PATIENT_RACE, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("race", "groups")
race_chi_table=rbind(a,b,c)
race_chi_table=as.data.frame(race_chi_table)
#table(race_chi_table$race, race_chi_table$groups)
race_chi=chisq.test(race_chi_table$race, race_chi_table$groups)
if(race_chi$p.value < 0.05){
  table_2[3,5]="p<0.05"
}else{
  table_2[3,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}  

#smoking
table_2[7,1]="Smoking (%)"
table_2[8,1]="Never"
table_2[9,1]="Yes"
table_2[10,1]="Quit"

#asthma
asthma_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% asthma_low,]
asthma_social=asthma_social[!(is.na(asthma_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(asthma_social$RANDOM_PAT_ID)
asthma_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=asthma_social[asthma_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    asthma_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    asthma_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_2[8,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_2[9,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_2[10,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)

#aco
aco_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% aco_low,]
aco_social=aco_social[!(is.na(aco_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(aco_social$RANDOM_PAT_ID)
aco_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=aco_social[aco_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    aco_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    aco_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_2[8,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_2[9,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_2[10,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)

#copd
copd_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% copd_low,]
copd_social=copd_social[!(is.na(copd_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(copd_social$RANDOM_PAT_ID)
copd_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=copd_social[copd_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    copd_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    copd_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_2[8,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_2[9,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_2[10,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)

#chi-sq
a=cbind(asthma_mode_TOBACCO_USER_list, rep(1, nrow(asthma_mode_TOBACCO_USER_list)))
colnames(a)=c("tobacco_user", "groups")
b=cbind(aco_mode_TOBACCO_USER_list, rep(2, nrow(aco_mode_TOBACCO_USER_list)))
colnames(b)=c("tobacco_user", "groups")
c=cbind(copd_mode_TOBACCO_USER_list, rep(3, nrow(copd_mode_TOBACCO_USER_list)))
colnames(c)=c("tobacco_user", "groups")
tobacco_user_chi_table=rbind(a,b,c)
tobacco_user_chi_table=as.data.frame(tobacco_user_chi_table)
#table(tobacco_user_chi_table$race, tobacco_user_chi_table$groups)
tobacco_user_chi=chisq.test(tobacco_user_chi_table$tobacco_user, tobacco_user_chi_table$groups)
if(tobacco_user_chi$p.value < 0.05){
  table_2[7,5]="p<0.05"
}else{
  table_2[7,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#
#table 3
#


table_3=as.data.frame(matrix(nrow=12, ncol=5))
colnames(table_3)=c("Category","Asthma", "ACO", "COPD", "P-value")

#age
table_3[1,1]="Age (years)"

#asthma
unique_pats=unique(asthma_high)
asthma_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=asthma_patients[asthma_patients$RANDOM_PAT_ID == unique_pats[pat],]
  asthma_med_age_list[pat,1]=median(encounters$AGE)
}
table_3[1,2]=paste0(median(asthma_med_age_list[,1]), " (", round(sd(asthma_med_age_list[,1]),1), ")")

#aco
unique_pats=unique(aco_high)
aco_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=aco_patients[aco_patients$RANDOM_PAT_ID == unique_pats[pat],]
  aco_med_age_list[pat,1]=median(encounters$AGE)
}
table_3[1,3]=paste0(median(aco_med_age_list[,1]), " (", round(sd(aco_med_age_list[,1]),1), ")")

#copd
unique_pats=unique(copd_high)
copd_med_age_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){
  encounters=copd_patients[copd_patients$RANDOM_PAT_ID == unique_pats[pat],]
  copd_med_age_list[pat,1]=median(encounters$AGE)
}
table_3[1,4]=paste0(median(copd_med_age_list[,1]), " (", round(sd(copd_med_age_list[,1]),1), ")")

#anova
a=cbind(asthma_med_age_list, rep(1, nrow(asthma_med_age_list)))
colnames(a)=c("ages", "groups")
b=cbind(aco_med_age_list, rep(2, nrow(aco_med_age_list)))
colnames(b)=c("ages", "groups")
c=cbind(copd_med_age_list, rep(3, nrow(copd_med_age_list)))
colnames(c)=c("ages", "groups")
age_anova_table=rbind(a,b,c)
age_anova=aov(ages ~ groups, age_anova_table)
k=summary(age_anova) #significantly different ages
if(k[[1]][["Pr(>F)"]][[1]] < 0.05){
  table_3[1,5]="p<0.05"
}else{
  table_3[1,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#sex
patient_demos=patient_demos[!(patient_demos$RANDOM_PAT_ID %in% excluded_patients),]
table_3[2,1]="Sex (% Male)"

#asthma
unique_pats=unique(asthma_high)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% asthma_high,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_3[2,2]=round((length(which(asthma_patients_demos$GENDER == "Male"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_high)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_high,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_3[2,3]=round((length(which(aco_patients_demos$GENDER == "Male"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_high)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_high,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$GENDER)),] #remove patients with NA's in the GENDER column
table_3[2,4]=round((length(which(copd_patients_demos$GENDER == "Male"))/nrow(copd_patients_demos))*100,0)

#chi-sq
a=cbind(asthma_patients_demos$GENDER, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("gender", "groups")
b=cbind(aco_patients_demos$GENDER, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("gender", "groups")
c=cbind(copd_patients_demos$GENDER, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("gender", "groups")
gender_chi_table=rbind(a,b,c)
gender_chi_table=as.data.frame(gender_chi_table)
#table(gender_chi_table$gender, gender_chi_table$groups)
gender_chi=chisq.test(gender_chi_table$gender, gender_chi_table$groups)
if(gender_chi$p.value < 0.05){
  table_3[2,5]="p<0.05"
}else{
  table_3[2,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}

#race
table_3[3,1]="Race (%)"
table_3[4,1]="White or Caucasian"
table_3[5,1]="Black or African American"
table_3[6,1]="Other"


#asthma
unique_pats=unique(asthma_high)
asthma_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% asthma_high,]
asthma_patients_demos=asthma_patients_demos[!(is.na(asthma_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
asthma_patients_demos=asthma_patients_demos[asthma_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_3[4,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(asthma_patients_demos))*100,0)
table_3[5,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(asthma_patients_demos))*100,0)
table_3[6,2]=round((length(which(asthma_patients_demos$PATIENT_RACE == "Other" | 
                                   asthma_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   asthma_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   asthma_patients_demos$PATIENT_RACE == "Asian" | 
                                   asthma_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   asthma_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(asthma_patients_demos))*100,0)

#aco
unique_pats=unique(aco_high)
aco_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% aco_high,]
aco_patients_demos=aco_patients_demos[!(is.na(aco_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
aco_patients_demos=aco_patients_demos[aco_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_3[4,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(aco_patients_demos))*100,0)
table_3[5,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(aco_patients_demos))*100,0)
table_3[6,3]=round((length(which(aco_patients_demos$PATIENT_RACE == "Other" | 
                                   aco_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   aco_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   aco_patients_demos$PATIENT_RACE == "Asian" | 
                                   aco_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   aco_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(aco_patients_demos))*100,0)

#copd
unique_pats=unique(copd_high)
copd_patients_demos=patient_demos[patient_demos$RANDOM_PAT_ID %in% copd_high,]
copd_patients_demos=copd_patients_demos[!(is.na(copd_patients_demos$PATIENT_RACE)),] #remove patients with NA's in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "",] #remove patients with "" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Unknown",] #remove patients with "Unknown" in the PATIENT_RACE column
copd_patients_demos=copd_patients_demos[copd_patients_demos$PATIENT_RACE != "Patient Refused",] #remove patients with "Patient Refused" in the PATIENT_RACE column
table_3[4,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "White or Caucasian"))/nrow(copd_patients_demos))*100,0)
table_3[5,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Black or African American"))/nrow(copd_patients_demos))*100,0)
table_3[6,4]=round((length(which(copd_patients_demos$PATIENT_RACE == "Other" | 
                                   copd_patients_demos$PATIENT_RACE == "Multiracial" | 
                                   copd_patients_demos$PATIENT_RACE == "Hispanic" | 
                                   copd_patients_demos$PATIENT_RACE == "Asian" | 
                                   copd_patients_demos$PATIENT_RACE == "American Indian or Alaska Native" | 
                                   copd_patients_demos$PATIENT_RACE == "Native Hawaiian or Other Pacific Islander"))/nrow(copd_patients_demos))*100,0)

#chi-sq
asthma_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", asthma_patients_demos$PATIENT_RACE)
aco_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", aco_patients_demos$PATIENT_RACE)
copd_patients_demos$PATIENT_RACE=gsub("Multiracial|Hispanic|Asian|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Other", copd_patients_demos$PATIENT_RACE)
a=cbind(asthma_patients_demos$PATIENT_RACE, rep(1, nrow(asthma_patients_demos)))
colnames(a)=c("race", "groups")
b=cbind(aco_patients_demos$PATIENT_RACE, rep(2, nrow(aco_patients_demos)))
colnames(b)=c("race", "groups")
c=cbind(copd_patients_demos$PATIENT_RACE, rep(3, nrow(copd_patients_demos)))
colnames(c)=c("race", "groups")
race_chi_table=rbind(a,b,c)
race_chi_table=as.data.frame(race_chi_table)
#table(race_chi_table$race, race_chi_table$groups)
race_chi=chisq.test(race_chi_table$race, race_chi_table$groups)
if(race_chi$p.value < 0.05){
  table_3[3,5]="p<0.05"
}else{
  table_3[3,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}  

#smoking
table_3[7,1]="Smoking (%)"
table_3[8,1]="Never"
table_3[9,1]="Yes"
table_3[10,1]="Quit"

#asthma
asthma_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% asthma_high,]
asthma_social=asthma_social[!(is.na(asthma_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
asthma_social=asthma_social[asthma_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(asthma_social$RANDOM_PAT_ID)
asthma_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=asthma_social[asthma_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    asthma_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    asthma_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_3[8,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_3[9,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)
table_3[10,2]=round((length(which(asthma_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(asthma_mode_TOBACCO_USER_list))*100,0)

#aco
aco_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% aco_high,]
aco_social=aco_social[!(is.na(aco_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
aco_social=aco_social[aco_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(aco_social$RANDOM_PAT_ID)
aco_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=aco_social[aco_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    aco_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    aco_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_3[8,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_3[9,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)
table_3[10,3]=round((length(which(aco_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(aco_mode_TOBACCO_USER_list))*100,0)

#copd
copd_social=cohort_all_social[cohort_all_social$RANDOM_PAT_ID %in% copd_high,]
copd_social=copd_social[!(is.na(copd_social$TOBACCO_USER)),] #remove patients with NA's in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "",] #remove patients with "" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Not Asked",] #remove patients with "Not Asked" in the TOBACCO_USER column
copd_social=copd_social[copd_social$TOBACCO_USER != "Passive",] #remove patients with "Passive" in the TOBACCO_USER column
unique_pats=unique(copd_social$RANDOM_PAT_ID)
copd_mode_TOBACCO_USER_list=as.data.frame(matrix(nrow=length(unique_pats), ncol=1))
for(pat in 1:length(unique_pats)){ #takes forever!
  encounters=copd_social[copd_social$RANDOM_PAT_ID == unique_pats[pat],]
  if(nrow(encounters)==0){
    copd_mode_TOBACCO_USER_list[pat,1]=NA
  }else{
    copd_mode_TOBACCO_USER_list[pat,1]=names(which.max(table(encounters$TOBACCO_USER)))
  }
}
table_3[8,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Never"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_3[9,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Yes"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)
table_3[10,4]=round((length(which(copd_mode_TOBACCO_USER_list[,1] == "Quit"))/nrow(copd_mode_TOBACCO_USER_list))*100,0)

#chi-sq
a=cbind(asthma_mode_TOBACCO_USER_list, rep(1, nrow(asthma_mode_TOBACCO_USER_list)))
colnames(a)=c("tobacco_user", "groups")
b=cbind(aco_mode_TOBACCO_USER_list, rep(2, nrow(aco_mode_TOBACCO_USER_list)))
colnames(b)=c("tobacco_user", "groups")
c=cbind(copd_mode_TOBACCO_USER_list, rep(3, nrow(copd_mode_TOBACCO_USER_list)))
colnames(c)=c("tobacco_user", "groups")
tobacco_user_chi_table=rbind(a,b,c)
tobacco_user_chi_table=as.data.frame(tobacco_user_chi_table)
#table(tobacco_user_chi_table$race, tobacco_user_chi_table$groups)
tobacco_user_chi=chisq.test(tobacco_user_chi_table$tobacco_user, tobacco_user_chi_table$groups)
if(tobacco_user_chi$p.value < 0.05){
  table_3[7,5]="p<0.05"
}else{
  table_3[7,5]=paste0("p=", k[[1]][["Pr(>F)"]][[1]])
}


# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  #dat <- c(m1-m2, se, t, 2*pt(-abs(t),df)) 
  p.only=2*pt(-abs(t),df)
  #names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  #return(dat) 
  return(p.only)
}

#asthma age
m1=38
m2=39
s1=16.3
s2=15.8
n1=8693
n2=889
t.test2(m1,m2,s1,s2,n1,n2)

#aco age
m1=57
m2=57
s1=13.6
s2=13.4
n1=701
n2=83
t.test2(m1,m2,s1,s2,n1,n2)

#copd age
m1=63
m2=62
s1=13.1
s2=12.6
n1=2047
n2=178
t.test2(m1,m2,s1,s2,n1,n2)

#asthma sex
prop.test(matrix(c(as.numeric(table_2[2,2])/100*8693, 
                   (1-as.numeric(table_2[2,2])/100)*8693, 
                   as.numeric(table_3[2,2])/100*889, 
                   (1-as.numeric(table_3[2,2])/100)*889), 
                 nrow=2, ncol=2, byrow=T))$p.value

#aco sex
prop.test(matrix(c(as.numeric(table_2[2,3])/100*701, 
                   (1-as.numeric(table_2[2,3])/100)*701, 
                   as.numeric(table_3[2,3])/100*83, 
                   (1-as.numeric(table_3[2,3])/100)*83), 
                 nrow=2, ncol=2, byrow=T))$p.value
#copd sex
prop.test(matrix(c(as.numeric(table_2[2,4])/100*2047, 
                   (1-as.numeric(table_2[2,4])/100)*2047, 
                   as.numeric(table_3[2,4])/100*178, 
                   (1-as.numeric(table_3[2,4])/100)*178), 
                 nrow=2, ncol=2, byrow=T))$p.value


#asthma race
prop.test(matrix(c(as.numeric(table_2[4,2])/100*8693, 
                   as.numeric(table_2[5,2])/100*8693,
                   as.numeric(table_2[6,2])/100*8693,
                   as.numeric(table_3[4,2])/100*889,
                   as.numeric(table_3[5,2])/100*889,
                   as.numeric(table_3[6,2])/100*889), 
                 nrow=3, ncol=2, byrow=F))$p.value

#aco race
prop.test(matrix(c(as.numeric(table_2[4,3])/100*701, 
                   as.numeric(table_2[5,3])/100*701,
                   as.numeric(table_2[6,3])/100*701,
                   as.numeric(table_3[4,3])/100*83,
                   as.numeric(table_3[5,3])/100*83,
                   as.numeric(table_3[6,3])/100*83), 
                 nrow=3, ncol=2, byrow=F))$p.value

#copd race
prop.test(matrix(c(as.numeric(table_2[4,4])/100*2047, 
                   as.numeric(table_2[5,4])/100*2074,
                   as.numeric(table_2[6,4])/100*2074,
                   as.numeric(table_3[4,4])/100*178,
                   as.numeric(table_3[5,4])/100*178,
                   as.numeric(table_3[6,4])/100*178), 
                 nrow=3, ncol=2, byrow=F))$p.value

#asthma smoking
prop.test(matrix(c(as.numeric(table_2[8,2])/100*8693, 
                   as.numeric(table_2[9,2])/100*8693,
                   as.numeric(table_2[10,2])/100*8693,
                   as.numeric(table_3[8,2])/100*889,
                   as.numeric(table_3[9,2])/100*889,
                   as.numeric(table_3[10,2])/100*889), 
                 nrow=3, ncol=2, byrow=F))$p.value

#aco smoking
prop.test(matrix(c(as.numeric(table_2[8,3])/100*701, 
                   as.numeric(table_2[9,3])/100*701,
                   as.numeric(table_2[10,3])/100*701,
                   as.numeric(table_3[8,3])/100*83,
                   as.numeric(table_3[9,3])/100*83,
                   as.numeric(table_3[10,3])/100*83), 
                 nrow=3, ncol=2, byrow=F))$p.value

#copd smoking
prop.test(matrix(c(as.numeric(table_2[8,4])/100*2047, 
                   as.numeric(table_2[9,4])/100*2074,
                   as.numeric(table_2[10,4])/100*2074,
                   as.numeric(table_3[8,4])/100*178,
                   as.numeric(table_3[9,4])/100*178,
                   as.numeric(table_3[10,4])/100*178), 
                 nrow=3, ncol=2, byrow=F))$p.value

#########

#Break down by disease only

##ALL

asthma=c(asthma_eos_ABS_L150, asthma_eos_ABS_150_199, asthma_eos_ABS_200_299, asthma_eos_ABS_300_399, asthma_eos_ABS_G400)
aco=c(aco_eos_ABS_L150, aco_eos_ABS_150_199, aco_eos_ABS_200_299, aco_eos_ABS_300_399, aco_eos_ABS_G400)
copd=c(copd_eos_ABS_L150, copd_eos_ABS_150_199, copd_eos_ABS_200_299, copd_eos_ABS_300_399, copd_eos_ABS_G400)

#List of populations to loop over
list_of_names=c("asthma", 
                "aco", 
                "copd") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9[i,]=apply(kitty,2,sum)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10[i,]=apply(kitty,2,sum)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9)=list_of_names
colnames(table_of_sums_ICD9)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10)=list_of_names
colnames(table_of_sums_ICD10)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#get N for each group
asthma_n=length(asthma)

aco_n=length(aco)

copd_n=length(copd)

#get sums and sds for each group
asthma_sum_ICD9 <- as.numeric(table_of_sums_ICD9[1,1:ncol(table_of_sums_ICD9)])
aco_sum_ICD9 <- as.numeric(table_of_sums_ICD9[2,1:ncol(table_of_sums_ICD9)])
copd_sum_ICD9 <- as.numeric(table_of_sums_ICD9[3,1:ncol(table_of_sums_ICD9)])
asthma_sum_ICD10 <- as.numeric(table_of_sums_ICD10[1,1:ncol(table_of_sums_ICD10)])
aco_sum_ICD10 <- as.numeric(table_of_sums_ICD10[2,1:ncol(table_of_sums_ICD10)])
copd_sum_ICD10 <- as.numeric(table_of_sums_ICD10[3,1:ncol(table_of_sums_ICD10)])

proportion_table_ICD9=cbind(asthma_sum_ICD9, asthma_n-asthma_sum_ICD9, aco_sum_ICD9, aco_n-aco_sum_ICD9, copd_sum_ICD9, copd_n-copd_sum_ICD9, rep(NA, length(asthma_sum_ICD9)))
for(i in 1:nrow(proportion_table_ICD9)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD9[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD9[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD9)=colnames(table_of_sums_ICD9)
colnames(proportion_table_ICD9)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD9),proportion_table_ICD9), paste0(location, "p_values_ICD9_all.txt"), sep="\t", row.names=F)

proportion_table_ICD10=cbind(asthma_sum_ICD10, asthma_n-asthma_sum_ICD10, aco_sum_ICD10, aco_n-aco_sum_ICD10, copd_sum_ICD10, copd_n-copd_sum_ICD10, rep(NA, length(asthma_sum_ICD10)))
for(i in 1:nrow(proportion_table_ICD10)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD10[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD10[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD10)=colnames(table_of_sums_ICD10)
colnames(proportion_table_ICD10)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD10),proportion_table_ICD10), paste0(location, "p_values_ICD10_all.txt"), sep="\t", row.names=F)


#calculate and save significantly different diagnoses (Bonferoni correction)
proportion_table_ICD9_bon=proportion_table_ICD9
proportion_table_ICD9_bon[,7]=proportion_table_ICD9[,7]*nrow(proportion_table_ICD9)
write.table(data.frame("Groups"=rownames(proportion_table_ICD9_bon),proportion_table_ICD9_bon), paste0(location, "p_values_ICD9_all_bon.txt"), sep="\t", row.names=F)
proportion_table_ICD10_bon=proportion_table_ICD10
proportion_table_ICD10_bon[,7]=proportion_table_ICD10[,7]*nrow(proportion_table_ICD10)
write.table(data.frame("Groups"=rownames(proportion_table_ICD10_bon),proportion_table_ICD10_bon), paste0(location, "p_values_ICD10_all_bon.txt"), sep="\t", row.names=F)

##MALES

asthma_M=c(intersect(asthma_eos_ABS_L150, males), intersect(asthma_eos_ABS_150_199, males), intersect(asthma_eos_ABS_200_299, males), intersect(asthma_eos_ABS_300_399, males), intersect(asthma_eos_ABS_G400, males))
aco_M=c(intersect(aco_eos_ABS_L150, males), intersect(aco_eos_ABS_150_199, males), intersect(aco_eos_ABS_200_299, males), intersect(aco_eos_ABS_300_399, males), intersect(aco_eos_ABS_G400, males))
copd_M=c(intersect(copd_eos_ABS_L150, males), intersect(copd_eos_ABS_150_199, males), intersect(copd_eos_ABS_200_299, males), intersect(copd_eos_ABS_300_399,males), intersect(copd_eos_ABS_G400, males))

#List of populations to loop over
list_of_names=c("asthma_M", 
                "aco_M", 
                "copd_M") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10_M=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9_M[i,]=apply(kitty,2,sum)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10_M[i,]=apply(kitty,2,sum)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9_M)=list_of_names
colnames(table_of_sums_ICD9_M)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10_M)=list_of_names
colnames(table_of_sums_ICD10_M)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#get N for each group
asthma_n_M=length(asthma_M)

aco_n_M=length(aco_M)

copd_n_M=length(copd_M)

#get sums and sds for each group
asthma_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[1,1:ncol(table_of_sums_ICD9_M)])
aco_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[2,1:ncol(table_of_sums_ICD9_M)])
copd_sum_ICD9_M <- as.numeric(table_of_sums_ICD9_M[3,1:ncol(table_of_sums_ICD9_M)])
asthma_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[1,1:ncol(table_of_sums_ICD10_M)])
aco_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[2,1:ncol(table_of_sums_ICD10_M)])
copd_sum_ICD10_M <- as.numeric(table_of_sums_ICD10_M[3,1:ncol(table_of_sums_ICD10_M)])

proportion_table_ICD9_M=cbind(asthma_sum_ICD9_M, asthma_n_M-asthma_sum_ICD9_M, aco_sum_ICD9_M, aco_n_M-aco_sum_ICD9_M, copd_sum_ICD9_M, copd_n_M-copd_sum_ICD9_M, rep(NA, length(asthma_sum_ICD9_M)))
for(i in 1:nrow(proportion_table_ICD9_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD9_M[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD9_M[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD9_M)=colnames(table_of_sums_ICD9_M)
colnames(proportion_table_ICD9_M)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD9_M),proportion_table_ICD9_M), paste0(location, "p_values_ICD9_all_M.txt"), sep="\t", row.names=F)

proportion_table_ICD10_M=cbind(asthma_sum_ICD10_M, asthma_n_M-asthma_sum_ICD10_M, aco_sum_ICD10_M, aco_n_M-aco_sum_ICD10_M, copd_sum_ICD10_M, copd_n_M-copd_sum_ICD10_M, rep(NA, length(asthma_sum_ICD10_M)))
for(i in 1:nrow(proportion_table_ICD10_M)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD10_M[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD10_M[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD10_M)=colnames(table_of_sums_ICD10_M)
colnames(proportion_table_ICD10_M)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD10_M),proportion_table_ICD10_M), paste0(location, "p_values_ICD10_all_M.txt"), sep="\t", row.names=F)


#calculate and save significantly different diagnoses (Bonferoni correction)
proportion_table_ICD9_bon_M=proportion_table_ICD9_M
proportion_table_ICD9_bon_M[,7]=proportion_table_ICD9_M[,7]*nrow(proportion_table_ICD9_M)
write.table(data.frame("Groups"=rownames(proportion_table_ICD9_bon_M),proportion_table_ICD9_bon_M), paste0(location, "p_values_ICD9_all_bon_M.txt"), sep="\t", row.names=F)
proportion_table_ICD10_bon_M=proportion_table_ICD10_M
proportion_table_ICD10_bon_M[,7]=proportion_table_ICD10_M[,7]*nrow(proportion_table_ICD10_M)
write.table(data.frame("Groups"=rownames(proportion_table_ICD10_bon_M),proportion_table_ICD10_bon_M), paste0(location, "p_values_ICD10_all_bon_M.txt"), sep="\t", row.names=F)



##FEMALES

asthma_F=c(intersect(asthma_eos_ABS_L150, females), intersect(asthma_eos_ABS_150_199, females), intersect(asthma_eos_ABS_200_299, females), intersect(asthma_eos_ABS_300_399, females), intersect(asthma_eos_ABS_G400, females))
aco_F=c(intersect(aco_eos_ABS_L150, females), intersect(aco_eos_ABS_150_199, females), intersect(aco_eos_ABS_200_299, females), intersect(aco_eos_ABS_300_399, females), intersect(aco_eos_ABS_G400, females))
copd_F=c(intersect(copd_eos_ABS_L150, females), intersect(copd_eos_ABS_150_199, females), intersect(copd_eos_ABS_200_299, females), intersect(copd_eos_ABS_300_399,females), intersect(copd_eos_ABS_G400, females))

#List of populations to loop over
list_of_names=c("asthma_F", 
                "aco_F", 
                "copd_F") 

#Tables to store sums and sds of each diagnosis in the populations
table_of_sums_ICD9_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME))))
table_of_sums_ICD10_F=as.data.frame(matrix(nrow=length(list_of_names), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME))))

#function to populate the sparse matrix
note_patient_diagnoses <- function(pat_idx, all_diags, patient_ids) {
  diags=unique(all_diags[which(all_diags$RANDOM_PAT_ID == patient_ids[pat_idx]),5])
  if (length(diags) > 0) kitty[pat_idx,colnames(kitty) %in% diags] <<- 1
  invisible(NULL)
}

#note the diagnoses and update the sparse matrices
for(i in 1:length(list_of_names)){
  print(i)
  pt_list <- get(list_of_names[i])
  #ICD-9
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD9$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD9$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD9, pt_list)
  table_of_sums_ICD9_F[i,]=apply(kitty,2,sum)
  
  #ICD-10
  kitty=Matrix(0, nrow=length(pt_list), ncol=length(unique(new_cohort_all_diagnosis_ICD10$NAME)), dimnames=list(pt_list, unique(new_cohort_all_diagnosis_ICD10$NAME)), sparse=T)
  sapply(1:length(pt_list), note_patient_diagnoses, new_cohort_all_diagnosis_ICD10, pt_list)
  table_of_sums_ICD10_F[i,]=apply(kitty,2,sum)
  
}

#correct the row and column names of these matrices
row.names(table_of_sums_ICD9_F)=list_of_names
colnames(table_of_sums_ICD9_F)=unique(new_cohort_all_diagnosis_ICD9$NAME)
row.names(table_of_sums_ICD10_F)=list_of_names
colnames(table_of_sums_ICD10_F)=unique(new_cohort_all_diagnosis_ICD10$NAME)

#get N for each group
asthma_n_F=length(asthma_F)

aco_n_F=length(aco_F)

copd_n_F=length(copd_F)

#get sums and sds for each group
asthma_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[1,1:ncol(table_of_sums_ICD9_F)])
aco_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[2,1:ncol(table_of_sums_ICD9_F)])
copd_sum_ICD9_F <- as.numeric(table_of_sums_ICD9_F[3,1:ncol(table_of_sums_ICD9_F)])
asthma_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[1,1:ncol(table_of_sums_ICD10_F)])
aco_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[2,1:ncol(table_of_sums_ICD10_F)])
copd_sum_ICD10_F <- as.numeric(table_of_sums_ICD10_F[3,1:ncol(table_of_sums_ICD10_F)])

proportion_table_ICD9_F=cbind(asthma_sum_ICD9_F, asthma_n_F-asthma_sum_ICD9_F, aco_sum_ICD9_F, aco_n_F-aco_sum_ICD9_F, copd_sum_ICD9_F, copd_n_F-copd_sum_ICD9_F, rep(NA, length(asthma_sum_ICD9_F)))
for(i in 1:nrow(proportion_table_ICD9_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD9_F[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD9_F[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD9_F)=colnames(table_of_sums_ICD9_F)
colnames(proportion_table_ICD9_F)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD9_F),proportion_table_ICD9_F), paste0(location, "p_values_ICD9_all_F.txt"), sep="\t", row.names=F)

proportion_table_ICD10_F=cbind(asthma_sum_ICD10_F, asthma_n_F-asthma_sum_ICD10_F, aco_sum_ICD10_F, aco_n_F-aco_sum_ICD10_F, copd_sum_ICD10_F, copd_n_F-copd_sum_ICD10_F, rep(NA, length(asthma_sum_ICD10_F)))
for(i in 1:nrow(proportion_table_ICD10_F)){
  if(i %% 100 == 0){
    print(i)
  }
  temp=matrix(proportion_table_ICD10_F[i,1:6], nrow=3, ncol=2, byrow=T)
  temp2=as.table(temp)
  proportion_table_ICD10_F[i,7]=prop.test(temp2)$p.value
}

row.names(proportion_table_ICD10_F)=colnames(table_of_sums_ICD10_F)
colnames(proportion_table_ICD10_F)=c("Yes_DX_asthma", "No_DX_asthma", "Yes_DX_acos", "No_DX_acos", "Yes_DX_copd", "No_DX_copd", "p-value")
write.table(data.frame("Groups"=rownames(proportion_table_ICD10_F),proportion_table_ICD10_F), paste0(location, "p_values_ICD10_all_F.txt"), sep="\t", row.names=F)


#calculate and save significantly different diagnoses (Bonferoni correction)
proportion_table_ICD9_bon_F=proportion_table_ICD9_F
proportion_table_ICD9_bon_F[,7]=proportion_table_ICD9_F[,7]*nrow(proportion_table_ICD9_F)
write.table(data.frame("Groups"=rownames(proportion_table_ICD9_bon_F),proportion_table_ICD9_bon_F), paste0(location, "p_values_ICD9_all_bon_F.txt"), sep="\t", row.names=F)
proportion_table_ICD10_bon_F=proportion_table_ICD10_F
proportion_table_ICD10_bon_F[,7]=proportion_table_ICD10_F[,7]*nrow(proportion_table_ICD10_F)
write.table(data.frame("Groups"=rownames(proportion_table_ICD10_bon_F),proportion_table_ICD10_bon_F), paste0(location, "p_values_ICD10_all_bon_F.txt"), sep="\t", row.names=F)






proportion_table_ICD9_bon[which(is.nan(proportion_table_ICD9_bon[,7])),7]<-1
proportion_table_ICD10_bon[which(is.nan(proportion_table_ICD10_bon[,7])),7]<-1
proportion_table_ICD9_bon_M[which(is.nan(proportion_table_ICD9_bon_M[,7])),7]<-1
proportion_table_ICD10_bon_M[which(is.nan(proportion_table_ICD10_bon_M[,7])),7]<-1
proportion_table_ICD9_bon_F[which(is.nan(proportion_table_ICD9_bon_F[,7])),7]<-1
proportion_table_ICD10_bon_F[which(is.nan(proportion_table_ICD10_bon_F[,7])),7]<-1

M_F_intersect_names_ICD9=intersect(row.names(proportion_table_ICD9_bon_M[which(proportion_table_ICD9_bon_M[,7] < 0.05),]),row.names(proportion_table_ICD9_bon_F[which(proportion_table_ICD9_bon_F[,7] < 0.05),]))
M_F_ICD9=proportion_table_ICD9_bon[(row.names(proportion_table_ICD9_bon) %in% M_F_intersect_names_ICD9),]
M_F_intersect_names_ICD10=intersect(row.names(proportion_table_ICD10_bon_M[which(proportion_table_ICD10_bon_M[,7] < 0.05),]),row.names(proportion_table_ICD10_bon_F[which(proportion_table_ICD10_bon_F[,7] < 0.05),]))
M_F_ICD10=proportion_table_ICD10_bon[(row.names(proportion_table_ICD10_bon) %in% M_F_intersect_names_ICD10),]
write.table(data.frame("DX"=rownames(M_F_ICD9),M_F_ICD9), paste0(location, "M_F_ICD9.txt"), sep="\t", row.names=F)
write.table(data.frame("DX"=rownames(M_F_ICD10),M_F_ICD10), paste0(location, "M_F_ICD10.txt"), sep="\t", row.names=F)

M_F_ICD9_limited=M_F_ICD9[-grep("asthma|Asthma|COPD|copd|Copd|ACO|ACOS|acos|aco|Aco|Acos|Chronic airway obstruction|Chronic obstructive pulmonary disease", row.names(M_F_ICD9)),]
M_F_ICD10_limited=M_F_ICD10[-grep("asthma|Asthma|COPD|copd|Copd|ACO|ACOS|acos|aco|Aco|Acos|Chronic airway obstruction|Chronic obstructive pulmonary disease", row.names(M_F_ICD10)),]
write.table(data.frame("DX"=rownames(M_F_ICD9_limited),M_F_ICD9_limited), paste0(location, "M_F_ICD9_limited.txt"), sep="\t", row.names=F)
write.table(data.frame("DX"=rownames(M_F_ICD10_limited),M_F_ICD10_limited), paste0(location, "M_F_ICD10_limited.txt"), sep="\t", row.names=F)

