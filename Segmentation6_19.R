SfN.Membership.Segmentation.v2 <- read.csv("~/Downloads/SfN Membership Segmentation v2.csv")
View(SfN.Membership.Segmentation.v2)
attach(SfN.Membership.Segmentation.v2)
table(member_type_category)

#Remove STudents!
#Create factors: Non-Dues Revenue, Activities, Author, Email Open Rate for time 1/1/18 thru 6/1/19.  Students removed from study.
#Non-Dues Revenue = Sum of non-dues spend in time frame
#Activities = count of meetings, committes, speaker, exhibitor, INXPO, Volunteer, Donor
#Author = count of manuscript submissions, Abstract submissions
#Email = Email open rate

SfN.Membership.Segmentation.v2$student<-ifelse(SfN.Membership.Segmentation.v2$member_type_category=="Student"|SfN.Membership.Segmentation.v2$member_type_category=="Undergrad",1,0)
attach(segmentation)
View(segmentation)
table(SfN.Membership.Segmentation.v2$student)
SfN.Membership.Segmentation.v2<-subset(SfN.Membership.Segmentation.v2,SfN.Membership.Segmentation.v2$student==0)
attach(segmentation)
View(segmentation)


names(SfN.Membership.Segmentation.v2)
keepvars<-c("individual_key","nondues_revenue","nondues_count","donor_revenue","donor_count","count_manuscript","count_meeting","count_inxpo","count_abstract","count_award","count_speaker","count_committee", "count_exhibitor" ,"count_volunteer","count_open","count_click")
segmentation<-SfN.Membership.Segmentation.v2[keepvars]
attach(segmentation)
View(segmentation)
names(segmentation)
detach(SfN.Membership.Segmentation.v2)
segmentation$nondues_revenue[is.na(segmentation$nondues_revenue)]<-0
segmentation$nondues_count[is.na(segmentation$nondues_count)]<-0
segmentation$donor_revenue[is.na(segmentation$donor_revenue)]<-0
segmentation$donor_count[is.na(segmentation$donor_count)]<-0
segmentation$count_manuscript[is.na(segmentation$count_manuscript)]<-0
segmentation$count_meeting[is.na(segmentation$count_meeting)]<-0
segmentation$count_inxpo[is.na(segmentation$count_inxpo)]<-0
segmentation$count_abstract[is.na(segmentation$count_abstract)]<-0
segmentation$count_award[is.na(segmentation$count_award)]<-0
segmentation$count_speaker[is.na(segmentation$count_speaker)]<-0
segmentation$count_committee[is.na(segmentation$count_committee)]<-0
segmentation$count_exhibitor[is.na(segmentation$count_exhibitor)]<-0
segmentation$count_volunteer[is.na(segmentation$count_volunteer)]<-0
segmentation$count_open[is.na(segmentation$count_open)]<-0
segmentation$count_click[is.na(segmentation$count_click)]<-0
attach(segmentation)
View(segmentation)

# Activities == get 1 point for each in 2 years; max is 7 = V1_activities
segmentation$donor<-ifelse(segmentation$donor_count>1,1,0)
segmentation$meeting<-ifelse(segmentation$count_meeting>1,1,0)
segmentation$inxpo<-ifelse(segmentation$count_inxpo>0,1,0)
segmentation$speaker<-ifelse(segmentation$count_speaker>0,1,0)
segmentation$committee<-ifelse(segmentation$count_committee>0,1,0)
segmentation$volunteer<-ifelse(segmentation$count_volunteer>0,1,0)
segmentation$exhibitor<-ifelse(segmentation$count_exhibitor>0,1,0)
attach(segmentation)
View(segmentation)
library(psych)
describe(segmentation)
segmentation$V1_activities<-(segmentation$donor+segmentation$meeting+segmentation$inxpo+segmentation$speaker+segmentation$committee+segmentation$volunteer+segmentation$exhibitor)
attach(segmentation)
View(segmentation)
names(segmentation)

# Author activity == get 1 point for each in 2 years; max is 2 = V2_author
segmentation$manuscript<-ifelse(segmentation$count_manuscript>0,1,0)
segmentation$abstract<-ifelse(segmentation$count_abstract>0,1,0)
attach(segmentation)
View(segmentation)
segmentation$V2_author<-segmentation$manuscript+segmentation$abstract
attach(segmentation)
View(segmentation)
describe(segmentation)

# Non-dues Revenue == sum of revenue during time period V3_Nondues_rev
segmentation$V3_Nondues_rev<-segmentation$nondues_revenue
attach(segmentation)
describe(segmentation)

#email click rate = V4_email need emails sent to create open ratio

segmentation$V4_email<-segmentation$count_click/segmentation$count_open
segmentation$V4_email[segmentation$count_open==0] <-0
attach(segmentation)
View(segmentation)
describe(segmentation)

#Standardize variables 
describe(Segments)
#    vars                 n           mean         sd      
#V1_activities      24 18781         0.47        0.81          
#V2_author          27 18781         0.61        0.66        
#V3_Nondues_rev     28 23912       366.04      450.27              
#V4_email           29 23912         0.17        0.38         

segmentation$S1_activities<-(segmentation$V1_activities-.47)/.81
segmentation$S1_author<-(segmentation$V2_author-0.61)/0.66
segmentation$S1_Nondues_rev<-(segmentation$V3_Nondues_rev-366.04)/450.27
segmentation$S1_email<-(segmentation$V4_email-.17)/.38
attach(segmentation)
View(segmentation)
summary(segmentation$S1_activities)
summary(segmentation$S1_author)
summary(segmentation$S1_Nondues_rev)
summary(segmentation$S1_email)

#> summary(segmentation$S1_activities)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.580200 -0.580200 -0.580200 -0.005132  0.654300  5.593000 
#> summary(segmentation$S1_author)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-0.924200 -0.924200  0.590900 -0.001566  0.590900  2.106000 
#> summary(segmentation$S1_Nondues_rev)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.81290 -0.81290 -0.14670 -0.00001  0.50850 39.52000 
#> summary(segmentation$S1_email)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.44740 -0.44740 -0.32590  0.01251  0.01703 28.50000 

#remove extreme values (+/-3)
#Remove Extreme Values (Area of judgement)
cluster_file<-subset(segmentation,S1_activities<3&S1_author<3&S1_Nondues_rev<3&S1_email<3)
attach(cluster_file)
summary(cluster_file)

write.csv(segmentation, file = "segmentation.csv")
write.csv(cluster_file, file = "cluster_file.csv")

#run cluster analysis
names(cluster_file)
keepvars<-c("S1_activities" ,   "S1_author"  ,      "S1_Nondues_rev" ,  "S1_email" ) 
cluster_file<-cluster_file[keepvars]
attach(cluster_file)
View(cluster_file) 

set.seed(1234)
library(NbClust)
fit.km<-kmeans(cluster_file,4,nstart = 25) #nstart=25 will generate 25 initial configurations
fit.km$size
aggregate(cluster_file,by=list(cluster=fit.km$cluster),mean)

#> fit.km$size
#[1] 3409 4097 7261 1337 1728
#> aggregate(cluster_file,by=list(cluster=fit.km$cluster),mean)
#cluster S1_activities  S1_author S1_Nondues_rev    S1_email
#1       1     0.8516925  0.2113434      1.0723918 -0.15665521
#2       2    -0.5118440  0.5983055     -0.1692358 -0.20619610
#3       3    -0.4869020 -0.9242424     -0.6210006 -0.24537285
#4       4    -0.3041543 -0.2964235     -0.2883095  1.99287078
#5       5     0.2199360  2.1060606      0.7338985 -0.06548493


#> fit.km$size
#[1] 4240 1376 4882 7334
#> aggregate(cluster_file,by=list(cluster=fit.km$cluster),mean)
#cluster S1_activities  S1_author S1_Nondues_rev   S1_email
#1       1     0.8546471  0.6266438      1.1090271 -0.1395972
#2       2    -0.3030075 -0.2195208     -0.2555735  2.0030542
#3       3    -0.5058997  0.8453999     -0.1067530 -0.2037309
#4       4    -0.4762160 -0.9242424     -0.6126796 -0.2425499




