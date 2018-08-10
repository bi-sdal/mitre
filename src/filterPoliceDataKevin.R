library(gdata)
library(dplyr)
library(sqldf)

#must be working out of wrong directory; had to load file and run it directly.
#source("./src/Police/geocode.r")

#
# Explore the police data 
#

# for now, trying to create 2 data objects with 3 counts: 
#one for domestic violence events - including counts of children, adults and elders 
#one for assaults of with victim at same address as arrestee/suspect, - victim counts of children, adults and elders 


dataDir = '/home/sdal/projects/arl/arlington911/data/original/police/aug082016/'

dispatch = read.xls(sprintf("%scfs2015.xls", dataDir))

table(dispatch$Original_Call)

#ASLT (assault), ASLTI (injuries), ASLTIP (in progress), ASLTJO (just occured), ASLTW (not in codebook?)
dispatchAssault = filter(dispatch,  substr(Original_Call,1,4) == "ASLT")
reportNumbersAssault = na.omit(dispatchAssault$Report_No)

#test2
#just domestic violence calls
dispatchDOME = filter(dispatch, Original_Call == "DOME")
reportNumbersDOME = na.omit(dispatchDOME$Report_No)

#head(dispatchDOME)
#summary(dispatchDOME)

# one record noted that it was a duplicate of P15001007; seeing if that record is also in set
#dispatchDOME[dispatchDOME$Call_No=="P15001007",] 
#not found

inper = read.xls(sprintf("%sinmast-inper62015.xls", dataDir))

inperAssault = filter(inper, Report_No %in% reportNumbersAssault)
inperAssault$Address_Geocoded <- google_geocode(inperAssault$Address)

table(inperAssault$Involvement)

inperDOME = filter(inper, Report_No %in% reportNumbersDOME)

table(inperDOME$Involvement)

#CDC defines elder abuse as victim being 60 or older, being cared for; a few cases where victim 60+ and suspect/arrestee nearly same age (partner?)
#https://www.cdc.gov/violenceprevention/elderabuse/definitions.html
#creating 3 buckets for victims: child (age>0 and age<18), adult (age >=18 and age <60), elder (age >=60)
table(inperDOME$Age)

#should we include VTM (victim-non NIBRS national incident based reporting system? )
#creating new fields to count victims, victim age, count perps, perp age

#sqldf is not the most efficient, yet the number of records are tiny and 
#this approach may be easy to edit for future tables/variables/Ages/Involvement codes than more elegant functions

#could do this in one big query, but doing in 2 steps to check interim output

#all 3 age groups of victims
DOMEvictimCount <- sqldf("select Report_No,  Involvement,
                          case 
                              when (Involvement = 'VIC' and Age > 0 and Age < 18) then 1
                              else 0 
                          end as child_victim_count,
                          case 
                              when (Involvement = 'VIC' and Age >=18 and Age < 60) then 1
                              else 0 
                          end as adult_victim_count,
                          case 
                              when (Involvement = 'VIC' and Age >=60) then 1
                              else 0 
                          end as elder_victim_count,
                          case
                            when (Involvement = 'VIC' and Age = 0) then 1
                                  else 0
                          end as unknown_age_victim_count,
                          case 
                              when (Involvement = 'VIC' and Age > 0 and Age < 18) then Age
                              else 0 
                          end as child_victim_age,
                          case 
                              when (Involvement = 'VIC' and Age >=18 and Age < 60) then Age
                              else 0 
                          end as adult_victim_age,
                          case 
                              when (Involvement = 'VIC' and Age >=60) then Age
                              else 0 
                          end as elder_victim_age,
                          case 
                              when (Involvement = 'ARR' OR Involvement = 'SUS') then 1
                              else 0 
                          end as perp_count,
                          case 
                              when (Involvement = 'ARR' OR Involvement = 'SUS') then Age
                              else 0
                          end as perp_age
                              from inperDOME 
                              where Involvement in ('VIC', 'SUS', 'ARR') ")

#group by Report_No to total up victims and take max age from each group
DOMEvictimAgg <- sqldf('select Report_No, sum(child_victim_count) as child_victim_count, max(child_victim_age) as max_child_victim_age,
                                          sum(adult_victim_count) as adult_victim_count, max(adult_victim_age) as max_adult_victim_age,
                                          sum(elder_victim_count) as elder_victim_count, max(elder_victim_age) as max_elder_victim_age,
                                          sum(unknown_age_victim_count) as unknown_age_victim_count, 
                                          sum(perp_count) as perp_count, max(perp_age) as max_perp_age
                                    from DOMEvictimCount
                                    group by Report_No')

DOMEvictimSummary <- colSums(DOMEvictimAgg[,c('child_victim_count','adult_victim_count', 'elder_victim_count')])
# 19 children, 251 adult, 11 elder (~3 elders could be switched to adult)

#looking at just potential elder abuse
DOMEelder <- DOMEvictimAgg[DOMEvictimAgg$elder_victim_count>0,]

DOMEelder %>% mutate(ElderVic2PerAgeDiff = max_elder_victim_age - max_perp_age)
#3 cases where age difference is small (<= 6 years, potential partner rather than caregiver)


DOMEfinal <- sqldf('select a.Report_No, child_victim_count, adult_victim_count, elder_victim_count, unknown_age_victim_count, b.Location  
                              from DOMEvictimAgg as a, dispatchDOME as b where a.Report_No = b.Report_No order by b.Location')

DOMEfinal


#want to consider Report_Nos where there were 2 people at the same household (at least one victim with that address and 1 perp with that address)
#150129033 report should show up
assaultAddressSimpleMatch <- sqldf("select Report_No, Address, count(Address) as address_count, 
                                   sum(Involvement = 'VIC') as victim_count, 
                                   sum((Involvement = 'VIC') * (Age >0 and Age <18)) as child_victim_count,
                                   sum((Involvement = 'VIC') * (Age >=18 and Age <60)) as adult_victim_count,
                                   sum((Involvement = 'VIC') * (Age >=60)) as elder_victim_count,
                                   sum((Involvement = 'VIC') * (Age =0 )) as unknown_age_victim_count,
                                   sum(Involvement = 'SUS' or Involvement = 'ARR') as perp_count
                                   from inperAssault 
                                   where Address not in ('', 'UNK', 'UNKNOWN', ' UNK', 'UNKNOWN', 'NO FIXED', ' NO FIXED')
                                   and Involvement in ('VIC', 'ARR', 'SUS')
                                   group by 1, 2 
                                   having address_count >= 2 and victim_count >= 1 and perp_count >= 1")

#only ~27 reports with assaults that could be a child abuse/IPV/elder abuse situation;

assaultASMsummary <- colSums(assaultAddressSimpleMatch[,c('child_victim_count','adult_victim_count', 'elder_victim_count')])
#11 children, 15 adult, 2 elder


ASSAULTfinal <- sqldf('select a.Report_No, child_victim_count, adult_victim_count, elder_victim_count, b.Location  
                              from assaultAddressSimpleMatch as a, dispatchAssault as b where a.Report_No = b.Report_No order by b.Location')

#final output - assault (27 locations, 28 victims)
ASSAULTfinal
DOMEfinal

