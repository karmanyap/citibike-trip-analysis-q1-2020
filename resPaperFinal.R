install.packages("dplyr")
install.packages("ggplot2",repos = "http://cran.us.r-project.org")

library(readr)
library(ggplot2)
library(dplyr)
"load all 2011 to 2018 inerstate accident merged data"

accidentaux2016 = read_csv("./CRS2016/ACC_AUX.csv")
persons2016 = read_csv("./CRS2016/PERSON.csv")
accident2016 = read_csv("./CRS2016/ACCIDENT.CSV")
vehicle2016 = read_csv("./CRS2016/VEHICLE.csv")


accidentaux2017 = read_csv("./CRS2017/ACC_AUX.csv")
persons2017 = read_csv("./CRS2017/PERSON.csv")
accident2017 = read_csv("./CRS2017/ACCIDENT.CSV")
vehicle2017 = read_csv("./CRS2017/VEHICLE.csv")


accidentaux2018 = read_csv("./CRS2018/ACC_AUX.csv")
persons2018 = read_csv("./CRS2018/PERSON.csv")
accident2018 = read_csv("./CRS2018/ACCIDENT.CSV")
vehicle2018 = read_csv("./CRS2018/VEHICLE.csv")

"data cleanup"

acc2016CuriousFields = accident2016 %>%
  inner_join(vehicle2016, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2016, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1) %>%
  select(
    HOUR_acc, YEAR, LGT_COND, WEATHER, MAN_COLL, HARM_EV, CF1, CF2, M_HARM, DR_SF1, DR_SF2, AGE, INJ_SEV, DRINKING, DRUGS, HOSPITAL, P_SF1, P_SF2, P_SF3
  )
acc2017CuriousFields = accident2017 %>%
  inner_join(vehicle2017, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2017, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1) %>%
  select(
    HOUR_acc, YEAR, LGT_COND, WEATHER, MAN_COLL, HARM_EV, CF1, CF2, M_HARM, DR_SF1, DR_SF2, AGE, INJ_SEV, DRINKING, DRUGS, HOSPITAL, P_SF1, P_SF2, P_SF3
  )

acc2018CuriousFields = accident2018 %>%
  inner_join(vehicle2018, by="CASENUM", suffix=c("_acc","_veh")) %>% 
  inner_join(persons2018, by=c("CASENUM"="CASENUM", "VEH_NO"="VEH_NO"), suffix=c("_acc","_per")) %>%
  filter(PER_TYP == 1, VEH_NO == 1) %>%
  select(
    HOUR_acc, YEAR, LGT_COND, WEATHER, MAN_COLL, HARM_EV, CF1, CF2, M_HARM, DR_SF1, DR_SF2, AGE, INJ_SEV, DRINKING, DRUGS, HOSPITAL, P_SF1, P_SF2, P_SF3
  )

allYearAcc = rbind(acc2016CuriousFields, acc2017CuriousFields, acc2018CuriousFields)


allYearAcc
