# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# load library
library(tidyverse)
library(readxl) # read_excel
library(janitor) # clean_names
# library(data.table) # melt
library(zoo) # na.locf
library(car) # recode

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_etal_2019/")
data_files = "data_files/"

# Load data ----

df_minc1 <- read_excel(paste0(data_files,"minc1.xlsx"), skip = 1)
df_minc4 <- read_excel(paste0(data_files,"minc4.xlsx"), skip = 1)

# Clean Minc1 data ----
df_minc1 <- clean_names(df_minc1) # janitor package

# site_code 
# 0 = Rural; 1 Winnipeg; 2 Dauphin
df_minc1 <- df_minc1 %>%
        filter(site_code < 3) # delete tabulations at the bottom of excel file

# discrepancy in coding of unemployment reason between minc1 and minc4
# 
# minc1:
# 0 = employed
# 17 = Has job but has not started working
# 18 = Too difficult to get to town (rural only)
# 19 = Wanted to help with family farm
# 
# minc4:
# 0 = JPL Note: there is no comparable code for employed, so I must create one later in the code
# 17 = Temporarily ill or disabled
# 18 = Permanently ill or disabled
# 19 = JPL Note: there is no comparable code for "wanted to help with the family farm"
# 
# therefore code 17-19 from minc1 into "other" reason

df_minc1 <- df_minc1 %>%
        select(famnum, site_code, agem, agef, famsize, mhnotlook, fhnotlook)

df_minc1$mhnotlook <- recode(df_minc1$mhnotlook,"17:19=19")
df_minc1$fhnotlook <- recode(df_minc1$fhnotlook,"17:19=19")

# Clean Minc4 data ----

df_minc4 <- clean_names(df_minc4) # janitor package

# For some reason column 6 and column 7 are given the same variable name
df_minc4 <- df_minc4 %>%
        arrange(famnum) %>%
        rename(di2=di2_6,
               di3=di2_7)


df_minc4 <- df_minc4 %>%
        select(famnum, plan, attrit, ftypw, 
               ofnnum, # Male ID
               fofnn, # Female ID
               matches("hours"), # M hours
               matches("uemst"), # M unemployment start
               matches("uemen"), # M unemployment end
               matches("uemr"), # M unemployment reason
               matches("fhours"), # F hours
               matches("fumst"), # F unemployment start
               matches("fumen"), # F unemployment end
               matches("fumr"), # F unemployment reason
        ) %>%
        rename(hours11=hours1,
               hours21=hours2,
               hours31=hours3,
               hours41=hours4,
               hours51=hours5,
               hours61=hours6,
               hours71=hours7,
               hours81=hours8,
               hours91=hours9,
               hours101=hours10,
               hours111=hours11) %>%
        rename(fhours11=fhours1,
               fhours21=fhours2,
               fhours31=fhours3,
               fhours41=fhours4,
               fhours51=fhours5,
               fhours61=fhours6,
               fhours71=fhours7,
               fhours81=fhours8,
               fhours91=fhours9,
               fhours101=fhours10,
               fhours111=fhours11) 

colnames(df_minc4)

# Merge Minc1 and Minc4 ----

# merge
df_merge <- merge(df_minc1,df_minc4, by = c("famnum"), all.x = TRUE) %>%
        arrange(famnum)
rm(df_minc1,df_minc4) 

# Reshape data ----

id_vars <- c("famnum", "plan", "attrit", "ftypw", "ofnnum", "fofnn",
             "site_code","agem","agef","famsize","mhnotlook","fhnotlook")
df_long <- melt(as.data.table(df_merge), 
                measure.vars = patterns("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"),
                value.name = c("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"))[
                        , variable := factor(variable, labels = times)][]

# reshape
times <- gsub("ac", "", grep("ac", names(df_merge), value = TRUE))
df_long <- melt(as.data.table(df_merge), 
                measure.vars = patterns("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"),
                value.name = c("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"))[
                        , variable := factor(variable, labels = times)][]

df_long$variable <- as.numeric(as.character(df_long$variable))
df_long <- df_long %>%
        rename(month=variable) %>%
        arrange(famnum,month) %>%
        select(famnum,month,everything())

# rename
df_mincome <- df_long %>%
        rename(famno = famnum,
               wpg_dummy = h2, # Winnipeg dummy
               dau_dummy = h3, # Dauphin dummy
               nrr = h8, # normal reduction rate (NRR)
               enrollment_date = h10,
               first_irf = h11, # First IRF period
               last_irf = h12 # Last IRF period
        ) %>%
        select(famno, month, 
               famsize, hhead, shead, agem, agef, numchild, # family vars at baseline
               fh_high, mh_high, # education (high school dummy)
               first_irf, last_irf,
               uic73, uic74, # unemployment insurance
               sa73, sa74, # social assistance (i.e. welfare)
               mhweeks73, mhweek74, fhweeks73, fhweek74, # weeks worked
               fhnotlook, mhnotlook, # why not looking for work
               wpg_dummy, dau_dummy, site_code, treat, # site treatment
               famsize_tvc, enrollment_date, # participation
               wages_tvc, matches("tot"), # earnings
               ac, payments # time varying vars
               ) %>%
        arrange(famno, month) %>%
        rename(twohead_dummy = hhead,
               onehead_dummy = shead) %>%
        select(famno, month, everything())

# Clean data ----

# destring variables
df_mincome <- data.frame(lapply(df_mincome, function(x) as.numeric(as.character(x))))

# change all values below 0 (-9, -7, -1) to missing
df_mincome[df_mincome < 0] <- NA

# Unique count
df_mincome <- df_mincome %>%
        group_by(famno) %>%
        mutate(count = row_number(),
               unique = ifelse(row_number()==n(), yes = 1, no = NA), # identify unique families at end of study period
        ) %>% 
        ungroup()

# Combine site and treatment ----

# The assignment cell (ac) is assigned to the unit at enrollment, 
# The first digit is the treatment plan assigned to the household.
# The last 2 digits are the normal income cell of the household which 
# determined the probability of being assigned to each of the 9 plans.
# (set User Manual for more information)

df_mincome <- df_mincome %>%
        group_by(famno) %>%
        mutate(ac = na.locf(ac, na.rm = FALSE), # replace missing value with previous non-missing value
               ac = last(ac), # replace all cases with the last value
        ) %>% 
        ungroup()

df_mincome$treat_recode <- recode(df_mincome$treat,"1:8=1; 9=0")
df_mincome$ac_recode <- recode(df_mincome$ac,"lo:899=1; 900:hi=0")

# Replace treatment and ac with 1 if case is in dauphin because if they are in dauphin, they are treated
# There are about 25% of cases with missing site and treat info
# basically it appears as if these people entered the data set after the baseline period
# Replace treatment with ac if treatment is missing, but ac is not

df_mincome <- df_mincome %>%
        mutate(treat_recode = ifelse(dau_dummy == 1 & is.na(treat_recode), yes = 1, no = treat_recode),
               ac_recode = ifelse(dau_dummy == 1 & is.na(treat_recode), yes = 1, no = ac_recode),
               site_code = ifelse(wpg_dummy == 1 & is.na(site_code), yes = 1, no = site_code),
               site_code = ifelse(dau_dummy == 1 & is.na(site_code), yes = 2, no = site_code),
               site_code = ifelse(wpg_dummy == 0 & dau_dummy == 0 & is.na(site_code), yes = 0, no = site_code),
               treat_recode = ifelse(is.na(treat_recode) & !is.na(ac_recode), yes = ac_recode, no = treat_recode),
        )

# if you ever received a payment, you are treated.
df_mincome <- df_mincome %>%
        mutate(test = ifelse(payments > 0 & !is.na(payments), yes = 1, no = NA)) %>%
        group_by(famno) %>%
        mutate(test = na.locf(test, na.rm = FALSE), # replace missing value with previous non-missing value
               test = last(test), # replace all cases with the last value
               test = ifelse(is.na(test), yes = 0, no = test),
               ) %>%
        ungroup() %>%
        mutate(treat_recode = ifelse(test == 1, yes = 1, no = treat_recode)) 

# Site treat
# sitetreat_1 1 "Winnipeg treat"
# sitetreat_1 2 "Winnipeg contr"
# sitetreat_1 3 "Dauphin"
# sitetreat_1 4 "Rural treat"
# sitetreat_1 5 "Rural contr"
df_mincome <- df_mincome %>%
        mutate(sitetreat_1 = ifelse(site_code == 1 & treat_recode == 1, yes = 1,
                                  ifelse(site_code == 1 & treat_recode == 0, yes = 2,
                                         ifelse(site_code == 2, yes = 3,
                                                ifelse(site_code == 0 & treat_recode == 1, yes = 4,
                                                       ifelse(site_code == 0 & treat_recode == 0, yes = 5, no = 0))))))

# sitetreat_2 0 "MB Control"
# sitetreat_2 1 "Dauphin", add
df_mincome <- df_mincome %>%
        mutate(sitetreat_2 = ifelse(sitetreat_1 == 3, yes = 1,
                                  ifelse(sitetreat_1 == 2 | sitetreat_1 == 5, yes = 0, no = NA)))

# sitetreat_3 1 "MB control"
# sitetreat_3 2 "Dauphin"
# sitetreat_3 3 "Rural treatment"

df_mincome <- df_mincome %>%
        mutate(sitetreat_3 = ifelse(sitetreat_2==0, yes = 1,
                                    ifelse(sitetreat_2==1, yes = 2, no = 0))) %>%
        mutate(sitetreat_3 = ifelse(sitetreat_1==4, yes = 3, no = sitetreat_3))

# Clean variables ----

# Family (baseline)
df_mincome <- df_mincome %>%
        mutate(kids = ifelse(numchild>0, yes = 1, no = 0))

# Unemployment
df_mincome$unemployed_1973 <- recode(df_mincome$uic73,"1:hi=1")
df_mincome$unemployed_1974 <- recode(df_mincome$uic74,"1:hi=1")

# Received unemployment in 1973 & 1974
df_mincome <- df_mincome %>% 
        mutate(unemployed_both = ifelse(unemployed_1973==1 & unemployed_1974==1, yes = 1, no = 0),
               unemployed_or = ifelse(unemployed_1973==1 | unemployed_1974==1, yes = 1, no = 0),
        )

# Welfare
df_mincome$welfare_1973 <- recode(df_mincome$sa73,"1:hi=1")
df_mincome$welfare_1974 <- recode(df_mincome$sa74,"1:hi=1")

# Received welfare in 1973 & 1974
df_mincome <- df_mincome %>% 
        mutate(welfare_both = ifelse(welfare_1973==1 & welfare_1974==1, yes = 1, no = 0),
               welfare_or = ifelse(welfare_1973==1 | welfare_1974==1, yes = 1, no = 0),
        )

# Baseline labor force participation
df_mincome$inlaborf73_m <- recode(df_mincome$mhweeks73,"1:hi=1")
df_mincome$inlaborf74_m <- recode(df_mincome$mhweek74,"1:hi=1")

df_mincome$inlaborf73_f <- recode(df_mincome$fhweeks73,"1:hi=1")
df_mincome$inlaborf74_f <- recode(df_mincome$fhweek74,"1:hi=1")

# Participation

df_mincome <- df_mincome %>%
        filter(!is.na(enrollment_date)) %>%
        mutate(participation_rate = ifelse(!is.na(famsize_tvc), yes = 1, no = NA),
               participation_cum = ifelse(!is.na(famsize_tvc), yes = 1, no = NA),
               benefit = ifelse(payments > 0, yes = 1, no = NA),
               walkin = ifelse(first_irf>1, yes = 1, no = 0),
               dropout = ifelse(last_irf<37, yes = 1, no = 0),
               walkin_date = ifelse(first_irf!=count, yes = NA, no = walkin),
               dropout_date = ifelse(last_irf!=count, yes = NA, no = dropout),
        ) %>%
        group_by(famno) %>%
        mutate(participation_cum = na.locf(participation_cum, na.rm = FALSE), # replace missing value with previous non-missing value
               benefit = na.locf(benefit, na.rm = FALSE), # replace missing value with previous non-missing value
               dropout_date = lag(dropout_date, 1),
        ) %>%
        ungroup()


# Earnings data ----
# Error note: Small coding error
# means that 1 family is now included, but was not in the original paper
# famno == 35723

# Baseline
df_mincome <- df_mincome %>%
        mutate(earnings_74 = rowSums(select(., .dots = c("totnhinc74", "mhtotern74", "fhtotern74")), na.rm = TRUE),
               earnings_73 = rowSums(select(., .dots = c("totnhinc73", "mhtotern73", "fhtotern73")), na.rm = TRUE),
        ) %>%
        mutate(earnings_74 = ifelse(earnings_74==0, yes = NA, no = earnings_74),
               earnings_74 = ifelse(is.na(earnings_74) & (mhtotern74 == 0 | fhtotern74 == 0 | fhtotern74 == 0), yes = 0, no = earnings_74),
               earnings_74 = earnings_74/12,
               earnings_73 = ifelse(earnings_73==0, yes = NA, no = earnings_73),
               # earnings_73 = ifelse(is.na(earnings_73) & (mhtotern74 == 0 | fhtotern74 == 0 | totnhinc74 == 0), yes = 0, no = earnings_73), # this original code is an error
               earnings_73 = ifelse(is.na(earnings_73) & (mhtotern73 == 0 | fhtotern73 == 0 | totnhinc73 == 0), yes = 0, no = earnings_73),
               earnings_73 = earnings_73/12,
        ) %>%
        mutate(month = month + 23)

select(df_mincome,famno,month,earnings_74,earnings_73,mhtotern74,fhtotern74,totnhinc74) %>% filter(famno == 35723)

# Expand
df_baseline <- df_mincome %>%
        group_by(famno) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 1)
df_months <- data.frame()
months <- seq(2,23,1)
for (m in months) {
        df_test <- df_baseline %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                

# Append earnings data
df_mincome <- rbind(df_mincome,df_baseline,df_months)
rm(df_baseline,df_months,df_test,m,months)

# Date

df_mincome$year <- recode(df_mincome$month, "1:12=1973;13:24=1974;25:36=1975;37:48=1976;49:60=1977")
df_mincome$halfyear <- recode(df_mincome$month, "1:6=1;7:12=2;13:18=3;19:24=4;25:30=5;31:36=6;37:42=7;43:48=8;49:54=9;55:60=10")

# Labor market participation
df_mincome <- df_mincome %>%
        arrange(famno,month) %>%
        mutate(wages = ifelse(month<13, yes = earnings_73, no = 0),
               wages = ifelse(month>12&month<24, yes = earnings_74, no = wages),
               wages = ifelse(month>23, yes = wages_tvc, no = wages)) %>% # First month is December, 1974
        mutate(lmp = ifelse(wages>0, yes = 1, no = 0),
               lmp_0 = ifelse(lmp==0, yes = 1, no = 0),
               lmp_1 = ifelse(lmp==1, yes = 1, no = 0))

# View(select(df_mincome, famno, year, month, wages, wages_tvc, earnings_73, earnings_74))

# Study period
df_mincome <- df_mincome %>%
        mutate(period = ifelse(month<24, yes = 0, 
                               ifelse(month>29, yes = 1, no = NA)))

# Turning into balanced panel ----
df_mincome_balanced <- df_mincome %>%
        filter(first_irf <= 6 & last_irf==37)

# Modify the walkin variable to indicate "ever walked in" as opposed to identifying the period at which a person walked in
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(walkin_ever = ifelse(walkin == 1, yes = 1, no = NA)) %>%
        group_by(famno) %>%
        mutate(walkin_ever = na.locf(walkin_ever, na.rm = FALSE), # replace missing value with previous non-missing value
               walkin_ever = last(walkin_ever), # replace all cases with the last value
        ) %>%
        ungroup()

# You are not in the sample if you are a walkin participant and have missing baseline information
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(missing = ifelse(unique == 1 & walkin_ever == 1 & is.na(earnings_74) & is.na(earnings_73), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)

# You are not in the sample if you are a panel participant and have missing baseline information
# There is an error in the original code
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(missing = ifelse(first_irf == 1 & last_irf == 37 & (is.na(earnings_74) | is.na(earnings_73)), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)



# Save data ----

saveRDS(df_mincome, file = paste0(data_files, "mincome.rds"))

saveRDS(df_mincome_balanced, file = paste0(data_files, "mincome_balanced.rds"))
