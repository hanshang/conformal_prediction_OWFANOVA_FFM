states = c("nfl", "pei", "nsc", "nbr", "que", "ont", "man", "sas", "alb", "bco", "nwt", "yuk")
n_states = length(states)

nfl_demo = read.demogdata(file = "Newfoundland_rate.txt", popfile = "Newfoundland_expo.txt", 
                          type = "mortality", label = "nfl")

pei_demo = read.demogdata(file = "PrinceEdwardIsland_rate.txt", popfile = "PrinceEdwardIsland_expo.txt", 
                          type = "mortality", label = "pei")

nsc_demo = read.demogdata(file = "NovaScotia_rate.txt", popfile = "NovaScotia_expo.txt", 
                          type = "mortality", label = "nsc")

nbr_demo = read.demogdata(file = "NewBrunswick_rate.txt", popfile = "NewBrunswick_expo.txt", 
                          type = "mortality", label = "nbr")

que_demo = read.demogdata(file = "Quebec_rate.txt", popfile = "Quebec_expo.txt", 
                          type = "mortality", label = "que")

ont_demo = read.demogdata(file = "Ontario_rate.txt", popfile = "Ontario_expo.txt", 
                          type = "mortality", label = "ont")

man_demo = read.demogdata(file = "Manitoba_rate.txt", popfile = "Manitoba_expo.txt", 
                          type = "mortality", label = "man")

sas_demo = read.demogdata(file = "Saskatchewan_rate.txt", popfile = "Saskatchewan_expo.txt", 
                          type = "mortality", label = "sas")

alb_demo = read.demogdata(file = "Alberta_rate.txt", popfile = "Alberta_expo.txt", 
                          type = "mortality", label = "alb")

bco_demo = read.demogdata(file = "BritishColumbia_rate.txt", popfile = "BritishColumbia_expo.txt", 
                          type = "mortality", label = "bco")

nwt_demo = read.demogdata(file = "Northwest_territories_rate.txt", popfile = "Northwest_territories_expo.txt", 
                          type = "mortality", label = "nwt")

yuk_demo = read.demogdata(file = "Yukon_rate.txt", popfile = "Yukon_expo.txt", 
                          type = "mortality", label = "yuk")

range(nfl_demo$year) # 1949 2023
range(pei_demo$year) # 1921 2023
range(nsc_demo$year) # 1921 2023
range(nbr_demo$year) # 1921 2023
range(que_demo$year) # 1921 2023
range(ont_demo$year) # 1921 2023
range(man_demo$year) # 1921 2023
range(sas_demo$year) # 1921 2023
range(alb_demo$year) # 1921 2023
range(bco_demo$year) # 1921 2023
range(nwt_demo$year) # 1950 2023
range(yuk_demo$year) # 1950 2016

# smooth demogdata

chosen_ages = 0:80
chosen_years = 1950:2016
n_chosen_ages = length(chosen_ages)
n_chosen_years = length(chosen_years)

nfl_demo_extract = extract.years(extract.ages(nfl_demo, chosen_ages), chosen_years)
nfl_demo_smooth = smooth.demogdata(nfl_demo_extract)

pei_demo_extract = extract.years(extract.ages(pei_demo, chosen_ages), chosen_years)
pei_demo_smooth = smooth.demogdata(pei_demo_extract)

nsc_demo_extract = extract.years(extract.ages(nsc_demo, chosen_ages), chosen_years)
nsc_demo_smooth = smooth.demogdata(nsc_demo_extract)

nbr_demo_extract = extract.years(extract.ages(nbr_demo, chosen_ages), chosen_years)
nbr_demo_smooth = smooth.demogdata(nbr_demo_extract)

que_demo_extract = extract.years(extract.ages(que_demo, chosen_ages), chosen_years)
que_demo_smooth = smooth.demogdata(que_demo_extract)

ont_demo_extract = extract.years(extract.ages(ont_demo, chosen_ages), chosen_years)
ont_demo_smooth = smooth.demogdata(ont_demo_extract)

man_demo_extract = extract.years(extract.ages(man_demo, chosen_ages), chosen_years)
man_demo_smooth = smooth.demogdata(man_demo_extract)

sas_demo_extract = extract.years(extract.ages(sas_demo, chosen_ages), chosen_years)
sas_demo_smooth = smooth.demogdata(sas_demo_extract)

alb_demo_extract = extract.years(extract.ages(alb_demo, chosen_ages), chosen_years)
alb_demo_smooth = smooth.demogdata(alb_demo_extract)

bco_demo_extract = extract.years(extract.ages(bco_demo, chosen_ages), chosen_years)
bco_demo_smooth = smooth.demogdata(bco_demo_extract)

nwt_demo_extract = extract.years(extract.ages(nwt_demo, chosen_ages), chosen_years)
nwt_demo_smooth = smooth.demogdata(nwt_demo_extract)

yuk_demo_extract = extract.years(extract.ages(yuk_demo, chosen_ages), chosen_years)
yuk_demo_smooth = smooth.demogdata(yuk_demo_extract)

# range

round(range(nfl_demo_extract$rate$female), 4) 
round(range(nfl_demo_extract$rate$male), 4)   
round(range(nfl_demo_smooth$rate$female), 4)  
round(range(nfl_demo_smooth$rate$male), 4)

round(range(pei_demo_extract$rate$female), 4)
round(range(pei_demo_extract$rate$male), 4)
round(range(pei_demo_smooth$rate$female), 4)
round(range(pei_demo_smooth$rate$male), 4)

round(range(nsc_demo_extract$rate$female), 4)
round(range(nsc_demo_extract$rate$male), 4)
round(range(nsc_demo_smooth$rate$female), 4)
round(range(nsc_demo_smooth$rate$male), 4)

round(range(nbr_demo_extract$rate$female), 4)
round(range(nbr_demo_extract$rate$male), 4)
round(range(nbr_demo_smooth$rate$female), 4)
round(range(nbr_demo_smooth$rate$male), 4)

round(range(que_demo_extract$rate$female), 4)
round(range(que_demo_extract$rate$male), 4)
round(range(que_demo_smooth$rate$female), 4)
round(range(que_demo_smooth$rate$male), 4)

round(range(ont_demo_extract$rate$female), 4)
round(range(ont_demo_extract$rate$male), 4)
round(range(ont_demo_smooth$rate$female), 4)
round(range(ont_demo_smooth$rate$male), 4)

round(range(man_demo_extract$rate$female), 4)
round(range(man_demo_extract$rate$male), 4)
round(range(man_demo_smooth$rate$female), 4)
round(range(man_demo_smooth$rate$male), 4)

round(range(sas_demo_extract$rate$female), 4)
round(range(sas_demo_extract$rate$male), 4)
round(range(sas_demo_smooth$rate$female), 4)
round(range(sas_demo_smooth$rate$male), 4)

round(range(alb_demo_extract$rate$female), 4)
round(range(alb_demo_extract$rate$male), 4)
round(range(alb_demo_smooth$rate$female), 4)
round(range(alb_demo_smooth$rate$male), 4)

round(range(bco_demo_extract$rate$female), 4)
round(range(bco_demo_extract$rate$male), 4)
round(range(bco_demo_smooth$rate$female), 4)
round(range(bco_demo_smooth$rate$male), 4)

round(range(nwt_demo_extract$rate$female), 4)
round(range(nwt_demo_extract$rate$male), 4)
round(range(nwt_demo_smooth$rate$female), 4)
round(range(nwt_demo_smooth$rate$male), 4)

round(range(yuk_demo_extract$rate$female), 4)
round(range(yuk_demo_extract$rate$male), 4)
round(range(yuk_demo_smooth$rate$female), 4)
round(range(yuk_demo_smooth$rate$male), 4)

CAN_female_prefecture_rate_array = CAN_male_prefecture_rate_array = array(NA, dim = c(n_chosen_ages, n_states, n_chosen_years))
CAN_female_prefecture_rate_array[,1,] = nfl_demo_extract$rate$female
CAN_female_prefecture_rate_array[,2,] = pei_demo_extract$rate$female
CAN_female_prefecture_rate_array[,3,] = nsc_demo_extract$rate$female
CAN_female_prefecture_rate_array[,4,] = nbr_demo_extract$rate$female
CAN_female_prefecture_rate_array[,5,] = que_demo_extract$rate$female
CAN_female_prefecture_rate_array[,6,] = ont_demo_extract$rate$female
CAN_female_prefecture_rate_array[,7,] = man_demo_extract$rate$female
CAN_female_prefecture_rate_array[,8,] = sas_demo_extract$rate$female
CAN_female_prefecture_rate_array[,9,] = alb_demo_extract$rate$female
CAN_female_prefecture_rate_array[,10,] = bco_demo_extract$rate$female
CAN_female_prefecture_rate_array[,11,] = nwt_demo_extract$rate$female
CAN_female_prefecture_rate_array[,12,] = yuk_demo_extract$rate$female

CAN_male_prefecture_rate_array[,1,] = nfl_demo_extract$rate$male
CAN_male_prefecture_rate_array[,2,] = pei_demo_extract$rate$male
CAN_male_prefecture_rate_array[,3,] = nsc_demo_extract$rate$male
CAN_male_prefecture_rate_array[,4,] = nbr_demo_extract$rate$male
CAN_male_prefecture_rate_array[,5,] = que_demo_extract$rate$male
CAN_male_prefecture_rate_array[,6,] = ont_demo_extract$rate$male
CAN_male_prefecture_rate_array[,7,] = man_demo_extract$rate$male
CAN_male_prefecture_rate_array[,8,] = sas_demo_extract$rate$male
CAN_male_prefecture_rate_array[,9,] = alb_demo_extract$rate$male
CAN_male_prefecture_rate_array[,10,] = bco_demo_extract$rate$male
CAN_male_prefecture_rate_array[,11,] = nwt_demo_extract$rate$male
CAN_male_prefecture_rate_array[,12,] = yuk_demo_extract$rate$male

CAN_female_prefecture_rate_array_smooth = CAN_male_prefecture_rate_array_smooth = array(NA, dim = c(n_chosen_ages, n_states, n_chosen_years))
CAN_female_prefecture_rate_array_smooth[,1,] = nfl_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,2,] = pei_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,3,] = nsc_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,4,] = nbr_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,5,] = que_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,6,] = ont_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,7,] = man_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,8,] = sas_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,9,] = alb_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,10,] = bco_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,11,] = nwt_demo_smooth$rate$female
CAN_female_prefecture_rate_array_smooth[,12,] = yuk_demo_smooth$rate$female

CAN_male_prefecture_rate_array_smooth[,1,] = nfl_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,2,] = pei_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,3,] = nsc_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,4,] = nbr_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,5,] = que_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,6,] = ont_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,7,] = man_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,8,] = sas_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,9,] = alb_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,10,] = bco_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,11,] = nwt_demo_smooth$rate$male
CAN_male_prefecture_rate_array_smooth[,12,] = yuk_demo_smooth$rate$male

