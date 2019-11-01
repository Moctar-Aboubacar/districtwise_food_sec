#Extracting a district-wise estimation from the mVAM data
# Round 5 Jan 2019
require(tidyverse)
require(foreign)
require(survey)
require(ggthemes)

# import
filepath.1 <- "C:/Users/moctar.aboubacar/Desktop/mVAM-EPR-CLEAR/mVAM/Karnali Ranking/HH_Dec_2018_working_Final.csv"
dat <- read.csv(filepath.1)
glimpse(dat)

# survey design
svy.dat <- svydesign(dat$cluster, weights = dat$Weight_district, strata = dat$strata, data = dat) 

# estimate FCS by district with SEs
table.1 <- svyby(~FC_score, ~district, svymean, design = svy.dat)

table.1 <- table.1 %>% 
  mutate(lower_bound = FC_score - (1.96* se),
         upper_bound = FC_score + (1.96*se)) %>% 
  filter(district != "Bajhang",
         district !="Bajura",
         district !="Darchula") %>% 
  arrange(desc(FC_score))

# plot1
ggplot(table.1, aes(x = reorder(district, - FC_score), y = FC_score))+
  geom_point(shape = 15,
             size = 3)+
  geom_errorbar(aes(ymin = lower_bound,
                    ymax = upper_bound),
                width = 0.05,
                size = 0.5)+
  theme(axis.title = element_text(face = "bold"))+ 
  ylab("Food Consumption Score")+
  xlab("District")+
  ggtitle("Food Consumption Score by District, January 2019")+
  geom_hline(yintercept = 42, color = "red")+
  theme_calc()

# conclusion
## As feared, our survey design is adapted to the ecological belt, and is not good for producing results at the district level. 
# How about by strata?
# test if we looked at different strata from the survey only.
# result: 95% CIs of about 5-7 FCS points. Much tigher. Makes sense as the sample is much bigger. 

table.test <- svyby(~FC_score, ~strata, svymean, design = svy.dat)

table.test <- table.test %>% 
  mutate(lower_bound = FC_score - (1.96* se),
         upper_bound = FC_score + (1.96*se)) %>% 
  arrange(desc(FC_score))

# plot2
ggplot(table.test, aes(x = reorder(strata, - FC_score), y = FC_score))+
  geom_point(shape = 15,
             size = 3)+
  geom_errorbar(aes(ymin = lower_bound,
                    ymax = upper_bound),
                width = 0.05,
                size = 0.5)+
  theme(axis.title = element_text(face = "bold"))+ 
  ylab("Food Consumption Score test")+
  xlab("Survey strata")+
  ggtitle("Food Consumption Score by District, January 2019")+
  geom_hline(yintercept = 42, color = "red")+
  theme_calc()
