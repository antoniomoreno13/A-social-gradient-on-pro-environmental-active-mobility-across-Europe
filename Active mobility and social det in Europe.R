
library("foreign")
library("plyr")
library("dplyr")
library("forcats")

#### SURVEYS HANDLING ####
eu2019 <- read.spss("C:/Users/Desktop/ZA7602_v1-0-0.sav", to.data.frame = T, use.missings = T)
eu2019 <- eu2019[c(9, 11, 13, 433, 436, 624, 472, 457, 625, 450, 626, 108, 116, 347, 628, 630, 629, 58, 129, 61)]
names(eu2019)[1:11] <- c("ID", "Country", "Age", "Gender", "Education", "Education_mod",  "Size of community", "Social class subjective", "Social class subjective_mod", "Bills", "Bills_mod")
names(eu2019)[12:17] <- c("not_friendly_travelling", "less_unnecesary_trips", "not_improve_pub_transport", "protect_env", "climate_change_is_problem", "env_affect_health")
names(eu2019)[18:20] <- c("Protect_env_original", "Env_direct_effect_original", "Climate_change_original")


#### COVARIATES REORDERING, ASIGNING MISSING VALUES AND DROPPING USELESS LEVELS ####
eu2019$`Social class subjective`[eu2019$`Social class subjective` == "The upper middle class of society"] <- "The higher class of society"
eu2019$`Social class subjective`[eu2019$`Social class subjective` == "The lower middle class of society"] <- "The working class of society"

eu2019$`Social class subjective`[eu2019$`Social class subjective` == "DK"] <- NA
eu2019$`Social class subjective`[eu2019$`Social class subjective` == "None (SPONTANEOUS)"] <- NA
eu2019$`Social class subjective`[eu2019$`Social class subjective` == "Other (SPONTANEOUS)"] <- NA
eu2019$`Social class subjective`[eu2019$`Social class subjective` == "Refusal (SPONTANEOUS)"] <- NA
eu2019$`Social class subjective`<- fct_drop(eu2019$`Social class subjective`, only = c("Other (SPONTANEOUS)", "None (SPONTANEOUS)", "DK", "Refusal (SPONTANEOUS)", 
                                                                                           "The upper middle class of society", "The lower middle class of society"))


eu2019$Age <- as.numeric(eu2019$Age)
eu2019$Age <- eu2019$Age + 14


eu2019$Country_rec <- eu2019$Country; eu2019$Country_rec <- fct_expand(eu2019$Country_rec, c("DE Germany")) 
eu2019$Country_rec[eu2019$Country_rec == "DE-W - Germany - West" | eu2019$Country_rec =="DE-E Germany East" ] <- "DE Germany"
eu2019$Country_rec<- fct_drop(eu2019$Country_rec, only = c("DE-W - Germany - West", "DE-E Germany East"))



#### MULTI-LEVEL BINOMIAL LOGISTIC MODELS (Figures 1 and S5) ####
library(moments)
library(lme4)
library(sjPlot)

eu2019$Gender <- relevel(eu2019$Gender, ref = "Man")
eu2019$Education_mod <- relevel(eu2019$Education_mod, ref = "20 years and older")
eu2019$`Social class subjective` <- relevel(eu2019$`Social class subjective`, ref = "The higher class of society")
eu2019$Bills_mod <- relevel(eu2019$Bills_mod, ref = "Almost never/never")
eu2019$`Size of community` <- relevel(eu2019$`Size of community`, ref = "Cities/large urban area")
eu2019$`Social class subjective_mod` <- relevel(eu2019$`Social class subjective_mod`, ref = "Upper middle and higher class")

eu2019$not_friendly_travelling <- relevel(eu2019$not_friendly_travelling, ref = "Not mentioned")
eu2019$less_unnecesary_trips <- relevel(eu2019$less_unnecesary_trips, ref = "Not mentioned")
eu2019$not_improve_pub_transport <- relevel(eu2019$not_improve_pub_transport, ref = "Not mentioned")
eu2019$protect_env <- relevel(eu2019$protect_env, ref = "Not very.../not at all important")
eu2019$climate_change_is_problem <- relevel(eu2019$climate_change_is_problem, ref = "Not a serious problem")
eu2019$env_affect_health <- relevel(eu2019$env_affect_health, ref = "Totally.../tend to disagree")


eu2019$protect_env_2[eu2019$protect_env == "Not very.../not at all important"] <- 0
eu2019$protect_env_2[eu2019$protect_env == "Very.../fairly important"] <- 1
eu2019$climate_change_is_problem_2[eu2019$climate_change_is_problem == "Not a serious problem"] <- 0
eu2019$climate_change_is_problem_2[eu2019$climate_change_is_problem == "A fairly.../very serious problem"] <- 1
eu2019$env_affect_health_2[eu2019$env_affect_health == "Totally.../tend to disagree"] <- 0
eu2019$env_affect_health_2[eu2019$env_affect_health == "Totally.../tend to agree"] <- 1

eu2019$environ_awarness <- eu2019$protect_env_2 + eu2019$climate_change_is_problem_2 + eu2019$env_affect_health_2
eu2019$environ_awarness_cat[eu2019$environ_awarness == 0] <- 0
eu2019$environ_awarness_cat[eu2019$environ_awarness >= 1] <- 1






# Friendly traveling (Fig 1)
model1 <- glmer(not_friendly_travelling ~ Age + Gender + Education_mod +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model10 <- glmer(not_friendly_travelling ~ Age + Gender + Education_mod + environ_awarness_cat +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model1, model10, digits.re = 3, digits = 3)



model11 <- glmer(not_friendly_travelling ~ Age + Gender + `Social class subjective` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model110 <- glmer(not_friendly_travelling ~ Age + Gender + `Social class subjective` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model11, model110, digits.re = 3, digits = 3)



model12 <- glmer(not_friendly_travelling ~ Age + Gender + Bills_mod +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model120 <- glmer(not_friendly_travelling ~ Age + Gender + Bills_mod + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model12, model120, digits.re = 3, digits = 3)



model13 <- glmer(not_friendly_travelling ~ Age + Gender + `Size of community` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model130 <- glmer(not_friendly_travelling ~ Age + Gender + `Size of community` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model13, model130, digits.re = 3, digits = 3)





# Less unnecesary trips (Fig 1)
model2 <- glmer(less_unnecesary_trips ~ Age + Gender + Education_mod +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model20 <- glmer(less_unnecesary_trips ~ Age + Gender + Education_mod + environ_awarness_cat +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model2, model20, digits.re = 3, digits = 3)



model21 <- glmer(less_unnecesary_trips ~ Age + Gender + `Social class subjective` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model210 <- glmer(less_unnecesary_trips ~ Age + Gender + `Social class subjective` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model21, model210, digits.re = 3, digits = 3)



model22 <- glmer(less_unnecesary_trips ~ Age + Gender + Bills_mod +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model220 <- glmer(less_unnecesary_trips ~ Age + Gender + Bills_mod + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model22, model220, digits.re = 3, digits = 3)



model23 <- glmer(less_unnecesary_trips ~ Age + Gender + `Size of community` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model230 <- glmer(less_unnecesary_trips ~ Age + Gender + `Size of community` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model23, model230, digits.re = 3, digits = 3)





# Improve public transport (Fig 1)
model3 <- glmer(not_improve_pub_transport ~ Age + Gender + Education_mod +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model30 <- glmer(not_improve_pub_transport ~ Age + Gender + Education_mod + environ_awarness_cat +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model3, model30, digits.re = 3, digits = 3)



model31 <- glmer(not_improve_pub_transport ~ Age + Gender + `Social class subjective` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model310 <- glmer(not_improve_pub_transport ~ Age + Gender + `Social class subjective` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model31, model310, digits.re = 3, digits = 3)



model32 <- glmer(not_improve_pub_transport ~ Age + Gender + Bills_mod +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model320 <- glmer(not_improve_pub_transport ~ Age + Gender + Bills_mod + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model32, model320, digits.re = 3, digits = 3)



model33 <- glmer(not_improve_pub_transport ~ Age + Gender + `Size of community` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)

model330 <- glmer(not_improve_pub_transport ~ Age + Gender + `Size of community` + environ_awarness_cat +
                     (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 10, na.action = na.exclude)
tab_model(model33, model330, digits.re = 3, digits = 3)




# Attitude for environment and climate change (Fig S5)
model4 <- glmer(environ_awarness_cat ~ Age + Gender + Education_mod +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model4, digits.re = 3, digits = 3)



model5 <- glmer(environ_awarness_cat ~ Age + Gender + `Social class subjective` +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model5, digits.re = 3, digits = 3)



model6 <- glmer(environ_awarness_cat ~ Age + Gender + Bills_mod +
                  (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model6, digits.re = 3, digits = 3)



model7 <- glmer(environ_awarness_cat ~ Age + Gender + `Size of community` +
                    (1 | Country_rec), data = eu2019, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10, na.action = na.exclude)
tab_model(model7, digits.re = 3, digits = 3)



#### Mediation analysis (Fig 2) ####
library(lavaan)
library(lavaanPlot)
library(effectsize)

## Continuous variables ##
eu2019$Protect_env_original_cont[eu2019$Protect_env_original == "Not at all important"] <- 0
eu2019$Protect_env_original_cont[eu2019$Protect_env_original == "Not very important"] <- 1
eu2019$Protect_env_original_cont[eu2019$Protect_env_original == "Fairly important"] <- 2
eu2019$Protect_env_original_cont[eu2019$Protect_env_original == "Very important"] <- 3

eu2019$Env_direct_effect_original_cont[eu2019$Env_direct_effect_original == "Totally disagree"] <- 0
eu2019$Env_direct_effect_original_cont[eu2019$Env_direct_effect_original == "Tend to disagree"] <- 1
eu2019$Env_direct_effect_original_cont[eu2019$Env_direct_effect_original == "Tend to agree"] <- 2
eu2019$Env_direct_effect_original_cont[eu2019$Env_direct_effect_original == "Totally agree"] <- 3

eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "1 Not at all a serious problem"] <- 0
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "2"] <- 1
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "3"] <- 2
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "4"] <- 3
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "5"] <- 4
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "6"] <- 5
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "7"] <- 6
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "8"] <- 7
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "9"] <- 8
eu2019$Climate_change_original_cont[eu2019$Climate_change_original == "10 An extremely serious problem"] <- 9


eu2019$Gender_cont[eu2019$Gender == "Woman"] <- 0
eu2019$Gender_cont[eu2019$Gender == "Man"] <- 1

eu2019$Education_mod_cont[eu2019$Education_mod == "Up to 15 years"] <- 0
eu2019$Education_mod_cont[eu2019$Education_mod == "16-19"] <- 1
eu2019$Education_mod_cont[eu2019$Education_mod == "20 years and older"] <- 2
eu2019$Education_mod_cont[eu2019$Education_mod == "Still Studying"] <- 3

eu2019$Social_class_subjective_cont[eu2019$`Social class subjective` == "The working class of society"] <- 0
eu2019$Social_class_subjective_cont[eu2019$`Social class subjective` == "The middle class of society"] <- 1
eu2019$Social_class_subjective_cont[eu2019$`Social class subjective` == "The higher class of society"] <- 2

eu2019$Bills_mod_cont[eu2019$Bills_mod == "Most of the time"] <- 0
eu2019$Bills_mod_cont[eu2019$Bills_mod == "From time to time"] <- 1
eu2019$Bills_mod_cont[eu2019$Bills_mod == "Almost never/never"] <- 2

eu2019$Size_of_community_cont[eu2019$`Size of community` == "Rural area"] <- 0
eu2019$Size_of_community_cont[eu2019$`Size of community` == "Towns and suburbs/small urban area"] <- 1
eu2019$Size_of_community_cont[eu2019$`Size of community` == "Cities/large urban area"] <- 2


eu2019$not_friendly_travelling_cont[eu2019$not_friendly_travelling == "Not mentioned"] <- 0
eu2019$not_friendly_travelling_cont[eu2019$not_friendly_travelling == "Chosen a more environmentally-friendly way of travelling (walk, bicycle, public transport, electric car)"] <- 1

eu2019$less_unnecesary_trips_cont[eu2019$less_unnecesary_trips == "Not mentioned"] <- 0
eu2019$less_unnecesary_trips_cont[eu2019$less_unnecesary_trips == "Used your car less by avoiding unnecessary trips, working from home (teleworking), etc."] <- 1

eu2019$not_improve_pub_transport_cont[eu2019$not_improve_pub_transport == "Not mentioned"] <- 0
eu2019$not_improve_pub_transport_cont[eu2019$not_improve_pub_transport == "To improve public transport and reduce air pollution"] <- 1


# Mediation models - WITH CONTROLS #

set.seed(123456)
specmod_travelling <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat + Gender_cont + Age
not_friendly_travelling_cont ~ b*Env_aware 
not_friendly_travelling_cont ~ c*SES + d*Sizecomm_lat + Gender_cont + Age

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_trav <- sem(specmod_travelling, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_trav, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_trav)
lavaanPlot(model = fitmod_trav, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = T, stars = "regress", 
           digits = 3)




specmod_less_trips <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat + Gender_cont + Age
less_unnecesary_trips_cont ~ b*Env_aware 
less_unnecesary_trips_cont ~ c*SES + d*Sizecomm_lat + Gender_cont + Age

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_less_trips <- sem(specmod_less_trips, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_less_trips, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_less_trips)
lavaanPlot(model = fitmod_less_trips, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = F, stars = "regress", 
           digits = 3)




specmod_pub_transp <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat + Gender_cont + Age
not_improve_pub_transport_cont ~ b*Env_aware 
not_improve_pub_transport_cont ~ c*SES + d*Sizecomm_lat + Gender_cont + Age

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_pub_transp <- sem(specmod_pub_transp, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_pub_transp, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_pub_transp)
lavaanPlot(model = fitmod_pub_transp, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = F, stars = "regress", 
           digits = 3)


## Mediation analyses - WITHOUT CONTROLS ##

set.seed(123456)
specmod_travelling_NO_CONTROL <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat
not_friendly_travelling_cont ~ b*Env_aware 
not_friendly_travelling_cont ~ c*SES + d*Sizecomm_lat

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_trav_NO_CONTROL <- sem(specmod_travelling_NO_CONTROL, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_trav_NO_CONTROL, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_trav_NO_CONTROL)
lavaanPlot(model = fitmod_trav_NO_CONTROL, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = T, stars = "regress", 
           digits = 3)




specmod_less_trips_NO_CONTROL <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat
less_unnecesary_trips_cont ~ b*Env_aware 
less_unnecesary_trips_cont ~ c*SES + d*Sizecomm_lat

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_less_trips_NO_CONTROL <- sem(specmod_less_trips_NO_CONTROL, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_less_trips_NO_CONTROL, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_less_trips_NO_CONTROL)
lavaanPlot(model = fitmod_less_trips_NO_CONTROL, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = F, stars = "regress", 
           digits = 3)




specmod_pub_transp_NO_CONTROL <- '#simple mediation
Env_aware ~ a*SES + e*Sizecomm_lat
not_improve_pub_transport_cont ~ b*Env_aware 
not_improve_pub_transport_cont ~ c*SES + d*Sizecomm_lat

#latent variables
SES =~ NA*Education_mod_cont + Social_class_subjective_cont + Bills_mod_cont
Env_aware =~ NA*Protect_env_original_cont + Env_direct_effect_original_cont + Climate_change_original_cont
Sizecomm_lat =~ Size_of_community_cont

#covariances
SES ~~ Sizecomm_lat

#indirect effect
ab:=a*b
eb:=e*b
total_ind:=ab+eb

#total effects
total_ses:=c+(a*b)
total_comm:=d+(e*b)
'

fitmod_pub_transp_NO_CONTROL <- sem(specmod_pub_transp_NO_CONTROL, data = eu2019, se = "bootstrap", std.ov = T)
summary(fitmod_pub_transp_NO_CONTROL, fit.measures=T, rsquare=T, ci=T)
interpret(fitmod_pub_transp_NO_CONTROL)
lavaanPlot(model = fitmod_pub_transp_NO_CONTROL, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = T, covs = F, stars = "regress", 
           digits = 3)

