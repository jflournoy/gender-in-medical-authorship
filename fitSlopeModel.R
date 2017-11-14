## ---- fitSlopeModel ----
require(lme4)

slopeModelFName <- './RData/lme4SlopeModel.RDS'

if(!file.exists(slopeModelFName)){
  slopeModel <- glmer(male ~ 1 + first_author*pubyear_c + last_author*pubyear_c + 
                        (1 + first_author*pubyear_c + last_author*pubyear_c | term),
                      data=indivLevDatSample, 
                      family='binomial')  
  saveRDS(slopeModel, slopeModelFName)
} else
  slopeModel <- readRDS(slopeModelFName)
}
summary(slopeModel)