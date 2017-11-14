## ---- fitIntModel ----

require(lme4)

intModelFName <- './RData/lme4IntModel.RDS'

if(!file.exists(intModelFName)){
  intModel <- glmer(male ~ 1 + first_author*pubyear_c + last_author*pubyear_c + 
                      (1 + first_author + last_author | term),
                    data=indivLevDatSample, 
                    family='binomial')
  saveRDS(intModel, intModelFName)
} else {
  intModel <- readRDS(intModelFName)
}
summary(intModel)
