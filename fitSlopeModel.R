## ---- fitSlopeModel ----
require(lme4)

slopeModelFName <- './RData/lme4SlopeModel.RDS'

if(!file.exists(slopeModelFName)){
  slopeModelEquation <- formula(male ~ 1 + first_author*pubyear_z + last_author*pubyear_z + 
                        (1 + first_author*pubyear_z + last_author*pubyear_z | term))
  
  slopeModelFormula <- lFormula(slopeModelEquation, data = indivLevDatSample)
  
  slopeModelFormulaNumFx <- length(dimnames(slopeModelFormula$X)[[2]])
  
  slopeModelFormulaNumRx <- sum(as.numeric(lapply(slopeModelFormula$reTrms$cnms, function(x) {
    l <- length(x)
    (l*(l-1))/2+l
  })))
  
  slopeModelFormulaMaxfun <- 10*(slopeModelFormulaNumFx+slopeModelFormulaNumRx+1)^2

  slopeModel <- glmer(slopeModelEquation,
                    data = indivLevDatSample, 
                    verbose = 2,
                    family='binomial',
                    control=glmerControl(optCtrl=list(maxfun=slopeModelFormulaMaxfun), 
                                         optimizer = "nloptwrap", 
                                         calc.derivs = FALSE))
  saveRDS(slopeModel, slopeModelFName)
} else {
  slopeModel <- readRDS(slopeModelFName)
}
summary(slopeModel)
