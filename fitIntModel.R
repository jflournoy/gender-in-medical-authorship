## ---- fitIntModel ----

require(lme4)

intModelFName <- './RData/lme4IntModel.RDS'

if(!file.exists(intModelFName)){
  intModelEquation <- formula(male ~ 1 + first_author*pubyear_z + last_author*pubyear_z + 
                                    (1 + first_author + last_author | term))
  
  intModelFormula <- lFormula(intModelEquation, data = indivLevDatSample)
  
  intModelFormulaNumFx <- length(dimnames(intModelFormula$X)[[2]])
  
  intModelFormulaNumRx <- sum(as.numeric(lapply(intModelFormula$reTrms$cnms, function(x) {
    l <- length(x)
    (l*(l-1))/2+l
  })))
  
  intModelFormulaMaxfun <- 10*(intModelFormulaNumFx+intModelFormulaNumRx+1)^2

  intModel <- glmer(intModelEquation,
                    data = indivLevDatSample, 
                    verbose = 2,
                    family='binomial',
                    control=glmerControl(optCtrl=list(maxfun=intModelFormulaMaxfun), 
                                         optimizer = "nloptwrap", 
                                         calc.derivs = FALSE))
  saveRDS(intModel, intModelFName)
} else {
  intModel <- readRDS(intModelFName)
}
summary(intModel)
