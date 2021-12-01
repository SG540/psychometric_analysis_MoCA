
library(psych)
library(lavaan)
library(semTools)


################
#Exploratory FA#
################


EFA<-readRDS(file.choose()) # select "Tmk.Rdata"
print(omega(EFA$rho,nfactors=7, n.obs=1204, sl=FALSE, digits=3))


#################
#Confirmatory FA#
#################


model_2nd <- '

F1=~Q1.TAT_SCO_1W+Q2.CUBE_SCO_1W+Q3.CLK_HANDS_1W+Q3.CLK_LINE_1W
F2=~Q4.NAM_RHINO_1W+Q4.NAM_CAMEL_1W+Q4.NAM_LION_1W+Q10.FRUENCY_SCO_1W
F3=~Q12.DLY_REC_RED_1W+Q12.DLY_REC_SHRI_1W+Q12.DLY_REC_RELLY_1W+Q12.DLY_REC_SILK_1W+Q12.DLY_REC_FACE_1W
F4=~Q6.DS_FORW_1W+Q6.DS_BAKW_1W+Q7.ATTENT_SCO_1W+Q9.REPETI_1_1W+Q9.REPETI_2_1W
F5=~Q8.SEVEN_72_1W+Q8.SEVEN_79_1W+Q8.SEVEN_65_1W+Q8.SEVEN_86_1W
F6=~Q11.SIMI_1_1W+Q11.SIMI_2_1W
F7=~Q13.OR_DAY_1W+Q13.OR_YEAR_1W+Q13.OR_PLACE_1W

g=~F1+F2+F3+F4+F5+F6+F7

'

findRMSEApower(rmsea0 = .05, rmseaA = .01, df = 317, alpha = 0.05, n = 1204, group = 1)



CFA<-readRDS(file.choose()) # select "matrix_fit_2nd.RDATA"

fit_2nd_MAT <- cfa(model_2nd, sample.cov = CFA[[1]], sample.mean = CFA[[2]],
               sample.th = CFA[[3]], sample.nobs = CFA[[4]],
               WLS.V = CFA[[5]], NACOV = CFA[[6]])

summary(fit_2nd_MAT)
fitMeasures(fit_2nd_MAT, fit.measures = c("chisq.scaled","pvalue.scaled",
                                          "cfi.scaled","srmr","rmsea.scaled",
                                          ))

########################
#Measurement Invariance#
########################



MI <- readRDS(file.choose()) # select "Kzm.RDATA"

MI[[1]] #age
MI[[2]] #education
MI[[3]] #gender
MI[[4]] #economic status







