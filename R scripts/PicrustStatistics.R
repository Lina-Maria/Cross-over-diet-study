library(reshape2)
setwd("~/Desktop/Amy R")
### Create the file in excel and then csv, if the following message appears:
##In read.table("ACB_phylum.csv", header = TRUE) :
##incomplete final line found by readTableHeader on 'ACB_phylum.csv'

#Open the file you wish to use in a text editor.
#Scroll to the bottom of the file.
#Put the cursor at the end of the last line.
#Hit Enter.
#Save your file.


Picrust<- read.csv(file = "PicrustR.csv", header = TRUE, sep = ",")

library(microbiome)
library(ggplot2)
library(dplyr)
library(IRanges)

library(lme4)
install.packages("performance")
install.packages("emmeans")
install.packages("lme4")
install.packages("MuMIn")

library(MuMIn)
library(lme4)
library(performance)
library(emmeans)

# Correct order for plot
Picrust$Diet <- factor(Picrust$Diet, levels = c("c", "a", "b"))
                    

###> fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)##not used
###out <- lmer(signal ~ group + (1|subject), data = dfs)##not used


lmer(log(Cancers) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cancers) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 268.333
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1337  
#Residual             0.4863  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#3.340e+00               1.890e-01               6.857e-05              -6.674e-03  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#5.699e-02               7.351e-02  

Cancers_mod <- lmer(log(Cancers) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Cancers_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cancers) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 268.3

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-4.1542 -0.5279 -0.0210  0.7369  2.3039 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01788  0.1337  
#Residual             0.23647  0.4863  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)             3.340e+00  7.693e-02  43.421
#Dieta                   1.890e-01  1.242e-01   1.522
#Dietb                   6.857e-05  1.242e-01   0.001
#Diet_Sequencebca       -6.674e-03  1.114e-01  -0.060
#Dieta:Diet_Sequencebca  5.699e-02  1.798e-01   0.317
#Dietb:Diet_Sequencebca  7.351e-02  1.798e-01   0.409

#Correlation of Fixed Effects:
  #(Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.538                             
#Dietb       -0.538  0.333                      
#Diet_Sqncbc -0.691  0.372  0.372               
#Dt:Dt_Sqncb  0.372 -0.691 -0.230 -0.538        
#Dtb:Dt_Sqnc  0.372 -0.230 -0.691 -0.538  0.333 
 
r.squaredGLMM(Cancers_mod) 

#R2m       R2c
#[1,] 0.03246411 0.1004841

install.packages("performance")
library(performance)
install.packages("see")
library(see)
check_model(Cancers_mod) 

library(emmeans)
install.packages("pbkrtest")
install.packages("lmerTest")
library(magrittr)

cancers.emm.s <- emmeans(Cancers_mod, specs = pairwise ~ Diet * Diet_Sequence)
#pairs(cancers.emm.s) ## not used

cancers.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             3.34 0.0769 110     3.19     3.49
#a    acb             3.53 0.1052 167     3.32     3.74
#b    acb             3.34 0.1052 167     3.13     3.55
#c    bca             3.33 0.0805 110     3.17     3.49
#a    bca             3.58 0.1101 167     3.36     3.80
#b    bca             3.41 0.1101 167     3.19     3.62

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

cancers.emm.s$contrasts

#contrast       estimate    SE  df t.ratio p.value
#c acb - a acb -1.89e-01 0.124 128 -1.522  0.6510 
#c acb - b acb -6.86e-05 0.124 128 -0.001  1.0000 
#c acb - c bca  6.67e-03 0.111 110  0.060  1.0000 
#c acb - a bca -2.39e-01 0.134 153 -1.782  0.4804 
#c acb - b bca -6.69e-02 0.134 153 -0.498  0.9962 
#a acb - b acb  1.89e-01 0.143 128  1.317  0.7750 
#a acb - c bca  1.96e-01 0.132 151  1.477  0.6793 
#a acb - a bca -5.03e-02 0.152 167 -0.331  0.9995 
#a acb - b bca  1.22e-01 0.152 167  0.802  0.9668 
#b acb - c bca  6.74e-03 0.132 151  0.051  1.0000 
#b acb - a bca -2.39e-01 0.152 167 -1.572  0.6184 
#b acb - b bca -6.68e-02 0.152 167 -0.439  0.9979 
#c bca - a bca -2.46e-01 0.130 128 -1.893  0.4115 
#c bca - b bca -7.36e-02 0.130 128 -0.566  0.9930 
#a bca - b bca  1.72e-01 0.150 128  1.149  0.8599 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Cancers_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response") ## data back transformed from the log scale

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb               28.2 2.17 110     24.2     32.9
#a    acb               34.1 3.59 167     27.7     42.0
#b    acb               28.2 2.97 167     22.9     34.7
#c    bca               28.0 2.26 110     23.9     32.9
#a    bca               35.9 3.95 167     28.9     44.6
#b    bca               30.2 3.32 167     24.3     37.5

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale

confint(cancers.emm.s$contrasts)## same results
summary(cancers.emm.s$contrasts, infer = TRUE)##same results

####Amino_Acid_Metabolism
class(Picrust$Amino_Acid_Metabolism)
lmer(log(Amino_Acid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Amino_Acid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 253.1059
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1290  
#Residual             0.4647  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.92314                 0.16357                -0.09177                -0.03013  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.09772                 0.13086  

Amino_Acid_Metabolism_mod <- lmer(log(Amino_Acid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Amino_Acid_Metabolism_mod) 

#Fixed effects:
  #Estimate Std. Error t value
#(Intercept)            14.92314    0.07361 202.735
#Dieta                   0.16357    0.11868   1.378
#Dietb                  -0.09177    0.11868  -0.773
#Diet_Sequencebca       -0.03013    0.10655  -0.283
#Dieta:Diet_Sequencebca  0.09772    0.17179   0.569
#Dietb:Diet_Sequencebca  0.13086    0.17179   0.762

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.537                             
#Dietb       -0.537  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.537        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.537  0.333 


r.squaredGLMM(Amino_Acid_Metabolism_mod)

#R2m       R2c
#[1,] 0.04242747 0.1108861


check_model(Amino_Acid_Metabolism_mod) 

Amino_Acid_Metabolism.emm.s <- emmeans(Amino_Acid_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)

Amino_Acid_Metabolism.emm.s$emmeans
#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.9 0.0736 110     14.8     15.1
#a    acb             15.1 0.1006 167     14.9     15.3
#b    acb             14.8 0.1006 167     14.6     15.0
#c    bca             14.9 0.0770 110     14.7     15.0
#a    bca             15.2 0.1052 167     14.9     15.4
#b    bca             14.9 0.1052 167     14.7     15.1

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 


Amino_Acid_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.16357 0.119 128 -1.378  0.7400 
#c acb - b acb  0.09177 0.119 128  0.773  0.9715 
#c acb - c bca  0.03013 0.107 110  0.283  0.9997 
#c acb - a bca -0.23116 0.128 153 -1.800  0.4689 
#c acb - b bca -0.00896 0.128 153 -0.070  1.0000 
#a acb - b acb  0.25534 0.137 128  1.863  0.4296 
#a acb - c bca  0.19370 0.127 151  1.529  0.6461 
#a acb - a bca -0.06759 0.146 167 -0.464  0.9973 
#a acb - b bca  0.15461 0.146 167  1.062  0.8956 
#b acb - c bca -0.06164 0.127 151 -0.487  0.9966 
#b acb - a bca -0.32293 0.146 167 -2.218  0.2349 
#b acb - b bca -0.10073 0.146 167 -0.692  0.9826 
#c bca - a bca -0.26129 0.124 128 -2.104  0.2920 
#c bca - b bca -0.03909 0.124 128 -0.315  0.9996 
#a bca - b bca  0.22220 0.143 128  1.549  0.6332 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Amino_Acid_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            3027159 222826 110  2616258  3502595
#a    acb            3565108 358528 167  2923119  4348094
#b    acb            2761721 277735 167  2264402  3368264
#c    bca            2937311 226274 110  2521430  3421786
#a    bca            3814418 401452 167  3098770  4695341
#b    bca            3054414 321465 167  2481356  3759819

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.849 0.1008 128 -1.378  0.7400 
#c acb / b acb 1.096 0.1301 128  0.773  0.9715 
#c acb / c bca 1.031 0.1098 110  0.283  0.9997 
#c acb / a bca 0.794 0.1019 153 -1.800  0.4689 
#c acb / b bca 0.991 0.1273 153 -0.070  1.0000 
#a acb / b acb 1.291 0.1769 128  1.863  0.4296 
#a acb / c bca 1.214 0.1538 151  1.529  0.6461 
#a acb / a bca 0.935 0.1361 167 -0.464  0.9973 
#a acb / b bca 1.167 0.1699 167  1.062  0.8956 
#b acb / c bca 0.940 0.1191 151 -0.487  0.9966 
#b acb / a bca 0.724 0.1054 167 -2.218  0.2349 
#b acb / b bca 0.904 0.1316 167 -0.692  0.9826 
#c bca / a bca 0.770 0.0956 128 -2.104  0.2920 
#c bca / b bca 0.962 0.1194 128 -0.315  0.9996 
#a bca / b bca 1.249 0.1791 128  1.549  0.6332 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Biosynthesis_of_Other_Secondary_Metabolites) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Biosynthesis_of_Other_Secondary_Metabolites) ~ 1 + Diet *      Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 248.4571
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1268  
#Residual             0.4585  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#12.53630                 0.15960                -0.10652                -0.01539  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.05023                 0.16118  

Biosynthesis_of_Other_Secondary_Metabolites_mod <- lmer(log(Biosynthesis_of_Other_Secondary_Metabolites) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Biosynthesis_of_Other_Secondary_Metabolites_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Biosynthesis_of_Other_Secondary_Metabolites) ~ 1 + Diet *      Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 248.5

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3600 -0.6127  0.0284  0.7252  2.3662 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01608  0.1268  
#Residual             0.21023  0.4585  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            12.53630    0.07259 172.696
#Dieta                   0.15960    0.11709   1.363
#Dietb                  -0.10652    0.11709  -0.910
#Diet_Sequencebca       -0.01539    0.10508  -0.146
#Dieta:Diet_Sequencebca  0.05023    0.16949   0.296
#Dietb:Diet_Sequencebca  0.16118    0.16949   0.951

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.538                             
#Dietb       -0.538  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.538        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.538  0.333 

r.squaredGLMM(Biosynthesis_of_Other_Secondary_Metabolites_mod)

#R2m       R2c
#[1,] 0.03677955 0.1052271

check_model(Biosynthesis_of_Other_Secondary_Metabolites_mod) 

Biosynthesis_of_Other_Secondary_Metabolites.emm.s <- emmeans(Biosynthesis_of_Other_Secondary_Metabolites_mod, specs = pairwise ~ Diet * Diet_Sequence)
Biosynthesis_of_Other_Secondary_Metabolites.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             12.5 0.0726 110     12.4     12.7
#a    acb             12.7 0.0992 167     12.5     12.9
#b    acb             12.4 0.0992 167     12.2     12.6
#c    bca             12.5 0.0760 110     12.4     12.7
#a    bca             12.7 0.1038 167     12.5     12.9
#b    bca             12.6 0.1038 167     12.4     12.8

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Biosynthesis_of_Other_Secondary_Metabolites.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1596 0.117 128 -1.363  0.7489 
#c acb - b acb   0.1065 0.117 128  0.910  0.9434 
#c acb - c bca   0.0154 0.105 110  0.146  1.0000 
#c acb - a bca  -0.1944 0.127 153 -1.535  0.6423 
#c acb - b bca  -0.0393 0.127 153 -0.310  0.9996 
#a acb - b acb   0.2661 0.135 128  1.968  0.3663 
#a acb - c bca   0.1750 0.125 151  1.401  0.7267 
#a acb - a bca  -0.0348 0.144 167 -0.243  0.9999 
#a acb - b bca   0.1203 0.144 167  0.838  0.9600 
#b acb - c bca  -0.0911 0.125 151 -0.729  0.9780 
#b acb - a bca  -0.3010 0.144 167 -2.096  0.2944 
#b acb - b bca  -0.1458 0.144 167 -1.015  0.9123 
#c bca - a bca  -0.2098 0.123 128 -1.712  0.5263 
#c bca - b bca  -0.0547 0.123 128 -0.446  0.9977 
#a bca - b bca   0.1552 0.142 128  1.097  0.8820 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Biosynthesis_of_Other_Secondary_Metabolites_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             278258 20199 110   240973   321311
#a    acb             326408 32378 167   268355   397020
#b    acb             250143 24813 167   205654   304256
#c    bca             274008 20816 110   235710   318530
#a    bca             337979 35086 167   275347   414858
#b    bca             289403 30044 167   235773   355232

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.852 0.0998 128 -1.363  0.7489 
#c acb / b acb 1.112 0.1303 128  0.910  0.9434 
#c acb / c bca 1.016 0.1067 110  0.146  1.0000 
#c acb / a bca 0.823 0.1043 153 -1.535  0.6423 
#c acb / b bca 0.961 0.1218 153 -0.310  0.9996 
#a acb / b acb 1.305 0.1764 128  1.968  0.3663 
#a acb / c bca 1.191 0.1488 151  1.401  0.7267 
#a acb / a bca 0.966 0.1387 167 -0.243  0.9999 
#a acb / b bca 1.128 0.1619 167  0.838  0.9600 
#b acb / c bca 0.913 0.1141 151 -0.729  0.9780 
#b acb / a bca 0.740 0.1063 167 -2.096  0.2944 
#b acb / b bca 0.864 0.1241 167 -1.015  0.9123 
#c bca / a bca 0.811 0.0993 128 -1.712  0.5263 
#c bca / b bca 0.947 0.1160 128 -0.446  0.9977 
#a bca / b bca 1.168 0.1653 128  1.097  0.8820 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Carbohydrate_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Carbohydrate_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 250.2303
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1274  
#Residual             0.4609  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#15.04403                 0.17928                -0.09703                -0.03322  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.09519                 0.15821  

Carbohydrate_Metabolism_mod <- lmer(log(Carbohydrate_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Carbohydrate_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Carbohydrate_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 250.2

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.2779 -0.6510  0.0104  0.6783  2.4928 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01623  0.1274  
#Residual             0.21245  0.4609  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            15.04403    0.07297 206.174
#Dieta                   0.17928    0.11771   1.523
#Dietb                  -0.09703    0.11771  -0.824
#Diet_Sequencebca       -0.03322    0.10562  -0.315
#Dieta:Diet_Sequencebca  0.09519    0.17039   0.559
#Dietb:Diet_Sequencebca  0.15821    0.17039   0.929

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.538                             
#Dietb       -0.538  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.538        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.538  0.333 

r.squaredGLMM(Carbohydrate_Metabolism_mod)

#R2m       R2c
#[1,] 0.04818267 0.1157357

check_model(Carbohydrate_Metabolism_mod) 

Carbohydrate_Metabolism.emm.s <- emmeans(Carbohydrate_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Carbohydrate_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             15.0 0.0730 110     14.9     15.2
#a    acb             15.2 0.0997 167     15.0     15.4
#b    acb             14.9 0.0997 167     14.8     15.1
#c    bca             15.0 0.0764 110     14.9     15.2
#a    bca             15.3 0.1044 167     15.1     15.5
#b    bca             15.1 0.1044 167     14.9     15.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Carbohydrate_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1793 0.118 128 -1.523  0.6501 
#c acb - b acb   0.0970 0.118 128  0.824  0.9625 
#c acb - c bca   0.0332 0.106 110  0.315  0.9996 
#c acb - a bca  -0.2412 0.127 153 -1.895  0.4095 
#c acb - b bca  -0.0280 0.127 153 -0.220  0.9999 
#a acb - b acb   0.2763 0.136 128  2.033  0.3297 
#a acb - c bca   0.2125 0.126 151  1.692  0.5392 
#a acb - a bca  -0.0620 0.144 167 -0.429  0.9981 
#a acb - b bca   0.1513 0.144 167  1.048  0.9007 
#b acb - c bca  -0.0638 0.126 151 -0.508  0.9958 
#b acb - a bca  -0.3383 0.144 167 -2.344  0.1827 
#b acb - b bca  -0.1250 0.144 167 -0.866  0.9540 
#c bca - a bca  -0.2745 0.123 128 -2.228  0.2323 
#c bca - b bca  -0.0612 0.123 128 -0.497  0.9962 
#a bca - b bca   0.2133 0.142 128  1.499  0.6653 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Carbohydrate_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            3416163 249269 110  2956219  3947667
#a    acb            4086951 407526 167  3356633  4976167
#b    acb            3100261 309139 167  2546260  3774799
#c    bca            3304530 252344 110  2840437  3844450
#a    bca            4348198 453752 167  3538630  5342979
#b    bca            3513020 366598 167  2858950  4316729

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.836 0.0984 128 -1.523  0.6501 
#c acb / b acb 1.102 0.1297 128  0.824  0.9625 
#c acb / c bca 1.034 0.1092 110  0.315  0.9996 
#c acb / a bca 0.786 0.1000 153 -1.895  0.4095 
#c acb / b bca 0.972 0.1238 153 -0.220  0.9999 
#a acb / b acb 1.318 0.1792 128  2.033  0.3297 
#a acb / c bca 1.237 0.1553 151  1.692  0.5392 
#a acb / a bca 0.940 0.1357 167 -0.429  0.9981 
#a acb / b bca 1.163 0.1679 167  1.048  0.9007 
#b acb / c bca 0.938 0.1178 151 -0.508  0.9958 
#b acb / a bca 0.713 0.1029 167 -2.344  0.1827 
#b acb / b bca 0.883 0.1274 167 -0.866  0.9540 
#c bca / a bca 0.760 0.0936 128 -2.228  0.2323 
#c bca / b bca 0.941 0.1159 128 -0.497  0.9962 
#a bca / b bca 1.238 0.1761 128  1.499  0.6653 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Cell_Growth_and_Death) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cell_Growth_and_Death) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 252.0749
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1368  
#Residual             0.4615  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#11.976614                0.224652               -0.039937               -0.004923  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.052173                0.115018  

Cell_Growth_and_Death_mod <- lmer(log(Cell_Growth_and_Death) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Cell_Growth_and_Death_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cell_Growth_and_Death) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 252.1

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.4104 -0.6097  0.0034  0.6697  2.3566 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01871  0.1368  
#Residual             0.21295  0.4615  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            11.976614   0.073774 162.341
#Dieta                   0.224652   0.117848   1.906
#Dietb                  -0.039937   0.117848  -0.339
#Diet_Sequencebca       -0.004923   0.106788  -0.046
#Dieta:Diet_Sequencebca  0.052173   0.170584   0.306
#Dietb:Diet_Sequencebca  0.115018   0.170584   0.674

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.532                             
#Dietb       -0.532  0.333                      
#Diet_Sqncbc -0.691  0.368  0.368               
#Dt:Dt_Sqncb  0.368 -0.691 -0.230 -0.532        
#Dtb:Dt_Sqnc  0.368 -0.230 -0.691 -0.532  0.333 

r.squaredGLMM(Cell_Growth_and_Death_mod)

#R2m       R2c
#[1,] 0.05010657 0.1268072

check_model(Cell_Growth_and_Death_mod)

Cell_Growth_and_Death.emm.s <- emmeans(Cell_Growth_and_Death_mod, specs = pairwise ~ Diet * Diet_Sequence)
Cell_Growth_and_Death.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             12.0 0.0738 108     11.8     12.1
#a    acb             12.2 0.1004 166     12.0     12.4
#b    acb             11.9 0.1004 166     11.7     12.1
#c    bca             12.0 0.0772 108     11.8     12.1
#a    bca             12.2 0.1050 166     12.0     12.5
#b    bca             12.0 0.1050 166     11.8     12.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Cell_Growth_and_Death.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.22465 0.118 128 -1.906  0.4031 
#c acb - b acb  0.03994 0.118 128  0.339  0.9994 
#c acb - c bca  0.00492 0.107 108  0.046  1.0000 
#c acb - a bca -0.27190 0.128 151 -2.118  0.2834 
#c acb - b bca -0.07016 0.128 151 -0.547  0.9941 
#a acb - b acb  0.26459 0.136 128  1.944  0.3803 
#a acb - c bca  0.22957 0.127 149  1.813  0.4606 
#a acb - a bca -0.04725 0.145 166 -0.325  0.9995 
#a acb - b bca  0.15449 0.145 166  1.063  0.8951 
#b acb - c bca -0.03501 0.127 149 -0.277  0.9998 
#b acb - a bca -0.31184 0.145 166 -2.147  0.2688 
#b acb - b bca -0.11009 0.145 166 -0.758  0.9740 
#c bca - a bca -0.27682 0.123 128 -2.245  0.2250 
#c bca - b bca -0.07508 0.123 128 -0.609  0.9902 
#a bca - b bca  0.20174 0.142 128  1.417  0.7169 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates

emmeans(Cell_Growth_and_Death_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             158993 11730 108   137362   184029
#a    acb             199041 19976 166   163264   242658
#b    acb             152768 15332 166   125309   186245
#c    bca             158212 12215 108   135761   184376
#a    bca             208671 21917 166   169592   256755
#b    bca             170548 17913 166   138608   209847

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.799 0.0941 128 -1.906  0.4031 
#c acb / b acb 1.041 0.1226 128  0.339  0.9994 
#c acb / c bca 1.005 0.1073 108  0.046  1.0000 
#c acb / a bca 0.762 0.0978 151 -2.118  0.2834 
#c acb / b bca 0.932 0.1197 151 -0.547  0.9941 
#a acb / b acb 1.303 0.1773 128  1.944  0.3803 
#a acb / c bca 1.258 0.1593 149  1.813  0.4606 
#a acb / a bca 0.954 0.1386 166 -0.325  0.9995 
#a acb / b bca 1.167 0.1695 166  1.063  0.8951 
#b acb / c bca 0.966 0.1223 149 -0.277  0.9998 
#b acb / a bca 0.732 0.1064 166 -2.147  0.2688 
#b acb / b bca 0.896 0.1301 166 -0.758  0.9740 
#c bca / a bca 0.758 0.0935 128 -2.245  0.2250 
#c bca / b bca 0.928 0.1144 128 -0.609  0.9902 
#a bca / b bca 1.224 0.1742 128  1.417  0.7169 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Cell_Motility) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cell_Motility) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 288.0252
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.09682 
#Residual             0.52402 
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.0874                  0.1224                 -0.1405                 -0.1206  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.2101                  0.2918  

Cell_Motility_mod <- lmer(log(Cell_Motility) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Cell_Motility_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cell_Motility) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 288

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3512 -0.5620  0.0543  0.5853  2.8650 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.009373 0.09682 
#Residual             0.274598 0.52402 
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.08744    0.07986 163.887
#Dieta                   0.12243    0.13382   0.915
#Dietb                  -0.14051    0.13382  -1.050
#Diet_Sequencebca       -0.12058    0.11559  -1.043
#Dieta:Diet_Sequencebca  0.21006    0.19371   1.084
#Dietb:Diet_Sequencebca  0.29178    0.19371   1.506

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.559                             
#Dietb       -0.559  0.333                      
#Diet_Sqncbc -0.691  0.386  0.386               
#Dt:Dt_Sqncb  0.386 -0.691 -0.230 -0.559        
#Dtb:Dt_Sqnc  0.386 -0.230 -0.691 -0.559  0.333 

r.squaredGLMM(Cell_Motility_mod)

#R2m        R2c
#[1,] 0.04554608 0.07705075

check_model(Cell_Motility_mod) 

Cell_Motility.emm.s <- emmeans(Cell_Motility_mod, specs = pairwise ~ Diet * Diet_Sequence)
Cell_Motility.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.1 0.0799 118     12.9     13.2
#a    acb             13.2 0.1111 169     13.0     13.4
#b    acb             12.9 0.1111 169     12.7     13.2
#c    bca             13.0 0.0836 118     12.8     13.1
#a    bca             13.3 0.1163 169     13.1     13.5
#b    bca             13.1 0.1163 169     12.9     13.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Cell_Motility.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1224 0.134 128 -0.915  0.9421 
#c acb - b acb   0.1405 0.134 128  1.050  0.8999 
#c acb - c bca   0.1206 0.116 118  1.043  0.9023 
#c acb - a bca  -0.2119 0.141 160 -1.502  0.6634 
#c acb - b bca  -0.0307 0.141 160 -0.218  0.9999 
#a acb - b acb   0.2629 0.155 128  1.702  0.5333 
#a acb - c bca   0.2430 0.139 158  1.748  0.5025 
#a acb - a bca  -0.0895 0.161 169 -0.556  0.9936 
#a acb - b bca   0.0917 0.161 169  0.570  0.9928 
#b acb - c bca  -0.0199 0.139 158 -0.143  1.0000 
#b acb - a bca  -0.3524 0.161 169 -2.191  0.2474 
#b acb - b bca  -0.1712 0.161 169 -1.064  0.8948 
#c bca - a bca  -0.3325 0.140 128 -2.374  0.1733 
#c bca - b bca  -0.1513 0.140 128 -1.080  0.8885 
#a bca - b bca   0.1812 0.162 128  1.121  0.8720 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Cell_Motility_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             482842 38558 118   412220   565563
#a    acb             545727 60639 169   438241   679575
#b    acb             419550 46618 169   336915   522451
#c    bca             427992 35768 118   362714   505019
#a    bca             596807 69400 169   474393   750809
#b    bca             497888 57897 169   395764   626365

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

lmer(log(Cellular_Processes_and_Signaling) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cellular_Processes_and_Signaling) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 250.2418
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1237  
#Residual             0.4618  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.09961                 0.10854                -0.13985                -0.04946  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.11956                 0.15239  

Cellular_Processes_and_Signaling_mod <- lmer(log(Cellular_Processes_and_Signaling) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Cellular_Processes_and_Signaling_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Cellular_Processes_and_Signaling) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 250.2

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.4250 -0.6306 -0.0219  0.7196  2.4372 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01529  0.1237  
#Residual             0.21323  0.4618  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            14.09961    0.07280 193.669
#Dieta                   0.10854    0.11793   0.920
#Dietb                  -0.13985    0.11793  -1.186
#Diet_Sequencebca       -0.04946    0.10538  -0.469
#Dieta:Diet_Sequencebca  0.11956    0.17070   0.700
#Dietb:Diet_Sequencebca  0.15239    0.17070   0.893

#Correlation of Fixed Effects:
  #(Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.540                             
#Dietb       -0.540  0.333                      
#Diet_Sqncbc -0.691  0.373  0.373               
#Dt:Dt_Sqncb  0.373 -0.691 -0.230 -0.540        
#Dtb:Dt_Sqnc  0.373 -0.230 -0.691 -0.540  0.333 

r.squaredGLMM(Cellular_Processes_and_Signaling_mod)

#R2m      R2c
#[1,] 0.0366672 0.101123

check_model(Cellular_Processes_and_Signaling_mod) 

Cellular_Processes_and_Signaling.emm.s <- emmeans(Cellular_Processes_and_Signaling_mod, specs = pairwise ~ Diet * Diet_Sequence)
Cellular_Processes_and_Signaling.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.1 0.0728 111     14.0     14.2
#a    acb             14.2 0.0997 167     14.0     14.4
#b    acb             14.0 0.0997 167     13.8     14.2
#c    bca             14.1 0.0762 111     13.9     14.2
#a    bca             14.3 0.1043 167     14.1     14.5
#b    bca             14.1 0.1043 167     13.9     14.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Cellular_Processes_and_Signaling.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1085 0.118 128 -0.920  0.9406 
#c acb - b acb   0.1399 0.118 128  1.186  0.8428 
#c acb - c bca   0.0495 0.105 111  0.469  0.9971 
#c acb - a bca  -0.1786 0.127 154 -1.404  0.7244 
#c acb - b bca   0.0369 0.127 154  0.290  0.9997 
#a acb - b acb   0.2484 0.136 128  1.824  0.4541 
#a acb - c bca   0.1580 0.125 152  1.259  0.8065 
#a acb - a bca  -0.0701 0.144 167 -0.486  0.9966 
#a acb - b bca   0.1455 0.144 167  1.008  0.9147 
#b acb - c bca  -0.0904 0.125 152 -0.720  0.9792 
#b acb - a bca  -0.3185 0.144 167 -2.207  0.2399 
#b acb - b bca  -0.1029 0.144 167 -0.713  0.9801 
#c bca - a bca  -0.2281 0.123 128 -1.848  0.4389 
#c bca - b bca  -0.0125 0.123 128 -0.102  1.0000 
#a bca - b bca   0.2156 0.143 128  1.513  0.6568 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Cellular_Processes_and_Signaling_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1328564  96723 111  1150080  1534748
#a    acb            1480885 147611 167  1216348  1802954
#b    acb            1155168 115145 167   948816  1406400
#c    bca            1264454  96340 111  1087259  1470528
#a    bca            1588420 165698 167  1292781  1951669
#b    bca            1280401 133567 167  1042091  1573210

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.897 0.1058 128 -0.920  0.9406 
#c acb / b acb 1.150 0.1356 128  1.186  0.8428 
#c acb / c bca 1.051 0.1107 111  0.469  0.9971 
#c acb / a bca 0.836 0.1064 154 -1.404  0.7244 
#c acb / b bca 1.038 0.1320 154  0.290  0.9997 
#a acb / b acb 1.282 0.1746 128  1.824  0.4541 
#a acb / c bca 1.171 0.1469 152  1.259  0.8065 
#a acb / a bca 0.932 0.1345 167 -0.486  0.9966 
#a acb / b bca 1.157 0.1669 167  1.008  0.9147 
#b acb / c bca 0.914 0.1146 152 -0.720  0.9792 
#b acb / a bca 0.727 0.1049 167 -2.207  0.2399 
#b acb / b bca 0.902 0.1302 167 -0.713  0.9801 
#c bca / a bca 0.796 0.0982 128 -1.848  0.4389 
#c bca / b bca 0.988 0.1219 128 -0.102  1.0000 
#a bca / b bca 1.241 0.1768 128  1.513  0.6568 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale


#######################################3
lmer(log(Circulatory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Circulatory_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 565.0468
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1321  
#Residual             1.1954  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#4.0985                 -0.5391                 -0.8744                 -0.7649  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.4443                  0.8884 

Circulatory_System_mod <- lmer(log(Circulatory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Circulatory_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Circulatory_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 565

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-2.4548 -0.6419 -0.1056  0.4584  3.4088 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01745  0.1321  
#Residual             1.42894  1.1954  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)              4.0985     0.1784  22.975
#Dieta                   -0.5391     0.3053  -1.766
#Dietb                   -0.8744     0.3053  -2.864
#Diet_Sequencebca        -0.7649     0.2582  -2.962
#Dieta:Diet_Sequencebca   0.4443     0.4419   1.005
#Dietb:Diet_Sequencebca   0.8884     0.4419   2.011

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.570                             
#Dietb       -0.570  0.333                      
#Diet_Sqncbc -0.691  0.394  0.394               
#Dt:Dt_Sqncb  0.394 -0.691 -0.230 -0.570        
#Dtb:Dt_Sqnc  0.394 -0.230 -0.691 -0.570  0.333

r.squaredGLMM(Circulatory_System_mod)

#R2m        R2c
#[1,] 0.07702268 0.08815527

check_model(Circulatory_System_mod) ### model not suitable

Circulatory_System.emm.s <- emmeans(Circulatory_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Circulatory_System.emm.s$emmeans

#Diet Diet_Sequence emmean    SE  df lower.CL upper.CL
#c    acb             4.10 0.178 123     3.75     4.45
#a    acb             3.56 0.251 170     3.06     4.05
#b    acb             3.22 0.251 170     2.73     3.72
#c    bca             3.33 0.187 123     2.96     3.70
#a    bca             3.24 0.262 170     2.72     3.76
#b    bca             3.35 0.262 170     2.83     3.87

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Circulatory_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb   0.5391 0.305 128  1.766  0.4913 
#c acb - b acb   0.8744 0.305 128  2.864  0.0540 
#c acb - c bca   0.7649 0.258 123  2.962  0.0417 
#c acb - a bca   0.8597 0.317 163  2.709  0.0789 
#c acb - b bca   0.7508 0.317 163  2.366  0.1745 
#a acb - b acb   0.3353 0.352 128  0.951  0.9322 
#a acb - c bca   0.2258 0.313 161  0.722  0.9790 
#a acb - a bca   0.3206 0.363 170  0.883  0.9501 
#a acb - b bca   0.2117 0.363 170  0.583  0.9920 
#b acb - c bca  -0.1095 0.313 161 -0.350  0.9993 
#b acb - a bca  -0.0146 0.363 170 -0.040  1.0000 
#b acb - b bca  -0.1235 0.363 170 -0.340  0.9994 
#c bca - a bca   0.0948 0.319 128  0.297  0.9997 
#c bca - b bca  -0.0141 0.319 128 -0.044  1.0000 
#a bca - b bca  -0.1089 0.369 128 -0.295  0.9997 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Circulatory_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb               60.3 10.75 123     42.3     85.8
#a    acb               35.1  8.81 170     21.4     57.7
#b    acb               25.1  6.30 170     15.3     41.2
#c    bca               28.0  5.23 123     19.4     40.6
#a    bca               25.5  6.69 170     15.2     42.8
#b    bca               28.4  7.46 170     16.9     47.7

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 1.714 0.523 128  1.766  0.4913 
#c acb / b acb 2.397 0.732 128  2.864  0.0540 
#c acb / c bca 2.149 0.555 123  2.962  0.0417 
#c acb / a bca 2.363 0.750 163  2.709  0.0789 
#c acb / b bca 2.119 0.672 163  2.366  0.1745 
#a acb / b acb 1.398 0.493 128  0.951  0.9322 
#a acb / c bca 1.253 0.392 161  0.722  0.9790 
#a acb / a bca 1.378 0.500 170  0.883  0.9501 
#a acb / b bca 1.236 0.449 170  0.583  0.9920 
#b acb / c bca 0.896 0.280 161 -0.350  0.9993 
#b acb / a bca 0.985 0.358 170 -0.040  1.0000 
#b acb / b bca 0.884 0.321 170 -0.340  0.9994 
#c bca / a bca 1.099 0.351 128  0.297  0.9997 
#c bca / b bca 0.986 0.315 128 -0.044  1.0000 
#a bca / b bca 0.897 0.331 128 -0.295  0.9997 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale

#############################
lmer(log(Digestive_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Digestive_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 312.5885
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.06731 
#Residual             0.56841 
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#8.69489                 0.44534                 0.11949                 0.06631  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#-0.38940                -0.02525  

Digestive_System_mod <- lmer(log(Digestive_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Digestive_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Digestive_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 312.6

#Scaled residuals: 
 # Min       1Q   Median       3Q      Max 
#-2.37970 -0.66688  0.03044  0.70853  2.55805 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.004531 0.06731 
#Residual             0.323092 0.56841 
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)             8.69489    0.08497 102.323
#Dieta                   0.44534    0.14516   3.068
#Dietb                   0.11949    0.14516   0.823
#Diet_Sequencebca        0.06631    0.12300   0.539
#Dieta:Diet_Sequencebca -0.38940    0.21012  -1.853
#Dietb:Diet_Sequencebca -0.02525    0.21012  -0.120

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.569                             
#Dietb       -0.569  0.333                      
#Diet_Sqncbc -0.691  0.393  0.393               
#Dt:Dt_Sqncb  0.393 -0.691 -0.230 -0.569        
#Dtb:Dt_Sqnc  0.393 -0.230 -0.691 -0.569  0.333 

r.squaredGLMM(Digestive_System_mod)

#R2m        R2c
#[1,] 0.05365411 0.06674202

check_model(Digestive_System_mod) ###uhmm....not sure

Digestive_System.emm.s <- emmeans(Digestive_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Digestive_System.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             8.69 0.0850 123     8.53     8.86
#a    acb             9.14 0.1194 170     8.90     9.38
#b    acb             8.81 0.1194 170     8.58     9.05
#c    bca             8.76 0.0889 123     8.59     8.94
#a    bca             8.82 0.1249 170     8.57     9.06
#b    bca             8.86 0.1249 170     8.61     9.10

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Digestive_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.44534 0.145 128 -3.068  0.0309 
#c acb - b acb -0.11949 0.145 128 -0.823  0.9628 
#c acb - c bca -0.06631 0.123 123 -0.539  0.9944 
#c acb - a bca -0.12226 0.151 162 -0.809  0.9655 
#c acb - b bca -0.16055 0.151 162 -1.063  0.8953 
#a acb - b acb  0.32586 0.168 128  1.944  0.3805 
#a acb - c bca  0.37903 0.149 161  2.547  0.1170 
#a acb - a bca  0.32309 0.173 170  1.870  0.4241 
#a acb - b bca  0.28480 0.173 170  1.649  0.5677 
#b acb - c bca  0.05317 0.149 161  0.357  0.9992 
#b acb - a bca -0.00277 0.173 170 -0.016  1.0000 
#b acb - b bca -0.04106 0.173 170 -0.238  0.9999 
#c bca - a bca -0.05594 0.152 128 -0.368  0.9991 
#c bca - b bca -0.09423 0.152 128 -0.620  0.9894 
#a bca - b bca -0.03829 0.175 128 -0.218  0.9999 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Digestive_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb               5972  507 123     5048     7066
#a    acb               9323 1113 170     7366    11800
#b    acb               6730  803 170     5318     8518
#c    bca               6382  568 123     5352     7610
#a    bca               6749  843 170     5274     8636
#b    bca               7012  876 170     5480     8973

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.641 0.093 128 -3.068  0.0309 
#c acb / b acb 0.887 0.129 128 -0.823  0.9628 
#c acb / c bca 0.936 0.115 123 -0.539  0.9944 
#c acb / a bca 0.885 0.134 162 -0.809  0.9655 
#c acb / b bca 0.852 0.129 162 -1.063  0.8953 
#a acb / b acb 1.385 0.232 128  1.944  0.3805 
#a acb / c bca 1.461 0.217 161  2.547  0.1170 
#a acb / a bca 1.381 0.239 170  1.870  0.4241 
#a acb / b bca 1.329 0.230 170  1.649  0.5677 
#b acb / c bca 1.055 0.157 161  0.357  0.9992 
#b acb / a bca 0.997 0.172 170 -0.016  1.0000 
#b acb / b bca 0.960 0.166 170 -0.238  0.9999 
#c bca / a bca 0.946 0.144 128 -0.368  0.9991 
#c bca / b bca 0.910 0.138 128 -0.620  0.9894 
#a bca / b bca 0.962 0.169 128 -0.218  0.9999 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Energy_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Energy_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 252.622
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1282  
#Residual             0.4642  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.42657                 0.16081                -0.09647                -0.02438  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.08576                 0.13352  

Energy_Metabolism_mod <- lmer(log(Energy_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Energy_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Energy_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 252.6

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3979 -0.6114  0.0154  0.6991  2.4543 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01643  0.1282  
#Residual             0.21549  0.4642  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            14.42657    0.07348 196.339
#Dieta                   0.16081    0.11855   1.357
#Dietb                  -0.09647    0.11855  -0.814
#Diet_Sequencebca       -0.02438    0.10636  -0.229
#Dieta:Diet_Sequencebca  0.08576    0.17160   0.500
#Dietb:Diet_Sequencebca  0.13352    0.17160   0.778

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.538                             
#Dietb       -0.538  0.333                      
#Diet_Sqncbc -0.691  0.372  0.372               
#Dt:Dt_Sqncb  0.372 -0.691 -0.230 -0.538        
#Dtb:Dt_Sqnc  0.372 -0.230 -0.691 -0.538  0.333 

r.squaredGLMM(Energy_Metabolism_mod)

#R2m       R2c
#[1,] 0.04040671 0.1084051

check_model(Energy_Metabolism_mod) 


Energy_Metabolism.emm.s <- emmeans(Energy_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Energy_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.4 0.0735 110     14.3     14.6
#a    acb             14.6 0.1004 167     14.4     14.8
#b    acb             14.3 0.1004 167     14.1     14.5
#c    bca             14.4 0.0769 110     14.2     14.6
#a    bca             14.6 0.1051 167     14.4     14.9
#b    bca             14.4 0.1051 167     14.2     14.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Energy_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1608 0.119 128 -1.357  0.7527 
#c acb - b acb   0.0965 0.119 128  0.814  0.9645 
#c acb - c bca   0.0244 0.106 110  0.229  0.9999 
#c acb - a bca  -0.2222 0.128 153 -1.733  0.5124 
#c acb - b bca  -0.0127 0.128 153 -0.099  1.0000 
#a acb - b acb   0.2573 0.137 128  1.880  0.4194 
#a acb - c bca   0.1852 0.126 151  1.464  0.6875 
#a acb - a bca  -0.0614 0.145 167 -0.422  0.9983 
#a acb - b bca   0.1481 0.145 167  1.019  0.9110 
#b acb - c bca  -0.0721 0.126 151 -0.570  0.9928 
#b acb - a bca  -0.3187 0.145 167 -2.192  0.2468 
#b acb - b bca  -0.1091 0.145 167 -0.751  0.9751 
#c bca - a bca  -0.2466 0.124 128 -1.987  0.3552 
#c bca - b bca  -0.0370 0.124 128 -0.299  0.9997 
#a bca - b bca   0.2095 0.143 128  1.463  0.6886 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates

emmeans(Energy_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1842379 135374 110  1592714  2131180
#a    acb            2163812 217283 167  1774686  2638260
#b    acb            1672945 167991 167  1372093  2039763
#c    bca            1798006 138262 110  1543857  2093994
#a    bca            2300784 241788 167  1869697  2831266
#b    bca            1865872 196084 167  1516272  2296078

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.851 0.101 128 -1.357  0.7527 
#c acb / b acb 1.101 0.131 128  0.814  0.9645 
#c acb / c bca 1.025 0.109 110  0.229  0.9999 
#c acb / a bca 0.801 0.103 153 -1.733  0.5124 
#c acb / b bca 0.987 0.127 153 -0.099  1.0000 
#a acb / b acb 1.293 0.177 128  1.880  0.4194 
#a acb / c bca 1.203 0.152 151  1.464  0.6875 
#a acb / a bca 0.940 0.137 167 -0.422  0.9983 
#a acb / b bca 1.160 0.169 167  1.019  0.9110 
#b acb / c bca 0.930 0.118 151 -0.570  0.9928 
#b acb / a bca 0.727 0.106 167 -2.192  0.2468 
#b acb / b bca 0.897 0.130 167 -0.751  0.9751 
#c bca / a bca 0.781 0.097 128 -1.987  0.3552 
#c bca / b bca 0.964 0.120 128 -0.299  0.9997 
#a bca / b bca 1.233 0.177 128  1.463  0.6886 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Enzyme_Families) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Enzyme_Families) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 253.3491
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1307  
#Residual             0.4647  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.44338                 0.12355                -0.10047                -0.04261  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.12373                 0.10389 

Enzyme_Families_mod <- lmer(log(Enzyme_Families) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Enzyme_Families_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Enzyme_Families) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 253.3

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.4031 -0.6286 -0.0205  0.7174  2.4591 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01708  0.1307  
#Residual             0.21595  0.4647  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.44338    0.07374 182.314
#Dieta                   0.12355    0.11867   1.041
#Dietb                  -0.10047    0.11867  -0.847
#Diet_Sequencebca       -0.04261    0.10673  -0.399
#Dieta:Diet_Sequencebca  0.12373    0.17178   0.720
#Dietb:Diet_Sequencebca  0.10389    0.17178   0.605

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.536                             
#Dietb       -0.536  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.536        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.536  0.333 

r.squaredGLMM(Enzyme_Families_mod)

#R2m       R2c
#[1,] 0.03647069 0.1071079

check_model(Enzyme_Families_mod) 

Enzyme_Families.emm.s <- emmeans(Enzyme_Families_mod, specs = pairwise ~ Diet * Diet_Sequence)
Enzyme_Families.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.4 0.0737 109     13.3     13.6
#a    acb             13.6 0.1007 167     13.4     13.8
#b    acb             13.3 0.1007 167     13.1     13.5
#c    bca             13.4 0.0772 109     13.2     13.6
#a    bca             13.6 0.1053 167     13.4     13.9
#b    bca             13.4 0.1053 167     13.2     13.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Enzyme_Families.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.12355 0.119 128 -1.041  0.9031 
#c acb - b acb  0.10047 0.119 128  0.847  0.9581 
#c acb - c bca  0.04261 0.107 109  0.399  0.9987 
#c acb - a bca -0.20468 0.129 153 -1.592  0.6052 
#c acb - b bca  0.03919 0.129 153  0.305  0.9996 
#a acb - b acb  0.22402 0.137 128  1.635  0.5772 
#a acb - c bca  0.16616 0.127 150  1.310  0.7791 
#a acb - a bca -0.08112 0.146 167 -0.557  0.9936 
#a acb - b bca  0.16274 0.146 167  1.117  0.8738 
#b acb - c bca -0.05785 0.127 150 -0.456  0.9975 
#b acb - a bca -0.30514 0.146 167 -2.094  0.2953 
#b acb - b bca -0.06128 0.146 167 -0.421  0.9983 
#c bca - a bca -0.24729 0.124 128 -1.991  0.3532 
#c bca - b bca -0.00342 0.124 128 -0.028  1.0000 
#a bca - b bca  0.24386 0.143 128  1.700  0.5340 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Enzyme_Families_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             689262 50825 109   595548   797724
#a    acb             779906 78503 167   639349   951364
#b    acb             623380 62747 167   511032   760426
#c    bca             660508 50971 109   566835   769662
#a    bca             845812 89098 167   686994  1041345
#b    bca             662774 69817 167   538325   815992

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.884 0.105 128 -1.041  0.9031 
#c acb / b acb 1.106 0.131 128  0.847  0.9581 
#c acb / c bca 1.044 0.111 109  0.399  0.9987 
#c acb / a bca 0.815 0.105 153 -1.592  0.6052 
#c acb / b bca 1.040 0.134 153  0.305  0.9996 
#a acb / b acb 1.251 0.171 128  1.635  0.5772 
#a acb / c bca 1.181 0.150 150  1.310  0.7791 
#a acb / a bca 0.922 0.134 167 -0.557  0.9936 
#a acb / b bca 1.177 0.171 167  1.117  0.8738 
#b acb / c bca 0.944 0.120 150 -0.456  0.9975 
#b acb / a bca 0.737 0.107 167 -2.094  0.2953 
#b acb / b bca 0.941 0.137 167 -0.421  0.9983 
#c bca / a bca 0.781 0.097 128 -1.991  0.3532 
#c bca / b bca 0.997 0.124 128 -0.028  1.0000 
#a bca / b bca 1.276 0.183 128  1.700  0.5340 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Endocrine_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Endocrine_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 252.7726
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.130   
#Residual             0.464   
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#11.27406                 0.20677                -0.01758                -0.01058  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.05576                 0.10226  

Endocrine_System_mod <- lmer(log(Endocrine_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Endocrine_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Endocrine_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 252.8

#Scaled residuals: 
 # Min       1Q   Median       3Q      Max 
#-3.15264 -0.63394 -0.03529  0.78036  2.19442 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.0169   0.130   
#Residual             0.2153   0.464   
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            11.27406    0.07359 153.200
#Dieta                   0.20677    0.11850   1.745
#Dietb                  -0.01758    0.11850  -0.148
#Diet_Sequencebca       -0.01058    0.10652  -0.099
#Dieta:Diet_Sequencebca  0.05576    0.17153   0.325
#Dietb:Diet_Sequencebca  0.10226    0.17153   0.596

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.537                             
#Dietb       -0.537  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.537        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.537  0.333 

r.squaredGLMM(Endocrine_System_mod)

#R2m       R2c
#[1,] 0.04210859 0.1118187

check_model(Endocrine_System_mod) 

Endocrine_System.emm.s <- emmeans(Endocrine_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Endocrine_System.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             11.3 0.0736 109     11.1     11.4
#a    acb             11.5 0.1005 167     11.3     11.7
#b    acb             11.3 0.1005 167     11.1     11.5
#c    bca             11.3 0.0770 109     11.1     11.4
#a    bca             11.5 0.1052 167     11.3     11.7
#b    bca             11.3 0.1052 167     11.1     11.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Endocrine_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.20677 0.119 128 -1.745  0.5050 
#c acb - b acb  0.01758 0.119 128  0.148  1.0000 
#c acb - c bca  0.01058 0.107 109  0.099  1.0000 
#c acb - a bca -0.25195 0.128 153 -1.963  0.3685 
#c acb - b bca -0.07410 0.128 153 -0.577  0.9924 
#a acb - b acb  0.22435 0.137 128  1.640  0.5740 
#a acb - c bca  0.21736 0.127 151  1.717  0.5229 
#a acb - a bca -0.04517 0.145 167 -0.311  0.9996 
#a acb - b bca  0.13267 0.145 167  0.912  0.9430 
#b acb - c bca -0.00699 0.127 151 -0.055  1.0000 
#b acb - a bca -0.26952 0.145 167 -1.853  0.4348 
#b acb - b bca -0.09168 0.145 167 -0.630  0.9886 
#c bca - a bca -0.26253 0.124 128 -2.117  0.2853 
#c bca - b bca -0.08469 0.124 128 -0.683  0.9836 
#a bca - b bca  0.17784 0.143 128  1.242  0.8153 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 


emmeans(Endocrine_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb              78752  5795 109    68065    91118
#a    acb              96842  9731 167    79416   118091
#b    acb              77380  7775 167    63456    94359
#c    bca              77923  6001 109    66893    90772
#a    bca             101317 10654 167    82323   124693
#b    bca              84810  8918 167    68910   104378

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.813 0.0964 128 -1.745  0.5050 
#c acb / b acb 1.018 0.1206 128  0.148  1.0000 
#c acb / c bca 1.011 0.1077 109  0.099  1.0000 
#c acb / a bca 0.777 0.0998 153 -1.963  0.3685 
#c acb / b bca 0.929 0.1192 153 -0.577  0.9924 
#a acb / b acb 1.252 0.1712 128  1.640  0.5740 
#a acb / c bca 1.243 0.1573 151  1.717  0.5229 
#a acb / a bca 0.956 0.1390 167 -0.311  0.9996 
#a acb / b bca 1.142 0.1661 167  0.912  0.9430 
#b acb / c bca 0.993 0.1257 151 -0.055  1.0000 
#b acb / a bca 0.764 0.1111 167 -1.853  0.4348 
#b acb / b bca 0.912 0.1327 167 -0.630  0.9886 
#c bca / a bca 0.769 0.0954 128 -2.117  0.2853 
#c bca / b bca 0.919 0.1139 128 -0.683  0.9836 
#a bca / b bca 1.195 0.1711 128  1.242  0.8153 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing

lmer(log(Environmental_Adaptation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Environmental_Adaptation) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 266.9302
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1264  
#Residual             0.4858  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#10.8114                  0.1591                 -0.1481                 -0.0357  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.1365                  0.1984 

Environmental_Adaptation_mod <- lmer(log(Environmental_Adaptation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Environmental_Adaptation_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Environmental_Adaptation) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 266.9

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.4859 -0.5996 -0.0234  0.6643  2.4993 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01597  0.1264  
#Residual             0.23595  0.4858  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            10.81143    0.07631 141.669
#Dieta                   0.15910    0.12405   1.283
#Dietb                  -0.14811    0.12405  -1.194
#Diet_Sequencebca       -0.03570    0.11047  -0.323
#Dieta:Diet_Sequencebca  0.13648    0.17956   0.760
#Dietb:Diet_Sequencebca  0.19836    0.17956   1.105

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.542                             
#Dietb       -0.542  0.333                      
#Diet_Sqncbc -0.691  0.374  0.374               
#Dt:Dt_Sqncb  0.374 -0.691 -0.230 -0.542        
#Dtb:Dt_Sqnc  0.374 -0.230 -0.691 -0.542  0.333 

r.squaredGLMM(Environmental_Adaptation_mod)

#R2m       R2c
#[1,] 0.05258727 0.1126623

check_model(Environmental_Adaptation_mod) 

Environmental_Adaptation.emm.s <- emmeans(Environmental_Adaptation_mod, specs = pairwise ~ Diet * Diet_Sequence)
Environmental_Adaptation.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             10.8 0.0763 111     10.7     11.0
#a    acb             11.0 0.1047 168     10.8     11.2
#b    acb             10.7 0.1047 168     10.5     10.9
#c    bca             10.8 0.0799 111     10.6     10.9
#a    bca             11.1 0.1095 168     10.9     11.3
#b    bca             10.8 0.1095 168     10.6     11.0

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95

Environmental_Adaptation.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1591 0.124 128 -1.283  0.7940 
#c acb - b acb   0.1481 0.124 128  1.194  0.8390 
#c acb - c bca   0.0357 0.110 111  0.323  0.9995 
#c acb - a bca  -0.2599 0.133 155 -1.947  0.3780 
#c acb - b bca  -0.0146 0.133 155 -0.109  1.0000 
#a acb - b acb   0.3072 0.143 128  2.145  0.2714 
#a acb - c bca   0.1948 0.132 152  1.480  0.6777 
#a acb - a bca  -0.1008 0.151 168 -0.665  0.9855 
#a acb - b bca   0.1446 0.151 168  0.954  0.9315 
#b acb - c bca  -0.1124 0.132 152 -0.854  0.9566 
#b acb - a bca  -0.4080 0.151 168 -2.693  0.0820 
#b acb - b bca  -0.1627 0.151 168 -1.074  0.8912 
#c bca - a bca  -0.2956 0.130 128 -2.277  0.2112 
#c bca - b bca  -0.0502 0.130 128 -0.387  0.9989 
#a bca - b bca   0.2453 0.150 128  1.637  0.5760 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Environmental_Adaptation_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              49584 3784 111    42626    57679
#a    acb              58135 6084 168    47283    71478
#b    acb              42758 4475 168    34777    52572
#c    bca              47846 3821 111    40843    56049
#a    bca              64300 7043 168    51797    79821
#b    bca              50311 5511 168    40528    62456

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.853 0.1058 128 -1.283  0.7940 
#c acb / b acb 1.160 0.1439 128  1.194  0.8390 
#c acb / c bca 1.036 0.1145 111  0.323  0.9995 
#c acb / a bca 0.771 0.1029 155 -1.947  0.3780 
#c acb / b bca 0.986 0.1316 155 -0.109  1.0000 
#a acb / b acb 1.360 0.1948 128  2.145  0.2714 
#a acb / c bca 1.215 0.1600 152  1.480  0.6777 
#a acb / a bca 0.904 0.1370 168 -0.665  0.9855 
#a acb / b bca 1.156 0.1751 168  0.954  0.9315 
#b acb / c bca 0.894 0.1177 152 -0.854  0.9566 
#b acb / a bca 0.665 0.1007 168 -2.693  0.0820 
#b acb / b bca 0.850 0.1288 168 -1.074  0.8912 
#c bca / a bca 0.744 0.0966 128 -2.277  0.2112 
#c bca / b bca 0.951 0.1235 128 -0.387  0.9989 
#a bca / b bca 1.278 0.1916 128  1.637  0.5760 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

########################################

lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 288.7197
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1328  
#Residual             0.5183  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#8.4724                 -0.0511                 -0.7802                 -0.2051  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.1317                  0.5534  

Excretory_System_mod <- lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Excretory_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 288.7

#Scaled residuals: 
 # Min       1Q   Median       3Q      Max 
#-2.71743 -0.57906  0.00591  0.73886  2.71892 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01764  0.1328  
#Residual             0.26865  0.5183  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)             8.47245    0.08128 104.232
#Dieta                  -0.05110    0.13236  -0.386
#Dietb                  -0.78024    0.13236  -5.895
#Diet_Sequencebca       -0.20510    0.11766  -1.743
#Dieta:Diet_Sequencebca  0.13167    0.19160   0.687
#Dietb:Diet_Sequencebca  0.55341    0.19160   2.888

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.543                             
#Dietb       -0.543  0.333                      
#Diet_Sqncbc -0.691  0.375  0.375               
#Dt:Dt_Sqncb  0.375 -0.691 -0.230 -0.543        
#Dtb:Dt_Sqnc  0.375 -0.230 -0.691 -0.543  0.333 

r.squaredGLMM(Excretory_System_mod)

#R2m       R2c
#[1,] 0.1832758 0.2336003

check_model(Excretory_System_mod) 

Excretory_System.emm.s <- emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Excretory_System.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             8.47 0.0813 112     8.31     8.63
#a    acb             8.42 0.1116 168     8.20     8.64
#b    acb             7.69 0.1116 168     7.47     7.91
#c    bca             8.27 0.0851 112     8.10     8.44
#a    bca             8.35 0.1168 168     8.12     8.58
#b    bca             8.04 0.1168 168     7.81     8.27

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Excretory_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb   0.0511 0.132 128  0.386  0.9989 
#c acb - b acb   0.7802 0.132 128  5.895  <.0001 
#c acb - c bca   0.2051 0.118 112  1.743  0.5065 
#c acb - a bca   0.1245 0.142 155  0.875  0.9519 
#c acb - b bca   0.4319 0.142 155  3.036  0.0329 
#a acb - b acb   0.7291 0.153 128  4.771  0.0001 
#a acb - c bca   0.1540 0.140 153  1.098  0.8816 
#a acb - a bca   0.0734 0.161 168  0.455  0.9975 
#a acb - b bca   0.3808 0.161 168  2.358  0.1772 
#b acb - c bca  -0.5751 0.140 153 -4.099  0.0009 
#b acb - a bca  -0.6557 0.161 168 -4.060  0.0010 
#b acb - b bca  -0.3483 0.161 168 -2.157  0.2638 
#c bca - a bca  -0.0806 0.139 128 -0.582  0.9921 
#c bca - b bca   0.2268 0.139 128  1.637  0.5755 
#a bca - b bca   0.3074 0.160 128  1.922  0.3938 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response  SE  df lower.CL upper.CL
#c    acb               4781 389 112     4070     5617
#a    acb               4543 507 168     3645     5662
#b    acb               2191 244 168     1758     2731
#c    bca               3895 331 112     3290     4610
#a    bca               4221 493 168     3352     5316
#b    bca               3104 362 168     2465     3909

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts

#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 1.052 0.1393 128  0.386  0.9989 
#c acb / b acb 2.182 0.2888 128  5.895  <.0001 
#c acb / c bca 1.228 0.1444 112  1.743  0.5065 
#c acb / a bca 1.133 0.1611 155  0.875  0.9519 
#c acb / b bca 1.540 0.2191 155  3.036  0.0329 
#a acb / b acb 2.073 0.3169 128  4.771  0.0001 
#a acb / c bca 1.166 0.1637 153  1.098  0.8816 
#a acb / a bca 1.076 0.1738 168  0.455  0.9975 
#a acb / b bca 1.464 0.2363 168  2.358  0.1772 
#b acb / c bca 0.563 0.0789 153 -4.099  0.0009 
#b acb / a bca 0.519 0.0838 168 -4.060  0.0010 
#b acb / b bca 0.706 0.1140 168 -2.157  0.2638 
#c bca / a bca 0.923 0.1278 128 -0.582  0.9921 
#c bca / b bca 1.255 0.1738 128  1.637  0.5755 
#a bca / b bca 1.360 0.2175 128  1.922  0.3938 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Folding_Sorting_and_Degradation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Folding_Sorting_and_Degradation) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 248.2751
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1301  
#Residual             0.4575  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.54134                 0.16187                -0.09697                -0.02676  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.05944                 0.13042  

Folding_Sorting_and_Degradation_mod <- lmer(log(Folding_Sorting_and_Degradation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Folding_Sorting_and_Degradation_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Folding_Sorting_and_Degradation) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 248.3

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.5053 -0.5889 -0.0139  0.6884  2.3829 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01693  0.1301  
#Residual             0.20932  0.4575  
#Number of obs: 176, groups:  Subject, 44


#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.54134    0.07271 186.243
#Dieta                   0.16187    0.11684   1.385
#Dietb                  -0.09697    0.11684  -0.830
#Diet_Sequencebca       -0.02676    0.10524  -0.254
#Dieta:Diet_Sequencebca  0.05944    0.16912   0.351
#Dietb:Diet_Sequencebca  0.13042    0.16912   0.771

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.536                             
#Dietb       -0.536  0.333                      
#Diet_Sqncbc -0.691  0.370  0.370               
#Dt:Dt_Sqncb  0.370 -0.691 -0.230 -0.536        
#Dtb:Dt_Sqnc  0.370 -0.230 -0.691 -0.536  0.333

r.squaredGLMM(Folding_Sorting_and_Degradation_mod)

#R2m       R2c
#[1,] 0.03708357 0.1091306

check_model(Folding_Sorting_and_Degradation_mod) 

Folding_Sorting_and_Degradation.emm.s <- emmeans(Folding_Sorting_and_Degradation_mod, specs = pairwise ~ Diet * Diet_Sequence)
Folding_Sorting_and_Degradation.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.5 0.0727 109     13.4     13.7
#a    acb             13.7 0.0992 167     13.5     13.9
#b    acb             13.4 0.0992 167     13.2     13.6
#c    bca             13.5 0.0761 109     13.4     13.7
#a    bca             13.7 0.1038 167     13.5     13.9
#b    bca             13.5 0.1038 167     13.3     13.8

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Folding_Sorting_and_Degradation.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.16187 0.117 128 -1.385  0.7357 
#c acb - b acb  0.09697 0.117 128  0.830  0.9614 
#c acb - c bca  0.02676 0.105 109  0.254  0.9999 
#c acb - a bca -0.19455 0.127 153 -1.535  0.6422 
#c acb - b bca -0.00669 0.127 153 -0.053  1.0000 
#a acb - b acb  0.25884 0.135 128  1.919  0.3957 
#a acb - c bca  0.18863 0.125 150  1.509  0.6591 
#a acb - a bca -0.03268 0.144 167 -0.228  0.9999 
#a acb - b bca  0.15518 0.144 167  1.081  0.8884 
#b acb - c bca -0.07022 0.125 150 -0.562  0.9933 
#b acb - a bca -0.29152 0.144 167 -2.031  0.3296 
#b acb - b bca -0.10367 0.144 167 -0.722  0.9790 
#c bca - a bca -0.22131 0.122 128 -1.810  0.4631 
#c bca - b bca -0.03345 0.122 128 -0.274  0.9998 
#a bca - b bca  0.18786 0.141 128  1.331  0.7676 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Folding_Sorting_and_Degradation_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             760200 55273 109   658179   878035
#a    acb             893771 88645 167   734829  1087091
#b    acb             689942 68429 167   567248   839174
#c    bca             740128 56317 109   636518   860603
#a    bca             923462 95852 167   752353  1133486
#b    bca             765304 79436 167   623500   939357

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.851 0.0994 128 -1.385  0.7357 
#c acb / b acb 1.102 0.1287 128  0.830  0.9614 
#c acb / c bca 1.027 0.1081 109  0.254  0.9999 
#c acb / a bca 0.823 0.1043 153 -1.535  0.6422 
#c acb / b bca 0.993 0.1259 153 -0.053  1.0000 
#a acb / b acb 1.295 0.1748 128  1.919  0.3957 
#a acb / c bca 1.208 0.1510 150  1.509  0.6591 
#a acb / a bca 0.968 0.1389 167 -0.228  0.9999 
#a acb / b bca 1.168 0.1677 167  1.081  0.8884 
#b acb / c bca 0.932 0.1165 150 -0.562  0.9933 
#b acb / a bca 0.747 0.1073 167 -2.031  0.3296 
#b acb / b bca 0.902 0.1294 167 -0.722  0.9790 
#c bca / a bca 0.801 0.0980 128 -1.810  0.4631 
#c bca / b bca 0.967 0.1183 128 -0.274  0.9998 
#a bca / b bca 1.207 0.1704 128  1.331  0.7676 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Genetic_Information_Processing) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Genetic_Information_Processing) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 247.3168
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1310  
#Residual             0.4559  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.65853                 0.15479                -0.08238                -0.03935  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.09933                 0.13349

Genetic_Information_Processing_mod <- lmer(log(Genetic_Information_Processing) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Genetic_Information_Processing_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Genetic_Information_Processing) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 247.3

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3690 -0.6438 -0.0032  0.6680  2.4377 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01717  0.1310  
#Residual             0.20787  0.4559  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.65853    0.07256 188.226
#Dieta                   0.15479    0.11643   1.329
#Dietb                  -0.08238    0.11643  -0.707
#Diet_Sequencebca       -0.03935    0.10504  -0.375
#Dieta:Diet_Sequencebca  0.09933    0.16854   0.589
#Dietb:Diet_Sequencebca  0.13349    0.16854   0.792

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.535                             
#Dietb       -0.535  0.333                      
#Diet_Sqncbc -0.691  0.370  0.370               
#Dt:Dt_Sqncb  0.370 -0.691 -0.230 -0.535        
#Dtb:Dt_Sqnc  0.370 -0.230 -0.691 -0.535  0.333 

r.squaredGLMM(Genetic_Information_Processing_mod)

#R2m       R2c
#[1,] 0.03938162 0.1126818

check_model(Genetic_Information_Processing_mod) 

Genetic_Information_Processing.emm.s <- emmeans(Genetic_Information_Processing_mod, specs = pairwise ~ Diet * Diet_Sequence)
Genetic_Information_Processing.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.7 0.0726 109     13.5     13.8
#a    acb             13.8 0.0989 167     13.6     14.0
#b    acb             13.6 0.0989 167     13.4     13.8
#c    bca             13.6 0.0759 109     13.5     13.8
#a    bca             13.9 0.1035 167     13.7     14.1
#b    bca             13.7 0.1035 167     13.5     13.9

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Genetic_Information_Processing.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1548 0.116 128 -1.329  0.7682 
#c acb - b acb   0.0824 0.116 128  0.707  0.9807 
#c acb - c bca   0.0393 0.105 109  0.375  0.9990 
#c acb - a bca  -0.2148 0.126 152 -1.699  0.5346 
#c acb - b bca  -0.0118 0.126 152 -0.093  1.0000 
#a acb - b acb   0.2372 0.134 128  1.764  0.4926 
#a acb - c bca   0.1941 0.125 150  1.557  0.6282 
#a acb - a bca  -0.0600 0.143 167 -0.419  0.9983 
#a acb - b bca   0.1430 0.143 167  0.999  0.9177 
#b acb - c bca  -0.0430 0.125 150 -0.345  0.9993 
#b acb - a bca  -0.2972 0.143 167 -2.075  0.3053 
#b acb - b bca  -0.0941 0.143 167 -0.657  0.9862 
#c bca - a bca  -0.2541 0.122 128 -2.086  0.3014 
#c bca - b bca  -0.0511 0.122 128 -0.419  0.9983 
#a bca - b bca   0.2030 0.141 128  1.443  0.7008 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Genetic_Information_Processing_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb             854720  62022 109   740221   986929
#a    acb             997814  98701 167   820796  1213007
#b    acb             787132  77861 167   647491   956890
#c    bca             821744  62404 109   706915   955224
#a    bca            1059504 109680 167   863657  1299761
#b    bca             864833  89528 167   704971  1060946

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.857 0.0997 128 -1.329  0.7682 
#c acb / b acb 1.086 0.1264 128  0.707  0.9807 
#c acb / c bca 1.040 0.1093 109  0.375  0.9990 
#c acb / a bca 0.807 0.1020 152 -1.699  0.5346 
#c acb / b bca 0.988 0.1249 152 -0.093  1.0000 
#a acb / b acb 1.268 0.1704 128  1.764  0.4926 
#a acb / c bca 1.214 0.1514 150  1.557  0.6282 
#a acb / a bca 0.942 0.1348 167 -0.419  0.9983 
#a acb / b bca 1.154 0.1652 167  0.999  0.9177 
#b acb / c bca 0.958 0.1195 150 -0.345  0.9993 
#b acb / a bca 0.743 0.1064 167 -2.075  0.3053 
#b acb / b bca 0.910 0.1303 167 -0.657  0.9862 
#c bca / a bca 0.776 0.0945 128 -2.086  0.3014 
#c bca / b bca 0.950 0.1158 128 -0.419  0.9983 
#a bca / b bca 1.225 0.1724 128  1.443  0.7008 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Glycan_Biosynthesis_and_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Glycan_Biosynthesis_and_Metabolism) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 236.4481
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1185  
#Residual             0.4435  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.38351                 0.19272                -0.14857                -0.01380  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#-0.06258                 0.20557  

Glycan_Biosynthesis_and_Metabolism_mod <- lmer(log(Glycan_Biosynthesis_and_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Glycan_Biosynthesis_and_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Glycan_Biosynthesis_and_Metabolism) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 236.4

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.1880 -0.6503  0.0310  0.7996  2.0674 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01405  0.1185  
#Residual             0.19665  0.4435  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            13.38351    0.06990 191.471
#Dieta                   0.19272    0.11325   1.702
#Dietb                  -0.14857    0.11325  -1.312
#Diet_Sequencebca       -0.01380    0.10118  -0.136
#Dieta:Diet_Sequencebca -0.06258    0.16393  -0.382
#Dietb:Diet_Sequencebca  0.20557    0.16393   1.254

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.540                             
#Dietb       -0.540  0.333                      
#Diet_Sqncbc -0.691  0.373  0.373               
#Dt:Dt_Sqncb  0.373 -0.691 -0.230 -0.540        
#Dtb:Dt_Sqnc  0.373 -0.230 -0.691 -0.540  0.333 

r.squaredGLMM(Glycan_Biosynthesis_and_Metabolism_mod)

#R2m       R2c
#[1,] 0.04188623 0.1057579

check_model(Glycan_Biosynthesis_and_Metabolism_mod) 

Glycan_Biosynthesis_and_Metabolism.emm.s <- emmeans(Glycan_Biosynthesis_and_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Glycan_Biosynthesis_and_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.4 0.0699 111     13.2     13.5
#a    acb             13.6 0.0957 167     13.4     13.8
#b    acb             13.2 0.0957 167     13.0     13.4
#c    bca             13.4 0.0732 111     13.2     13.5
#a    bca             13.5 0.1002 167     13.3     13.7
#b    bca             13.4 0.1002 167     13.2     13.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

#Glycan_Biosynthesis_and_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1927 0.113 128 -1.702  0.5332 
#c acb - b acb   0.1486 0.113 128  1.312  0.7780 
#c acb - c bca   0.0138 0.101 111  0.136  1.0000 
#c acb - a bca  -0.1163 0.122 154 -0.953  0.9319 
#c acb - b bca  -0.0432 0.122 154 -0.354  0.9993 
#a acb - b acb   0.3413 0.131 128  2.610  0.1023 
#a acb - c bca   0.2065 0.120 152  1.714  0.5245 
#a acb - a bca   0.0764 0.139 167  0.551  0.9939 
#a acb - b bca   0.1495 0.139 167  1.079  0.8890 
#b acb - c bca  -0.1348 0.120 152 -1.119  0.8730 
#b acb - a bca  -0.2649 0.139 167 -1.912  0.3984 
#b acb - b bca  -0.1918 0.139 167 -1.384  0.7365 
#c bca - a bca  -0.1301 0.119 128 -1.098  0.8813 
#c bca - b bca  -0.0570 0.119 128 -0.481  0.9968 
#a bca - b bca   0.0732 0.137 128  0.535  0.9947 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Glycan_Biosynthesis_and_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             649209 45379 111   565236   745658
#a    acb             787196 75345 167   651657   950926
#b    acb             559576 53558 167   463229   675963
#c    bca             640309 46839 111   553905   740192
#a    bca             729309 73052 167   598452   888780
#b    bca             677864 67899 167   556237   826086

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.825 0.0934 128 -1.702  0.5332 
#c acb / b acb 1.160 0.1314 128  1.312  0.7780 
#c acb / c bca 1.014 0.1026 111  0.136  1.0000 
#c acb / a bca 0.890 0.1087 154 -0.953  0.9319 
#c acb / b bca 0.958 0.1170 154 -0.354  0.9993 
#a acb / b acb 1.407 0.1840 128  2.610  0.1023 
#a acb / c bca 1.229 0.1481 152  1.714  0.5245 
#a acb / a bca 1.079 0.1495 167  0.551  0.9939 
#a acb / b bca 1.161 0.1609 167  1.079  0.8890 
#b acb / c bca 0.874 0.1053 152 -1.119  0.8730 
#b acb / a bca 0.767 0.1063 167 -1.912  0.3984 
#b acb / b bca 0.825 0.1144 167 -1.384  0.7365 
#c bca / a bca 0.878 0.1041 128 -1.098  0.8813 
#c bca / b bca 0.945 0.1120 128 -0.481  0.9968 
#a bca / b bca 1.076 0.1472 128  0.535  0.9947 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Immune_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Immune_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 274.2898
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1285  
#Residual             0.4965  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#10.150604                0.233586               -0.115918                0.016995  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.000993                0.155622  

Immune_System_mod <- lmer(log(Immune_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Immune_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Immune_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 274.3

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-4.0706 -0.5480  0.0429  0.6800  2.3658 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01652  0.1285  
#Residual             0.24653  0.4965  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            10.150604   0.077958 130.206
#Dieta                   0.233586   0.126798   1.842
#Dietb                  -0.115918   0.126798  -0.914
#Diet_Sequencebca        0.016995   0.112844   0.151
#Dieta:Diet_Sequencebca  0.000993   0.183540   0.005
#Dietb:Diet_Sequencebca  0.155622   0.183540   0.848

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.542                             
#Dietb       -0.542  0.333                      
#Diet_Sqncbc -0.691  0.375  0.375               
#Dt:Dt_Sqncb  0.375 -0.691 -0.230 -0.542        
#Dtb:Dt_Sqnc  0.375 -0.230 -0.691 -0.542  0.333 

r.squaredGLMM(Immune_System_mod)

#R2m       R2c
#[1,] 0.04987721 0.1095402

check_model(Immune_System_mod) 

Immune_System.emm.s <- emmeans(Immune_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Immune_System.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             10.2 0.0780 112    10.00     10.3
#a    acb             10.4 0.1069 168    10.17     10.6
#b    acb             10.0 0.1069 168     9.82     10.2
#c    bca             10.2 0.0816 112    10.01     10.3
#a    bca             10.4 0.1119 168    10.18     10.6
#b    bca             10.2 0.1119 168     9.99     10.4

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Immune_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.2336 0.127 128 -1.842  0.4427 
#c acb - b acb   0.1159 0.127 128  0.914  0.9423 
#c acb - c bca  -0.0170 0.113 112 -0.151  1.0000 
#c acb - a bca  -0.2516 0.136 155 -1.844  0.4405 
#c acb - b bca  -0.0567 0.136 155 -0.416  0.9984 
#a acb - b acb   0.3495 0.146 128  2.387  0.1686 
#a acb - c bca   0.2166 0.135 152  1.610  0.5931 
#a acb - a bca  -0.0180 0.155 168 -0.116  1.0000 
#a acb - b bca   0.1769 0.155 168  1.143  0.8628 
#b acb - c bca  -0.1329 0.135 152 -0.988  0.9211 
#b acb - a bca  -0.3675 0.155 168 -2.374  0.1714 
#b acb - b bca  -0.1726 0.155 168 -1.115  0.8746 
#c bca - a bca  -0.2346 0.133 128 -1.768  0.4902 
#c bca - b bca  -0.0397 0.133 128 -0.299  0.9997 
#a bca - b bca   0.1949 0.153 128  1.272  0.7998 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Immune_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              25607 1996 112    21941    29884
#a    acb              32344 3459 168    26188    39947
#b    acb              22804 2439 168    18464    28164
#c    bca              26045 2125 112    22158    30615
#a    bca              32931 3686 168    26403    41074
#b    bca              27100 3033 168    21728    33801

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.792 0.100 128 -1.842  0.4427 
#c acb / b acb 1.123 0.142 128  0.914  0.9423 
#c acb / c bca 0.983 0.111 112 -0.151  1.0000 
#c acb / a bca 0.778 0.106 155 -1.844  0.4405 
#c acb / b bca 0.945 0.129 155 -0.416  0.9984 
#a acb / b acb 1.418 0.208 128  2.387  0.1686 
#a acb / c bca 1.242 0.167 152  1.610  0.5931 
#a acb / a bca 0.982 0.152 168 -0.116  1.0000 
#a acb / b bca 1.193 0.185 168  1.143  0.8628 
#b acb / c bca 0.876 0.118 152 -0.988  0.9211 
#b acb / a bca 0.692 0.107 168 -2.374  0.1714 
#b acb / b bca 0.841 0.130 168 -1.115  0.8746 
#c bca / a bca 0.791 0.105 128 -1.768  0.4902 
#c bca / b bca 0.961 0.128 128 -0.299  0.9997 
#a bca / b bca 1.215 0.186 128  1.272  0.7998 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Immune_System_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Immune_System_Diseases) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 260.6709
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1333  
#Residual             0.4749  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#9.599e+00               3.232e-01               1.130e-02              -1.056e-05  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#4.310e-02               7.726e-02  

Immune_System_Diseases_mod <- lmer(log(Immune_System_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Immune_System_Diseases_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Immune_System_Diseases) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 260.7

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.1681 -0.6354 -0.0220  0.7177  2.0588 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01776  0.1333  
#Residual             0.22551  0.4749  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)             9.599e+00  7.533e-02 127.430
#Dieta                   3.232e-01  1.213e-01   2.665
#Dietb                   1.130e-02  1.213e-01   0.093
#Diet_Sequencebca       -1.056e-05  1.090e-01   0.000
#Dieta:Diet_Sequencebca  4.310e-02  1.755e-01   0.246
#Dietb:Diet_Sequencebca  7.726e-02  1.755e-01   0.440

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.537                             
#Dietb       -0.537  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.537        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.537  0.333 

r.squaredGLMM(Immune_System_Diseases_mod)

#R2m       R2c
#[1,] 0.07991269 0.1470816

check_model(Immune_System_Diseases_mod) 


Immune_System_Diseases.emm.s <- emmeans(Immune_System_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence)
Immune_System_Diseases.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             9.60 0.0753 109     9.45     9.75
#a    acb             9.92 0.1028 167     9.72    10.13
#b    acb             9.61 0.1028 167     9.41     9.81
#c    bca             9.60 0.0788 109     9.44     9.76
#a    bca             9.97 0.1076 167     9.75    10.18
#b    bca             9.69 0.1076 167     9.48     9.90

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Immune_System_Diseases.emm.s$contrasts

#contrast       estimate    SE  df t.ratio p.value
#c acb - a acb -3.23e-01 0.121 128 -2.665  0.0896 
#c acb - b acb -1.13e-02 0.121 128 -0.093  1.0000 
#c acb - c bca  1.06e-05 0.109 109  0.000  1.0000 
#c acb - a bca -3.66e-01 0.131 153 -2.788  0.0648 
#c acb - b bca -8.85e-02 0.131 153 -0.674  0.9845 
#a acb - b acb  3.12e-01 0.140 128  2.227  0.2327 
#a acb - c bca  3.23e-01 0.130 151  2.494  0.1324 
#a acb - a bca -4.31e-02 0.149 167 -0.289  0.9997 
#a acb - b bca  2.35e-01 0.149 167  1.576  0.6154 
#b acb - c bca  1.13e-02 0.130 151  0.087  1.0000 
#b acb - a bca -3.55e-01 0.149 167 -2.384  0.1677 
#b acb - b bca -7.72e-02 0.149 167 -0.519  0.9954 
#c bca - a bca -3.66e-01 0.127 128 -2.886  0.0510 
#c bca - b bca -8.86e-02 0.127 128 -0.698  0.9819 
#a bca - b bca  2.78e-01 0.147 128  1.895  0.4100 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Immune_System_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              14753 1111 109    12707    17129
#a    acb              20382 2096 167    16637    24971
#b    acb              14921 1535 167    12179    18280
#c    bca              14753 1163 109    12619    17248
#a    bca              21279 2290 167    17206    26317
#b    bca              16119 1735 167    13034    19936

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.724 0.0878 128 -2.665  0.0896 
#c acb / b acb 0.989 0.1199 128 -0.093  1.0000 
#c acb / c bca 1.000 0.1090 109  0.000  1.0000 
#c acb / a bca 0.693 0.0911 153 -2.788  0.0648 
#c acb / b bca 0.915 0.1202 153 -0.674  0.9845 
#a acb / b acb 1.366 0.1913 128  2.227  0.2327 
#a acb / c bca 1.382 0.1790 151  2.494  0.1324 
#a acb / a bca 0.958 0.1426 167 -0.289  0.9997 
#a acb / b bca 1.264 0.1882 167  1.576  0.6154 
#b acb / c bca 1.011 0.1311 151  0.087  1.0000 
#b acb / a bca 0.701 0.1044 167 -2.384  0.1677 
#b acb / b bca 0.926 0.1378 167 -0.519  0.9954 
#c bca / a bca 0.693 0.0880 128 -2.886  0.0510 
#c bca / b bca 0.915 0.1162 128 -0.698  0.9819 
#a bca / b bca 1.320 0.1935 128  1.895  0.4100 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Infectious_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Infectious_Diseases) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 248.1606
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1199  
#Residual             0.4596  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#11.66944                 0.13967                -0.10613                -0.04904  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.07668                 0.11763 

Infectious_Diseases_mod <- lmer(log(Infectious_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Infectious_Diseases_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Infectious_Diseases) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 248.2

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.5638 -0.6103 -0.0001  0.7329  2.4462 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01438  0.1199  
#Residual             0.21123  0.4596  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            11.66944    0.07223 161.562
#Dieta                   0.13967    0.11737   1.190
#Dietb                  -0.10613    0.11737  -0.904
#Diet_Sequencebca       -0.04904    0.10455  -0.469
#Dieta:Diet_Sequencebca  0.07668    0.16989   0.451
#Dietb:Diet_Sequencebca  0.11763    0.16989   0.692

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.542                             
#Dietb       -0.542  0.333                      
#Diet_Sqncbc -0.691  0.374  0.374               
#Dt:Dt_Sqncb  0.374 -0.691 -0.230 -0.542        
#Dtb:Dt_Sqnc  0.374 -0.230 -0.691 -0.542  0.333 

r.squaredGLMM(Infectious_Diseases_mod)

#R2m        R2c
#[1,] 0.03458672 0.09610651

check_model(Infectious_Diseases_mod) 

Infectious_Diseases.emm.s <- emmeans(Infectious_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence)
Infectious_Diseases.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             11.7 0.0722 111     11.5     11.8
#a    acb             11.8 0.0990 168     11.6     12.0
#b    acb             11.6 0.0990 168     11.4     11.8
#c    bca             11.6 0.0756 111     11.5     11.8
#a    bca             11.8 0.1036 168     11.6     12.0
#b    bca             11.6 0.1036 168     11.4     11.8

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Infectious_Diseases.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1397 0.117 128 -1.190  0.8409 
#c acb - b acb   0.1061 0.117 128  0.904  0.9448 
#c acb - c bca   0.0490 0.105 111  0.469  0.9971 
#c acb - a bca  -0.1673 0.126 155 -1.324  0.7711 
#c acb - b bca   0.0375 0.126 155  0.297  0.9997 
#a acb - b acb   0.2458 0.136 128  1.814  0.4607 
#a acb - c bca   0.1887 0.125 152  1.515  0.6554 
#a acb - a bca  -0.0276 0.143 168 -0.193  1.0000 
#a acb - b bca   0.1772 0.143 168  1.236  0.8185 
#b acb - c bca  -0.0571 0.125 152 -0.458  0.9974 
#b acb - a bca  -0.2734 0.143 168 -1.907  0.4013 
#b acb - b bca  -0.0686 0.143 168 -0.478  0.9968 
#c bca - a bca  -0.2164 0.123 128 -1.761  0.4943 
#c bca - b bca  -0.0115 0.123 128 -0.094  1.0000 
#a bca - b bca   0.2049 0.142 128  1.444  0.6999 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Infectious_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             116943  8447 111   101349   134937
#a    acb             134472 13318 168   110590   163511
#b    acb             105168 10416 168    86490   127879
#c    bca             111346  8417 111    95858   129338
#a    bca             138241 14329 168   112660   169631
#b    bca             112634 11674 168    91792   138210

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.870 0.1021 128 -1.190  0.8409 
#c acb / b acb 1.112 0.1305 128  0.904  0.9448 
#c acb / c bca 1.050 0.1098 111  0.469  0.9971 
#c acb / a bca 0.846 0.1069 155 -1.324  0.7711 
#c acb / b bca 1.038 0.1312 155  0.297  0.9997 
#a acb / b acb 1.279 0.1733 128  1.814  0.4607 
#a acb / c bca 1.208 0.1505 152  1.515  0.6554 
#a acb / a bca 0.973 0.1395 168 -0.193  1.0000 
#a acb / b bca 1.194 0.1712 168  1.236  0.8185 
#b acb / c bca 0.945 0.1177 152 -0.458  0.9974 
#b acb / a bca 0.761 0.1091 168 -1.907  0.4013 
#b acb / b bca 0.934 0.1339 168 -0.478  0.9968 
#c bca / a bca 0.805 0.0989 128 -1.761  0.4943 
#c bca / b bca 0.989 0.1214 128 -0.094  1.0000 
#a bca / b bca 1.227 0.1741 128  1.444  0.6999 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Lipid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Lipid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 248.4475
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1289  
#Residual             0.4580  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.68038                 0.14016                -0.12382                -0.05508  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.12092                 0.15933

Lipid_Metabolism_mod <- lmer(log(Lipid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Lipid_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Lipid_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 248.4

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3317 -0.6482  0.0181  0.7133  2.4436 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01661  0.1289  
#Residual             0.20980  0.4580  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.68038    0.07268 188.217
#Dieta                   0.14016    0.11697   1.198
#Dietb                  -0.12382    0.11697  -1.059
#Diet_Sequencebca       -0.05508    0.10521  -0.524
#Dieta:Diet_Sequencebca  0.12092    0.16932   0.714
#Dietb:Diet_Sequencebca  0.15933    0.16932   0.941

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.536                             
#Dietb       -0.536  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.536        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.536  0.333 

r.squaredGLMM(Lipid_Metabolism_mod)

#R2m      R2c
#[1,] 0.04366225 0.113811

check_model(Lipid_Metabolism_mod) 


Lipid_Metabolism.emm.s <- emmeans(Lipid_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Lipid_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.7 0.0727 109     13.5     13.8
#a    acb             13.8 0.0992 167     13.6     14.0
#b    acb             13.6 0.0992 167     13.4     13.8
#c    bca             13.6 0.0761 109     13.5     13.8
#a    bca             13.9 0.1038 167     13.7     14.1
#b    bca             13.7 0.1038 167     13.5     13.9

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Lipid_Metabolism.emm.s$contrasts

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.7 0.0727 109     13.5     13.8
#a    acb             13.8 0.0992 167     13.6     14.0
#b    acb             13.6 0.0992 167     13.4     13.8
#c    bca             13.6 0.0761 109     13.5     13.8
#a    bca             13.9 0.1038 167     13.7     14.1
#b    bca             13.7 0.1038 167     13.5     13.9

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

#> Lipid_Metabolism.emm.s$contrasts
#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1402 0.117 128 -1.198  0.8370 
#c acb - b acb   0.1238 0.117 128  1.059  0.8967 
#c acb - c bca   0.0551 0.105 109  0.524  0.9951 
#c acb - a bca  -0.2060 0.127 153 -1.625  0.5831 
#c acb - b bca   0.0196 0.127 153  0.154  1.0000 
#a acb - b acb   0.2640 0.135 128  1.954  0.3744 
#a acb - c bca   0.1952 0.125 150  1.562  0.6249 
#a acb - a bca  -0.0658 0.144 167 -0.458  0.9974 
#a acb - b bca   0.1597 0.144 167  1.112  0.8757 
#b acb - c bca  -0.0687 0.125 150 -0.550  0.9939 
#b acb - a bca  -0.3298 0.144 167 -2.297  0.2013 
#b acb - b bca  -0.1042 0.144 167 -0.726  0.9785 
#c bca - a bca  -0.2611 0.122 128 -2.133  0.2773 
#c bca - b bca  -0.0355 0.122 128 -0.290  0.9997 
#a bca - b bca   0.2256 0.141 128  1.596  0.6028 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Lipid_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb             873602  63497 109   756401  1008961
#a    acb            1005045  99717 167   826259  1222517
#b    acb             771859  76581 167   634554   938874
#c    bca             826781  62890 109   711079   961309
#a    bca            1073444 111459 167   874482  1317673
#b    bca             856664  88950 167   697882  1051572

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.869 0.1017 128 -1.198  0.8370 
#c acb / b acb 1.132 0.1324 128  1.059  0.8967 
#c acb / c bca 1.057 0.1112 109  0.524  0.9951 
#c acb / a bca 0.814 0.1031 153 -1.625  0.5831 
#c acb / b bca 1.020 0.1293 153  0.154  1.0000 
#a acb / b acb 1.302 0.1759 128  1.954  0.3744 
#a acb / c bca 1.216 0.1520 150  1.562  0.6249 
#a acb / a bca 0.936 0.1345 167 -0.458  0.9974 
#a acb / b bca 1.173 0.1685 167  1.112  0.8757 
#b acb / c bca 0.934 0.1167 150 -0.550  0.9939 
#b acb / a bca 0.719 0.1033 167 -2.297  0.2013 
#b acb / b bca 0.901 0.1294 167 -0.726  0.9785 
#c bca / a bca 0.770 0.0943 128 -2.133  0.2773 
#c bca / b bca 0.965 0.1181 128 -0.290  0.9997 
#a bca / b bca 1.253 0.1771 128  1.596  0.6028 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Membrane_Transport) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Membrane_Transport) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 262.1227
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1230  
#Residual             0.4793  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#15.23331                 0.14302                -0.03805                -0.05908  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.18395                 0.10694  

Membrane_Transport_mod <- lmer(log(Membrane_Transport) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Membrane_Transport_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Membrane_Transport) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 262.1

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3258 -0.6216  0.0079  0.6617  2.6176 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01512  0.1230  
#Residual             0.22971  0.4793  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            15.23331    0.07517 202.640
#Dieta                   0.14302    0.12240   1.169
#Dietb                  -0.03805    0.12240  -0.311
#Diet_Sequencebca       -0.05908    0.10881  -0.543
#Dieta:Diet_Sequencebca  0.18395    0.17717   1.038
#Dietb:Diet_Sequencebca  0.10694    0.17717   0.604

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.543                             
#Dietb       -0.543  0.333                      
#Diet_Sqncbc -0.691  0.375  0.375               
#Dt:Dt_Sqncb  0.375 -0.691 -0.230 -0.543        
#Dtb:Dt_Sqnc  0.375 -0.230 -0.691 -0.543  0.333 

r.squaredGLMM(Membrane_Transport_mod)

#R2m       R2c
#[1,] 0.04397179 0.1030241
 
check_model(Membrane_Transport_mod) 

Membrane_Transport.emm.s <- emmeans(Membrane_Transport_mod, specs = pairwise ~ Diet * Diet_Sequence)
Membrane_Transport.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             15.2 0.0752 112     15.1     15.4
#a    acb             15.4 0.1032 168     15.2     15.6
#b    acb             15.2 0.1032 168     15.0     15.4
#c    bca             15.2 0.0787 112     15.0     15.3
#a    bca             15.5 0.1080 168     15.3     15.7
#b    bca             15.2 0.1080 168     15.0     15.5

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Membrane_Transport.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.14302 0.122 128 -1.169  0.8509 
#c acb - b acb  0.03805 0.122 128  0.311  0.9996 
#c acb - c bca  0.05908 0.109 112  0.543  0.9942 
#c acb - a bca -0.26789 0.132 155 -2.036  0.3269 
#c acb - b bca -0.00981 0.132 155 -0.075  1.0000 
#a acb - b acb  0.18107 0.141 128  1.281  0.7948 
#a acb - c bca  0.20211 0.130 153  1.558  0.6275 
#a acb - a bca -0.12486 0.149 168 -0.836  0.9604 
#a acb - b bca  0.13321 0.149 168  0.892  0.9480 
#b acb - c bca  0.02104 0.130 153  0.162  1.0000 
#b acb - a bca -0.30594 0.149 168 -2.049  0.3197 
#b acb - b bca -0.04786 0.149 168 -0.320  0.9995 
#c bca - a bca -0.32697 0.128 128 -2.553  0.1169 
#c bca - b bca -0.06890 0.128 128 -0.538  0.9945 
#a bca - b bca  0.25808 0.148 128  1.745  0.5050 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Membrane_Transport_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            4128039 310322 112  3556765  4791068
#a    acb            4762749 491391 168  3885063  5838716
#b    acb            3973922 410005 168  3241602  4871682
#c    bca            3891201 306131 112  3329544  4547604
#a    bca            5396170 582652 168  4360230  6678238
#b    bca            4168738 450120 168  3368437  5159182

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.867 0.1061 128 -1.169  0.8509 
#c acb / b acb 1.039 0.1271 128  0.311  0.9996 
#c acb / c bca 1.061 0.1154 112  0.543  0.9942 
#c acb / a bca 0.765 0.1006 155 -2.036  0.3269 
#c acb / b bca 0.990 0.1303 155 -0.075  1.0000 
#a acb / b acb 1.199 0.1694 128  1.281  0.7948 
#a acb / c bca 1.224 0.1588 153  1.558  0.6275 
#a acb / a bca 0.883 0.1318 168 -0.836  0.9604 
#a acb / b bca 1.142 0.1706 168  0.892  0.9480 
#b acb / c bca 1.021 0.1325 153  0.162  1.0000 
#b acb / a bca 0.736 0.1100 168 -2.049  0.3197 
#b acb / b bca 0.953 0.1424 168 -0.320  0.9995 
#c bca / a bca 0.721 0.0924 128 -2.553  0.1169 
#c bca / b bca 0.933 0.1196 128 -0.538  0.9945 
#a bca / b bca 1.294 0.1915 128  1.745  0.5050 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Metabolic_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolic_Diseases) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 251.0397
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1317  
#Residual             0.4611  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#10.283787                0.189192               -0.003205               -0.025920  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.092225                0.098134  

Metabolic_Diseases_mod <- lmer(log(Metabolic_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Metabolic_Diseases_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolic_Diseases) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 251

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3905 -0.6681 -0.0009  0.7068  2.4723 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01735  0.1317  
#Residual             0.21264  0.4611  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            10.283787   0.073327 140.246
#Dieta                   0.189192   0.117761   1.607
#Dietb                  -0.003205   0.117761  -0.027
#Diet_Sequencebca       -0.025920   0.106140  -0.244
#Dieta:Diet_Sequencebca  0.092225   0.170459   0.541
#Dietb:Diet_Sequencebca  0.098134   0.170459   0.576

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.535                             
#Dietb       -0.535  0.333                      
#Diet_Sqncbc -0.691  0.370  0.370               
#Dt:Dt_Sqncb  0.370 -0.691 -0.230 -0.535        
#Dtb:Dt_Sqnc  0.370 -0.230 -0.691 -0.535  0.333 

r.squaredGLMM(Metabolic_Diseases_mod)

#R2m       R2c
#[1,] 0.04175943 0.1140403

check_model(Metabolic_Diseases_mod) 

Metabolic_Diseases.emm.s <- emmeans(Metabolic_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence)
Metabolic_Diseases.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             10.3 0.0733 109     10.1     10.4
#a    acb             10.5 0.1000 167     10.3     10.7
#b    acb             10.3 0.1000 167     10.1     10.5
#c    bca             10.3 0.0767 109     10.1     10.4
#a    bca             10.5 0.1047 167     10.3     10.7
#b    bca             10.4 0.1047 167     10.1     10.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Metabolic_Diseases.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.18919 0.118 128 -1.607  0.5958 
#c acb - b acb  0.00321 0.118 128  0.027  1.0000 
#c acb - c bca  0.02592 0.106 109  0.244  0.9999 
#c acb - a bca -0.25550 0.128 152 -1.999  0.3475 
#c acb - b bca -0.06901 0.128 152 -0.540  0.9944 
#a acb - b acb  0.19240 0.136 128  1.415  0.7180 
#a acb - c bca  0.21511 0.126 150  1.707  0.5296 
#a acb - a bca -0.06630 0.145 167 -0.458  0.9974 
#a acb - b bca  0.12018 0.145 167  0.830  0.9615 
#b acb - c bca  0.02271 0.126 150  0.180  1.0000 
#b acb - a bca -0.25870 0.145 167 -1.787  0.4767 
#b acb - b bca -0.07221 0.145 167 -0.499  0.9962 
#c bca - a bca -0.28142 0.123 128 -2.283  0.2085 
#c bca - b bca -0.09493 0.123 128 -0.770  0.9720 
#a bca - b bca  0.18649 0.142 128  1.310  0.7788 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Metabolic_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              29254 2145 109    25297    33831
#a    acb              35347 3535 167    29015    43062
#b    acb              29161 2916 167    23936    35525
#c    bca              28506 2188 109    24484    33189
#a    bca              37770 3953 167    30720    46439
#b    bca              31345 3280 167    25494    38538

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.828 0.0975 128 -1.607  0.5958 
#c acb / b acb 1.003 0.1181 128  0.027  1.0000 
#c acb / c bca 1.026 0.1089 109  0.244  0.9999 
#c acb / a bca 0.775 0.0990 152 -1.999  0.3475 
#c acb / b bca 0.933 0.1193 152 -0.540  0.9944 
#a acb / b acb 1.212 0.1648 128  1.415  0.7180 
#a acb / c bca 1.240 0.1563 150  1.707  0.5296 
#a acb / a bca 0.936 0.1355 167 -0.458  0.9974 
#a acb / b bca 1.128 0.1632 167  0.830  0.9615 
#b acb / c bca 1.023 0.1289 150  0.180  1.0000 
#b acb / a bca 0.772 0.1118 167 -1.787  0.4767 
#b acb / b bca 0.930 0.1347 167 -0.499  0.9962 
#c bca / a bca 0.755 0.0930 128 -2.283  0.2085 
#c bca / b bca 0.909 0.1121 128 -0.770  0.9720 
#a bca / b bca 1.205 0.1715 128  1.310  0.7788 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 248.557
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1275  
#Residual             0.4585  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.63277                 0.07391                -0.18094                -0.07119  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.14007                 0.17120  

Metabolism_mod <- lmer(log(Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 248.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3373 -0.6244  0.0323  0.7246  2.4719 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01626  0.1275  
#Residual             0.21022  0.4585  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            13.63277    0.07264 187.666
#Dieta                   0.07391    0.11709   0.631
#Dietb                  -0.18094    0.11709  -1.545
#Diet_Sequencebca       -0.07119    0.10515  -0.677
#Dieta:Diet_Sequencebca  0.14007    0.16949   0.826
#Dietb:Diet_Sequencebca  0.17120    0.16949   1.010

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.537                             
#Dietb       -0.537  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.537        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.537  0.333 

r.squaredGLMM(Metabolism_mod)

#R2m       R2c
#[1,] 0.0378505 0.1069444

check_model(Metabolism_mod) 

Metabolism.emm.s <- emmeans(Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.6 0.0726 110     13.5     13.8
#a    acb             13.7 0.0992 167     13.5     13.9
#b    acb             13.5 0.0992 167     13.3     13.6
#c    bca             13.6 0.0760 110     13.4     13.7
#a    bca             13.8 0.1039 167     13.6     14.0
#b    bca             13.6 0.1039 167     13.3     13.8

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.07391 0.117 128 -0.631  0.9885 
#c acb - b acb  0.18094 0.117 128  1.545  0.6357 
#c acb - c bca  0.07119 0.105 110  0.677  0.9841 
#c acb - a bca -0.14279 0.127 153 -1.127  0.8696 
#c acb - b bca  0.08093 0.127 153  0.639  0.9879 
#a acb - b acb  0.25485 0.135 128  1.885  0.4161 
#a acb - c bca  0.14510 0.125 151  1.161  0.8546 
#a acb - a bca -0.06888 0.144 167 -0.480  0.9968 
#a acb - b bca  0.15484 0.144 167  1.078  0.8895 
#b acb - c bca -0.10975 0.125 151 -0.878  0.9512 
#b acb - a bca -0.32373 0.144 167 -2.254  0.2193 
#b acb - b bca -0.10001 0.144 167 -0.696  0.9822 
#c bca - a bca -0.21397 0.123 128 -1.746  0.5042 
#c bca - b bca  0.00974 0.123 128  0.079  1.0000 
#a bca - b bca  0.22371 0.141 128  1.581  0.6125 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             832982 60511 110   721292   961966
#a    acb             896877 88999 167   737310  1090979
#b    acb             695109 68978 167   571439   845544
#c    bca             775744 58975 110   667244   901888
#a    bca             960830 99783 167   782716  1179476
#b    bca             768225 79781 167   625815   943042

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.929 0.1087 128 -0.631  0.9885 
#c acb / b acb 1.198 0.1403 128  1.545  0.6357 
#c acb / c bca 1.074 0.1129 110  0.677  0.9841 
#c acb / a bca 0.867 0.1099 153 -1.127  0.8696 
#c acb / b bca 1.084 0.1374 153  0.639  0.9879 
#a acb / b acb 1.290 0.1744 128  1.885  0.4161 
#a acb / c bca 1.156 0.1445 151  1.161  0.8546 
#a acb / a bca 0.933 0.1341 167 -0.480  0.9968 
#a acb / b bca 1.167 0.1677 167  1.078  0.8895 
#b acb / c bca 0.896 0.1120 151 -0.878  0.9512 
#b acb / a bca 0.723 0.1039 167 -2.254  0.2193 
#b acb / b bca 0.905 0.1300 167 -0.696  0.9822 
#c bca / a bca 0.807 0.0989 128 -1.746  0.5042 
#c bca / b bca 1.010 0.1237 128  0.079  1.0000 
#a bca / b bca 1.251 0.1770 128  1.581  0.6125 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Metabolism_of_Cofactors_and_Vitamins) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Cofactors_and_Vitamins) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 247.571
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.128   
#Residual             0.457   
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.18648                 0.14613                -0.15485                -0.02972  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.07968                 0.18137  

Metabolism_of_Cofactors_and_Vitamins_mod <- lmer(log(Metabolism_of_Cofactors_and_Vitamins) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Metabolism_of_Cofactors_and_Vitamins_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Cofactors_and_Vitamins) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 247.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3560 -0.6228 -0.0145  0.6994  2.4372 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01638  0.128   
#Residual             0.20883  0.457   
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            14.18648    0.07247 195.751
#Dieta                   0.14613    0.11670   1.252
#Dietb                  -0.15485    0.11670  -1.327
#Diet_Sequencebca       -0.02972    0.10490  -0.283
#Dieta:Diet_Sequencebca  0.07968    0.16893   0.472
#Dietb:Diet_Sequencebca  0.18137    0.16893   1.074

#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.537                             
#Dietb       -0.537  0.333                      
#Diet_Sqncbc -0.691  0.371  0.371               
#Dt:Dt_Sqncb  0.371 -0.691 -0.230 -0.537        
#Dtb:Dt_Sqnc  0.371 -0.230 -0.691 -0.537  0.333 

r.squaredGLMM(Metabolism_of_Cofactors_and_Vitamins_mod)

#R2m       R2c
#[1,] 0.04480884 0.1142951

check_model(Metabolism_of_Cofactors_and_Vitamins_mod) 

Metabolism_of_Cofactors_and_Vitamins.emm.s <- emmeans(Metabolism_of_Cofactors_and_Vitamins_mod, specs = pairwise ~ Diet * Diet_Sequence)
Metabolism_of_Cofactors_and_Vitamins.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.2 0.0725 109     14.0     14.3
#a    acb             14.3 0.0990 167     14.1     14.5
#b    acb             14.0 0.0990 167     13.8     14.2
#c    bca             14.2 0.0758 109     14.0     14.3
#a    bca             14.4 0.1036 167     14.2     14.6
#b    bca             14.2 0.1036 167     14.0     14.4

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Metabolism_of_Cofactors_and_Vitamins.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1461 0.117 128 -1.252  0.8101 
#c acb - b acb   0.1548 0.117 128  1.327  0.7696 
#c acb - c bca   0.0297 0.105 109  0.283  0.9997 
#c acb - a bca  -0.1961 0.126 153 -1.551  0.6317 
#c acb - b bca   0.0032 0.126 153  0.025  1.0000 
#a acb - b acb   0.3010 0.135 128  2.233  0.2299 
#a acb - c bca   0.1758 0.125 151  1.410  0.7207 
#a acb - a bca  -0.0500 0.143 167 -0.349  0.9993 
#a acb - b bca   0.1493 0.143 167  1.043  0.9028 
#b acb - c bca  -0.1251 0.125 151 -1.004  0.9161 
#b acb - a bca  -0.3509 0.143 167 -2.450  0.1454 
#b acb - b bca  -0.1516 0.143 167 -1.059  0.8969 
#c bca - a bca  -0.2258 0.122 128 -1.849  0.4385 
#c bca - b bca  -0.0265 0.122 128 -0.217  0.9999 
#a bca - b bca   0.1993 0.141 128  1.413  0.7191 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Metabolism_of_Cofactors_and_Vitamins_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1449132 105021 109  1255250  1672961
#a    acb            1677142 165961 167  1379510  2038989
#b    acb            1241245 122827 167  1020969  1509047
#c    bca            1406700 106690 109  1210378  1634866
#a    bca            1763055 182581 167  1437052  2163014
#b    bca            1444498 149592 167  1177399  1772190

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.864 0.1008 128 -1.252  0.8101 
#c acb / b acb 1.167 0.1362 128  1.327  0.7696 
#c acb / c bca 1.030 0.1081 109  0.283  0.9997 
#c acb / a bca 0.822 0.1039 153 -1.551  0.6317 
#c acb / b bca 1.003 0.1268 153  0.025  1.0000 
#a acb / b acb 1.351 0.1821 128  2.233  0.2299 
#a acb / c bca 1.192 0.1486 151  1.410  0.7207 
#a acb / a bca 0.951 0.1363 167 -0.349  0.9993 
#a acb / b bca 1.161 0.1663 167  1.043  0.9028 
#b acb / c bca 0.882 0.1100 151 -1.004  0.9161 
#b acb / a bca 0.704 0.1008 167 -2.450  0.1454 
#b acb / b bca 0.859 0.1231 167 -1.059  0.8969 
#c bca / a bca 0.798 0.0974 128 -1.849  0.4385 
#c bca / b bca 0.974 0.1189 128 -0.217  0.9999 
#a bca / b bca 1.221 0.1721 128  1.413  0.7191 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Metabolism_of_Other_Amino_Acids) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Other_Amino_Acids) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 244.5352
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1314  
#Residual             0.4519  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.05479                 0.11584                -0.10612                -0.05040  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.08567                 0.12539  

Metabolism_of_Other_Amino_Acids_mod <- lmer(log(Metabolism_of_Other_Amino_Acids) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Metabolism_of_Other_Amino_Acids_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Other_Amino_Acids) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 244.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.4285 -0.6137 -0.0067  0.7513  2.3944 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01726  0.1314  
#Residual             0.20421  0.4519  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            13.05479    0.07204 181.213
#Dieta                   0.11584    0.11540   1.004
#Dietb                  -0.10612    0.11540  -0.920
#Diet_Sequencebca       -0.05040    0.10428  -0.483
#Dieta:Diet_Sequencebca  0.08567    0.16705   0.513
#Dietb:Diet_Sequencebca  0.12539    0.16705   0.751

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.534                             
#Dietb       -0.534  0.333                      
#Diet_Sqncbc -0.691  0.369  0.369               
#Dt:Dt_Sqncb  0.369 -0.691 -0.230 -0.534        
#Dtb:Dt_Sqnc  0.369 -0.230 -0.691 -0.534  0.333 

r.squaredGLMM(Metabolism_of_Other_Amino_Acids_mod)

#R2m       R2c
#[1,] 0.02935017 0.1050109

check_model(Metabolism_of_Other_Amino_Acids_mod) 

Metabolism_of_Other_Amino_Acids.emm.s <- emmeans(Metabolism_of_Other_Amino_Acids_mod, specs = pairwise ~ Diet * Diet_Sequence)
Metabolism_of_Other_Amino_Acids.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.1 0.0720 108     12.9     13.2
#a    acb             13.2 0.0981 167     13.0     13.4
#b    acb             12.9 0.0981 167     12.8     13.1
#c    bca             13.0 0.0754 108     12.9     13.2
#a    bca             13.2 0.1027 167     13.0     13.4
#b    bca             13.0 0.1027 167     12.8     13.2

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Metabolism_of_Other_Amino_Acids.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1158 0.115 128 -1.004  0.9159 
#c acb - b acb   0.1061 0.115 128  0.920  0.9408 
#c acb - c bca   0.0504 0.104 108  0.483  0.9967 
#c acb - a bca  -0.1511 0.125 152 -1.205  0.8341 
#c acb - b bca   0.0311 0.125 152  0.248  0.9999 
#a acb - b acb   0.2220 0.133 128  1.666  0.5569 
#a acb - c bca   0.1662 0.124 150  1.343  0.7603 
#a acb - a bca  -0.0353 0.142 167 -0.248  0.9999 
#a acb - b bca   0.1470 0.142 167  1.035  0.9056 
#b acb - c bca  -0.0557 0.124 150 -0.450  0.9976 
#b acb - a bca  -0.2572 0.142 167 -1.811  0.4616 
#b acb - b bca  -0.0750 0.142 167 -0.528  0.9950 
#c bca - a bca  -0.2015 0.121 128 -1.668  0.5551 
#c bca - b bca  -0.0193 0.121 128 -0.160  1.0000 
#a bca - b bca   0.1822 0.139 128  1.307  0.7808 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Metabolism_of_Other_Amino_Acids_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             467329 33667 108   405144   539059
#a    acb             524722 51490 167   432306   636895
#b    acb             420275 41241 167   346254   510120
#c    bca             444357 33502 108   382677   515979
#a    bca             543556 55820 167   443803   665730
#b    bca             453001 46521 167   369867   554821

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.891 0.1028 128 -1.004  0.9159 
#c acb / b acb 1.112 0.1283 128  0.920  0.9408 
#c acb / c bca 1.052 0.1097 108  0.483  0.9967 
#c acb / a bca 0.860 0.1079 152 -1.205  0.8341 
#c acb / b bca 1.032 0.1294 152  0.248  0.9999 
#a acb / b acb 1.249 0.1664 128  1.666  0.5569 
#a acb / c bca 1.181 0.1461 150  1.343  0.7603 
#b acb / c bca 0.946 0.1170 150 -0.450  0.9976 
#b acb / a bca 0.773 0.1098 167 -1.811  0.4616 
#b acb / b bca 0.928 0.1318 167 -0.528  0.9950 
#c bca / a bca 0.818 0.0987 128 -1.668  0.5551 
#c bca / b bca 0.981 0.1185 128 -0.160  1.0000 
#a bca / b bca 1.200 0.1673 128  1.307  0.7808 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Metabolism_of_Terpenoids_and_Polyketides) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Terpenoids_and_Polyketides) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1297  
#Residual             0.4550  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
 # (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.12637                 0.17074                -0.08029                -0.03439  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.07899                 0.13368  

Metabolism_of_Terpenoids_and_Polyketides_mod <- lmer(log(Metabolism_of_Terpenoids_and_Polyketides) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Metabolism_of_Terpenoids_and_Polyketides_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Metabolism_of_Terpenoids_and_Polyketides) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 246.4

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3265 -0.6515  0.0136  0.6883  2.4012 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01681  0.1297  
#Residual             0.20703  0.4550  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            13.12637    0.07233 181.480
#Dieta                   0.17074    0.11620   1.469
#Dietb                  -0.08029    0.11620  -0.691
#Diet_Sequencebca       -0.03439    0.10470  -0.328
#Dieta:Diet_Sequencebca  0.07899    0.16820   0.470
#Dietb:Diet_Sequencebca  0.13368    0.16820   0.795

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.536                             
#Dietb       -0.536  0.333                      
#Diet_Sqncbc -0.691  0.370  0.370               
#Dt:Dt_Sqncb  0.370 -0.691 -0.230 -0.536        
#Dtb:Dt_Sqnc  0.370 -0.230 -0.691 -0.536  0.333

r.squaredGLMM(Metabolism_of_Terpenoids_and_Polyketides_mod)

#R2m       R2c
#[1,] 0.04099125 0.1130117

check_model(Metabolism_of_Terpenoids_and_Polyketides_mod) 


Metabolism_of_Terpenoids_and_Polyketides.emm.s <- emmeans(Metabolism_of_Terpenoids_and_Polyketides_mod, specs = pairwise ~ Diet * Diet_Sequence)
Metabolism_of_Terpenoids_and_Polyketides.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.1 0.0723 109     13.0     13.3
#a    acb             13.3 0.0987 167     13.1     13.5
#b    acb             13.0 0.0987 167     12.9     13.2
#c    bca             13.1 0.0757 109     12.9     13.2
#a    bca             13.3 0.1032 167     13.1     13.5
#b    bca             13.1 0.1032 167     12.9     13.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Metabolism_of_Terpenoids_and_Polyketides.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1707 0.116 128 -1.469  0.6843 
#c acb - b acb   0.0803 0.116 128  0.691  0.9827 
#c acb - c bca   0.0344 0.105 109  0.328  0.9995 
#c acb - a bca  -0.2153 0.126 153 -1.708  0.5285 
#c acb - b bca  -0.0190 0.126 153 -0.151  1.0000 
#a acb - b acb   0.2510 0.134 128  1.871  0.4247 
#a acb - c bca   0.2051 0.124 150  1.650  0.5671 
#a acb - a bca  -0.0446 0.143 167 -0.312  0.9996 
#a acb - b bca   0.1517 0.143 167  1.063  0.8954 
#b acb - c bca  -0.0459 0.124 150 -0.369  0.9991 
#b acb - a bca  -0.2956 0.143 167 -2.070  0.3080 
#b acb - b bca  -0.0993 0.143 167 -0.695  0.9823 
#c bca - a bca  -0.2497 0.122 128 -2.054  0.3184 
#c bca - b bca  -0.0534 0.122 128 -0.439  0.9979 
#a bca - b bca   0.1963 0.140 128  1.398  0.7280 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Metabolism_of_Terpenoids_and_Polyketides_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             502006 36310 109   434961   579385
#a    acb             595471 58745 167   490088   723514
#b    acb             463274 45703 167   381286   562890
#c    bca             485034 36715 109   417462   563544
#a    bca             622626 64282 167   507814   763397
#b    bca             511633 52823 167   417288   627309

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.843 0.0980 128 -1.469  0.6843 
#c acb / b acb 1.084 0.1259 128  0.691  0.9827 
#c acb / c bca 1.035 0.1084 109  0.328  0.9995 
#c acb / a bca 0.806 0.1016 153 -1.708  0.5285 
#c acb / b bca 0.981 0.1237 153 -0.151  1.0000 
#a acb / b acb 1.285 0.1725 128  1.871  0.4247 
#a acb / c bca 1.228 0.1527 150  1.650  0.5671 
#a acb / a bca 0.956 0.1366 167 -0.312  0.9996 
#a acb / b bca 1.164 0.1662 167  1.063  0.8954 
#b acb / c bca 0.955 0.1188 150 -0.369  0.9991 
#b acb / a bca 0.744 0.1063 167 -2.070  0.3080 
#b acb / b bca 0.905 0.1293 167 -0.695  0.9823 
#c bca / a bca 0.779 0.0947 128 -2.054  0.3184 
#c bca / b bca 0.948 0.1153 128 -0.439  0.9979 
#a bca / b bca 1.217 0.1709 128  1.398  0.7280 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Nervous_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Nervous_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 263.7997
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1205  
#Residual             0.4823  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#10.35387                 0.09696                -0.11425                -0.03987  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.15121                 0.11670  

Nervous_System_mod <- lmer(log(Nervous_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Nervous_System_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Nervous_System) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 263.8

#Scaled residuals: 
 # Min      1Q  Median      3Q     Max 
#-3.3044 -0.6531 -0.0088  0.7341  2.6106 

#Random effects:
 # Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01451  0.1205  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
 # Estimate Std. Error t value
#(Intercept)            10.35387    0.07542 137.287
#Dieta                   0.09696    0.12317   0.787
#Dietb                  -0.11425    0.12317  -0.928
#Diet_Sequencebca       -0.03987    0.10917  -0.365
#Dieta:Diet_Sequencebca  0.15121    0.17829   0.848
#Dietb:Diet_Sequencebca  0.11670    0.17829   0.655

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.544                             
#Dietb       -0.544  0.333                      
#Diet_Sqncbc -0.691  0.376  0.376               
#Dt:Dt_Sqncb  0.376 -0.691 -0.230 -0.544        
#Dtb:Dt_Sqnc  0.376 -0.230 -0.691 -0.544  0.333 

r.squaredGLMM(Nervous_System_mod)

#R2m       R2c
#[1,] 0.0337526 0.0904902

check_model(Nervous_System_mod) 

Nervous_System.emm.s <- emmeans(Nervous_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Nervous_System.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             10.4 0.0754 112     10.2     10.5
#a    acb             10.5 0.1037 168     10.2     10.7
#b    acb             10.2 0.1037 168     10.0     10.4
#c    bca             10.3 0.0789 112     10.2     10.5
#a    bca             10.6 0.1085 168     10.3     10.8
#b    bca             10.3 0.1085 168     10.1     10.5

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Nervous_System.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.09696 0.123 128 -0.787  0.9692 
#c acb - b acb  0.11425 0.123 128  0.928  0.9387 
#c acb - c bca  0.03987 0.109 112  0.365  0.9991 
#c acb - a bca -0.20830 0.132 155 -1.577  0.6152 
#c acb - b bca  0.03741 0.132 155  0.283  0.9998 
#a acb - b acb  0.21121 0.142 128  1.485  0.6744 
#a acb - c bca  0.13683 0.130 153  1.050  0.8999 
#a acb - a bca -0.11134 0.150 168 -0.742  0.9763 
#a acb - b bca  0.13438 0.150 168  0.896  0.9471 
#b acb - c bca -0.07438 0.130 153 -0.571  0.9928 
#b acb - a bca -0.32255 0.150 168 -2.150  0.2672 
#b acb - b bca -0.07683 0.150 168 -0.512  0.9957 
#c bca - a bca -0.24817 0.129 128 -1.925  0.3917 
#c bca - b bca -0.00245 0.129 128 -0.019  1.0000 
#a bca - b bca  0.24571 0.149 128  1.651  0.5667 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Nervous_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              31378 2366 112    27023    36435
#a    acb              34573 3584 168    28175    42424
#b    acb              27991 2901 168    22811    34347
#c    bca              30152 2380 112    25787    35256
#a    bca              38645 4192 168    31195    47874
#b    bca              30226 3279 168    24399    37445

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.908 0.112 128 -0.787  0.9692 
#c acb / b acb 1.121 0.138 128  0.928  0.9387 
#c acb / c bca 1.041 0.114 112  0.365  0.9991 
#c acb / a bca 0.812 0.107 155 -1.577  0.6152 
#c acb / b bca 1.038 0.137 155  0.283  0.9998 
#a acb / b acb 1.235 0.176 128  1.485  0.6744 
#a acb / c bca 1.147 0.149 153  1.050  0.8999 
#a acb / a bca 0.895 0.134 168 -0.742  0.9763 
#a acb / b bca 1.144 0.172 168  0.896  0.9471 
#b acb / c bca 0.928 0.121 153 -0.571  0.9928 
#b acb / a bca 0.724 0.109 168 -2.150  0.2672 
#b acb / b bca 0.926 0.139 168 -0.512  0.9957 
#c bca / a bca 0.780 0.101 128 -1.925  0.3917 
#c bca / b bca 0.998 0.129 128 -0.019  1.0000 
#a bca / b bca 1.279 0.190 128  1.651  0.5667 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Neurodegenerative_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Neurodegenerative_Diseases) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 263.1567
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1291  
#Residual             0.4795  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#10.403147               -0.001686               -0.165064               -0.082240  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.067356                0.051766  

Neurodegenerative_Diseases_mod <- lmer(log(Neurodegenerative_Diseases) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Neurodegenerative_Diseases_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Neurodegenerative_Diseases) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 263.2

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-4.0630 -0.6333  0.0172  0.6727  2.2607 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01666  0.1291  
#Residual             0.22992  0.4795  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            10.403147   0.075650 137.517
#Dieta                  -0.001686   0.122454  -0.014
#Dietb                  -0.165064   0.122454  -1.348
#Diet_Sequencebca       -0.082240   0.109503  -0.751
#Dieta:Diet_Sequencebca  0.067356   0.177252   0.380
#Dietb:Diet_Sequencebca  0.051766   0.177252   0.292
#Correlation of Fixed Effects:
 # (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.540                             
#Dietb       -0.540  0.333                      
#Diet_Sqncbc -0.691  0.373  0.373               
#Dt:Dt_Sqncb  0.373 -0.691 -0.230 -0.540        
#Dtb:Dt_Sqnc  0.373 -0.230 -0.691 -0.540  0.333 

r.squaredGLMM(Neurodegenerative_Diseases_mod)

#R2m        R2c
#[1,] 0.02122763 0.08737281

check_model(Neurodegenerative_Diseases_mod) 

Neurodegenerative_Diseases.emm.s <- emmeans(Neurodegenerative_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence)
Neurodegenerative_Diseases.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             10.4 0.0756 111    10.25     10.6
#a    acb             10.4 0.1035 167    10.20     10.6
#b    acb             10.2 0.1035 167    10.03     10.4
#c    bca             10.3 0.0792 111    10.16     10.5
#a    bca             10.4 0.1084 167    10.17     10.6
#b    bca             10.2 0.1084 167     9.99     10.4

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Neurodegenerative_Diseases.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  0.00169 0.122 128  0.014  1.0000 
#c acb - b acb  0.16506 0.122 128  1.348  0.7576 
#c acb - c bca  0.08224 0.110 111  0.751  0.9749 
#c acb - a bca  0.01657 0.132 154  0.125  1.0000 
#c acb - b bca  0.19554 0.132 154  1.480  0.6778 
#a acb - b acb  0.16338 0.141 128  1.155  0.8569 
#a acb - c bca  0.08055 0.130 152  0.618  0.9896 
#a acb - a bca  0.01488 0.150 167  0.099  1.0000 
#a acb - b bca  0.19385 0.150 167  1.293  0.7883 
#b acb - c bca -0.08282 0.130 152 -0.635  0.9882 
#b acb - a bca -0.14849 0.150 167 -0.991  0.9204 
#b acb - b bca  0.03047 0.150 167  0.203  1.0000 
#c bca - a bca -0.06567 0.128 128 -0.512  0.9956 
#c bca - b bca  0.11330 0.128 128  0.884  0.9497 
#a bca - b bca  0.17897 0.148 128  1.209  0.8316 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Neurodegenerative_Diseases_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              32963 2494 111    28374    38294
#a    acb              32908 3407 167    26824    40372
#b    acb              27948 2894 167    22781    34286
#c    bca              30361 2404 111    25952    35518
#a    bca              32422 3513 167    26177    40155
#b    bca              27109 2938 167    21888    33575

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 1.002 0.123 128  0.014  1.0000 
#c acb / b acb 1.179 0.144 128  1.348  0.7576 
#c acb / c bca 1.086 0.119 111  0.751  0.9749 
#c acb / a bca 1.017 0.134 154  0.125  1.0000 
#c acb / b bca 1.216 0.161 154  1.480  0.6778 
#a acb / b acb 1.177 0.166 128  1.155  0.8569 
#a acb / c bca 1.084 0.141 152  0.618  0.9896 
#a acb / a bca 1.015 0.152 167  0.099  1.0000 
#a acb / b bca 1.214 0.182 167  1.293  0.7883 
#b acb / c bca 0.921 0.120 152 -0.635  0.9882 
#b acb / a bca 0.862 0.129 167 -0.991  0.9204 
#b acb / b bca 1.031 0.155 167  0.203  1.0000 
#c bca / a bca 0.936 0.120 128 -0.512  0.9956 
#c bca / b bca 1.120 0.144 128  0.884  0.9497 
#a bca / b bca 1.196 0.177 128  1.209  0.8316 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 


lmer(log(Nucleotide_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Nucleotide_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 247.628
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1344  
#Residual             0.4556  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.08279                 0.16779                -0.08411                -0.02739  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.07499                 0.11713 

Nucleotide_Metabolism_mod <- lmer(log(Nucleotide_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Nucleotide_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Nucleotide_Metabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 247.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3367 -0.6303 -0.0015  0.7087  2.4191 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01805  0.1344  
#Residual             0.20759  0.4556  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            14.08279    0.07279 193.484
#Dieta                   0.16779    0.11635   1.442
#Dietb                  -0.08411    0.11635  -0.723
#Diet_Sequencebca       -0.02739    0.10536  -0.260
#Dieta:Diet_Sequencebca  0.07499    0.16842   0.445
#Dietb:Diet_Sequencebca  0.11713    0.16842   0.695

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.533                             
#Dietb       -0.533  0.333                      
#Diet_Sqncbc -0.691  0.368  0.368               
#Dt:Dt_Sqncb  0.368 -0.691 -0.230 -0.533        
#Dtb:Dt_Sqnc  0.368 -0.230 -0.691 -0.533  0.333 

r.squaredGLMM(Nucleotide_Metabolism_mod)

#R2m       R2c
#[1,] 0.04011117 0.1169143

check_model(Nucleotide_Metabolism_mod) 

Nucleotide_Metabolism.emm.s <- emmeans(Nucleotide_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Nucleotide_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.1 0.0728 108     13.9     14.2
#a    acb             14.3 0.0990 166     14.1     14.4
#b    acb             14.0 0.0990 166     13.8     14.2
#c    bca             14.1 0.0762 108     13.9     14.2
#a    bca             14.3 0.1037 166     14.1     14.5
#b    bca             14.1 0.1037 166     13.9     14.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Nucleotide_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.16779 0.116 128 -1.442  0.7013 
#c acb - b acb  0.08411 0.116 128  0.723  0.9788 
#c acb - c bca  0.02739 0.105 108  0.260  0.9998 
#c acb - a bca -0.21539 0.127 152 -1.701  0.5336 
#c acb - b bca -0.00563 0.127 152 -0.044  1.0000 
#a acb - b acb  0.25190 0.134 128  1.875  0.4223 
#a acb - c bca  0.19518 0.125 149  1.562  0.6247 
#a acb - a bca -0.04760 0.143 166 -0.332  0.9995 
#a acb - b bca  0.16216 0.143 166  1.131  0.8678 
#b acb - c bca -0.05673 0.125 149 -0.454  0.9975 
#b acb - a bca -0.29951 0.143 166 -2.089  0.2981 
#c bca - a bca -0.24278 0.122 128 -1.994  0.3516 
#c bca - b bca -0.03302 0.122 128 -0.271  0.9998 
#a bca - b bca  0.20976 0.141 128  1.492  0.6701 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Nucleotide_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1306406  95087 108  1130892  1509161
#a    acb            1545071 153035 166  1270637  1878777
#b    acb            1201013 118957 166   987690  1460409
#c    bca            1271110  96823 108  1092975  1478279
#a    bca            1620396 167965 166  1320512  1988384
#b    bca            1313778 136182 166  1070639  1612134

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.846 0.0984 128 -1.442  0.7013 
#c acb / b acb 1.088 0.1266 128  0.723  0.9788 
#c acb / c bca 1.028 0.1083 108  0.260  0.9998 
#c acb / a bca 0.806 0.1021 152 -1.701  0.5336 
#c acb / b bca 0.994 0.1259 152 -0.044  1.0000 
#a acb / b acb 1.286 0.1728 128  1.875  0.4223 
#a acb / c bca 1.216 0.1519 149  1.562  0.6247 
#a acb / a bca 0.954 0.1367 166 -0.332  0.9995 
#a acb / b bca 1.176 0.1686 166  1.131  0.8678 
#b acb / c bca 0.945 0.1181 149 -0.454  0.9975 
#b acb / a bca 0.741 0.1063 166 -2.089  0.2981 
#b acb / b bca 0.914 0.1311 166 -0.626  0.9890 
#c bca / a bca 0.784 0.0955 128 -1.994  0.3516 
#c bca / b bca 0.968 0.1178 128 -0.271  0.9998 
#a bca / b bca 1.233 0.1734 128  1.492  0.6701 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Poorly_Characterized) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Poorly_Characterized) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 247.294
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1256  
#Residual             0.4571  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.25633                 0.13843                -0.09859                -0.04471  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.09893                 0.13634 

Poorly_Characterized_mod <- lmer(log(Poorly_Characterized) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Poorly_Characterized_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Poorly_Characterized) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
3Data: Picrust

#REML criterion at convergence: 247.3

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3490 -0.6582  0.0175  0.6926  2.4493 

#Random effects:
#  Groups   Name        Variance Std.Dev.
3Subject  (Intercept) 0.01578  0.1256  
#Residual             0.20896  0.4571  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            14.25633    0.07231 197.160
#Dieta                   0.13843    0.11674   1.186
#Dietb                  -0.09859    0.11674  -0.845
#Diet_Sequencebca       -0.04471    0.10467  -0.427
#Dieta:Diet_Sequencebca  0.09893    0.16898   0.585
#Dietb:Diet_Sequencebca  0.13634    0.16898   0.807

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.538                             
#Dietb       -0.538  0.333                      
#Diet_Sqncbc -0.691  0.372  0.372               
#Dt:Dt_Sqncb  0.372 -0.691 -0.230 -0.538        
#Dtb:Dt_Sqnc  0.372 -0.230 -0.691 -0.538  0.333 

r.squaredGLMM(Poorly_Characterized_mod)

#R2m       R2c
#[1,] 0.03625893 0.1039149

check_model(Poorly_Characterized_mod) 

Poorly_Characterized.emm.s <- emmeans(Poorly_Characterized_mod, specs = pairwise ~ Diet * Diet_Sequence)
Poorly_Characterized.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.3 0.0723 110     14.1     14.4
#a    acb             14.4 0.0988 167     14.2     14.6
#b    acb             14.2 0.0988 167     14.0     14.4
#c    bca             14.2 0.0757 110     14.1     14.4
#a    bca             14.4 0.1034 167     14.2     14.7
#b    bca             14.2 0.1034 167     14.0     14.5

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Poorly_Characterized.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.13843 0.117 128 -1.186  0.8429 
#c acb - b acb  0.09859 0.117 128  0.845  0.9585 
#c acb - c bca  0.04471 0.105 110  0.427  0.9981 
#c acb - a bca -0.19265 0.126 153 -1.526  0.6479 
#c acb - b bca  0.00696 0.126 153  0.055  1.0000 
#a acb - b acb  0.23702 0.135 128  1.758  0.4962 
#a acb - c bca  0.18315 0.124 151  1.471  0.6831 
#a acb - a bca -0.05422 0.143 167 -0.379  0.9990 
#a acb - b bca  0.14539 0.143 167  1.016  0.9120 
#b acb - c bca -0.05388 0.124 151 -0.433  0.9980 
#b acb - a bca -0.29124 0.143 167 -2.035  0.3269 
#b acb - b bca -0.09163 0.143 167 -0.640  0.9878 
#c bca - a bca -0.23736 0.122 128 -1.943  0.3812 
#c bca - b bca -0.03775 0.122 128 -0.309  0.9996 
#a bca - b bca  0.19961 0.141 128  1.415  0.7179 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Poorly_Characterized_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1553984 112366 110  1346520  1793412
#a    acb            1784711 176417 167  1468298  2169310
#b    acb            1408089 139188 167  1158448  1711527
#c    bca            1486034 112453 110  1279084  1726468
#b    bca            1543210 159643 167  1258135  1892879

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.871 0.1016 128 -1.186  0.8429 
#c acb / b acb 1.104 0.1288 128  0.845  0.9585 
#c acb / c bca 1.046 0.1095 110  0.427  0.9981 
#c acb / a bca 0.825 0.1041 153 -1.526  0.6479 
#c acb / b bca 1.007 0.1271 153  0.055  1.0000 
#a acb / b acb 1.267 0.1709 128  1.758  0.4962 
#a acb / c bca 1.201 0.1495 151  1.471  0.6831 
#a acb / a bca 0.947 0.1355 167 -0.379  0.9990 
#a acb / b bca 1.156 0.1655 167  1.016  0.9120 
#b acb / c bca 0.948 0.1180 151 -0.433  0.9980 
#b acb / a bca 0.747 0.1069 167 -2.035  0.3269 
#b acb / b bca 0.912 0.1306 167 -0.640  0.9878 
#c bca / a bca 0.789 0.0964 128 -1.943  0.3812 
#c bca / b bca 0.963 0.1176 128 -0.309  0.9996 
#a bca / b bca 1.221 0.1722 128  1.415  0.7179 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Replication_and_Repair) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Replication_and_Repair) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 251.4595
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1348  
#Residual             0.4610  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.84710                 0.18101                -0.04884                -0.02072  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.08485                 0.10148

Replication_and_Repair_mod <- lmer(log(Replication_and_Repair) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Replication_and_Repair_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Replication_and_Repair) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 251.5

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3855 -0.6173 -0.0057  0.7072  2.4192 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01818  0.1348  
#Residual             0.21254  0.4610  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            14.84710    0.07356 201.842
#Dieta                   0.18101    0.11774   1.537
#Dietb                  -0.04884    0.11774  -0.415
#Diet_Sequencebca       -0.02072    0.10647  -0.195
#Dieta:Diet_Sequencebca  0.08485    0.17042   0.498
#Dietb:Diet_Sequencebca  0.10148    0.17042   0.595

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.534                             
#Dietb       -0.534  0.333                      
#Diet_Sqncbc -0.691  0.369  0.369               
#Dt:Dt_Sqncb  0.369 -0.691 -0.230 -0.534        
#Dtb:Dt_Sqnc  0.369 -0.230 -0.691 -0.534  0.333 #

r.squaredGLMM(Replication_and_Repair_mod)

#R2m       R2c
#[1,] 0.04148155 0.1169944

check_model(Replication_and_Repair_mod) 

Replication_and_Repair.emm.s <- emmeans(Replication_and_Repair_mod, specs = pairwise ~ Diet * Diet_Sequence)
Replication_and_Repair.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.8 0.0736 108     14.7     15.0
#a    acb             15.0 0.1002 167     14.8     15.2
#b    acb             14.8 0.1002 167     14.6     15.0
#c    bca             14.8 0.0770 108     14.7     15.0
#a    bca             15.1 0.1048 167     14.9     15.3
#b    bca             14.9 0.1048 167     14.7     15.1

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Replication_and_Repair.emm.s$contrasts

?#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1810 0.118 128 -1.537  0.6408 
#c acb - b acb   0.0488 0.118 128  0.415  0.9984 
#c acb - c bca   0.0207 0.106 108  0.195  1.0000 
#c acb - a bca  -0.2451 0.128 152 -1.914  0.3974 
#c acb - b bca  -0.0319 0.128 152 -0.249  0.9999 
#a acb - b acb   0.2299 0.136 128  1.691  0.5404 
#a acb - c bca   0.2017 0.126 149  1.597  0.6018 
#a acb - a bca  -0.0641 0.145 167 -0.442  0.9978 
#a acb - b bca   0.1491 0.145 167  1.028  0.9078 
#b acb - c bca  -0.0281 0.126 149 -0.223  0.9999 #
#b acb - a bca  -0.2940 0.145 167 -2.028  0.3311 
#b acb - b bca  -0.0808 0.145 167 -0.557  0.9936 
#c bca - a bca  -0.2659 0.123 128 -2.158  0.2650 
#c bca - b bca  -0.0526 0.123 128 -0.427  0.9982 
#a bca - b bca   0.2132 0.142 128  1.499  0.6658 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Replication_and_Repair_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            2805527 206369 108  2424898  3245902
#a    acb            3362233 336750 167  2758992  4097370
#c    bca            2747984 211543 108  2359101  3200972
#a    bca            3584906 375760 167  2914768  4409116
#b    bca            2896532 303607 167  2355074  3562477

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.834 0.0982 128 -1.537  0.6408 
#c acb / b acb 1.050 0.1236 128  0.415  0.9984 
#c acb / c bca 1.021 0.1087 108  0.195  1.0000 
#c acb / a bca 0.783 0.1002 152 -1.914  0.3974 
#c acb / b bca 0.969 0.1240 152 -0.249  0.9999 
#a acb / b acb 1.258 0.1711 128  1.691  0.5404 
#a acb / c bca 1.224 0.1546 149  1.597  0.6018 
#a acb / a bca 0.938 0.1360 167 -0.442  0.9978 
#a acb / b bca 1.161 0.1683 167  1.028  0.9078 
#b acb / c bca 0.972 0.1228 149 -0.223  0.9999 
#b acb / a bca 0.745 0.1080 167 -2.028  0.3311 
#b acb / b bca 0.922 0.1337 167 -0.557  0.9936 
#c bca / a bca 0.767 0.0944 128 -2.158  0.2650 
#c bca / b bca 0.949 0.1169 128 -0.427  0.9982 
#a bca / b bca 1.238 0.1761 128  1.499  0.6658 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Signal_Transduction) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Signal_Transduction) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 251.5639
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1198  
#Residual             0.4645  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.09397                 0.12081                -0.13113                -0.07731  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.12900                 0.15641  

Signal_Transduction_mod <- lmer(log(Signal_Transduction) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Signal_Transduction_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Signal_Transduction) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 251.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.2729 -0.5970  0.0596  0.6696  2.4102 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01436  0.1198  
#Residual             0.21576  0.4645  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            13.09397    0.07290 179.613
#Dieta                   0.12081    0.11862   1.018
#Dietb                  -0.13113    0.11862  -1.105
#Diet_Sequencebca       -0.07731    0.10552  -0.733
#Dieta:Diet_Sequencebca  0.12900    0.17170   0.751
#Dietb:Diet_Sequencebca  0.15641    0.17170   0.911

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.542                             
#Dietb       -0.542  0.333                      
#Diet_Sqncbc -0.691  0.375  0.375               
#Dt:Dt_Sqncb  0.375 -0.691 -0.230 -0.542        
#Dtb:Dt_Sqnc  0.375 -0.230 -0.691 -0.542  0.333

r.squaredGLMM(Signal_Transduction_mod)

#R2m        R2c
#[1,] 0.03956153 0.09948106

check_model(Signal_Transduction_mod) 


Signal_Transduction.emm.s <- emmeans(Signal_Transduction_mod, specs = pairwise ~ Diet * Diet_Sequence)
Signal_Transduction.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.1 0.0729 112     12.9     13.2
#a    acb             13.2 0.1000 168     13.0     13.4
#b    acb             13.0 0.1000 168     12.8     13.2
#c    bca             13.0 0.0763 112     12.9     13.2
#a    bca             13.3 0.1047 168     13.1     13.5
#b    bca             13.0 0.1047 168     12.8     13.2

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Signal_Transduction.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1208 0.119 128 -1.018  0.9110 
#c acb - b acb   0.1311 0.119 128  1.105  0.8784 
#c acb - c bca   0.0773 0.106 112  0.733  0.9775 
#c acb - a bca  -0.1725 0.128 155 -1.352  0.7552 
#c acb - b bca   0.0520 0.128 155  0.408  0.9985 
#a acb - b acb   0.2519 0.137 128  1.839  0.4445 
#a acb - c bca   0.1981 0.126 153  1.575  0.6163 
#a acb - a bca  -0.0517 0.145 168 -0.357  0.9992 
#a acb - b bca   0.1728 0.145 168  1.194  0.8394 
#b acb - c bca  -0.0538 0.126 153 -0.428  0.9981 
#b acb - a bca  -0.3036 0.145 168 -2.097  0.2938 
#b acb - b bca  -0.0791 0.145 168 -0.546  0.9941 
#c bca - a bca  -0.2498 0.124 128 -2.012  0.3412 
#c bca - b bca  -0.0253 0.124 128 -0.204  1.0000 
#a bca - b bca   0.2245 0.143 128  1.566  0.6221 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Signal_Transduction_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1208 0.119 128 -1.018  0.9110 
#c acb - b acb   0.1311 0.119 128  1.105  0.8784 
#c acb - c bca   0.0773 0.106 112  0.733  0.9775 
#c acb - a bca  -0.1725 0.128 155 -1.352  0.7552 
#c acb - b bca   0.0520 0.128 155  0.408  0.9985 
#a acb - b acb   0.2519 0.137 128  1.839  0.4445 
#a acb - c bca   0.1981 0.126 153  1.575  0.6163 
#a acb - a bca  -0.0517 0.145 168 -0.357  0.9992 
#a acb - b bca   0.1728 0.145 168  1.194  0.8394 
#b acb - c bca  -0.0538 0.126 153 -0.428  0.9981 
#b acb - a bca  -0.3036 0.145 168 -2.097  0.2938 
#b acb - b bca  -0.0791 0.145 168 -0.546  0.9941 
#c bca - a bca  -0.2498 0.124 128 -2.012  0.3412 
#c bca - b bca  -0.0253 0.124 128 -0.204  1.0000 
#a bca - b bca   0.2245 0.143 128  1.566  0.6221 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

lmer(log(Signaling_Molecules_and_Interaction) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Signaling_Molecules_and_Interaction) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 243.7949
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1563  
#Residual             0.4448  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#11.06866                 0.04656                -0.13027                -0.06886  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.09420                 0.08343  
 
Signaling_Molecules_and_Interaction_mod <- lmer(log(Signaling_Molecules_and_Interaction) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Signaling_Molecules_and_Interaction_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Signaling_Molecules_and_Interaction) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 243.8

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3698 -0.6040 -0.0379  0.6748  2.2174 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.02443  0.1563  
#Residual             0.19786  0.4448  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            11.06866    0.07324 151.134
#Dieta                   0.04656    0.11360   0.410
#Dietb                  -0.13027    0.11360  -1.147
#Diet_Sequencebca       -0.06886    0.10601  -0.650
#Dieta:Diet_Sequencebca  0.09420    0.16443   0.573
#Dietb:Diet_Sequencebca  0.08343    0.16443   0.507#

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.517                             
#Dietb       -0.517  0.333                      
#Diet_Sqncbc -0.691  0.357  0.357               
#Dt:Dt_Sqncb  0.357 -0.691 -0.230 -0.517        
#Dtb:Dt_Sqnc  0.357 -0.230 -0.691 -0.517  0.333 

r.squaredGLMM(Signaling_Molecules_and_Interaction_mod)

#R2m       R2c
#[1,] 0.02118393 0.1287663

check_model(Signaling_Molecules_and_Interaction_mod) 

Signaling_Molecules_and_Interaction.emm.s <- emmeans(Signaling_Molecules_and_Interaction_mod, specs = pairwise ~ Diet * Diet_Sequence)
Signaling_Molecules_and_Interaction.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             11.1 0.0732 102     10.9     11.2
#a    acb             11.1 0.0983 164     10.9     11.3
#b    acb             10.9 0.0983 164     10.7     11.1
#c    bca             11.0 0.0766 102     10.8     11.2
#a    bca             11.1 0.1029 164     10.9     11.3
#b    bca             11.0 0.1029 164     10.7     11.2

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Signaling_Molecules_and_Interaction.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.0466 0.114 128 -0.410  0.9985 
#c acb - b acb   0.1303 0.114 128  1.147  0.8607 
#c acb - c bca   0.0689 0.106 102  0.650  0.9868 
#c acb - a bca  -0.0719 0.126 146 -0.569  0.9929 
#c acb - b bca   0.1157 0.126 146  0.916  0.9418 
#a acb - b acb   0.1768 0.131 128  1.348  0.7576 
#a acb - c bca   0.1154 0.125 143  0.926  0.9392 
#a acb - a bca  -0.0253 0.142 164 -0.178  1.0000 
#a acb - b bca   0.1623 0.142 164  1.140  0.8638 
#b acb - c bca  -0.0614 0.125 143 -0.493  0.9964 
#b acb - a bca  -0.2022 0.142 164 -1.421  0.7145 
#b acb - b bca  -0.0146 0.142 164 -0.102  1.0000 
#c bca - a bca  -0.1408 0.119 128 -1.184  0.8438 
#c bca - b bca   0.0468 0.119 128  0.394  0.9987 
#a bca - b bca   0.1876 0.137 128  1.367  0.7468 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Signaling_Molecules_and_Interaction_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              64129 4697 102    55459    74156
#a    acb              67186 6605 164    55331    81580
#b    acb              56297 5535 164    46364    68358
#c    bca              59862 4588 102    51419    69690
#a    bca              68909 7090 164    56240    84432
#b    bca              57123 5877 164    46621    69990

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.955 0.108 128 -0.410  0.9985 
#c acb / b acb 1.139 0.129 128  1.147  0.8607 
#c acb / c bca 1.071 0.114 102  0.650  0.9868 
#c acb / a bca 0.931 0.118 146 -0.569  0.9929 
#c acb / b bca 1.123 0.142 146  0.916  0.9418 
#a acb / b acb 1.193 0.157 128  1.348  0.7576 
#a acb / c bca 1.122 0.140 143  0.926  0.9392 
#a acb / a bca 0.975 0.139 164 -0.178  1.0000 
#a acb / b bca 1.176 0.167 164  1.140  0.8638 
#b acb / c bca 0.940 0.117 143 -0.493  0.9964 
#b acb / a bca 0.817 0.116 164 -1.421  0.7145 
#b acb / b bca 0.986 0.140 164 -0.102  1.0000 
#c bca / a bca 0.869 0.103 128 -1.184  0.8438 
#c bca / b bca 1.048 0.125 128  0.394  0.9987 
#a bca / b bca 1.206 0.166 128  1.367  0.7468 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Transcription) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Transcription) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 267.852
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1217  
#Residual             0.4881  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.71062                 0.14017                -0.08266                -0.04342  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.16389                 0.11493  

Transcription_mod <- lmer(log(Transcription) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Transcription_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Transcription) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 267.9

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.5213 -0.6322 -0.0404  0.6831  2.5956 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01482  0.1217  
#Residual             0.23826  0.4881  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            13.71062    0.07631 179.660
#Dieta                   0.14017    0.12466   1.124
#Dietb                  -0.08266    0.12466  -0.663
#Diet_Sequencebca       -0.04342    0.11046  -0.393
#Dieta:Diet_Sequencebca  0.16389    0.18044   0.908
#Dietb:Diet_Sequencebca  0.11493    0.18044   0.637

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.544                             
#Dietb       -0.544  0.333                      
#Diet_Sqncbc -0.691  0.376  0.376               
#Dt:Dt_Sqncb  0.376 -0.691 -0.230 -0.544        
#Dtb:Dt_Sqnc  0.376 -0.230 -0.691 -0.544  0.333 

r.squaredGLMM(Transcription_mod)

#R2m        R2c
#[1,] 0.04299699 0.09902588

check_model(Transcription_mod) 

Transcription.emm.s <- emmeans(Transcription_mod, specs = pairwise ~ Diet * Diet_Sequence)
Transcription.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.7 0.0763 113     13.6     13.9
#a    acb             13.9 0.1049 168     13.6     14.1
#b    acb             13.6 0.1049 168     13.4     13.8
#c    bca             13.7 0.0799 113     13.5     13.8
#a    bca             14.0 0.1098 168     13.8     14.2
#b    bca             13.7 0.1098 168     13.5     13.9

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Transcription.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1402 0.125 128 -1.124  0.8704 
#c acb - b acb   0.0827 0.125 128  0.663  0.9856 
#c acb - c bca   0.0434 0.110 113  0.393  0.9988 
#c acb - a bca  -0.2606 0.134 155 -1.949  0.3764 
#c acb - b bca   0.0111 0.134 155  0.083  1.0000 
#a acb - b acb   0.2228 0.144 128  1.548  0.6340 
#a acb - c bca   0.1836 0.132 153  1.392  0.7315 
#a acb - a bca  -0.1205 0.152 168 -0.793  0.9683 
#a acb - b bca   0.1513 0.152 168  0.997  0.9185 
#b acb - c bca  -0.0392 0.132 153 -0.298  0.9997 
#b acb - a bca  -0.3433 0.152 168 -2.261  0.2161 
#b acb - b bca  -0.0715 0.152 168 -0.471  0.9971 
#c bca - a bca  -0.3041 0.130 128 -2.331  0.1895 
#c bca - b bca  -0.0323 0.130 128 -0.247  0.9999 
#a bca - b bca   0.2718 0.151 128  1.804  0.4667 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Transcription_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb             900419  68715 113   774068  1047393
#a    acb            1035903 108664 168   842136  1274255
#b    acb             828985  86959 168   673922  1019726
#c    bca             862160  68857 113   735981  1009970
#a    bca            1168530 128280 168   940844  1451316
#b    bca             890437  97752 168   716936  1105924

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.869 0.1084 128 -1.124  0.8704 
#c acb / b acb 1.086 0.1354 128  0.663  0.9856 
#c acb / c bca 1.044 0.1154 113  0.393  0.9988 
#c acb / a bca 0.771 0.1030 155 -1.949  0.3764 
#c acb / b bca 1.011 0.1352 155  0.083  1.0000 
#a acb / b acb 1.250 0.1799 128  1.548  0.6340 
#a acb / c bca 1.202 0.1584 153  1.392  0.7315 
#a acb / a bca 0.887 0.1346 168 -0.793  0.9683 
#a acb / b bca 1.163 0.1766 168  0.997  0.9185 
#b acb / c bca 0.962 0.1268 153 -0.298  0.9997 
#b acb / a bca 0.709 0.1077 168 -2.261  0.2161 
#b acb / b bca 0.931 0.1414 168 -0.471  0.9971 
#c bca / a bca 0.738 0.0963 128 -2.331  0.1895 
#c bca / b bca 0.968 0.1263 128 -0.247  0.9999 
#a bca / b bca 1.312 0.1977 128  1.804  0.4667 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Translation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Translation) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 251.3018
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1362  
#Residual             0.4605  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#14.39326                 0.17623                -0.04926                -0.02644  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.08829                 0.09728  

Translation_mod <- lmer(log(Translation) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Translation_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Translation) ~ 1 + Diet * Diet_Sequence + (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 251.3

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3233 -0.6289 -0.0188  0.7075  2.4463 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01854  0.1362  
#Residual             0.21204  0.4605  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            14.39326    0.07359 195.578
#Dieta                   0.17623    0.11760   1.499
#Dietb                  -0.04926    0.11760  -0.419
#Diet_Sequencebca       -0.02644    0.10653  -0.248
#Dieta:Diet_Sequencebca  0.08829    0.17022   0.519
#Dietb:Diet_Sequencebca  0.09728    0.17022   0.572

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.533                             
#Dietb       -0.533  0.333                      
#Diet_Sqncbc -0.691  0.368  0.368               
#Dt:Dt_Sqncb  0.368 -0.691 -0.230 -0.533        
#Dtb:Dt_Sqnc  0.368 -0.230 -0.691 -0.533  0.333 

r.squaredGLMM(Translation_mod)

#R2m      R2c
#[1,] 0.0404212 0.117594

check_model(Translation_mod) 

Translation.emm.s <- emmeans(Translation_mod, specs = pairwise ~ Diet * Diet_Sequence)
Translation.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             14.4 0.0736 108     14.2     14.5
#a    acb             14.6 0.1001 166     14.4     14.8
#b    acb             14.3 0.1001 166     14.1     14.5
#c    bca             14.4 0.0770 108     14.2     14.5
#a    bca             14.6 0.1048 166     14.4     14.8
#b    bca             14.4 0.1048 166     14.2     14.6

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Translation.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1762 0.118 128 -1.499  0.6658 
#c acb - b acb   0.0493 0.118 128  0.419  0.9983 
#c acb - c bca   0.0264 0.107 108  0.248  0.9999 
#c acb - a bca  -0.2381 0.128 152 -1.859  0.4313 
#c acb - b bca  -0.0216 0.128 152 -0.169  1.0000 
#a acb - b acb   0.2255 0.136 128  1.661  0.5602 
#a acb - c bca   0.2027 0.126 149  1.604  0.5970 
#a acb - a bca  -0.0619 0.145 166 -0.427  0.9982 
#a acb - b bca   0.1546 0.145 166  1.067  0.8938 
#b acb - c bca  -0.0228 0.126 149 -0.181  1.0000 
#b acb - a bca  -0.2873 0.145 166 -1.983  0.3567 
#b acb - b bca  -0.0708 0.145 166 -0.489  0.9965 
#c bca - a bca  -0.2645 0.123 128 -2.149  0.2691 
#c bca - b bca  -0.0480 0.123 128 -0.390  0.9988 
#a bca - b bca   0.2165 0.142 128  1.523  0.6499 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Translation_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response     SE  df lower.CL upper.CL
#c    acb            1782030 131145 108  1540145  2061904
#a    acb            2125439 212816 166  1744195  2590014
#b    acb            1696369 169854 166  1392088  2067159
#c    bca            1735536 133668 108  1489814  2021788
#a    bca            2261055 236931 166  1838494  2780738
#b    bca            1820914 190809 166  1480610  2239434

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.838 0.0986 128 -1.499  0.6658 
#c acb / b acb 1.050 0.1235 128  0.419  0.9983 
#c acb / c bca 1.027 0.1094 108  0.248  0.9999 
#c acb / a bca 0.788 0.1009 152 -1.859  0.4313 
#c acb / b bca 0.979 0.1253 152 -0.169  1.0000 
#a acb / b acb 1.253 0.1701 128  1.661  0.5602 
#a acb / c bca 1.225 0.1547 149  1.604  0.5970 
#a acb / a bca 0.940 0.1362 166 -0.427  0.9982 
#a acb / b bca 1.167 0.1692 166  1.067  0.8938 
#b acb / c bca 0.977 0.1235 149 -0.181  1.0000 
#b acb / a bca 0.750 0.1087 166 -1.983  0.3567 
#b acb / b bca 0.932 0.1350 166 -0.489  0.9965 
#c bca / a bca 0.768 0.0945 128 -2.149  0.2691 
#c bca / b bca 0.953 0.1173 128 -0.390  0.9988 
#a bca / b bca 1.242 0.1765 128  1.523  0.6499 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Transport_and_Catabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Transport_and_Catabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust
#REML criterion at convergence: 259.2997
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1106  
#Residual             0.4776  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#11.096093                0.166007               -0.092010               -0.005734  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  

Transport_and_Catabolism_mod <- lmer(log(Transport_and_Catabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Transport_and_Catabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Transport_and_Catabolism) ~ 1 + Diet * Diet_Sequence + (1 |      Subject)
#Data: Picrust

#REML criterion at convergence: 259.3

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-3.12760 -0.60224 -0.03888  0.78272  2.19803 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.01224  0.1106  
#Residual             0.22813  0.4776  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)            11.096093   0.074105 149.735
#Dieta                   0.166007   0.121976   1.361
#Dietb                  -0.092010   0.121976  -0.754
#Diet_Sequencebca       -0.005734   0.107266  -0.053
#Dieta:Diet_Sequencebca -0.064479   0.176560  -0.365
#Dietb:Diet_Sequencebca  0.105269   0.176560   0.596

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.549                             
#Dietb       -0.549  0.333                      
#Diet_Sqncbc -0.691  0.379  0.379               
#Dt:Dt_Sqncb  0.379 -0.691 -0.230 -0.549        
#Dtb:Dt_Sqnc  0.379 -0.230 -0.691 -0.549  0.333 

r.squaredGLMM(Transport_and_Catabolism_mod)

#R2m        R2c
#[1,] 0.02206108 0.07185585

check_model(Transport_and_Catabolism_mod) 


Transport_and_Catabolism.emm.s <- emmeans(Transport_and_Catabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Transport_and_Catabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             11.1 0.0741 114     10.9     11.2
#a    acb             11.3 0.1022 168     11.1     11.5
#b    acb             11.0 0.1022 168     10.8     11.2
#c    bca             11.1 0.0776 114     10.9     11.2
#a    bca             11.2 0.1070 168     11.0     11.4
#b    bca             11.1 0.1070 168     10.9     11.3

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Transport_and_Catabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb -0.16601 0.122 128 -1.361  0.7501 
#c acb - b acb  0.09201 0.122 128  0.754  0.9745 
#c acb - c bca  0.00573 0.107 114  0.053  1.0000 
#c acb - a bca -0.09580 0.130 157 -0.736  0.9771 
#c acb - b bca -0.00753 0.130 157 -0.058  1.0000 
#a acb - b acb  0.25802 0.141 128  1.832  0.4492 
#a acb - c bca  0.17174 0.128 155  1.338  0.7632 
#a acb - a bca  0.07021 0.148 168  0.474  0.9970 
#a acb - b bca  0.15848 0.148 168  1.071  0.8922 
#b acb - c bca -0.08628 0.128 155 -0.672  0.9847 
#b acb - a bca -0.18780 0.148 168 -1.269  0.8014 
#b acb - b bca -0.09954 0.148 168 -0.673  0.9847 
#c bca - a bca -0.10153 0.128 128 -0.795  0.9679 
#c bca - b bca -0.01326 0.128 128 -0.104  1.0000 
#a bca - b bca  0.08827 0.147 128  0.599  0.9909 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Transport_and_Catabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response   SE  df lower.CL upper.CL
#c    acb              65913 4884 114    56914    76335
#a    acb              77816 7955 168    63595    95217
#b    acb              60119 6146 168    49132    73563
#c    bca              65536 5083 114    56203    76419
#a    bca              72540 7761 168    58728    89599
#b    bca              66411 7105 168    53767    82029

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio    SE  df t.ratio p.value
#c acb / a acb 0.847 0.103 128 -1.361  0.7501 
#c acb / b acb 1.096 0.134 128  0.754  0.9745 
#c acb / c bca 1.006 0.108 114  0.053  1.0000 
#c acb / a bca 0.909 0.118 157 -0.736  0.9771 
#c acb / b bca 0.993 0.129 157 -0.058  1.0000 
#a acb / b acb 1.294 0.182 128  1.832  0.4492 
#a acb / c bca 1.187 0.152 155  1.338  0.7632 
#a acb / a bca 1.073 0.159 168  0.474  0.9970 
#a acb / b bca 1.172 0.173 168  1.071  0.8922 
#b acb / c bca 0.917 0.118 155 -0.672  0.9847 
#b acb / a bca 0.829 0.123 168 -1.269  0.8014 
#b acb / b bca 0.905 0.134 168 -0.673  0.9847 
#c bca / a bca 0.903 0.115 128 -0.795  0.9679 
#c bca / b bca 0.987 0.126 128 -0.104  1.0000 
#a bca / b bca 1.092 0.161 128  0.599  0.9909 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 

lmer(log(Xenobiotics_Biodegradation_and_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Xenobiotics_Biodegradation_and_Metabolism) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust
#REML criterion at convergence: 241.6367
#Random effects:
#  Groups   Name        Std.Dev.
#Subject  (Intercept) 0.1368  
#Residual             0.4466  
#Number of obs: 176, groups:  Subject, 44
#Fixed Effects:
#  (Intercept)                   Dieta                   Dietb        Diet_Sequencebca  
#13.29095                 0.10336                -0.12596                -0.08088  
#Dieta:Diet_Sequencebca  Dietb:Diet_Sequencebca  
#0.13844                 0.14022  

Xenobiotics_Biodegradation_and_Metabolism_mod <- lmer(log(Xenobiotics_Biodegradation_and_Metabolism) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Xenobiotics_Biodegradation_and_Metabolism_mod) 

#Linear mixed model fit by REML ['lmerMod']
#Formula: log(Xenobiotics_Biodegradation_and_Metabolism) ~ 1 + Diet * Diet_Sequence +      (1 | Subject)
#Data: Picrust

#REML criterion at convergence: 241.6

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.3310 -0.6549  0.0142  0.7133  2.4177 

#Random effects:
#  Groups   Name        Variance Std.Dev.
#Subject  (Intercept) 0.0187   0.1368  
#Residual             0.1994   0.4466  
#Number of obs: 176, groups:  Subject, 44

#Fixed effects:
#(Intercept)            13.29095    0.07175 185.239
#Dieta                   0.10336    0.11404   0.906
#Dietb                  -0.12596    0.11404  -1.104
#Diet_Sequencebca       -0.08088    0.10386  -0.779
#Dieta:Diet_Sequencebca  0.13844    0.16507   0.839
#Dietb:Diet_Sequencebca  0.14022    0.16507   0.849

#Correlation of Fixed Effects:
#  (Intr) Dieta  Dietb  Dt_Sqn Dt:Dt_S
#Dieta       -0.530                             
#Dietb       -0.530  0.333                      
#Diet_Sqncbc -0.691  0.366  0.366               
#Dt:Dt_Sqncb  0.366 -0.691 -0.230 -0.530        
#Dtb:Dt_Sqnc  0.366 -0.230 -0.691 -0.530  0.333 

r.squaredGLMM(Xenobiotics_Biodegradation_and_Metabolism_mod)

#R2m       R2c
#[1,] 0.03783616 0.1203398

check_model(Xenobiotics_Biodegradation_and_Metabolism_mod) 

Xenobiotics_Biodegradation_and_Metabolism.emm.s <- emmeans(Xenobiotics_Biodegradation_and_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence)
Xenobiotics_Biodegradation_and_Metabolism.emm.s$emmeans

#Diet Diet_Sequence emmean     SE  df lower.CL upper.CL
#c    acb             13.3 0.0718 107     13.1     13.4
#a    acb             13.4 0.0974 166     13.2     13.6
#b    acb             13.2 0.0974 166     13.0     13.4
#c    bca             13.2 0.0751 107     13.1     13.4
#a    bca             13.5 0.1019 166     13.3     13.7
#b    bca             13.2 0.1019 166     13.0     13.4

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 

Xenobiotics_Biodegradation_and_Metabolism.emm.s$contrasts

#contrast      estimate    SE  df t.ratio p.value
#c acb - a acb  -0.1034 0.114 128 -0.906  0.9443 
#c acb - b acb   0.1260 0.114 128  1.104  0.8787 
#c acb - c bca   0.0809 0.104 107  0.779  0.9706 
#c acb - a bca  -0.1609 0.125 151 -1.291  0.7896 
#c acb - b bca   0.0666 0.125 151  0.534  0.9947 
#a acb - b acb   0.2293 0.132 128  1.741  0.5073 
#a acb - c bca   0.1842 0.123 148  1.498  0.6659 
#a acb - a bca  -0.0576 0.141 166 -0.408  0.9985 
#a acb - b bca   0.1700 0.141 166  1.206  0.8335 
#b acb - c bca  -0.0451 0.123 148 -0.367  0.9991 
#b acb - a bca  -0.2869 0.141 166 -2.035  0.3271 
#b acb - b bca  -0.0593 0.141 166 -0.421  0.9983 
#c bca - a bca  -0.2418 0.119 128 -2.026  0.3335 
#c bca - b bca  -0.0143 0.119 128 -0.120  1.0000 
#a bca - b bca   0.2275 0.138 128  1.651  0.5665 

#Degrees-of-freedom method: kenward-roger 
#Results are given on the log (not the response) scale. 
#P value adjustment: tukey method for comparing a family of 6 estimates 

emmeans(Xenobiotics_Biodegradation_and_Metabolism_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")

#$emmeans
#Diet Diet_Sequence response    SE  df lower.CL upper.CL
#c    acb             591814 42463 107   513348   682275
#a    acb             656255 63907 166   541468   795376
#b    acb             521776 50811 166   430511   632388
#c    bca             545832 40986 107   470338   633443
#a    bca             695133 70843 166   568437   850068
#b    bca             553674 56426 166   452761   677080

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 
#Intervals are back-transformed from the log scale 

#$contrasts
#contrast      ratio     SE  df t.ratio p.value
#c acb / a acb 0.902 0.1028 128 -0.906  0.9443 
#c acb / b acb 1.134 0.1293 128  1.104  0.8787 
#c acb / c bca 1.084 0.1126 107  0.779  0.9706 
#c acb / a bca 0.851 0.1061 151 -1.291  0.7896 
#c acb / b bca 1.069 0.1332 151  0.534  0.9947 
#a acb / b acb 1.258 0.1656 128  1.741  0.5073 
#a acb / c bca 1.202 0.1478 148  1.498  0.6659 
#a acb / a bca 0.944 0.1331 166 -0.408  0.9985 
#a acb / b bca 1.185 0.1671 166  1.206  0.8335 
#b acb / c bca 0.956 0.1175 148 -0.367  0.9991 
#b acb / a bca 0.751 0.1058 166 -2.035  0.3271 
#b acb / b bca 0.942 0.1328 166 -0.421  0.9983 
#c bca / a bca 0.785 0.0937 128 -2.026  0.3335 
#c bca / b bca 0.986 0.1177 128 -0.120  1.0000 
#a bca / b bca 1.255 0.1730 128  1.651  0.5665 

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 6 estimates 
#Tests are performed on the log scale 


lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
Excretory_System_mod <- lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Excretory_System_mod) 
r.squaredGLMM(Excretory_System_mod)
check_model(Excretory_System_mod) 
Excretory_System.emm.s <- emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Excretory_System.emm.s$emmeans
Excretory_System.emm.s$contrasts
emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")



lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
Excretory_System_mod <- lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Excretory_System_mod) 
r.squaredGLMM(Excretory_System_mod)
check_model(Excretory_System_mod) 
Excretory_System.emm.s <- emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Excretory_System.emm.s$emmeans
Excretory_System.emm.s$contrasts
emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")



lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
Excretory_System_mod <- lmer(log(Excretory_System) ~ 1 + Diet * Diet_Sequence + (1|Subject), data = Picrust)
summary(Excretory_System_mod) 
r.squaredGLMM(Excretory_System_mod)
check_model(Excretory_System_mod) 
Excretory_System.emm.s <- emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence)
Excretory_System.emm.s$emmeans
Excretory_System.emm.s$contrasts
emmeans(Excretory_System_mod, specs = pairwise ~ Diet * Diet_Sequence, type = "response")