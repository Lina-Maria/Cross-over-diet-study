# AP. Woodward PhD and LM. Martinez-Lopez PhD, 2020-21, University of Melbourne.
# Implements a Dirichlet regression model (continuous multivariate proportions;  https://doi.org/10.1111/2041-210X.13234) 
#     using package 'brms' (Burkner 2017; https://doi.org/10.18637/jss.v080.i01).
# The default multivariate logit link and flat priors for coefficients are retained.
# Results are expressed as interval plots of the posterior predictions and posterior estimates (means), and these are also tabulated.

library(brms)
library(ggplot2)
library(dplyr)

options(max.print = 100000)

# The alpha-diversity data are contained in the data frame 'alpha_data2'.

head(alpha_data2)
write.table(alpha_data2, file = 'diet_sample_alpha_data.txt', row.names = FALSE)

# Run the linear mixed model for alpha diversity (Shannon index).

mod_alpha <- lmer(SHANNON~1+DIET*SEQUENCE + (1|SUBJECT), data = alpha_data2)

hist(residuals(mod_alpha))
plot(fitted(mod_alpha),residuals(mod_alpha))
confint(mod_alpha)


# The family data are contained in the data frame 'sample_data2'.
#     Note that this contains a matrix column 'y', which is a named matrix of the family-level observations.

head(sample_data2)
write.table(sample_data2, file = 'diet_sample_family_data.txt', row.names = FALSE)

# Run the Dirichlet regression model for family abundance.

seq_mod_21_family <- brm(y~DIET*SEQUENCE+(1|SUBJECT),data = sample_data2, family = dirichlet(), iter = 10000, cores = 4)

summary(seq_mod_21_family)

# From the selected model, constuct a posterior predictions (i.e. with residual variance) interval plot.
# The data frame 'new_dietdata' specifies the new factor levels to be predicted for 'SEQUENCE' and 'DIET'.

new_dietdata

diet_pred <- predict(seq_mod_21_family, newdata = new_dietdata, probs = c(0.055,0.945), re_formula = NA)

diet_pred_list <- vector(mode = 'list', length = (dim(diet_pred)[3]))

for (i in 1:dim(diet_pred)[3]){
  
  diet_pred_list[[i]] <- data.frame(diet_pred[,,i])
  colnames(diet_pred_list[[i]]) <- dimnames(diet_pred)[[2]]
  
  diet_pred_list[[i]]$Family <- dimnames(diet_pred)[[3]][i]
  diet_pred_list[[i]]$SEQUENCE <- new_dietdata$SEQUENCE
  diet_pred_list[[i]]$DIET <- new_dietdata$DIET
}

full_pred <- bind_rows(diet_pred_list)

ggplot(data = full_pred, aes(y = Estimate, x = Family, color = DIET)) + geom_point(position = position_dodge(width = 0.7)) + coord_flip() + geom_errorbar(aes(ymin = Q5.5, ymax = Q94.5),position = position_dodge(width = 0.7)) + facet_wrap(vars(SEQUENCE), labeller = as_labeller(c('ACB' = 'Sequence: ACB','BCA' =  'Sequence: BCA'))) + theme(axis.text.y = element_text(face='italic')) + labs(y = 'Proportional Abundance', x = 'Family') + scale_color_discrete(labels = c('High-Protein','High-Fiber','Hydrolysed','Washout'))
ggsave('predicted_abundances.pdf', width = 18, height = 8, units = 'in')

full_pred

# From the selected model, constuct a posterior estimates (population estimates) interval plot.

diet_fitted <- fitted(seq_mod_21_family, newdata = new_dietdata, probs = c(0.055,0.945), re_formula = NA)

diet_fitted_list <- vector(mode = 'list', length = (dim(diet_fitted)[3]))

for (i in 1:dim(diet_fitted)[3]){
  
  diet_fitted_list[[i]] <- data.frame(diet_fitted[,,i])
  colnames(diet_fitted_list[[i]]) <- dimnames(diet_fitted)[[2]]
  
  diet_fitted_list[[i]]$Family <- substr((dimnames(diet_fitted)[[3]][i]),7,nchar(dimnames(diet_fitted)[[3]][i])-1)
  diet_fitted_list[[i]]$SEQUENCE <- new_dietdata$SEQUENCE
  diet_fitted_list[[i]]$DIET <- new_dietdata$DIET
}

full_fitted <- bind_rows(diet_fitted_list)

ggplot(data = full_fitted, aes(y = Estimate, x = Family, color = DIET)) + geom_point(position = position_dodge(width = 0.7)) + coord_flip() + geom_errorbar(aes(ymin = Q5.5, ymax = Q94.5),position = position_dodge(width = 0.7)) + facet_wrap(vars(SEQUENCE), labeller = as_labeller(c('ACB' = 'Sequence: ACB','BCA' =  'Sequence: BCA'))) + theme(axis.text.y = element_text(face='italic')) + labs(y = 'Proportional Abundance', x = 'Family') + scale_color_discrete(labels = c('High-Protein','High-Fiber','Hydrolysed','Washout'))
ggsave('fitted_abundances.pdf', width = 18, height = 8, units = 'in')

full_fitted

# The family data are contained in the data frame 'phylum_data2'.
#     Note that this contains a matrix column 'y', which is a named matrix of the family-level observations.

head(phylum_data2)
write.table(phylum_data2, file = 'diet_sample_phylum_data.txt', row.names = FALSE)

# Run the Dirichlet regression model for phylum abundance.

seq_mod_21_phylum <- brm(y~DIET*SEQUENCE+(1|SUBJECT),data = phylum_data2, family = dirichlet(), iter = 10000, cores = 4)

summary(seq_mod_21_phylum)

# From the selected model, constuct a posterior predictions (i.e. with residual variance) interval plot.
# The data frame 'new_dietdata' specifies the new factor levels to be predicted for 'SEQUENCE' and 'DIET'.

new_dietdata

phyl_pred <- predict(seq_mod_21_phylum, newdata = new_dietdata, probs = c(0.055,0.945), re_formula = NA)

phyl_pred_list <- vector(mode = 'list', length = (dim(phyl_pred)[3]))

for (i in 1:dim(phyl_pred)[3]){
  
  phyl_pred_list[[i]] <- data.frame(phyl_pred[,,i])
  colnames(phyl_pred_list[[i]]) <- dimnames(phyl_pred)[[2]]
  
  phyl_pred_list[[i]]$Phylum <- dimnames(phyl_pred)[[3]][i]
  phyl_pred_list[[i]]$SEQUENCE <- new_dietdata$SEQUENCE
  phyl_pred_list[[i]]$DIET <- new_dietdata$DIET
}

full_phyl_pred <- bind_rows(phyl_pred_list)

ggplot(data = full_phyl_pred, aes(y = Estimate, x = Phylum, color = DIET)) + geom_point(position = position_dodge(width = 0.7)) + coord_flip() + geom_errorbar(aes(ymin = Q5.5, ymax = Q94.5),position = position_dodge(width = 0.7)) + facet_wrap(vars(SEQUENCE), labeller = as_labeller(c('ACB' = 'Sequence: ACB','BCA' =  'Sequence: BCA'))) + theme(axis.text.y = element_text(face='italic')) + labs(y = 'Proportional Abundance', x = 'Phylum') + scale_color_discrete(labels = c('High-Protein','High-Fiber','Hydrolysed','Washout'))
ggsave('predicted_phylum.pdf', width = 18, height = 8, units = 'in')

full_phyl_pred

#

phyl_fitted <- fitted(seq_mod_21_phylum, newdata = new_dietdata, probs = c(0.055,0.945), re_formula = NA)

phyl_fitted_list <- vector(mode = 'list', length = (dim(phyl_fitted)[3]))

for (i in 1:dim(phyl_pred)[3]){
  
  phyl_fitted_list[[i]] <- data.frame(phyl_fitted[,,i])
  colnames(phyl_fitted_list[[i]]) <- dimnames(phyl_fitted)[[2]]
  
  phyl_fitted_list[[i]]$Phylum <- substr((dimnames(phyl_fitted)[[3]][i]),7,nchar(dimnames(phyl_fitted)[[3]][i])-1)
  phyl_fitted_list[[i]]$SEQUENCE <- new_dietdata$SEQUENCE
  phyl_fitted_list[[i]]$DIET <- new_dietdata$DIET
}

full_phyl_fitted <- bind_rows(phyl_fitted_list)

ggplot(data = full_phyl_fitted, aes(y = Estimate, x = Phylum, color = DIET)) + geom_point(position = position_dodge(width = 0.7)) + coord_flip() + geom_errorbar(aes(ymin = Q5.5, ymax = Q94.5),position = position_dodge(width = 0.7)) + facet_wrap(vars(SEQUENCE), labeller = as_labeller(c('ACB' = 'Sequence: ACB','BCA' =  'Sequence: BCA'))) + theme(axis.text.y = element_text(face='italic')) + labs(y = 'Proportional Abundance', x = 'Phylum') + scale_color_discrete(labels = c('High-Protein','High-Fiber','Hydrolysed','Washout'))
ggsave('fitted_phylum.pdf', width = 18, height = 8, units = 'in')

full_phyl_fitted