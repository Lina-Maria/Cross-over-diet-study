## Load packages
library("vegan")

physeq

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 361 taxa and 177 samples ]
#sample_data() Sample Data:       [ 177 samples by 21 sample variables ]
#tax_table()   Taxonomy Table:    [ 361 taxa by 6 taxonomic ranks ]

# Correct order for plot
testphyseq <- sample_data(physeq)$Diet_Category
testphyseq <- factor(testphyseq, levels = c("High_Protein","High_Fibre","Hypoallergenic"))
sample_data(physeq)$Diet_Category <- testphyseq


physeqpruned <- prune_taxa(taxa_sums(physeq) >= 1, physeq)
physeqpruned

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 86 taxa and 177 samples ]
#sample_data() Sample Data:       [ 177 samples by 21 sample variables ]
#tax_table()   Taxonomy Table:    [ 86 taxa by 6 taxonomic ranks ]

# Correct order for plot
testphyseq1 <- sample_data(physeqpruned)$Diet_Category
testphyseq1 <- factor(testphyseq1, levels = c("High_Protein","High_Fibre","Hypoallergenic"))
sample_data(physeqpruned)$Diet_Category <- testphyseq1



## your data by Diet_Category
physeqpruned_dist = phyloseq::distance(physeqpruned, "bray")
physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


adonis(physeqpruned_dist ~ 1 + Diet_Category * Diet_Sequence, data= physeqpruned_dist_T)

#Call:
# adonis(formula = physeqpruned_dist ~ 1 + Diet_Category * Diet_Sequence,      data = physeqpruned_dist_T) 

#Permutation: free
#Number of permutations: 999

#Terms added sequentially (first to last)

#Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
#Diet_Category                 2    4.4427 2.22135 21.7327 0.18841  0.001 ***
#  Diet_Sequence                 1    0.3210 0.32095  3.1400 0.01361  0.007 ** 
#  Diet_Category:Diet_Sequence   2    1.3382 0.66909  6.5461 0.05675  0.001 ***
#  Residuals                   171   17.4783 0.10221         0.74123           
#Total                       176   23.5801                 1.00000           
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Adonis by Normal_Diet by Normal_Diet

## subset only Normal_Diet 


OTU_NormalDiet <- subset_samples(physeqpruned, Normal_Diet%in%c("Baseline","Washout"))
OTU_NormalDiet

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 86 taxa and 88 samples ]
#sample_data() Sample Data:       [ 88 samples by 22 sample variables ]
#tax_table()   Taxonomy Table:    [ 86 taxa by 6 taxonomic ranks ]


# Correct order for plot
testOTU_NormalDiet <- sample_data(OTU_NormalDiet)$Normal_Diet
testOTU_NormalDiet <- factor(testOTU_NormalDiet, levels = c("Baseline","Washout"))
sample_data(OTU_NormalDiet)$Normal_Diet <- testOTU_NormalDiet

## your data by Normal_Diet
OTU_NormalDiet_dist = phyloseq::distance(OTU_NormalDiet, "bray")
OTU_NormalDiet_dist_T <- data.frame(sample_data(OTU_NormalDiet))
adonis(OTU_NormalDiet_dist ~ Normal_Diet, data=OTU_NormalDiet_dist_T)

#Call:
#adonis(formula = OTU_NormalDiet_dist ~ Normal_Diet, data = OTU_NormalDiet_dist_T) 

#Permutation: free
#Number of permutations: 999

#Terms added sequentially (first to last)

#Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
#Normal_Diet  1    2.4010 2.40102  27.388 0.24154  0.001 ***
#Residuals   86    7.5394 0.08767         0.75846           
#Total       87    9.9404                 1.00000           
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

