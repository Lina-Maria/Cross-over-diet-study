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


## Data by Diet_Category
physeqpruned_dist = phyloseq::distance(physeqpruned, "bray")
physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)

### Convert distance matrix to a table
DistanceTable <- data.frame(t(combn(rownames(sample_data(physeqpruned)),2)), as.numeric(physeqpruned_dist))
names(DistanceTable) <- c("c1", "c2", "distance")

write.table(DistanceTable, file = "distance diet", quote = FALSE, sep = "\t", row.names = TRUE)

##
OTU_bcaBaseline <- subset_samples(OTU_bca1, Time_Point%in%c("Baseline"))
OTU_bcaBaseline

## Data by Diet_Category
OTU_bcaBaseline_dist = phyloseq::distance(OTU_bcaBaseline, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTablebcaBaseline <- data.frame(t(combn(rownames(sample_data(OTU_bcaBaseline)),2)), as.numeric(OTU_bcaBaseline_dist))
names(DistanceTablebcaBaseline) <- c("c1", "c2", "distance")

write.table(DistanceTablebcaBaseline, file = "distance dietbcaBaseline", quote = FALSE, sep = "\t", row.names = TRUE)


##
OTU_bcaEnd_Hypoallergenic <- subset_samples(OTU_bca1, Time_Point%in%c("End_Hypoallergenic"))
OTU_bcaEnd_Hypoallergenic

## Data by Diet_Category
OTU_bcaEnd_Hypoallergenic_dist = phyloseq::distance(OTU_bcaEnd_Hypoallergenic, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTablebcaEnd_Hypoallergenic <- data.frame(t(combn(rownames(sample_data(OTU_bcaEnd_Hypoallergenic)),2)), as.numeric(OTU_bcaEnd_Hypoallergenic_dist))
names(DistanceTablebcaEnd_Hypoallergenic) <- c("c1", "c2", "distance")

write.table(DistanceTablebcaEnd_Hypoallergenic, file = "distance dietbcaEnd_Hypoallergenic", quote = FALSE, sep = "\t", row.names = TRUE)

##
OTU_bcaWashout <- subset_samples(OTU_bca1, Time_Point%in%c("Washout"))
OTU_bcaWashout

## Data by Diet_Category
OTU_bcaWashout_dist = phyloseq::distance(OTU_bcaWashout, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTablebcaWashout <- data.frame(t(combn(rownames(sample_data(OTU_bcaWashout)),2)), as.numeric(OTU_bcaWashout_dist))
names(DistanceTablebcaWashout) <- c("c1", "c2", "distance")

write.table(DistanceTablebcaWashout, file = "distance dietbcaWashout", quote = FALSE, sep = "\t", row.names = TRUE)


## subset only acb

OTU_acb <- subset_samples(physeqpruned, Diet_Sequence%in%c("acb"))
OTU_acb

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 86 taxa and 92 samples ]
#sample_data() Sample Data:       [ 92 samples by 23 sample variables ]
#tax_table()   Taxonomy Table:    [ 86 taxa by 6 taxonomic ranks ]

##
OTU_acbBaseline <- subset_samples(OTU_acb, Time_Point%in%c("Baseline"))
OTU_acbBaseline

## your data by Diet_Category
OTU_acbBaseline_dist = phyloseq::distance(OTU_acbBaseline, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTableacbBaseline <- data.frame(t(combn(rownames(sample_data(OTU_acbBaseline)),2)), as.numeric(OTU_acbBaseline_dist))
names(DistanceTableacbBaseline) <- c("c1", "c2", "distance")

write.table(DistanceTableacbBaseline, file = "distance dietacbBaseline", quote = FALSE, sep = "\t", row.names = TRUE)


##
OTU_acbEnd_Hypoallergenic <- subset_samples(OTU_acb, Time_Point%in%c("End_Hypoallergenic"))
OTU_acbEnd_Hypoallergenic

## your data by Diet_Category
OTU_acbEnd_Hypoallergenic_dist = phyloseq::distance(OTU_acbEnd_Hypoallergenic, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTableacbEnd_Hypoallergenic <- data.frame(t(combn(rownames(sample_data(OTU_acbEnd_Hypoallergenic)),2)), as.numeric(OTU_acbEnd_Hypoallergenic_dist))
names(DistanceTableacbEnd_Hypoallergenic) <- c("c1", "c2", "distance")

write.table(DistanceTableacbEnd_Hypoallergenic, file = "distance dietacbEnd_Hypoallergenic", quote = FALSE, sep = "\t", row.names = TRUE)

##
OTU_acbWashout <- subset_samples(OTU_acb, Time_Point%in%c("Washout"))
OTU_acbWashout

## your data by Diet_Category
OTU_acbWashout_dist = phyloseq::distance(OTU_acbWashout, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTableacbWashout <- data.frame(t(combn(rownames(sample_data(OTU_acbWashout)),2)), as.numeric(OTU_acbWashout_dist))
names(DistanceTableacbWashout) <- c("c1", "c2", "distance")

write.table(DistanceTableacbWashout, file = "distance dietacbWashout", quote = FALSE, sep = "\t", row.names = TRUE)


##
OTU_acbEnd_High_Fibre <- subset_samples(OTU_acb, Time_Point%in%c("End_High_Fibre"))
OTU_acbEnd_High_Fibre

## your data by Diet_Category
OTU_acbEnd_High_Fibre_dist = phyloseq::distance(OTU_acbEnd_High_Fibre, "bray")
#physeqpruned_dist_T <- data.frame(sample_data(physeqpruned))
#adonis(physeqpruned_dist ~ Diet_Category, data=physeqpruned_dist_T)


### Convert distance matrix to a table
DistanceTableacbEnd_High_Fibre <- data.frame(t(combn(rownames(sample_data(OTU_acbEnd_High_Fibre)),2)), as.numeric(OTU_acbEnd_High_Fibre_dist))
names(DistanceTableacbEnd_High_Fibre) <- c("c1", "c2", "distance")

write.table(DistanceTableacbEnd_High_Fibre, file = "distance dietacbEnd_High_Fibre", quote = FALSE, sep = "\t", row.names = TRUE)


