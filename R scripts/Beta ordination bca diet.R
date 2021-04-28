


## subset only bca

OTU_bca <- subset_samples(physeqpruned, Diet_Sequence%in%c("bca"))
OTU_bca

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 86 taxa and 85 samples ]
#sample_data() Sample Data:       [ 85 samples by 23 sample variables ]
#tax_table()   Taxonomy Table:    [ 86 taxa by 6 taxonomic ranks ]

OTU_bca1 <- subset_samples(OTU_bca, Time_Point%in%c("Baseline","End_High_Fibre","Washout","End_Hypoallergenic"))
OTU_bca1

# Correct order for plot
testOTU_bca <- sample_data(OTU_bca1)$Diet
testOTU_bca <- factor(testOTU_bca, levels = c("b","c","a"))
sample_data(OTU_bca1)$Diet <- testOTU_bca

# Correct order for plot
testOTU_bcaTP <- sample_data(OTU_bca1)$Time_Point
testOTU_bcaTP <- factor(testOTU_bcaTP, levels = c("Baseline","End_High_Fibre","Washout","End_Hypoallergenic"))
sample_data(OTU_bca1)$Time_Point <- testOTU_bcaTP


##PCoA method (Principal coordinates analysis)
ordination_OTUbcaDiet<-ordinate(OTU_bca1, method="PCoA", distance="bray")
p_Otus<-plot_ordination(OTU_bca1, ordination_OTUbcaDiet, type="samples", color="Time_Point")
p_Otus + geom_point(size = 3) + ggtitle(label ="Bray-Curtis") + theme(axis.title.y = element_text(size=12), axis.title.x = element_text(size=12)) + theme(axis.text.y =element_text(size=12), axis.text.x=element_text(size=12), plot.title = element_text(size=12))  + theme(legend.title = element_text(size=12), legend.text = element_text(size=12)) 

allGroupsColorsbca<- c("blue", "darkorange", "chartreuse4", "deeppink")

p_Otus + geom_point(size = 3) + ggtitle(label ="Bray-Curtis") + theme(axis.title.y = element_text(size=12), axis.title.x = element_text(size=12)) + theme(axis.text.y =element_text(size=12), axis.text.x=element_text(size=12), plot.title = element_text(size=12))  + theme(legend.title = element_text(size=12), legend.text = element_text(size=12)) + scale_color_manual(values = allGroupsColorsbca)

