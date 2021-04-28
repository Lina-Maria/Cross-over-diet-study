
###OTU-based metrics 
##Bray-Curtis metric that takes into account both the presence/absence and abundance of OTUs in your samples  (i.e. diversity)

## subset only acb

OTU_acb <- subset_samples(physeqpruned, Diet_Sequence%in%c("acb"))
OTU_acb

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 86 taxa and 92 samples ]
#sample_data() Sample Data:       [ 92 samples by 23 sample variables ]
#tax_table()   Taxonomy Table:    [ 86 taxa by 6 taxonomic ranks ]

# Correct order for plot
testOTU_acb <- sample_data(OTU_acb)$Diet
testOTU_acb <- factor(testOTU_acb, levels = c("a","c","b"))
sample_data(OTU_acb)$Diet <- testOTU_acb


# Correct order for plot
testOTU_acbTP <- sample_data(OTU_acb)$Time_Point
testOTU_acbTP <- factor(testOTU_acbTP, levels = c("Baseline","End_Hypoallergenic","Washout","End_High_Fibre"))
sample_data(OTU_acb)$Time_Point <- testOTU_acbTP


##PCoA method (Principal coordinates analysis)
ordination_OTUacbDiet<-ordinate(OTU_acb, method="PCoA", distance="bray")
p_Otus<-plot_ordination(OTU_acb, ordination_OTUacbDiet, type="samples", color="Time_Point")
p_Otus + geom_point(size = 3) + ggtitle(label ="Bray-Curtis") + theme(axis.title.y = element_text(size=12), axis.title.x = element_text(size=12)) + theme(axis.text.y =element_text(size=12), axis.text.x=element_text(size=12), plot.title = element_text(size=12))  + theme(legend.title = element_text(size=12), legend.text = element_text(size=12)) 

allGroupsColorsacb<- c("blue","deeppink","chartreuse4","darkorange")

p_Otus + geom_point(size = 3) + ggtitle(label ="Bray-Curtis") + theme(axis.title.y = element_text(size=12), axis.title.x = element_text(size=12)) + theme(axis.text.y =element_text(size=12), axis.text.x=element_text(size=12), plot.title = element_text(size=12))  + theme(legend.title = element_text(size=12), legend.text = element_text(size=12)) + scale_color_manual(values = allGroupsColorsacb)
