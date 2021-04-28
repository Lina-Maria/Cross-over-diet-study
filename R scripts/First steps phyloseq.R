## Load packages
library("phyloseq")
library("ggplot2")

#### ONLY INDIVIDUAL  SAMPLES  INCLUDED NOT POOLED###
####################################################################
##                                                               ###
##              IMPORT AND SUBSET DATA                           ###
####################################################################

otu <-read.table(file="otu_table.txt", header=TRUE)
head(otu)

tax <-read.table(file = "taxonomy.txt", sep='\t', header= TRUE)
head(tax)

merged_File <-merge(otu,tax, by.x = c("OTUID"), by.y=c("OTUID"))
head(merged_File)

write.table(merged_File, file = "combined_otu_tax.csv", sep = '\t', col.names =  TRUE, row.names = FALSE)

library("ape") 

## for making this file you have to open the combined otu tax file in open office and then copy and paste in excel and the delete X from the sample name or number
## here sample name should be sa125 and no X125
otu_table <- read.csv("otu_matrix.csv", sep = ",", row.names = 1)
otu_table <- as.matrix(otu_table)


OTU <- otu_table(otu_table, taxa_are_rows = TRUE)


taxonomy = read.csv("taxonomy.csv", sep=",", row.names=1)
taxonomy = as.matrix (taxonomy)

TAX = tax_table(taxonomy) 

## the file must be saved in open office calculator and then copy and paste in excel and save as csv. Be careful that sample name contains sa plus number sa125
metadata	=	read.csv("Metadata_Diet.csv", sep=",", row.names=1)
META = sample_data(metadata)


###phy_tree = read_tree("tree.nwk")


taxa_names(TAX) 
taxa_names(OTU) 
##taxa_names(phy_tree)

sample_names(OTU) 
sample_names(META)

physeq = phyloseq(OTU, TAX, META)
physeq

#phyloseq-class experiment-level object
#otu_table()   OTU Table:         [ 361 taxa and 177 samples ]
#sample_data() Sample Data:       [ 177 samples by 23 sample variables ]
#tax_table()   Taxonomy Table:    [ 361 taxa by 6 taxonomic ranks ]

## if you want to check every component in detail:
##head: only the first rows of information
## tail: only the last rows
##print: everything

## for sample data use:
head(sample_data(physeq))
tail(sample_data(physeq))
print(sample_data(physeq))


write.table(otu_table (physeq), file = "physeqOTU.csv", quote = FALSE, sep = "\t", row.names = TRUE)

## for OTU table, replace sample_data for Otu_table and for taxonomy table, replace sample_date for tax_table:
head(otu_table(physeq))
head(tax_table(physeq))


