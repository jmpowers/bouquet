# install("bouquet")
# library(bouquet)
# library(magrittr) # pipe operator %>% to chain functions
# metadata <- read.table("metadata.csv", header=TRUE, sep="\t") %>% load_metadata("SampleDate", "Sample", c("Species", "Population", "Drought"), "Type", "Biomass")
# longdata <- read.table("GCMS_output.csv", header=TRUE, sep="\t") %>% load_longdata("Sample", "Time", "NIST_ID", "Area", "Score")
# sampletable <- make_sampletable(longdata)
# chemtable <- make_chemtable(longdata, metadata) %>% filter_RT(4,15) %>% filter_match(0.8) %>% filter_freq(0.2, group = TRUE) %>% filter_contaminant(cont.list = "Caprolactam") %>% filter_area(min_maximum = 20000) %>% filter_ambient_ttest(sampletable, metadata, alpha = 0.05, p.adjust= "fdr") %>% combine_filters("AND") %>% augment_chemtable_pubchem()
# finaltable <- prune_sampletable(sampletable, chemtable) %>% standardize_sampletable(metadata)
