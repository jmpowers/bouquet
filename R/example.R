# library(magrittr) # pipe operator %>% to chain functions
# data(GCMSfloral) #contains sample GCMS_output and GCMS_metadata
# metadata <- load_metadata(GCMS_metadata, "SampleDate", "Filename", c("Cross", "Time"), "Type", "Flrs")
# longdata <- load_longdata(GCMS_output, "Sample", "RT", "Name", "Area", "Match", maxmatch=100)
# sampletable <- make_sampletable(longdata)
# chemtable <-
#   make_chemtable(longdata, metadata) %>%
#   filter_RT(4, 15) %>%
#   filter_match(0.8)
# chemtable <- make_chemtable(longdata, metadata) %>% filter_RT(4,15) %>% filter_match(0.8) %>% filter_freq(0.2, group = TRUE) %>% filter_contaminant(cont.list = "Caprolactam") %>% filter_area(min_maximum = 20000) %>% filter_ambient_ttest(sampletable, metadata, alpha = 0.05, p.adjust= "fdr") %>% combine_filters("AND") %>% augment_chemtable_pubchem()
# finaltable <- prune_sampletable(sampletable, chemtable) %>% standardize_sampletable(metadata)
