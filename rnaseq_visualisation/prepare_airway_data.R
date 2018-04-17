# Loading packages
library(airway)
library(DESeq2)
library(tidyverse)


# Create a DESeqDataSeq object from the airway data
data("airway")
dds <- DESeqDataSet(airway, design = ~ cell + dex)
rm(airway)

# Fit the model
dds <- DESeq(dds)


#### Prepare matrix of counts ####

# Normalise and transform read counts
norm_cts <- varianceStabilizingTransformation(dds, blind = FALSE) %>% 
  assay()


#### Prepare gene table ####

# Contrast treatments and filter for those with very low p-value
## retain only gene names
genes_of_interest <- results(dds, contrast = c("dex", "trt", "untrt"), 
                             lfcThreshold = 1,
                             tidy = TRUE) %>% 
  filter(padj < 0.05) %>% 
  pull(row)

# Get the gene annotation
genes_of_interest <- rowRanges(dds) %>% 
  as_tibble() %>% 
  filter(group_name %in% genes_of_interest) %>% 
  droplevels() %>% 
  select(gene = group_name,
         chrom = seqnames,
         start, end) %>% 
  group_by(gene) %>% 
  summarise(chrom = unique(chrom),
            start = min(c(start, end)),
            end = max(c(start, end))) %>% 
  arrange(chrom, start, end, gene)

test_result <- results(dds, contrast = c("dex", "trt", "untrt"), 
                             lfcThreshold = 1,
                             tidy = TRUE) %>% 
  rename(gene = row) %>% 
  as_tibble()


#### Prepare sample metadata table ####

sample_info <- colData(dds) %>% 
  as_tibble() %>% 
  select(sample = Run,
         cell, dex) %>% 
  mutate_all(funs(as.character))




#### Save R object for loading ####

save(test_result, norm_cts, sample_info,
     file = "~/Documents/work/teaching/2018-02-22-KCL/expression_lesson/expression_lesson.RData",
     compress = "bzip2")

