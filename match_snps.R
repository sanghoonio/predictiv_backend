library(tidyverse) ### install.packages('tidyverse')
library(openxlsx) ### install.packages('openxlsx')

### first goal is to read in excel files
vitamin_data <- read.xlsx('data/updated_vitamin_data.xlsx', 2) ### sheet 2 has the updated vitamin table
customer_data <- read.xlsx('data/updated_customer_data.xlsx', 1) ### only has 1 sheet

### remove leading and trailing spaces in all columns
vitamin_data <- mutate_if(vitamin_data, is.character, trimws)
customer_data <- mutate_if(customer_data, is.character, trimws)

### declare hashtable functions
assign_hash <- Vectorize(assign, vectorize.args = c('x', 'value'))
get_hash <- Vectorize(get, vectorize.args = 'x')
exists_hash <- Vectorize(exists, vectorize.args = 'x')

### create an empty hashtable to store VITAMIN data
hashtable <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)

### hashtables need unique keys, so filter the vitamin table to include only unique chr:position values (keys)
vitamin_unique <- vitamin_data[, c(5, 8, 9, 2)]
vitamin_unique <- unique(vitamin_unique)

### we are storing SNPs from the vitamin data into the hashtable
### assign chr:position as keys and allele data as values from filtered VITAMIN data in hashtable
assign_hash(vitamin_unique$`Chr:GRCh38.Position`, paste(vitamin_unique$`Normal.Allele(s)`, vitamin_unique$Risk.Allele, vitamin_unique$Vitamin, sep = ''), hashtable)

### check if all chr:positions in the CUSTOMER data exist in the VITAMIN data hashtable
### this produces an index of logical values (vector) we can use to filter only for SNPs that are in both data tables
snp_in_vitamin <- exists_hash(customer_data$Chr.Position, hashtable)

### filter CUSTOMER data to the SNPs we need. we select the rows in filtered_customer_data that are TRUE in snp_in_vitamin
filtered_customer_data <- customer_data[snp_in_vitamin, ]

### extract normal (major) allele from the vitamin hashtable
filtered_customer_data$Normal.Allele <- substr(get_hash(filtered_customer_data$Chr.Position, hashtable), 1, 1)

### extract risk (minor) allele from the vitamin hashtable
filtered_customer_data$Risk.Allele <- substr(get_hash(filtered_customer_data$Chr.Position, hashtable), 2, 2)

### extract vitamin label from the vitamin hashtable
filtered_customer_data$Vitamin <- substr(get_hash(filtered_customer_data$Chr.Position, hashtable), 3, nchar(get_hash(filtered_customer_data$Chr.Position, hashtable)))


### first, use logical condition to check if customer Allele1 OR Allele2 are in major+minor allele substring from vitamin hashtable.
### supplying this condition to filter() allows us to filter our customer data to only include rows with allele data that have matchable values in the vitamin data.
### basically removes rows that have Allele1 or Allele2 that are neither normal or risk alleles.
filtered_customer_data <- filter(filtered_customer_data, (str_detect(substr(get_hash(filtered_customer_data$Chr.Position, hashtable), 1, 2), filtered_customer_data$Allele1.Plus) | str_detect(substr(get_hash(filtered_customer_data$Chr.Position, hashtable), 1, 2), filtered_customer_data$Allele2.Plus)))

### next, classifying genotypes with logical statements. 
### if allele1 == normal and allele2 == normal, then set 'Nonvariant' in new column. 
### if else allele1 == risk and allele2 == risk, then set 'Homozygous'. 
### else set 'Heterozygous'
filtered_customer_data$Genotype.Classification <- ifelse((filtered_customer_data$Allele1 == filtered_customer_data$Normal.Allele) & (filtered_customer_data$Allele2 == filtered_customer_data$Normal.Allele), 'Nonvariant', ifelse((filtered_customer_data$Allele1 == filtered_customer_data$Risk.Allele) & (filtered_customer_data$Allele2 == filtered_customer_data$Risk.Allele), 'Homozygous', 'Heterozygous'))

### split the customer dataframe grouped by each vitamin.
### produces a list of dataframes.
split_customer_data <- split(filtered_customer_data, filtered_customer_data$Vitamin)

### use logical conditions/statements to code each vitamin phenotype into the customer data according to vitamin excel file sheet 1.
### we are assigning the phenotype to a new column 'Phenotype' 
### ifelse example syntax: ifelse(1 == 1, '1 is 1', '1 is 2') will return '1 is 1'
split_customer_data[['Vitamin A']]$Phenotype <- ifelse(length(which(split_customer_data[['Vitamin A']]$Genotype.Classification == 'Nonvariant')) == 2, 'Normal', ifelse((length(which(split_customer_data[['Vitamin A']]$Genotype.Classification == 'Nonvariant')) == 1) & (length(which(split_customer_data[['Vitamin A']]$Genotype.Classification == 'Heterozygous')) == 1), 'Below Average', 'Low'))
split_customer_data[['Vitamin B6']]$Phenotype <- ifelse(split_customer_data[['Vitamin B6']]$Genotype.Classification == 'Homozygous', 'Low', ifelse(split_customer_data[['Vitamin B6']]$Genotype.Classification == 'Heterozygous', 'Below Average', 'No Variant'))
### vitamin B9 todo
split_customer_data[['Vitamin B12']]$Phenotype <- ifelse(split_customer_data[['Vitamin B12']]$Genotype.Classification == 'Homozygous', 'Above Average', 'Normal')
### vitamin C todo
split_customer_data[['Vitamin D']]$Phenotype <- ifelse(length(which(split_customer_data[['Vitamin D']]$Genotype.Classification == 'Nonvariant')) == 3, 'Normal', ifelse((length(which(split_customer_data[['Vitamin D']]$Genotype.Classification == 'Nonvariant')) == 2) & (length(which(split_customer_data[['Vitamin D']]$Genotype.Classification == 'Heterozygous')) == 1), 'Below Average', 'Low'))
### vitamin E todo

### we are very close to finishing! your task is to add the remaining conditional statements for vitamins B9, C, and E by friday (8/5/22).
### on friday, we will go over your code and produce the output file for the front-end team.
### please note: I had to make some updates to the data files since our last meeting, and so be sure to pull from the git repo.
