library(tidyverse) ### install.packages('tidyverse')
library(openxlsx) ### install.packages('openxlsx')

### first goal is to read in excel files
vitamin_data <- read.xlsx('data/updated_vitamin_data.xlsx', 2) ### sheet 2 has the updated vitamin table
customer_data <- read.xlsx('data/updated_customer_data.xlsx', 1) ### only has 1 sheet

### declare hashtable functions
assign_hash <- Vectorize(assign, vectorize.args = c('x', 'value'))
get_hash <- Vectorize(get, vectorize.args = 'x')
exists_hash <- Vectorize(exists, vectorize.args = 'x')

### create an empty hashtable to store VITAMIN data
hashtable <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)

### hashtables need unique keys, so filter the vitamin table to include only unique chr:position values (keys)
vitamin_unique <- vitamin_data[, c(5, 7)]
vitamin_unique <- unique(vitamin_unique)

### we are storing SNPs from the vitamin data into the hashtable
### assign chr:position as keys and allele data as values from filtered VITAMIN data in hashtable
assign_hash(vitamin_unique$`Chr:GRCh38.Position`, vitamin_unique$Alleles, hashtable)

### check if all chr:positions in the CUSTOMER data exist in the VITAMIN data hashtable
### this produces an index of logical values (vector) we can use to filter only for SNPs that are in both data tables
snp_in_vitamin <- exists_hash(customer_data$Chr.Position, hashtable)

### filter CUSTOMER data to the SNPs we need. we select the rows in filtered_customer_data that are TRUE in snp_in_vitamin
filtered_customer_data <- customer_data[snp_in_vitamin, ]

### this for loop takes each row of the filtered customer data and gets the associated allele values from the vitamin hashtable to check the genotype
for (row in 1:nrow(filtered_customer_data)) {
    vitamin_search <- get_hash(filtered_customer_data$Chr.Position[row], hashtable) ### get the vitamin allele value for the current row
    split_vitamin <- str_split(vitamin_search, ' / ') ### split the allele value string by the slash delimiter to get a list of allele values (char)
    
    print(paste(filtered_customer_data$Chr.Position[row], filtered_customer_data$`Allele1.Plus`[row], filtered_customer_data$`Allele2.Plus`[row], sep = ', ')) ### print the chr:position, allele1, allele2 from customer
    for (item in 1:length(split_vitamin[[1]])) { ### for every item in the list of allele values, we can print each individual vitamin value
        print(split_vitamin[[1]][item])
    }
    
    
    ### with this loop we can isolate individual vitamin allele values for each SNP in the customer.
    ### now the goal is to check the customer genotypes to these values and label each SNP as heterozygous, homozygous, or nonvariant.
    ### somehow, we need to check whether allele 1 and allele 2 in each row are the major or minor allele to label each SNP.
    ### remember, in the format 'A > G', A is the major (nonvariant) and G is the minor (variant) allele. 
    
    
    ### by next wednesday (8/3), update this script to accomplish the goal, or come up with a written step-by-step plan to share in the meeting.
    ### we will incorporate your changes and suggetsions to the script, and then discuss how to associate heterozygous/homozygous/nonvariant labels for 
    ### each SNP to the level of risk as according to sheet 1 on the updated vitamin table for next friday.
}



