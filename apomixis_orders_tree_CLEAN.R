#This script is a modification of the code Sarah Carey used to make 
#figure 3 for this paper: https://pubmed.ncbi.nlm.nih.gov/35306894/

#The ref column of 'apomixis_database_all_Aug2023_modified_w_refs.csv' was
#deleted because the commas would have made R throw a damn fit. 

library(ggtree)
library(ggplot2)
library(ggplotify)
library(utils)
library(dplyr)
library(tidyverse)
library(pheatmap)
library(ggnewscale)
library(ggtreeExtra)
library(ggtext)
library(naniar)


setwd("/Users/Goeckeritz/Desktop/Desktop - Charity’s MacBook Pro/PRFB_apomixis_project/iScience_perspective_revisions_6.2024/revision2/")

#As Sarah had for A, B, and C in her file, I made counts for: number of families, number of genera, Apo, Diplo, Adventitious embryony, and maybe genome availability?
#FORMATTING MODIFICATIONS
raw_data <- read.csv(file="apomixis_database_all_Aug2023_modified_w_refs2.csv", sep=',', header = TRUE) #make sure there are no unwanted spaces!!!! >.<

genus_count <- raw_data %>%
  dplyr::select(Order,Family) %>%
  group_by(Order) %>% summarise(COUNT_genus=n()) 

#this should add to 326 since all genera are listed only once in the file (they are all unique instances)
sum(genus_count$COUNT_genus)

  
family_count <- raw_data %>%
  dplyr::select(Order,Family) %>%
  group_by(Order, Family) %>% 
  unique() %>%
  group_by(Order) %>% summarise(COUNT_family=n())

#this should add to 80 based on my look at this number in bash, and after I got rid of the goddamn spaces
sum(family_count$COUNT_family)

raw_data$apospory_num <- as.numeric(str_replace_all(raw_data$Apospory, "Y", "1")) 
apospory_count <- raw_data %>%
  dplyr::select(Order,Family,apospory_num) %>%
  drop_na() %>%
  group_by(Order) %>% summarise(COUNT_apospory=n())

raw_data$diplospory_num <- as.numeric(str_replace_all(raw_data$Diplospory, "Y", "1")) 
diplospory_count <- raw_data %>%
  dplyr::select(Order,Family,diplospory_num) %>%
  drop_na() %>%
  group_by(Order) %>% summarise(COUNT_diplospory=n())

raw_data$AE_num <- as.numeric(str_replace_all(raw_data$Adventitious_Embryony, "Y", "1")) 
AE_count <- raw_data %>%
  dplyr::select(Order,Family,AE_num) %>%
  drop_na() %>%
  group_by(Order) %>% summarise(COUNT_AdventEmbryo=n())

order_list <- read.csv(file="orders_list.csv", sep=',', header=TRUE)
counts <- left_join(order_list, family_count, by="Order") %>%
  left_join(genus_count, by="Order") %>%
  left_join(apospory_count, by="Order") %>%
  left_join(diplospory_count, by="Order") %>%
  left_join(AE_count, by="Order") %>%
  replace(is.na(.), 0)

counts_simple <- counts %>%
  dplyr::select(Order,COUNT_family,COUNT_genus)
#END OF FORMATTING MODIFICATIONS

############# Modification of Sarah's code to generate tree:

#there are 64 orders in the IV tree.
angio_tree <- read.tree("angiosperm_orders_corrected_4_28_24.tree") #newick format

rownames(counts) <- counts[,1] #make the order names (first column) the row names
counts[,1] <- NULL #drop the first column now, don't need it.
counts[1:5] <- lapply(counts[1:5], as.numeric) #make the counts numeric in all your columns if they aren't already.

head(angio_tree)
tree_plot <- ggtree(angio_tree, branch.length = "none",
                    size=1, color="gray75") +
  geom_tiplab(size=4, color="black", font="bold") +
  xlim(NA,60) +
  theme_tree("white") #make a simple tree with all the orders, based on the IV classification

tree_plot #looks good
colnames(counts)=c("a","b","c","d","e")

heatmap <- gheatmap(tree_plot, data=log(counts), offset=5, width=0.25,
                    colnames_angle = 0, colnames_position="top",
                    colnames_offset_y=0.5, color="black") +
  scale_fill_distiller(palette='OrRd', direction=1, name="Log no. counts", na.value = "gray75") + 
  theme(legend.position = c(0.1, 0.8), plot.margin=unit(c(0,-4,0,-0.5),"in")) 

#reformat counts a bit in anticipation of making the figure with numbers overlaid on the heat map
counts2 <-
  counts %>%
  rownames_to_column("name") %>%
  pivot_longer(-name, names_to = "column", values_to = "count") %>%
  mutate(
    name = as.factor(name),
    column = as.factor(column),
    count = as.numeric(count),
    count_char = as.character(count)) %>%
  replace_with_na(replace = list(count=0)) %>%
  replace_with_na(replace = list(count_char="0")) %>%
  as.data.frame()

###
 tree_plot +
  geom_fruit(
    data=counts2,
    geom=geom_tile,
    mapping=aes(x=column, y=name, fill=log(count)),
    pwidth=0.22, color="white", offset=0.125) +
    scale_fill_distiller(palette='OrRd', direction=1, name="Log no. counts", na.value = "gray75") +
  theme(legend.position = c(0.15, 0.8), plot.margin=unit(c(0,-5.5,0,-0.7),"in")) +
  geom_fruit(
    data=counts2,
    geom=geom_text,
    mapping=aes(x=column, y=name, label=count_char), fontface="bold", pwidth=0.22, size=3.3, position=position_nudge(x=28.05), color="black") 
#note: missing values should be where there are no known cases of apomixis
 
 ggsave("Fig1_iScience_Review_simplified_red_grey_w_counts_TREE_CORRECTION.pdf", f, units="in", width=12, height=10, dpi=600,
       device="pdf")
