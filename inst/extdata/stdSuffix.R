library(rvest)
library(dplyr)
library(stringr)

# download webpage content
webpage <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm")

# identify table notes
tables <- html_nodes(webpage, "table")

# extract the third node, which contains the suffix data
tablesList <- webpage %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

# convert data to a data frame
stdSuffixTbl <- do.call(rbind, tablesList)

# clean table
stdSuffixTbl %>%
  filter(X1 != "PrimaryStreet SuffixName") %>%
  rename(suf_pri = X1) %>%
  rename(suf_com = X2) %>%
  rename(suf_std = X3) -> stdSuffixTbl

stdSuffixTbl$suf_pri <- str_to_title(stdSuffixTbl$suf_pri)
stdSuffixTbl$suf_com <- str_to_title(stdSuffixTbl$suf_com)
stdSuffixTbl$suf_std <- str_to_title(stdSuffixTbl$suf_std)

# save as .RData file for inclusion in stldata package

save(stdSuffixTbl, file = "data/stdSuffixTbl.RData")
