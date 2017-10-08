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
stdSuffix <- do.call(rbind, tablesList)

# clean table
stdSuffix %>%
  filter(X1 != "PrimaryStreet SuffixName") %>%
  rename(suf_pri = X1) %>%
  rename(suf_com = X2) %>%
  rename(suf_std = X3) -> stdSuffix

stdSuffix$suf_pri <- str_to_title(stdSuffix$suf_pri)
stdSuffix$suf_com <- str_to_title(stdSuffix$suf_com)
stdSuffix$suf_std <- str_to_title(stdSuffix$suf_std)

# save as .RData file for inclusion in stldata package

save(stdSuffix, file = "data/stdSuffix.RData")
