require(dplyr)
require(tidyr)
require(ggplot2)
require(jsonlite)
require(RCurl)
require(stringr)

dfexpend <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from ENERGY_EXPENDITURES;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

dfprice <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from ENERGY_PRICES;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

MSN_ref <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from ENERGY_MSN_LOOKUP;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

# Find general Energy Type labels from MSN reference table
MSN_ref %>% filter(UNIT != "Percent Share") %>% mutate(energy_label = word(DESCRIPTION,1)) %>% mutate(msn_short = substr(MSN, 0, 2)) %>% melt(id="energy_label") %>% rename(MSN = value) -> MSN_ref

# create unique row ids by concatenating State and MSN code
# pop last char of MSN, which codes for unit of measure, to obtain matching MSNs across expenditure and price
dfexpend %>% mutate(row_id = paste(STATE, '_', substr(MSN, 0, 4), sep='')) -> dfexpend
dfprice %>% mutate(row_id = paste(STATE, '_', substr(MSN, 0, 4), sep='')) -> dfprice

# melt data down so year is variable, not column name
newex <- dfexpend %>% select(-DATA_STATUS) %>% melt(., id=c('row_id','STATE','MSN')) %>% rename(year = variable) %>% rename(expenditure = value)
newpr <- dfprice %>% select(-DATA_STATUS) %>% melt(., id=c('row_id','STATE','MSN')) %>% rename(year = variable) %>% rename(price = value)

# append energy labels to newex
left_join(newex, MSN_ref, by=("MSN")) %>% select(-variable) -> newex

# join expenses and prices
dplyr::full_join(newex, newpr, by=c("row_id","year","STATE")) -> prex
prex %>% mutate(year = substr(year, 2, 5)) %>% mutate(energy_type = substr(MSN.x, 0, 3)) -> prex


prex %>% filter(STATE != "US") %>% filter(energy_label != "All") %>% filter(energy_label != "Total") %>% filter(energy_label != "Primary") %>% filter(price < 1000000) %>% filter(expenditure < 100000) %>% ggplot(aes(x=as.numeric(expenditure), y =as.numeric(price), color=as.numeric(year))) + geom_point() + facet_wrap(~energy_label) + labs(x="Expenditures", y="Price") + labs(title="Price to Expenditure Ratio By Energy Sector") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

