require(tidyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require("jsonlite")
require("RCurl")

allProd <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_production_data;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

allUse <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_btu;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

allExpend <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_expenditures;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

allPrices <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_prices;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

qdf1 <- allProd %>% rename(., STATE = STATECODE) %>% filter (MSN == "NGMPB", STATE == "US") %>% select (-DATA_STATUS, -STATE) %>% melt(., id = "MSN") %>% select(-MSN) %>% rename(., YEAR = variable, PRODUCTION = value)

qdf2 <- allUse %>% filter (MSN == "NGTCB", STATE == "US") %>% select (-DATA_STATUS, -STATE) %>% melt(., id = "MSN") %>% select(-MSN) %>% rename(., YEAR = variable, CONSUMPTION = value)

qdf3 <- inner_join(qdf1, qdf2, "YEAR") %>% sapply(.,gsub,pattern="X",replacement="") %>% data.frame(.) %>% melt(., id="YEAR")

p1 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='U.S Natural Gas Production and Consumption') +
  labs(x="YEAR", y="BILLION BTU") +
  layer(data=qdf3, 
        mapping=aes(x= as.numeric(as.character(YEAR)), y=as.numeric(as.character(value)), color=variable), 
        stat="identity", 
        stat_params=list(), 
        geom="line",
        geom_params=list(),
        geom_line(aes(group = variable)),
        position=position_jitter(width=0.2, height=0)
  )

qdf4 <- allPrices %>% filter (MSN == "NGTCD", STATE == "US") %>% select (-DATA_STATUS, -STATE) %>% melt(., id = "MSN") %>% select(-MSN) %>% rename(., YEAR = variable, PRICE = value)

qdf5 <- allExpend %>% filter (MSN == "NGTCV", STATE == "US") %>% select (-DATA_STATUS, -STATE) %>% melt(., id = "MSN") %>% select(-MSN) %>% rename(., YEAR = variable, EXPEND = value) %>% inner_join(., qdf1, "YEAR") %>% mutate(EXPENDITURE =  1000*as.numeric(EXPEND)/as.numeric(PRODUCTION)) %>% select (-EXPEND, -PRODUCTION) %>% inner_join(., qdf4, "YEAR") %>% sapply(.,gsub,pattern="X",replacement="") %>% data.frame(.) %>% melt(., id="YEAR")

p2 <- ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='U.S Natural Gas Price and Expenditure') +
  labs(x="YEAR", y="DOLLAR PER MILLION BTU") +
  layer(data=qdf5, 
        mapping=aes(x= as.numeric(as.character(YEAR)), y=as.numeric(as.character(value)), color=variable), 
        stat="identity", 
        stat_params=list(), 
        geom="line",
        geom_params=list(),
        geom_line(aes(group = variable)),
        position=position_jitter(width=0.2, height=0)
  )
p1
p2
