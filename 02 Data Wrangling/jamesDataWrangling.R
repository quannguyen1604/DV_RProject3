require(dplyr)
require(tidyr)
require(ggplot2)

energyProductionDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_production_data;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_expendituresDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_expenditures;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_prices;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_msn_lookupDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_msn_lookup;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_adjusted_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_adjusted_prices;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_btuDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_btu;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_phyDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_phy;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#Energy production is the odd one out and uses STATECODE as colomn name instead of STATE.Must rename it.

energyProductionDF <- rename(energyProductionDF, STATE = STATECODE)

df_Prod_vs_Price <- dplyr::full_join(energyProductionDF, energy_adjusted_pricesDF, by = "STATE") %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN.x %in% c("^GE", "^HY", "^NU", "^SO", "^WY" ) )
