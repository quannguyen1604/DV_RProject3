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

#need to reformate dataframes into a more usable form using the transposition function t().

# first we Want to filter by state from the consumption dataset, and by MSN code for hydroelectric, nuclear, geothermal, and solar electricity generated, as well as total population. 
altEnergyDF <- energy_usage_phyDF %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN %in% c("HYEGP", "NUEGP", "GEEGP", "SOEGP","TPOPP")) %>% arrange(STATE)

#Filter out the states we're interested in from the price dataset, as well as the (unadjusted) price for electricity in these states, including residential electricity and electricty for all uses (comercial, industrial, etc etc).
priceDF <- energy_pricesDF %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN %in% c("ESTCD", "ESRCD"))

#Do a row bind (not a col join, our data is weird and the unique variables were placed in the rows, not the colomns) to combine these two data sets.
df_altEnergy <- dplyr::bind_rows(priceDF, altEnergyDF)

#Create each state's new datasheet. 
CA_energy <- df_altEnergy %>% filter (STATE == "CA") %>% select (-DATA_STATUS, -STATE)

TX_energy <- df_altEnergy %>% filter (STATE == "TX") %>% select (-DATA_STATUS, -STATE)
  
NV_energy <- df_altEnergy %>% filter (STATE == "NV") %>% select (-DATA_STATUS, -STATE)
  
NJ_energy <- df_altEnergy %>% filter (STATE == "NJ") %>% select (-DATA_STATUS, -STATE)


#transforming and joining the appropriate dataframes into the needed form

#make cols for price
CA_price <- CA_energy %>% filter (MSN == "ESRCD")
CA_price <- melt(CA_price, id = "MSN")
CA_price <- rename(CA_price, ESRCD = value)
CA_price <- rename(CA_price, YEAR = variable)
CA_price <- data.frame(CA_price) %>% select(YEAR, ESRCD)
View(CA_price)

#make cols for geothermal production
CA_geo <- CA_energy %>% filter (MSN == "GEEGP")
CA_geo <- melt(CA_geo, id = "MSN")
CA_geo <- rename(CA_geo, GEEGP = value)
CA_geo <- rename(CA_geo, YEAR = variable)
CA_geo <- data.frame(CA_geo) %>% select(YEAR, GEEGP)
View(CA_geo)

#make cols for hydroelectricity production
CA_hy <- CA_energy %>% filter (MSN == "HYEGP")
CA_hy <- melt(CA_hy, id = "MSN")
CA_hy <- rename(CA_hy, HYEGP = value)
CA_hy <- rename(CA_hy, YEAR = variable)
CA_hy <- data.frame(CA_hy) %>% select(YEAR, HYEGP)
View(CA_hy)

# make cols for nuclear electricity production
CA_nu <- CA_energy %>% filter (MSN == "NUEGP")
CA_nu <- melt(CA_nu, id = "MSN")
CA_nu <- rename(CA_nu, NUEGP = value)
CA_nu <- rename(CA_nu, YEAR = variable)
CA_nu <- data.frame(CA_nu) %>% select(YEAR, NUEGP)
View(CA_nu)


df_altEnergy

#pass into ggplot
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_grid(~STATE) +
  labs(title='Alternate Energy Production and Price by Year') +
  labs(x="Year", y=paste("Production (phy untis), Price (dollars)")) +
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  layer(data=df_altEnergy, 
        mapping=aes(x=(X1970:X2013), y=), color=MSN), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(), 
        #position=position_identity()
        position=position_jitter(width=0.2, height=0)
  )
