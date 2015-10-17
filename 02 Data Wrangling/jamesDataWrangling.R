require(dplyr)
require(tidyr)
require(ggplot2)
require(jsonlite)

energyProductionDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_production_data;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_expendituresDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_expenditures;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_prices WHERE x1960 >= -100000000;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_msn_lookupDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_msn_lookup;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_adjusted_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_adjusted_prices;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_btuDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_btu;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_phyDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_phy WHERE X1960 >= 0;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#Energy production is the odd one out and uses STATECODE as colomn name instead of STATE.Must rename it.

energyProductionDF <- rename(energyProductionDF, STATE = STATECODE)

#need to reformate dataframes into a more usable form using the transposition function t().

# first we Want to filter by state from the consumption dataset, and by MSN code for hydroelectric, nuclear, geothermal, and solar electricity generated, as well as total population. 
altEnergyDF <- energy_usage_phyDF %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN %in% c("HYEGP", "NUEGP", "GEEGP", "SOEGP","TPOPP", "WYEGP")) %>% arrange(STATE))

test01 <- energy_usage_phyDF %>% filter (STATE == "CA", MSN == "TPOPP") 

#Filter out the states we're interested in from the price dataset, as well as the (unadjusted) price for electricity in these states, including residential electricity and electricty for all uses (comercial, industrial, etc etc).
priceDF <- energy_pricesDF %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN %in% c("ESTCD", "ESRCD"))


catest1 <- altEnergyDF %>% filter (STATE == "CA", MSN == "SOEGP")
catest2 <- priceDF %>% filter (STATE == "CA")
catest3 <- dplyr::bind_rows(catest2, catest1)
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


#make cols for geothermal production
CA_geo <- CA_energy %>% filter (MSN == "GEEGP")
CA_geo <- melt(CA_geo, id = "MSN")
CA_geo <- rename(CA_geo, GEEGP = value)
CA_geo <- rename(CA_geo, YEAR = variable)
CA_geo <- data.frame(CA_geo) %>% select(YEAR, GEEGP)


#make cols for hydroelectricity production
CA_hy <- CA_energy %>% filter (MSN == "HYEGP")
CA_hy <- melt(CA_hy, id = "MSN")
CA_hy <- rename(CA_hy, HYEGP = value)
CA_hy <- rename(CA_hy, YEAR = variable)
CA_hy <- data.frame(CA_hy) %>% select(YEAR, HYEGP)


# make cols for nuclear electricity production
CA_nu <- CA_energy %>% filter (MSN == "NUEGP")
CA_nu <- melt(CA_nu, id = "MSN")
CA_nu <- rename(CA_nu, NUEGP = value)
CA_nu <- rename(CA_nu, YEAR = variable)
CA_nu <- data.frame(CA_nu) %>% select(YEAR, NUEGP)


# make cols for solar electricity production
CA_so <- CA_energy %>% filter (MSN == "SOEGP")
CA_so <- melt(CA_so, id = "MSN")
CA_so <- rename(CA_so, SOEGP = value)
CA_so <- rename(CA_so, YEAR = variable)
CA_so <- data.frame(CA_so) %>% select(YEAR, SOEGP)

# make cols for wind electricity production
CA_wy <- CA_energy %>% filter (MSN == "WYEGP")
CA_wy <- melt(CA_wy, id = "MSN")
CA_wy <- rename(CA_wy, WYEGP = value)
CA_wy <- rename(CA_wy, YEAR = variable)
CA_wy <- data.frame(CA_wy) %>% select(YEAR, WYEGP)

#make cols for total population
CA_pop <- CA_energy %>% filter (MSN == "TPOPP")
CA_pop <- melt(CA_pop, id = "MSN")
CA_pop <- rename(CA_pop, TPOPP = value)
CA_pop <- rename(CA_pop, YEAR = variable)
CA_pop <- data.frame(CA_pop) %>% select(YEAR, TPOPP)


#join rows in desired form.
CA_all <- dplyr::full_join(CA_price, CA_geo, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_hy, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_nu, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_so, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_wy, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_pop, "YEAR")

#mutate in cols that account for population, want produciton per population... Is treating the different values as discrete, not continuous. Need to change.
CA_ratio <- CA_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

CA_ratio <- CA_ratio %>% arrange(year)
View(CA_all)

#once again we melt in order to view multiple variables on one plot
CA_graph <- melt(CA_ratio, id.vars = "year")

ggplot(CA_graph, aes(y = value, x = year, color = variable)) +
  geom_point() +
  geom_line(aes(group = variable)) +
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  coord_cartesian()

#pass into ggplot
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  #facet_grid(~STATE) +
  labs(title='Alternate Energy Production and Price by Year') +
  labs(x="Year", y=paste("Production (phy untis), Price (dollars)")) +
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  layer(data=CA_graph, 
        mapping=aes(x=(year), y=value, color = variable), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(),
        geom_line(aes(group = variable)), 
        #position=position_identity()
        position=position_jitter(width=0.2, height=0)
  )
