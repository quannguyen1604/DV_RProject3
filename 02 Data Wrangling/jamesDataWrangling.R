require(dplyr)
require(tidyr)
require(ggplot2)
require(jsonlite)

energyProductionDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_production_data;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_expendituresDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_expenditures;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_msn_lookupDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_msn_lookup;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_adjusted_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_adjusted_prices;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_btuDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_btu;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_prices WHERE x1960 >= -100000000;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_phyDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_phy WHERE X1960 >= 0;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#Energy production is the odd one out and uses STATECODE as colomn name instead of STATE.Must rename it.

energyProductionDF <- rename(energyProductionDF, STATE = STATECODE)

#need to reformate dataframes into a more usable form using the transposition function t().

# first we Want to filter by state from the consumption dataset, and by MSN code for hydroelectric, nuclear, geothermal, and solar electricity generated, as well as total population. 
altEnergyDF <- energy_usage_phyDF %>% filter (STATE %in% c("TX", "NV", "CA", "NJ"), MSN %in% c("HYEGP", "NUEGP", "GEEGP", "SOEGP","TPOPP", "WYEGP")) %>% arrange(STATE)


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

TX_price <- TX_energy %>% filter (MSN == "ESRCD")
TX_price <- melt(TX_price, id = "MSN")
TX_price <- rename(TX_price, ESRCD = value)
TX_price <- rename(TX_price, YEAR = variable)
TX_price <- data.frame(TX_price) %>% select(YEAR, ESRCD)

#make cols for geothermal production
CA_geo <- CA_energy %>% filter (MSN == "GEEGP")
CA_geo <- melt(CA_geo, id = "MSN")
CA_geo <- rename(CA_geo, GEEGP = value)
CA_geo <- rename(CA_geo, YEAR = variable)
CA_geo <- data.frame(CA_geo) %>% select(YEAR, GEEGP)

TX_geo <- TX_energy %>% filter (MSN == "GEEGP")
TX_geo <- melt(TX_geo, id = "MSN")
TX_geo <- rename(TX_geo, GEEGP = value)
TX_geo <- rename(TX_geo, YEAR = variable)
TX_geo <- data.frame(TX_geo) %>% select(YEAR, GEEGP)

#make cols for hydroelectricity production
CA_hy <- CA_energy %>% filter (MSN == "HYEGP")
CA_hy <- melt(CA_hy, id = "MSN")
CA_hy <- rename(CA_hy, HYEGP = value)
CA_hy <- rename(CA_hy, YEAR = variable)
CA_hy <- data.frame(CA_hy) %>% select(YEAR, HYEGP)

TX_hy <- TX_energy %>% filter (MSN == "HYEGP")
TX_hy <- melt(TX_hy, id = "MSN")
TX_hy <- rename(TX_hy, HYEGP = value)
TX_hy <- rename(TX_hy, YEAR = variable)
TX_hy <- data.frame(TX_hy) %>% select(YEAR, HYEGP)


# make cols for nuclear electricity production
CA_nu <- CA_energy %>% filter (MSN == "NUEGP")
CA_nu <- melt(CA_nu, id = "MSN")
CA_nu <- rename(CA_nu, NUEGP = value)
CA_nu <- rename(CA_nu, YEAR = variable)
CA_nu <- data.frame(CA_nu) %>% select(YEAR, NUEGP)

TX_nu <- TX_energy %>% filter (MSN == "NUEGP")
TX_nu <- melt(TX_nu, id = "MSN")
TX_nu <- rename(TX_nu, NUEGP = value)
TX_nu <- rename(TX_nu, YEAR = variable)
TX_nu <- data.frame(TX_nu) %>% select(YEAR, NUEGP)


# make cols for solar electricity production
CA_so <- CA_energy %>% filter (MSN == "SOEGP")
CA_so <- melt(CA_so, id = "MSN")
CA_so <- rename(CA_so, SOEGP = value)
CA_so <- rename(CA_so, YEAR = variable)
CA_so <- data.frame(CA_so) %>% select(YEAR, SOEGP)

TX_so <- TX_energy %>% filter (MSN == "SOEGP")
TX_so <- melt(TX_so, id = "MSN")
TX_so <- rename(TX_so, SOEGP = value)
TX_so <- rename(TX_so, YEAR = variable)
TX_so <- data.frame(TX_so) %>% select(YEAR, SOEGP)

# make cols for wind electricity production
CA_wy <- CA_energy %>% filter (MSN == "WYEGP")
CA_wy <- melt(CA_wy, id = "MSN")
CA_wy <- rename(CA_wy, WYEGP = value)
CA_wy <- rename(CA_wy, YEAR = variable)
CA_wy <- data.frame(CA_wy) %>% select(YEAR, WYEGP)

TX_wy <- TX_energy %>% filter (MSN == "WYEGP")
TX_wy <- melt(TX_wy, id = "MSN")
TX_wy <- rename(TX_wy, WYEGP = value)
TX_wy <- rename(TX_wy, YEAR = variable)
TX_wy <- data.frame(TX_wy) %>% select(YEAR, WYEGP)

#make cols for total population
CA_pop <- CA_energy %>% filter (MSN == "TPOPP")
CA_pop <- melt(CA_pop, id = "MSN")
CA_pop <- rename(CA_pop, TPOPP = value)
CA_pop <- rename(CA_pop, YEAR = variable)
CA_pop <- data.frame(CA_pop) %>% select(YEAR, TPOPP)

TX_pop <- TX_energy %>% filter (MSN == "TPOPP")
TX_pop <- melt(TX_pop, id = "MSN")
TX_pop <- rename(TX_pop, TPOPP = value)
TX_pop <- rename(TX_pop, YEAR = variable)
TX_pop <- data.frame(TX_pop) %>% select(YEAR, TPOPP)


#join rows in desired form.
CA_all <- dplyr::full_join(CA_price, CA_geo, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_hy, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_nu, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_so, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_wy, "YEAR")
CA_all <- dplyr::full_join(CA_all, CA_pop, "YEAR")

TX_all <- dplyr::full_join(TX_price, TX_geo, "YEAR")
TX_all <- dplyr::full_join(TX_all, TX_hy, "YEAR")
TX_all <- dplyr::full_join(TX_all, TX_nu, "YEAR")
TX_all <- dplyr::full_join(TX_all, TX_so, "YEAR")
TX_all <- dplyr::full_join(TX_all, TX_wy, "YEAR")
TX_all <- dplyr::full_join(TX_all, TX_pop, "YEAR")

#mutate in cols that account for population, want produciton per population... Is treating the different values as discrete, not continuous. Need to change.
CA_ratio <- CA_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

TX_ratio <- TX_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

CA_ratio <- CA_ratio %>% arrange(year) %>% mutate (STATE = "CA")

TX_ratio <- TX_ratio %>% arrange(year) %>% mutate (STATE = "TX")

graph_all <- CA_ratio
graph_all <- dplyr::bind_rows(graph_all, TX_ratio)

#once again we melt in order to view multiple variables on one plot
CA_graph <- melt(CA_ratio, id.vars = "year")
graph <-  melt(graph_all, id.vars = c("year", "STATE"))


ggplot(graph, aes(y = value, x = year, color = variable)) +
  facet_grid(~STATE)+
  geom_point() +
  geom_line(aes(group = variable)) +
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  coord_cartesian()

