require(dplyr)
require(tidyr)
require(ggplot2)
require(jsonlite)
require(reshape2)

energy_pricesDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_prices WHERE x1960 >= -100000000;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

energy_usage_phyDF <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from energy_usage_phy WHERE X1960 >= 0;"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))


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


#transforming and joining the appropriate dataframes into the needed form so that we can plot against time

#make cols for price using melt
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

NV_price <- NV_energy %>% filter (MSN == "ESRCD")
NV_price <- melt(NV_price, id = "MSN")
NV_price <- rename(NV_price, ESRCD = value)
NV_price <- rename(NV_price, YEAR = variable)
NV_price <- data.frame(NV_price) %>% select(YEAR, ESRCD)

NJ_price <- NJ_energy %>% filter (MSN == "ESRCD")
NJ_price <- melt(NJ_price, id = "MSN")
NJ_price <- rename(NJ_price, ESRCD = value)
NJ_price <- rename(NJ_price, YEAR = variable)
NJ_price <- data.frame(NJ_price) %>% select(YEAR, ESRCD)

#make cols for geothermal production using melt
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

NV_geo <- NV_energy %>% filter (MSN == "GEEGP")
NV_geo <- melt(NV_geo, id = "MSN")
NV_geo <- rename(NV_geo, GEEGP = value)
NV_geo <- rename(NV_geo, YEAR = variable)
NV_geo <- data.frame(NV_geo) %>% select(YEAR, GEEGP)

NJ_geo <- NJ_energy %>% filter (MSN == "GEEGP")
NJ_geo <- melt(NJ_geo, id = "MSN")
NJ_geo <- rename(NJ_geo, GEEGP = value)
NJ_geo <- rename(NJ_geo, YEAR = variable)
NJ_geo <- data.frame(NJ_geo) %>% select(YEAR, GEEGP)

#make cols for hydroelectricity production using melt
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

NV_hy <- NV_energy %>% filter (MSN == "HYEGP")
NV_hy <- melt(NV_hy, id = "MSN")
NV_hy <- rename(NV_hy, HYEGP = value)
NV_hy <- rename(NV_hy, YEAR = variable)
NV_hy <- data.frame(NV_hy) %>% select(YEAR, HYEGP)

NJ_hy <- NJ_energy %>% filter (MSN == "HYEGP")
NJ_hy <- melt(NJ_hy, id = "MSN")
NJ_hy <- rename(NJ_hy, HYEGP = value)
NJ_hy <- rename(NJ_hy, YEAR = variable)
NJ_hy <- data.frame(NJ_hy) %>% select(YEAR, HYEGP)


# make cols for nuclear electricity production using melt
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

NV_nu <- NV_energy %>% filter (MSN == "NUEGP")
NV_nu <- melt(NV_nu, id = "MSN")
NV_nu <- rename(NV_nu, NUEGP = value)
NV_nu <- rename(NV_nu, YEAR = variable)
NV_nu <- data.frame(NV_nu) %>% select(YEAR, NUEGP)

NJ_nu <- NJ_energy %>% filter (MSN == "NUEGP")
NJ_nu <- melt(NJ_nu, id = "MSN")
NJ_nu <- rename(NJ_nu, NUEGP = value)
NJ_nu <- rename(NJ_nu, YEAR = variable)
NJ_nu <- data.frame(NJ_nu) %>% select(YEAR, NUEGP)

# make cols for solar electricity production using melt
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

NV_so <- NV_energy %>% filter (MSN == "SOEGP")
NV_so <- melt(NV_so, id = "MSN")
NV_so <- rename(NV_so, SOEGP = value)
NV_so <- rename(NV_so, YEAR = variable)
NV_so <- data.frame(NV_so) %>% select(YEAR, SOEGP)

NJ_so <- NJ_energy %>% filter (MSN == "SOEGP")
NJ_so <- melt(NJ_so, id = "MSN")
NJ_so <- rename(NJ_so, SOEGP = value)
NJ_so <- rename(NJ_so, YEAR = variable)
NJ_so <- data.frame(NJ_so) %>% select(YEAR, SOEGP)

# make cols for wind electricity production using melt
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

NV_wy <- NV_energy %>% filter (MSN == "WYEGP")
NV_wy <- melt(NV_wy, id = "MSN")
NV_wy <- rename(NV_wy, WYEGP = value)
NV_wy <- rename(NV_wy, YEAR = variable)
NV_wy <- data.frame(NV_wy) %>% select(YEAR, WYEGP)

NJ_wy <- NJ_energy %>% filter (MSN == "WYEGP")
NJ_wy <- melt(NJ_wy, id = "MSN")
NJ_wy <- rename(NJ_wy, WYEGP = value)
NJ_wy <- rename(NJ_wy, YEAR = variable)
NJ_wy <- data.frame(NJ_wy) %>% select(YEAR, WYEGP)

#make cols for total population using melt
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

NV_pop <- NV_energy %>% filter (MSN == "TPOPP")
NV_pop <- melt(NV_pop, id = "MSN")
NV_pop <- rename(NV_pop, TPOPP = value)
NV_pop <- rename(NV_pop, YEAR = variable)
NV_pop <- data.frame(NV_pop) %>% select(YEAR, TPOPP)

NJ_pop <- NJ_energy %>% filter (MSN == "TPOPP")
NJ_pop <- melt(NJ_pop, id = "MSN")
NJ_pop <- rename(NJ_pop, TPOPP = value)
NJ_pop <- rename(NJ_pop, YEAR = variable)
NJ_pop <- data.frame(NJ_pop) %>% select(YEAR, TPOPP)


#use full_join to aggregate into the full dataset.
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

NV_all <- dplyr::full_join(NV_price, NV_geo, "YEAR")
NV_all <- dplyr::full_join(NV_all, NV_hy, "YEAR")
NV_all <- dplyr::full_join(NV_all, NV_nu, "YEAR")
NV_all <- dplyr::full_join(NV_all, NV_so, "YEAR")
NV_all <- dplyr::full_join(NV_all, NV_wy, "YEAR")
NV_all <- dplyr::full_join(NV_all, NV_pop, "YEAR")

NJ_all <- dplyr::full_join(NJ_price, NJ_geo, "YEAR")
NJ_all <- dplyr::full_join(NJ_all, NJ_hy, "YEAR")
NJ_all <- dplyr::full_join(NJ_all, NJ_nu, "YEAR")
NJ_all <- dplyr::full_join(NJ_all, NJ_so, "YEAR")
NJ_all <- dplyr::full_join(NJ_all, NJ_wy, "YEAR")
NJ_all <- dplyr::full_join(NJ_all, NJ_pop, "YEAR")

#mutate in cols that account for population, want produciton per population.
CA_ratio <- CA_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

TX_ratio <- TX_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

NV_ratio <- NV_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)

NJ_ratio <- NJ_all %>% mutate(geo_per_1k =  (as.numeric(GEEGP)/as.numeric(TPOPP)), nuc_per_1k = as.numeric(NUEGP)/as.numeric(TPOPP), hydro_per_1k = as.numeric(HYEGP)/as.numeric(TPOPP), wind_per_1k = as.numeric(WYEGP)/as.numeric(TPOPP), sol_per_1k =as.numeric(SOEGP)/as.numeric(TPOPP), pop = as.numeric (TPOPP), price = as.numeric(ESRCD), year = as.character(YEAR)) %>% select(year, geo_per_1k, nuc_per_1k, hydro_per_1k, wind_per_1k, sol_per_1k)


# mutate in a STATE col so can differentiate
CA_ratio <- CA_ratio %>% arrange(year) %>% mutate (STATE = "CA")

TX_ratio <- TX_ratio %>% arrange(year) %>% mutate (STATE = "TX")

NV_ratio <- NV_ratio %>% arrange(year) %>% mutate (STATE = "NV")

NJ_ratio <- NJ_ratio %>% arrange(year) %>% mutate (STATE = "NJ")

#bind rows to create full dataframe to be graphed
graph_all <- CA_ratio
graph_all <- dplyr::bind_rows(graph_all, TX_ratio)
graph_all <- dplyr::bind_rows(graph_all, NV_ratio)
graph_all <- dplyr::bind_rows(graph_all, NJ_ratio)


#once again we melt in order to view multiple variables on one plot
graph <-  melt(graph_all, id.vars = c("year", "STATE"))


ggplot(graph, aes(y = value, x = year, color = variable)) +
  labs(title='Alternate Energy Production by State') +
  labs(x="Year", y=paste("Electricity Produced per 1000 citizens")) +
  facet_grid(~STATE)+
  geom_point() +
  geom_line(aes(group = variable)) +
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  scale_x_discrete(breaks = c("X1960", "X1965", "X1970", "X1975", "X1980", "X1985", "X1990", "X1995", "X2000", "X2005", "X2010"))
  coord_cartesian()

