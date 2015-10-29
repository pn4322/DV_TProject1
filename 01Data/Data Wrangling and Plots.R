require(tidyr)
require(dplyr)
require(ggplot2)
require(extrafont)
require("jsonlite")
require("RCurl")



Wine <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from Wine"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))



Wine %>% select(HLY_HIDX_NORMAL, HLY_WIND_AVGSPD) %>% arrange(desc(HLY_HIDX_NORMAL)) %>% ggplot(aes(x=HLY_HIDX_NORMAL, y=HLY_WIND_AVGSPD)) + geom_point()

Wine %>% filter(HLY_WIND_AVGSPD > 100) %>% select(DATE_, HLY_WIND_AVGSPD) %>% ggplot(aes(x=DATE_, y=HLY_WIND_AVGSPD)) + geom_point()


Wine %>% mutate(TempHeatIndex = cume_dist(HLY_HIDX_NORMAL)) %>% filter(TempHeatIndex <= .20) %>% arrange(desc(TempHeatIndex)) %>% ggplot(aes(x=DATE_, y=TempHeatIndex)) + geom_point()


####Head and Summary for Wine
summary(Wine)
head(Wine)


