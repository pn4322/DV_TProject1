require(tidyr)
require(dplyr)
require(ggplot2)
require(extrafont)
require("jsonlite")
require("RCurl")



TexasWeather1 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from TexasWeather1"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

TexasWeather2 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from TexasWeather2"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))



TexasWeather1 %>% select(HLY_HIDX_NORMAL, HLY_WIND_AVGSPD) %>% arrange(desc(HLY_HIDX_NORMAL)) %>% ggplot(aes(x=HLY_HIDX_NORMAL, y=HLY_WIND_AVGSPD)) + geom_point()

TexasWeather1 %>% filter(HLY_WIND_AVGSPD > 100) %>% select(DATE_, HLY_WIND_AVGSPD) %>% ggplot(aes(x=DATE_, y=HLY_WIND_AVGSPD)) + geom_point()


TexasWeather1 %>% mutate(TempHeatIndex = cume_dist(HLY_HIDX_NORMAL)) %>% filter(TempHeatIndex <= .20) %>% arrange(desc(TempHeatIndex)) %>% ggplot(aes(x=DATE_, y=TempHeatIndex)) + geom_point()


TexasWeather2 %>% ggplot(aes(x=DATE_, y=HLY_WCHL_NORMAL)) + geom_point()

TexasWeather2 %>% filter(HLY_TEMP_NORMAL > 900) %>% select(DATE_, HLY_TEMP_NORMAL) %>% ggplot(aes(x=DATE_, y=HLY_TEMP_NORMAL)) + geom_point()

TexasWeather2 %>% mutate(TempPercent = cume_dist(HLY_TEMP_NORMAL)) %>% filter(TempPercent <= .10) %>% arrange(desc(TempPercent)) %>% ggplot(aes(x=DATE_, y=TempPercent)) + geom_point()

#***************Combine the two data sets***********************
#inner join, wrangling and visualization

dplyr::inner_join(TexasWeather1, TexasWeather2, by="DATE_") %>% View
df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", '129.152.144.84:5001/rest/native/?query="select * from TexasWeather1 e join TexasWeather2 d on e.DATE_ = d.DATE_"')),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); tbl_df(df)


df %>% mutate(WindPercent = cume_dist(HLY_WIND_AVGSPD)) %>% filter(WindPercent <= .50) %>% arrange(desc(WindPercent)) %>% ggplot(aes(x=HLY_HIDX_NORMAL, y=WindPercent)) + geom_point()

#left join, wrangling and visualization

dplyr::left_join(TexasWeather1, TexasWeather2, by="DATE_") %>% View
df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", '129.152.144.84:5001/rest/native/?query="select * from TexasWeather1 e left join TexasWeather2 d on e.DATE_ = d.DATE_"')),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); tbl_df(df)


df %>% filter(HLY_WIND_AVGSPD < 100) %>% select(HLY_WCHL_NORMAL, HLY_WIND_AVGSPD) %>% ggplot(aes(x=HLY_WCHL_NORMAL, y=HLY_WIND_AVGSPD)) + geom_point()


dplyr::full_join(TexasWeather1, TexasWeather2, by="DATE_") %>% View
df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", '129.152.144.84:5001/rest/native/?query=
                                                "select * 
                                                from TexasWeather1 e full outer join TexasWeather2 d
                                                on e.DATE_ = d.DATE_"
                                                ')),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); tbl_df(df)


df %>% ggplot(aes(x=HLY_WCHL_NORMAL, y=HLY_HIDX_NORMAL)) + geom_point()

####Head and Summary for table 1
summary(TexasWeather1)
head(TexasWeather1)


