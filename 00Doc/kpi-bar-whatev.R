require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"

# These will be made to more resemble Tableau Parameters when we study Shiny.
KPI_Low_Max_value = 0.05    
KPI_Medium_Max_value = 0.14

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select FIXED_ACIDITY, QUALITY, CITRIC_ACID, round(sum_RESIDUAL_SUGAR) as sum_residual_sugar, kpi as ratio, wine_id, 
case
when kpi < "p1" then \\\'03 Low\\\'
when kpi < "p2" then \\\'02 Medium\\\'
else \\\'01 High\\\'
end kpi
from (select FIXED_ACIDITY, QUALITY, wine_id, 
   CITRIC_ACID, sum(RESIDUAL_SUGAR) as sum_RESIDUAL_SUGAR, 
   sum(CITRIC_ACID) / sum(RESIDUAL_SUGAR) as kpi
   from dataset
   group by FIXED_ACIDITY, QUALITY, citric_acid, wine_id)
order by QUALITY;"
')), httPHeader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

spread(df, FIXED_ACIDITY, CITRIC_ACID) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Wine Crosstab\nSUM_CITRIC_ACID, SUM_RESIDUAL_SUGAR, SUM_CITRIC_ACID / SUM_RESIDUAL_SUGAR') +
  labs(x=paste("FIXED_ACIDITY"), y=paste("QUALITY")) +
  layer(data=df, 
        mapping=aes(x=FIXED_ACIDITY, y=QUALITY, label=round(CITRIC_ACID, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=FIXED_ACIDITY, y=QUALITY, label=SUM_RESIDUAL_SUGAR), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=FIXED_ACIDITY, y=QUALITY, label=round(RATIO, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=4), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=FIXED_ACIDITY, y=QUALITY, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alPHa=0.50), 
        position=position_identity()
  )

# The following is equivalent to Windowing Story 5 Sheet 4 in "Crosstabs, KPIs, Barchart.twb"

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select QUALITY, PH, AVG_ALCOHOL, 
                                                avg(AVG_ALCOHOL) 
                                                OVER (PARTITION BY PH ) as window_AVG_ALCOHOL
                                                from (select QUALITY, PH, avg(Alcohol) AVG_ALCOHOL
                                                from dataset
                                                group by QUALITY, PH)
                                                order by PH;"
                                                ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

# df <- diamonds %>% group_by(QUALITY, PH) %>% summarize(AVG_ALCOHOL = mean(Alcohol)) %>% rename(QUALITY=QUALITY, PH=PH)
# df1 <- df %>% ungroup %>% group_by(PH) %>% summarize(WINDOW_AVG_ALCOHOL=mean(AVG_ALCOHOL))
# df <- inner_join(df, df1, by="PH")

spread(df, QUALITY, AVG_ALCOHOL) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~PH, ncol=1) +
  labs(title='Diamonds Barchart\nAVERAGE_Alcohol, WINDOW_AVG_ALCOHOL, ') +
  labs(x=paste("QUALITY"), y=paste("AVG_ALCOHOL")) +
  layer(data=df, 
        mapping=aes(x=QUALITY, y=AVG_ALCOHOL), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=df, 
        mapping=aes(x=QUALITY, y=AVG_ALCOHOL, label=round(AVG_ALCOHOL)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-0.5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=QUALITY, y=AVG_ALCOHOL, label=round(WINDOW_AVG_ALCOHOL)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=QUALITY, y=AVG_ALCOHOL, label=round(AVG_ALCOHOL - WINDOW_AVG_ALCOHOL)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(yintercept = WINDOW_AVG_ALCOHOL), 
        geom="hline",
        geom_params=list(colour="red")
  ) 
