require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"

# These will be made to more resemble Tableau Parameters when we study Shiny.
KPI_Low_Max_value = 0.05    
KPI_Medium_Max_value = 0.14

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select PH, QUALITY, sum_CITRIC_ACID, round(sum_RESIDUAL_SUGAR) as sum_sugar, kpi as ratio, 
case
when kpi < "p1" then \\\'03 Low\\\'
when kpi < "p2" then \\\'02 Medium\\\'
else \\\'01 High\\\'
end kpi
from (select PH, QUALITY, 
   sum(CITRIC_ACID) as sum_CITRIC_ACID, sum(RESIDUAL_SUGAR) as sum_RESIDUAL_SUGAR, 
   sum(CITRIC_ACID) / sum(RESIDUAL_SUGAR) as kpi
   from dataset
   group by PH, QUALITY)
order by QUALITY;"
')), httPHeader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

spread(df, PH, SUM_CITRIC_ACID) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Wine Crosstab\nSUM_CITRIC_ACID, SUM_RESIDUAL_SUGAR, SUM_CITRIC_ACID / SUM_RESIDUAL_SUGAR') +
  labs(x=paste("PH"), y=paste("QUALITY")) +
  layer(data=df, 
        mapping=aes(x=PH, y=QUALITY, label=SUM_CITRIC_ACID), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=QUALITY, label=SUM_SUGAR), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=QUALITY, label=round(RATIO, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=4), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=QUALITY, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alPHa=0.50), 
        position=position_identity()
  )

# The following is equivalent to Windowing Story 5 Sheet 4 in "Crosstabs, KPIs, Barchart.twb"

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select PH, QUALITY, avg_CITRIC_ACID, 
                                                avg(avg_CITRIC_ACID) 
                                                OVER (PARTITION BY QUALITY ) as window_avg_CITRIC_ACID
                                                from (select PH, QUALITY, avg(CITRIC_ACID) avg_CITRIC_ACID
                                                from diamonds
                                                group by PH, QUALITY)
                                                order by QUALITY;"
                                                ')), httPHeader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

# df <- diamonds %>% group_by(PH, QUALITY) %>% summarize(AVG_CITRIC_ACID = mean(CITRIC_ACID)) %>% rename(PH=PH, QUALITY=QUALITY)
# df1 <- df %>% ungroup %>% group_by(QUALITY) %>% summarize(WINDOW_AVG_CITRIC_ACID=mean(AVG_CITRIC_ACID))
# df <- inner_join(df, df1, by="QUALITY")

spread(df, PH, AVG_CITRIC_ACID) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~QUALITY, ncol=1) +
  labs(title='Diamonds Barchart\nAVERAGE_CITRIC_ACID, WINDOW_AVG_CITRIC_ACID, ') +
  labs(x=paste("PH"), y=paste("AVG_CITRIC_ACID")) +
  layer(data=df, 
        mapping=aes(x=PH, y=AVG_CITRIC_ACID), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=df, 
        mapping=aes(x=PH, y=AVG_CITRIC_ACID, label=round(AVG_CITRIC_ACID)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-0.5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=AVG_CITRIC_ACID, label=round(WINDOW_AVG_CITRIC_ACID)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=AVG_CITRIC_ACID, label=round(AVG_CITRIC_ACID - WINDOW_AVG_CITRIC_ACID)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(yintercept = WINDOW_AVG_CITRIC_ACID), 
        geom="hline",
        geom_params=list(colour="red")
  ) 
