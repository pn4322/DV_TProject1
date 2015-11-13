require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"

# These will be made to more resemble Tableau Parameters when we study Shiny.
KPI_Low_Max_value = 0.05    
KPI_Medium_Max_value = 0.14

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select PH, QUALITY, sum_CITRIC_ACID, round(sum_RESIDUAL_SUGAR) as sum_residual_sugar, kpi as ratio, 
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

spread(df, PH, sum_CITRIC_ACID) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Wine Crosstab\nSUM_CITRIC_ACID, SUM_RESIDUAL_SUGAR, SUM_CITRIC_ACID / SUM_RESIDUAL_SUGAR') +
  labs(x=paste("PH"), y=paste("QUALITY")) +
  layer(data=df, 
        mapping=aes(x=round(PH,1), y=QUALITY, label=round(SUM_CITRIC_ACID, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=PH, y=QUALITY, label=SUM_RESIDUAL_SUGAR), 
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
        geom_params=list(alpha=0.5), 
        position=position_identity()
  )