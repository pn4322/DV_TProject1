require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# The following is equivalent to KPI Story 2 Sheet 2 and Parameters Story 3 in "Crosstabs, KPIs, Barchart.twb"

# These will be made to more resemble Tableau Parameters when we study Shiny.
KPI_Low_Max_value = 0.05    
KPI_Medium_Max_value = 0.14

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select PH, quality, sum_citric_acid, round(sum_residual_sugar) as sum_sugar, kpi as ratio, 
case
when kpi < "p1" then \\\'03 Low\\\'
when kpi < "p2" then \\\'02 Medium\\\'
else \\\'01 High\\\'
end kpi
from (select ph, quality, 
   sum(citric_acid) as sum_citric_acid, sum(residual_sugar) as sum_residual_sugar, 
   sum(citric_acid) / sum(residual_sugar) as kpi
   from dataset
   group by ph, quality)
order by quality;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

# df <- DataSet %>% group_by(color, clarity) %>% summarize(sum_citric_acid = sum(citric_acid), sum_residual_sugar = sum(residual_sugar)) %>% mutate(ratio = sum_citric_acid / sum_residual_sugar) %>% mutate(kpi = ifelse(ratio <= KPI_Low_Max_value, '03 Low', ifelse(ratio <= KPI_Medium_Max_value, '02 Medium', '01 High'))) %>% rename(COLOR=color, CLARITY=clarity, SUM_citric_acid=sum_citric_acid, SUM_residual_sugar=sum_residual_sugar, RATIO=ratio, KPI=kpi)

spread(df, COLOR, SUM_citric_acid) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Diamonds Crosstab\nSUM_citric_acid, SUM_residual_sugar, SUM_citric_acid / SUM_residual_sugar') +
  labs(x=paste("COLOR"), y=paste("CLARITY")) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=CLARITY, label=SUM_citric_acid), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=CLARITY, label=SUM_residual_sugar), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=CLARITY, label=round(RATIO, 2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", vjust=4), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=CLARITY, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  )

# The following is equivalent to Windowing Story 5 Sheet 4 in "Crosstabs, KPIs, Barchart.twb"

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select color, clarity, avg_citric_acid, 
avg(avg_citric_acid) 
OVER (PARTITION BY clarity ) as window_avg_citric_acid
from (select color, clarity, avg(citric_acid) avg_citric_acid
   from diamonds
   group by color, clarity)
order by clarity;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pn4322', PASS='orcl_pn4322', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

# df <- diamonds %>% group_by(color, clarity) %>% summarize(AVG_citric_acid = mean(citric_acid)) %>% rename(COLOR=color, CLARITY=clarity)
# df1 <- df %>% ungroup %>% group_by(CLARITY) %>% summarize(WINDOW_AVG_citric_acid=mean(AVG_citric_acid))
# df <- inner_join(df, df1, by="CLARITY")

spread(df, COLOR, AVG_citric_acid) %>% View

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~CLARITY, ncol=1) +
  labs(title='Diamonds Barchart\nAVERAGE_citric_acid, WINDOW_AVG_citric_acid, ') +
  labs(x=paste("COLOR"), y=paste("AVG_citric_acid")) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=AVG_citric_acid), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=df, 
        mapping=aes(x=COLOR, y=AVG_citric_acid, label=round(AVG_citric_acid)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-0.5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=AVG_citric_acid, label=round(WINDOW_AVG_citric_acid)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-2), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=COLOR, y=AVG_citric_acid, label=round(AVG_citric_acid - WINDOW_AVG_citric_acid)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=-5), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(yintercept = WINDOW_AVG_citric_acid), 
        geom="hline",
        geom_params=list(colour="red")
  ) 
