library(tidyr)        # string functions
library(tidyverse)    #tidyverse functions
library(dplyr)        #general functions

data_shmi<-  read_rds("../data/NHSD SHMI.rds")
getwd()
render_fun <-function(ProviderCode){
  rmarkdown::render(
    input= "../analysis/SHMI_trust_dashboard.rmd",
    params=list(trust= ProviderCode),
    output_file=glue::glue("../output/{ProviderCode}-report.html")
    )
}

#only produce reports for trusts with data in latest period and not England
latest_publication=max(data_shmi$PeriodEnd)

list_of_trusts<- data_shmi %>%filter(PeriodEnd==latest_publication &
                                       ProviderCode!="ENG" &!is.na(ProviderCode) ) %>% 
                                        distinct(ProviderCode)
#join to exclude 
data_shmi<-inner_join(data_shmi,list_of_trusts)

list_of_trusts%>%
  pull() %>%
  as.character()%>%
  purrr::walk(render_fun)

#list_of_trusts<-list_of_trusts[1:59,1]