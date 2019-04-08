## ----libraries, message=FALSE--------------------------------------------
library(fingertipsR)
library(fingertipscharts)
library(dplyr)
library(tidyr)

## ----overview data, message=FALSE----------------------------------------
region <- "North East region"
top_names <- c("England", region)
dfdom <- fingertips_data(DomainID = 8000037) %>%
        group_by(IndicatorID) %>%
        filter(TimeperiodSortable == max(TimeperiodSortable) &
                       Sex == "Persons" &
                       (AreaName %in% top_names |
                                ParentName == region)) %>%
        ungroup() %>%
        mutate(Value = round(Value, 1))

## ----overview, out.width='100%', fig.width=12, fig.height=6, fig.align='center'----
p <- overview(data = dfdom, 
              area = AreaName, 
              indicator = IndicatorName, 
              value = Value,
              fill = ComparedtoEnglandvalueorpercentiles,
              timeperiod = Timeperiod,
              top_areas = top_names, wrap_length = 40,
              value_label_size = 0.7)
p

## ----compare indicators data---------------------------------------------
df <- fingertips_data(c(90362, 90366)) %>%
        group_by(IndicatorID) %>%
        filter(TimeperiodSortable == max(TimeperiodSortable),
               Sex == "Male",
               Age == "All ages") %>%
        ungroup() %>%
        select(IndicatorID, AreaName, Value) %>%
        mutate(IndicatorID = paste0("x", IndicatorID)) %>%
        spread(IndicatorID, Value)

## ----compare indicators, out.width='80%', fig.width=9, fig.height=5, fig.align='center',warning=FALSE----
p <- compare_indicators(data = df,
                        x = x90362,
                        y = x90366,
                        xlab = "Healthy life expectancy at birth",
                        ylab = "Life expectancy at birth",
                        highlight = c("England", "Dorset"),
                        area = AreaName,
                        add_R2 = TRUE)
p

## ----map get data--------------------------------------------------------
df <- fingertips_data(90366) %>%
        filter(Sex == "Male" &
                       AreaType == "County & UA" &
                       TimeperiodSortable == max(TimeperiodSortable))

ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"


## ----static reorder------------------------------------------------------
ordered_levels <- c("Better",
                    "Similar", 
                    "Worse",
                    "Not compared")
df <- df %>%
        mutate(ComparedtoEnglandvalueorpercentiles = 
                       factor(ComparedtoEnglandvalueorpercentiles,
                              levels = ordered_levels))

## ----map static, out.width='80%', fig.width=8, fig.height=8, fig.align='center', warning=FALSE----
p <- fingertipscharts::map(data = df,
                    ons_api = ons_api,
                    area_code = AreaCode,
                    fill = ComparedtoEnglandvalueorpercentiles,
                    title = "Life expectancy at birth",
                    subtitle = "Males in Upper Tier Local Authorities England",
                    copyright_size = 3)
p

## ----map interactive, out.width='80%', fig.width=8, fig.height=8, fig.align='center', warning=FALSE----
p <- map(df,
         ons_api = ons_api,
         area_code = AreaCode,
         fill = ComparedtoEnglandvalueorpercentiles,
         type = "interactive",
         value = Value,
         name_for_label = AreaName,
         title = "Life expectancy at birth<br>Males within UTLAs in England")
p

## ----trends get data-----------------------------------------------------
df <- fingertips_data(90366) %>%
        filter(Sex == "Male",
               Age == "All ages")

## ----trends, out.width='70%', fig.width=9, fig.height=5, fig.align='center'----
p <- trends(df,
            timeperiod = Timeperiod,
            value = Value,
            area = AreaName,
            comparator = "England",
            area_name = "Cambridgeshire",
            fill = ComparedtoEnglandvalueorpercentiles,
            lowerci = LowerCI95.0limit,
            upperci = UpperCI95.0limit,
            title = "Life expectancy at birth",
            subtitle = "Cambridgeshire compared to England",
            xlab = "Year",
            ylab = "Age (years)")
p

## ----get fingertips data-------------------------------------------------
region <- "South East region"
top_names <- c("England", region)
df <- fingertips_data(90316) %>%
        group_by(IndicatorID) %>%
        filter(is.na(CategoryType) &
                       TimeperiodSortable == max(TimeperiodSortable) &
                       (AreaName %in% top_names |
                                ParentName == region)) %>%
        ungroup()

## ----compare areas reorder-----------------------------------------------
ordered_levels <- c("Better",
                    "Similar", 
                    "Worse",
                    "Not compared")
df <- df %>%
        mutate(ComparedtoEnglandvalueorpercentiles = 
                       factor(ComparedtoEnglandvalueorpercentiles,
                              levels = ordered_levels)) %>%
        filter(Sex == "Persons")

## ----compare areas, out.width='80%', fig.width=9, fig.height=6, fig.align='center'----
p <- compare_areas(df, AreaName, Value,
                      fill = ComparedtoEnglandvalueorpercentiles,
                      lowerci = LowerCI95.0limit,
                      upperci = UpperCI95.0limit,
                      order = "desc",
                      top_areas = top_names,
                      title = unique(df$IndicatorName))
p

## ----area profiles data, cache=TRUE--------------------------------------
dfspine <- fingertips_data(DomainID = 1938133222, rank = TRUE) %>%
        filter(Timeperiod == "2016")

## ----area profiles, warning=FALSE, out.width='100%', fig.width=10, fig.height=4.5----
p <- area_profiles(dfspine,
                   value = Value,
                   count = Count,
                   area_code = AreaCode,
                   local_area_code = "E06000020",
                   indicator = IndicatorName,
                   timeperiod = Timeperiod,
                   polarity = Polarity,
                   significance = ComparedtoEnglandvalueorpercentiles,
                   area_type = AreaType,
                   cols = "fingertips",
                   median_line_area_code = "E92000001",
                   comparator_area_code = "E12000005",
                   datatable = TRUE,
                   header_positions = c(-1.43, -0.63, -0.43, -0.31, -0.19, -0.05, 1.08),
                   relative_domain_text_size = 0.75,
                   relative_text_size = 1.2,
                   bar_width = 0.68)
p

## ----population data-----------------------------------------------------
agelevels <- c("0-4", "5-9","10-14","15-19",
               "20-24","25-29","30-34",
               "35-39","40-44","45-49",
               "50-54","55-59","60-64",
               "65-69","70-74","75-79",
               "80-84","85-89","90+")
pops <- fingertips_data(92708) %>%
        filter(TimeperiodSortable == max(TimeperiodSortable) &
                       Sex %in% c("Male", "Female") &
                       Age != "All ages") %>%
        mutate(Age = gsub(" yrs","", Age),
               Age = factor(Age,
                            levels = agelevels)) %>%
        droplevels()

## ----population, out.width='70%', fig.width=8, fig.height=7, fig.align='center'----
p <- population(pops,
                value = Value,
                sex = Sex,
                age = Age,
                area = AreaName,
                area_name = "Nottingham",
                comparator_1 = "England",
                comparator_2 = "East Midlands region",
                title = "Age Profile",
                subtitle = paste(unique(pops$IndicatorName),
                                 unique(pops$Timeperiod)),
                xlab = "% of total population")
p

## ----boxplots data-------------------------------------------------------
df <- fingertips_data(90366) %>%
        filter(Sex == "Male",
               AreaType == "County & UA",
               Age == "All ages")

## ----boxplots, out.width='80%', fig.width=10, fig.height=7, fig.align='center'----
p <- box_plots(df,
               timeperiod = Timeperiod,
               value = Value,
               title = "Life expectancy at birth",
               subtitle = "Males in Upper Tier Local Authorities within England",
               ylab = "Age (years)")
p

