## ----libraries, message=FALSE-------------------------------------------------
library(fingertipscharts)
library(dplyr)
library(tidyr)

## ----overview data, message=FALSE---------------------------------------------
region <- "PAC19"
top_names <- c("C001", region)
dfdom <- create_test_data() %>%
        filter((AreaCode %in% top_names |
                                ParentAreaCode == region)) %>%
        mutate(Value = round(Value, 1))

## ----overview, out.width='100%', fig.width=12, fig.height=5, fig.align='center', warning=FALSE----
p <- overview(data = dfdom, 
              area = AreaCode, 
              indicator = IndicatorName, 
              value = Value,
              fill = Significance,
              timeperiod = Timeperiod,
              top_areas = top_names, wrap_length = 40,
              value_label_size = 0.7)
p

## ----compare indicators data--------------------------------------------------
df <- create_test_data() %>%
  filter(IndicatorName %in% c("Indicator 1", "Indicator 2")) %>%
  select(IndicatorName, AreaCode, Value) %>%
  mutate(IndicatorName = gsub(" ", "", IndicatorName)) %>%
  pivot_wider(names_from = IndicatorName,
              values_from = Value)

## ----compare indicators, out.width='80%', fig.width=9, fig.height=5, fig.align='center',warning=FALSE----
p <- compare_indicators(data = df,
                        x = Indicator1,
                        y = Indicator2,
                        xlab = "This is indicator 1",
                        ylab = "This is indicator 2",
                        highlight = c("C001", "AC101"),
                        area = AreaCode,
                        add_R2 = TRUE)
p

## ----map get data-------------------------------------------------------------
ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"


## ----static reorder-----------------------------------------------------------
ordered_levels <- c("Better",
                    "Similar", 
                    "Worse",
                    "Not compared")
df <- mapdata %>%
        mutate(Significance = 
                       factor(Significance,
                              levels = ordered_levels))

## ----map static, out.width='80%', fig.width=8, fig.height=8, fig.align='center', warning=FALSE----
p <- fingertipscharts::map(data = df,
                    ons_api = ons_api,
                    area_code = AreaCode,
                    fill = Significance,
                    title = "Map title",
                    subtitle = "Map subtitle",
                    copyright_size = 3)
p

## ----map interactive, out.width='80%', fig.width=8, fig.height=8, fig.align='center', warning=FALSE----
p <- map(df,
         ons_api = ons_api,
         area_code = AreaCode,
         fill = Significance,
         type = "interactive",
         value = Value,
         name_for_label = AreaName,
         title = "Map title<br>with a line break")
p

## ----trends get data----------------------------------------------------------
df <- create_test_data() %>%
        arrange(IndicatorName) %>%
        mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                each = 111))
country_val <- df %>%
  filter(AreaCode == "C001") %>%
  select(Timeperiod, Country_val = Value)

df <- df %>%
  left_join(country_val, by = "Timeperiod") %>%
  mutate(Significance = case_when(
    LCI > Country_val ~ "Higher",
    UCI < Country_val ~ "Lower",
    TRUE ~ "Similar"
  ))


## ----trends, out.width='70%', fig.width=9, fig.height=5, fig.align='center'----
p <- trends(df,
            timeperiod = Timeperiod,
            value = Value,
            area = AreaCode,
            comparator = "C001",
            area_name = "AC103",
            fill = Significance,
            lowerci = LCI,
            upperci = UCI,
            title = "Title of graph",
            subtitle = "AC103 compared to C001",
            xlab = "Year",
            ylab = "Unit of measurement")
p

## ----get fingertips data------------------------------------------------------
region <- "PAC10"
top_names <- c("C001", region)
df <- create_test_data() %>%
  filter(IndicatorName == "Indicator 3",
         (ParentAreaCode == region |
            AreaCode %in% top_names))

## ----compare areas reorder----------------------------------------------------
ordered_levels <- c("Better",
                    "Similar", 
                    "Worse",
                    "Not compared")
df <- df %>%
        mutate(Significance = 
                       factor(Significance,
                              levels = ordered_levels))

## ----compare areas, out.width='80%', fig.width=9, fig.height=6, fig.align='center'----
p <- compare_areas(df,
                   AreaCode, 
                   Value,
                      fill = Significance,
                      lowerci = LCI,
                      upperci = UCI,
                      order = "desc",
                      top_areas = top_names,
                      title = unique(df$IndicatorName))
p

## ----area profiles data, cache=TRUE-------------------------------------------
dfspine <- create_test_data()

## ----area profiles, warning=FALSE, out.width='100%', fig.width=10, fig.height=2.5----
p <- area_profiles(dfspine,
                   value = Value,
                   count = Count,
                   area_code = AreaCode,
                   local_area_code = "AC110",
                   indicator = IndicatorName,
                   timeperiod = Timeperiod,
                   trend = Trend,
                   polarity = Polarity,
                   significance = Significance,
                   area_type = AreaType,
                   cols = "fingertips",
                   median_line_area_code = "C001",
                   comparator_area_code = "PAC11",
                   datatable = TRUE,
                   header_positions = c(-1.2, -0.9, -0.63, 
                                        -0.48, -0.36, -0.21,
                                        -0.05, 1.08),
                   relative_domain_text_size = 0.75,
                   relative_text_size = 1.2,
                   bar_width = 0.68,
                   horizontal_arrow_multiplier = 0.7)
p


## ----population data----------------------------------------------------------
set.seed(1234)
agelevels <- c("0-4", "5-9","10-14","15-19",
               "20-24","25-29","30-34",
               "35-39","40-44","45-49",
               "50-54","55-59","60-64",
               "65-69","70-74","75-79",
               "80-84","85-89","90+")
local_males <- dnorm(seq(1, 19), mean = 5, sd = 5) * 1e5
local_females <- dnorm(seq(1, 19), mean = 4, sd = 5) * 1e5
region_males <- dnorm(seq(1, 19), mean = 5, sd = 7) * 1e5
region_females <- dnorm(seq(1, 19), mean = 4.5, sd = 6.5) * 1e5
country_males <- dnorm(seq(1, 19), mean = 4, sd = 3) * 1e5
country_females <- dnorm(seq(1, 19), mean = 5, sd = 3) * 1e5

pops <- data.frame(Value = c(local_males,
                             local_females,
                             region_males,
                             region_females,
                             country_males,
                             country_females),
                   AreaName = c(rep("Local", 38),
                                rep("Region", 38),
                                rep("Country", 38)),
                   Sex = rep(c(rep("Male", 19),
                              rep("Female", 19)),
                             times = 3),
                   stringsAsFactors = FALSE) %>%
  mutate(Age = factor(rep(agelevels, 6),
                          levels = agelevels))



## ----population, out.width='70%', fig.width=8, fig.height=7, fig.align='center'----
p <- population(pops,
                value = Value,
                sex = Sex,
                age = Age,
                area = AreaName,
                area_name = "Local",
                comparator_1 = "Country",
                comparator_2 = "Region",
                title = "Age Profile",
                subtitle = "Area population for this time period",
                xlab = "% of total population")
p

## ----boxplots data------------------------------------------------------------
df <- create_test_data() %>%
        arrange(IndicatorName) %>%
        mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                each = 111))

## ----boxplots, out.width='80%', fig.width=10, fig.height=7, fig.align='center'----
p <- box_plots(df,
               timeperiod = Timeperiod,
               value = Value,
               title = "Box plot for indicator",
               subtitle = "Add a subtitle here",
               ylab = "Unit of measurement")
p

