library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(forcats)
library(gtable)
library(countrycode)
library(extrafont)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
source("styles.R")


#TODO: CONSIDER REPLACING gather() with pivot_longer()
# https://tidyr.tidyverse.org/reference/pivot_longer.html

fig_sdg1_income_histogram_poverty <- function(years = c(1990, 2013), region = "WLD") {
  df <- read_excel("inputs/sdg1/SDG1_f1_global_dist_histogram.xlsx")
  
  df <- df %>%
    filter(regioncode == region) %>%
    select(year, povertyline, people, p_label) %>%
    filter(year %in% years) %>%
    mutate(people = people * 1e6) %>%         # people cols are in millions
    mutate(p_label = recode(p_label,
      ".5-1"  = "0.5-1",
      "1-1.9" = "1-1.90",
      "1.9-4" = "1.90-4"
    ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(p_label = endashify(p_label)) %>%  # use en-dashes for display
        # fct_reorder() uses the median of as.numeric(povertyline) to order levels of 
        ## p_label descending
        # It converts .f (i.e p_label) to a ordered factor
        mutate(p_label = fct_reorder(p_label, as.numeric(povertyline)))
      
      ggplot(df, aes(p_label, people, fill = (povertyline <= 1.90))) +
        # If you want the heights of the bars to represent values in the data,
        ## use geom_col() 
        geom_col() +
        # The y-axis, 'people' is being converted to billions
        ## for more detial see billions() and label_divide().
        scale_y_continuous(labels = billions()) +
        # Simply using the passed style_atlast() object to retireve attributes (i.e style = style_atlas())
        ##
        scale_fill_manual(
          values = c(
            `FALSE` = style$colors$spot.secondary.light,    
            `TRUE` = style$colors$spot.primary
          )
        ) +
        xlab("Income or consumption per day (2011 PPP $)") +
        facet_wrap(~ year, ncol = 1) +
        style$theme() +
        style$theme_x_title()
    },
    title = "Ending extreme poverty is at the heart of the SDG agenda. Between 1990 and 2013 the number of people living below $1.90 a day fell by over 1 billion.",
    subtitle = wbg_name(indicator = "People", denom = "billions"),
    source = "Source: World Bank PovcalNet (database). http://iresearch.worldbank.org/PovcalNet/home.aspx"
  )
}

fig_sdg1_poor_population_area_chart <- function(years = c(1990, 2013)) {
  df <- read_excel("inputs/sdg1/SDG1_f2_global_dist.xls")
  
  df <- df %>%
    filter(year %in% years, regioncid != "WLD") %>%
    mutate(population = population * 1e6) %>%  # population is in millions
    mutate(poverty = hc, not_poverty = (1 - hc)) %>%
    select(year, regioncid, population, poverty, not_poverty) %>%
    rename(iso3c = regioncid)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      
      # Arrange data ascending in poor % in first year
      df <- df %>%
        # fct_reorder2() uses the last2() on year == min(years) and poverty to order levels of 
        ## iso3c descending
        ## last2() is a helper for fct_reorder2();
        ##it finds the last value of y when sorted by x.
        # It converts .f (i.e iso3c) to a ordered factor
        mutate(iso3c = fct_reorder2(iso3c, year == min(years), poverty)) %>%
        arrange(year, iso3c)
      
      # Generate rectangle bounds for this marimekko-style plot
      df <- df %>%
        group_by(year) %>%
        
        # cumsum Returns a vector whose elements are the cumulative sums,
        ## e.g cumsum(1:10)
        ## 1  3  6 10 15 21 28 36 45 55
        mutate(x_max = cumsum(population)) %>%
        mutate(x_min = x_max - population) %>%
        ungroup()
      df <- df %>%
        # Gather changes the df to long form on specific columns
        ## indicatorID is the new column which will be in long format based
        ## on columns poverty and not_poverty i.e 
        gather(indicatorID, value, c(poverty, not_poverty)) %>%
        group_by(iso3c, year) %>%
        mutate(y_max = cumsum(value)) %>%
        mutate(y_min = y_max - value) %>%
        ungroup()
      
      df <- df %>%
        mutate(indicatorID = factor(indicatorID, levels = c("poverty", "not_poverty")))
      
      df <- df %>% left_join(wbgref$regions_excl_high_income$regions) %>%
        mutate(region_iso3c = ifelse(iso3c == "OHI", "OHI", region_iso3c))
      
      p <- ggplot(df, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = indicatorID)) +
        geom_rect(color = "white", size = 0.1) +
        geom_text( # numeric labels
          data = . %>% filter(indicatorID=="poverty"),
          aes(x = x_min + (x_max - x_min) / 2, 
              y = y_max + 0.03,
              label = round(value*100,1)
          ),
          position = position_dodge(0.9),
          hjust = 0,
          size = style$gg_text_size*0.8,
          family = style$family
        ) +
        geom_text( # region labels
          data = . %>% filter(year == 2013, indicatorID == "not_poverty"),
          aes(
            x = x_min + (x_max - x_min) / 2, 
            y = y_max + 0.03,
            # Here a vector is created with c(wbgref$regions$labels,"OHI" = "Other high income")[df$region_iso3c]
            ## Create key = value pairs for the region_iso3c (e.g EAS = "East Asia & Pacific")
            label = c(wbgref$regions$labels,"OHI" = "Other high income")[region_iso3c]
          ),
          # position_dodge() is a ggplot2 function to avoid overlapping
          position = position_dodge(),
          hjust = 0,
          
          # gg_text_size is passed from style_base() to style_atlas()
          size = style$gg_text_size,
          family = style$family
        ) +
        scale_fill_manual(
          values = c(poverty = style$colors$spot.primary, not_poverty = style$colors$neutral),
          labels = c(poverty = "Poor", not_poverty = "Not Poor")
        ) +
        scale_y_continuous(
          name="Share of population (%)", 
          breaks=c(0,0.25,0.50,0.75,1.00),
          labels=c("0","25","50","75","100")
        ) +
        
        # coord_flip() is ggplot2 function which flipping x & y
        coord_flip() +
        # facet_grid() is ggplot2 function
        ## scales = "free_x" allows scale to vary for each row/facet based on x
        ## space = "free_x" allows width to vary for each row/facet based on x
        facet_grid(. ~ year, scales = "free_x", space = "free_x") +
        style$theme() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x= element_text(margin = margin(1,0,0,0,"char")),
          panel.grid.major = element_blank(),
          legend.position = c(0.13, 0.9),
          legend.justification = c(0, 1),
          legend.direction = "horizontal",
          panel.spacing.x = unit(0.05, "npc"),
          plot.margin=margin(0,7,0.5,0,"char")
        )
      
      
      # Hard left-align legend
      ## ggplot2::ggplotGrob = ggplot2::ggplot_gtable(ggplot_build(x))
      ###it's just a convenience function to save some typing.
      ### The end goal is to convert the ggplot object, which is essentially a list describing how to build the plot, into a gtable, which is a grid graphical object (grob) that can be drawn on a device. 
      ### So if what you're after is altering the output of ggplotGrob to change the layout or add graphical components to it
      ### https://stackoverflow.com/questions/35713118/how-does-ggplotgrob-work

      #Grobs are instances of gtable::gtable()
      ## A grob table captures all the information needed to layout grobs in a table structure. 
      ## It supports row and column spanning, offers some tools to automatically figure out the correct dimensions, and makes it easy to align and combine multiple tables.
      ## Each grob is put in its own viewport - grobs in the same location are not combined into one cell. 
      ## Each grob takes up the entire cell viewport so justification control is not available.
      ### revert to documentation https://www.rdocumentation.org/packages/gtable/versions/0.3.0/topics/gtable
      
      # Turn off clipping so right labels are visible
      g <- ggplotGrob(p)
      g$layout$clip[g$layout$name=="panel-2-1"] <- "off"
      g$theme <- style$theme()
      g
    },
    title = "The world's population has grown and the regional distribution of poverty has changed. Compared with 1990, there are now more poor people in Sub-Saharan Africa and fewer in South Asia and East Asia & Pacific.",
    note = "Note: Poor refers to people living on less than $1.90 a day (2011 PPP). Regional aggregates exclude certain high-income countries.",
    source = "Source: World Bank PovcalNet (database). http://iresearch.worldbank.org/PovcalNet/home.aspx"
  )
}

fig_sdg1_poor_number_map <- function(years = 2010:2013) {
  indicators = c("SI.POV.DDAY", "SP.POP.TOTL")
  
  # wbgdata is a wrapper for the wbstats::wb function.
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    year=years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    ## Just using data from inputs and not calling databank API
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg1_poor_number_map.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup
  
  df$count <- df$SI.POV.DDAY/100 * df$SP.POP.TOTL
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      breaks = c(10e+06, 50e+06,200e+06)
      
      # wbg_bubble_map() is a fuunction from wbggeo 
      ## https://github.com/worldbank/wbgviz/blob/56699d212879d50d07c325fc06f2f768bd39a3fb/wbggeo/R/demo.R
      wbg_bubble_map(df, wbgmaps[[quality]], style, "count", 
                     breaks, 
                     max_size = 0.8, 
                     labels = millions())
      
    }, 
    aspect_ratio = 1.5, 
    title = "Populous countries such as China, India, Indonesia, and Bangladesh are home to a significant share of the total number of people living in extreme poverty.", 
    subtitle = wbg_name(indicator="Number of people living on less than $1.90 a day (2011 PPP)",mrv=years,denom="millions"), 
    source_url = "Source: World Bank PovcalNet. World Development Indicators (SI.POV.DDAY; SP.POP.TOTL)"
  )
}

fig_sdg1_pov_national_ur <- function(years = 2010:2015){
  indicators <- c(
    "SI.POV.RUHC",
    "SI.POV.URHC",
    "SI.POV.DDAY",
    "SI.POV.NAHC"
  )
  
  df <- wbgdata(
    wbgref$countries$iso3c, 
    indicators, years = years,
    # indicator.wide	returns the indicators in wide (not long) format
    ## i.e each indicator is a column
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg1_pov_national_ur.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      
      p.national <- ggplot(
        df %>% mutate(panel=paste0("Poverty rate at national poverty lines")),
        aes(SI.POV.DDAY,SI.POV.NAHC, region_iso3c)
      ) +
        # geom_abline() is a ggplot2 function.
        ##These geoms add reference lines (sometimes called rules) to a plot,
        ##either horizontal, vertical, or diagonal (specified by slope and intercept).
        geom_abline(intercept = 0, color = style$colors$reference, linetype=style$linetypes$reference) +
        geom_point(alpha = 0.9, aes(color = region_iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke)+
        scale_color_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0, 80)) +
        scale_x_continuous(limits = c(0, 80)) +
        facet_grid(~ panel) +
        xlab("Poverty rate at $1.90 a day (2011 PPP)") +
        style$theme() +
        theme(
          panel.grid.major.x = NULL,
          legend.position = c(1, 1.23),
          legend.direction="horizontal",
          legend.box = "horizontal",
          plot.margin=margin(10,2,3,0,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        ) +
        style$theme_scatter()
      
      p.ur <- ggplot(
        df %>% mutate(panel=paste0("Rural poverty rate at national poverty lines")),
        aes(SI.POV.URHC,SI.POV.RUHC, region_iso3c)
      ) +
        geom_abline(intercept = 0, color = style$colors$reference, linetype=style$linetypes$reference) +
        geom_point(alpha = 0.9, aes(color = region_iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke)+
        scale_color_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0, 80)) +
        scale_x_continuous(limits = c(0, 80)) +
        facet_grid(~ panel) +
        xlab("Urban poverty rate at national poverty lines") +
        style$theme() +
        theme(
          panel.grid.major.x = NULL,
          plot.margin=margin(10,0,3,2,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        ) +
        style$theme_scatter()
      
      pt.national <- ggplotGrob(p.national)
      pt.ur <- ggplotGrob(p.ur)
      
      # gtable_row() is a function from gtable package.
      ##It puts grobs in a list side-by-side in a single-row gtable from 
      ##left to right witrh the given widths and height.
      
      # unit() is a function from the grid package.
      ## Tt creates a unit object — a vector of unit values. 
      ## A unit value is typically just a single numeric value with an associated unit.
      chart <- gtable_row("chart", list(pt.national,pt.ur), height = unit(1, "null"), widths = unit(c(1,1),"null"))
      chart$theme <- style$theme()
      chart  
      
    },
    aspect_ratio = 1,
    title = "Poverty rates at national poverty lines are generally higher than at the international $1.90 a day line, and they are higher in rural areas than in urban areas.",
    subtitle = wbg_name(indicator="Poverty headcount ratio", mrv=years, denom="% of population"),
    source = paste("Source: World Bank PovcalNet. World Development Indicators (SI.POV.DDAY; SI.POV.NAHC; SI.POV.RUHC; SI.POV.URHC).")
  )
  
}

fig_sdg1_sp_quintile_targeting <- function() {
  # read_excel() is a function from readxl
  ## range  = A cell range to read from.
  ##Includes typical Excel ranges like "B3:D87", 
  ##possibly including the sheet name like "Budget!B2:G14"
  df <- read_excel(
    "inputs/sdg1/income group poorest quintile and total social protection.xlsx",
    range = "A1:D6"
  )
  
  df <- df %>%
    # Makes dataframe long using the `Total population` and `Poorest quintile` columns
    gather(indicatorID, value, -c(countrycode, Group)) %>%
    rename("iso3c" = countrycode) %>%
    select(-Group)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
    
      df <- df %>%
        mutate(
          # fct_reorder() uses the median of -value to order levels of 
          ## indicatorID descending
          # It converts .f (i.e indicatorID) to a ordered factor
          indicatorID = fct_reorder(indicatorID, -value),
          iso3c = factor(iso3c, levels = c("LIC", "LMC", "UMC", "HIC", "WLD"))
        )
      
      ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        # geom_col() is a ggplot2 function which creates a bar chart
        ## The heights of the bars  represent values in the data
        geom_col(position = "bullet", width=.8) +
        scale_fill_manual(
          values=c(
            `Poorest quintile` = style$colors$spot.primary,
            `Total population` = style$colors$spot.secondary.light
          )
        ) +
        scale_x_discrete(labels = wbgref$all_geo$labels) +
        # scale_y_continuous() is a ggplot2 function.
        ##Vector of range expansion constants used to add some padding around the data,
        ##to ensure that they are placed some distance away from the axes.
        scale_y_continuous(expand=c(0,0),limits=c(0,102))+
        coord_flip()+
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") 
    },
    title = "Richer countries have more comprehensive social protection programs. Within countries, the poorest are more likely to be covered by such programs, but identifying and targeting support towards the poor remains challenging.",
    subtitle = wbg_name(indicator = "Share of population covered by any social protection and labor program, most recent survey in 2008-16", denom = "%"),
    note = "Note: Calculated using simple averages of country-level coverage rates across income groups. Actual coverage may be higher as not all programs are captured by household surveys in some countries. Poorest quintile is calculated using pre-transfer welfare (income or consumption) per capita.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_sp_program_spending <- function() {
  df <- read_excel("inputs/sdg1/regions by type of instrument share.xlsx", range = "A1:F7")
  
  df <- df %>%
    # Makes dataframe long using the indicator columns (e.g Cash (UCT, CCT, Social pensions) ) columns
    gather(indicatorID, value, -c(regioncode, Region)) %>%
    rename("iso3c" = regioncode) %>%
    select(-Region)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(
          # fct_reorder() uses the median of -value to order levels of 
          ## indicatorID descending
          # It converts .f (i.e indicatorID) to a ordered factor
          indicatorID = fct_reorder(indicatorID, -value),
          # fct_reorder2() uses the last2() on indicatorID == "Cash (UCT, CCT, Social pensions)" and -value to order levels of 
          ## iso3c descending
          ## last2() is a helper for fct_reorder2();
          ##it finds the last value of y when sorted by x.
          # It converts .f (i.e iso3c) to a ordered factor
          iso3c = fct_reorder2(iso3c, indicatorID == "Cash (UCT, CCT, Social pensions)", -value),
          value = value * 100
        ) %>%
        mutate(
          # recode() is dplyr function
          ## It simply substitutes certain column values
          ## In this case it is substituting indicatorID values
          indicatorID = recode(
            indicatorID, 
            "Cash (UCT, CCT, Social pensions)" = "Cash-based",
            "In-kind (school feeding, fee waivers, in-kind transfers)" = "In-kind"
          )
        )
      
      ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values = style$colors$categorical) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(expand=c(0,0), limits=c(0,102)) +
        coord_flip() +
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart()
      
    },
    title = "The most common social protection programs in every region are cash-based.",
    subtitle = wbg_name(indicator = "Share of spending on the social safety net, by program", denom = "%"),
    note = "Note: Based on administrative data. Cash-based programs include universal cash transfers, conditional cash transfers, and social pensions. In-kind programs include school feeding, fee waivers and other in-kind transfers.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_sp_cash_transfer <- function() {
  df <- read_excel("inputs/sdg1/instrument by quintile share.xlsx", range = "A1:F9")
  
  df <- df %>%
    gather(quintile, value, -Program) %>%
    rename("indicatorID" = Program)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      # fct_reorder2() uses the last2() on quintile == "Q1" and -value to order levels of 
      ## indicatorID descending
      ## last2() is a helper for fct_reorder2();
      ##it finds the last value of y when sorted by x
      # It converts .f (i.e indicatorID) to a ordered factor
      df <- df %>% mutate(
        indicatorID = fct_reorder2(indicatorID, quintile == "Q1", -value)
      )
      
      ggplot(df, aes(x = indicatorID, y = value, fill = quintile)) +
        
        #position_stack() is a ggplot2 function
        ## reverse = If TRUE, will reverse the default stacking order. 
        ##This is useful if you're rotating both the plot and legend.
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(
          palette = style$colors$continuous,
          # Here the labels are being assigned to specific values
          ## for quintile column
          labels = c("Q1" = "Q1 (Poorest)", "Q5" = "Q5 (Richest)")
        ) +
        scale_y_continuous(expand=c(0,0), limits=c(0,102)) +
        coord_flip() +
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart()
      
    },
    title = "Cash transfer programs are the most likely to be directed toward the poor.",
    subtitle = wbg_name(indicator = "Share of social security programs benefitting each population quintile, most recent survey in 2008-16", denom = "%"),
    note = "Note: Calculated using simple averages of country-level coverage rates across regions. Poorest quintile is calculated using pre-transfer welfare (income or consumption) per capita.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_map_cadaster_etc <- function(year = 2017) {
  geographic <- read_excel("./inputs/sdg1/registering_property.xlsx", sheet = "geographic_coverage") 
  geographic <- geographic %>% rename("country" = "Economy")
  
  infrastructure <- read_excel("./inputs/sdg1/registering_property.xlsx", sheet = "reliability_infrastructure")
  infrastructure <- infrastructure %>% rename("country" = "Economy")
  
  df <- geographic %>%
    # full_join() is dplyr function
    ## returns all rows and all columns from both x and y. 
    ## Where there are not matching values, returns NA for the one missing.
    full_join(infrastructure, by = "country") %>%
    select(country, 
           #TODO: consider using same syntax for column mapping (i.e either `` or ""), for consistency's sake
           # Creating a 'immovable' column based on column 
           ##  "Are all privately held land plots in the largest business city formally registered at the immovable property registry?"
           ## from geographic dataframe
           immovable = "Are all privately held land plots in the largest business city formally registered at the immovable property registry?",
           
           # Creating a 'mapped' column based on column 
           ## "Are all privately held land plots in the largest business city mapped?"
           ## from geographic dataframe
           mapped = "Are all privately held land plots in the largest business city mapped?",
           
           # Creating a 'format' column based on column 
           ## `In what format are the majority of maps of land plots kept in the largest business city—in a paper format or in a computerized format (scanned or fully digital)?`
           ## from infrastructure dataframe
           format = `In what format are the majority of maps of land plots kept in the largest business city—in a paper format or in a computerized format (scanned or fully digital)?`,
           
           # Creating a 'database' column based on column 
           ## `Is the information recorded by the immovable property registration agency and the cadastral or mapping agency kept in a single database, in different but linked databases or in separate databases?`
           ## from infrastructure dataframe
           database = `Is the information recorded by the immovable property registration agency and the cadastral or mapping agency kept in a single database, in different but linked databases or in separate databases?`
    ) %>%
    mutate(format = ifelse(format == "Computer/Fully digital", "Yes", "No"),
           database = ifelse(database == "Separate databases", "No", "Yes")) %>%
    gather(indicatorID, value, c(immovable, mapped, format, database)) %>%
    mutate(coded = ifelse(value == "Yes", 1, 0))  %>%
    
    # Here the rows are grouped according to 'country' coulumn using dplyr::group_by()
    ## An aggregate calculation is carried out in the dplyr::summarize() for calculating the
    ## of of the coded for each group (i.e each country).
    group_by(country) %>%
    summarize(total = sum(coded)) %>%
    
    # countrycode::countrycode() is used to map the 'country' column values to 
    ##iso3c, along with a custom match for Kosovo
    mutate(iso3c = countrycode(country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))) %>%
    
    #merge() is a base function
    ## simply merging the two dataframes
    ## all.y param means:
    ###logical; if TRUE, then extra rows will be added to the output, one for each row in y that has no matching row in x. 
    ###These rows will have NAs in those columns that are usually filled with values from x. 
    merge(data.frame("iso3c" = wbgref$countries$iso3c), by = "iso3c", all.y = TRUE) %>%
    
    # Again using dplyr::group_by() and dplyr::summarize()
    ## to perform aggregate max calculation for total
    ### create new column 'value'
    group_by(iso3c) %>%
    summarize(value = max(total))
  
  df$bins <- as.character(as.integer(df$value))
  
  #TODO:HERE
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      
      # wbggeo::wbg_choropleth()
      ## https://github.com/worldbank/wbgviz/blob/56699d212879d50d07c325fc06f2f768bd39a3fb/wbggeo/R/demo.R
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins",legend.nrow = 1) 
      g$theme <- style$theme()
      g
    },
    title="Land rights provide security of tenure and are important for reducing poverty. But many countries lack comprehensive land registries that record ownership.",
    subtitle = wbg_name(indicator = "Number of components related to property registration from Doing Business Index", denom = "0-4, higher is better"),
    source = paste0("Source: ", "World Bank Doing Business (database). http://www.doingbusiness.org")
  )
}

fig_sdg1_tenure_vs_rights <- function(years = 2010:2015) {
  df <- read_excel("inputs/sdg1/tenure_vs_rights.xlsx",range = "A2:C11")
  
  df <- df %>%
    #dplyr::rename() used to rename columns
    ## "new_name" = `old_name`
    rename("tenure_insecurity" = `Share of households who perceived tenure insecurity (%), 2010 - 2015`,
           "formal_document" = `Share of households that own formally documented agricultural land (%), 2010 - 2015`) %>%
    # countrycode::countrycode() is used to map the 'country' column values to
    ##iso3c
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
    
    #TODO: consider using column names as vector as done above with gather()
    # for consistency sake
    gather(indicatorID, value, 2:3)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(
          # fct_reorder2() uses the last2() on indicatorID == "format_document" and -value to order levels of 
          ## indicatorID descending
          ## last2() is a helper for fct_reorder2();
          ##it finds the last value of y when sorted by x
          # It converts .f (i.e iso3c) to a ordered factor
          iso3c = fct_reorder2(iso3c, indicatorID == "format_document", -value),
          indicatorID = factor(indicatorID, levels = c("tenure_insecurity", "formal_document"))
        )
      
      p <- ggplot(df, aes(x = iso3c, y = value, fill = indicatorID, order = indicatorID)) +
        # ggplot2::geom_col() used the position 'bullet' so the bar for each y (indicatorID) 
        ## will consist of an inner bar (tenure_insecurity) and an outer bar (formal_document).
        ## Here is an example https://stackoverflow.com/questions/25319767/create-with-ggplot2-a-barplot-with-bar-bullet-in-it?rq=1
        geom_col(position = "bullet") +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand=c(0,0), limits = c(0, 102)) +
        scale_fill_manual(
          values = c(
            tenure_insecurity = style$colors$spot.primary,
            formal_document = style$colors$spot.secondary.light
          ),
          labels = c(
            tenure_insecurity = "Perceived tenure insecurity",
            formal_document = "Own formally documented agricultural land"
          )
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("topleft") +
        theme(legend.direction = "vertical")
      
      # Hard left-align legend
      g <- ggplotGrob(p)
      
      # The layout details are stored in a data frame with one row for each grob, and columns
      ## l left extent of grob
      ## name, a character vector used to name each grob and its viewport
      
      ## We're shifting 'guide-box' left by 1 unit
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title="People with documented ownership of land and property feel more secure.",
    note="Note: Data from a study covering selected countries.",
    subtitle = wbg_name(indicator = "Share of households", mrv = years, denom = "%"),
    source = "Source: G. Carletto, K. Deininger, T. Hilhorst, and W. Zakout (2018)."
  )
}

fig_sdg1_females_land_titles <- function(years = 2001:2015) {
  df <- read_excel("./inputs/sdg1/female_land_title.xlsx", range = "A1:D19")
  
  df <- df %>%
    rename("country" = `2001 -`) %>%
    gather(indicatorID, value, 2:4) %>%
    mutate(value = as.numeric(gsub("%", "", value)) * 100,
           iso3c = countrycode(country, "country.name", "iso3c"))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        # fct_reorder2() uses the last2() on indicatorID == "Formal document with female included on title" and value to order levels of 
        ## indicatorID descending
        ## last2() is a helper for fct_reorder2();
        ##it finds the last value of y when sorted by x
        # It converts .f (i.e iso3c) to a ordered factor
        iso3c = fct_reorder2(iso3c, indicatorID == "Formal document with female included on title", value),
        indicatorID = factor(indicatorID, levels = c(
          "No formal document",
          "Formal document with no female on title",
          "Formal document with female included on title"
        ))
      )
      
      p <- ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col() +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand=c(0,0),limits=c(0,102))+
        scale_fill_manual(
          values = c(
            style$colors$spot.secondary.light,
            style$colors$spot.secondary,
            style$colors$spot.primary
          ),
          labels= c(
            "No formal land title",
            "No female on land title",
            "Female included on land title"
          ),
          guide = guide_legend(reverse = TRUE)
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(legend.direction = "vertical")
      
      # Hard-left align legend
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title = "In some countries, few women are documented on formal land titles.",
    note = "Note: Data from a study covering selected countries.",
    subtitle = wbg_name(indicator = "Share of households that own agricultural land or houses", mrv = years, denom = "%"),
    source = paste0("Source: G. Carletto, K. Deininger, T. Hilhorst, and W. Zakout (2018).")
  )
}

#make_all(path = "docs/sdg1/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg1", styler = style_atlas, saver = figure_save_draft_png) {
  #page 1
  saver(fig_sdg1_income_histogram_poverty(), styler, file.path(path, "fig_sdg1_income_histogram_poverty.png"), width = 5.5, height = 3.5)
  saver(fig_sdg1_poor_population_area_chart(), styler, file.path(path, "fig_sdg1_poor_population_area_chart.png"), width = 5.5, height = 3.5)

  #page 2
  saver(fig_sdg1_poor_number_map(), styler, file.path(path, "fig_sdg1_poor_number_map.png"), width = 5.5, height = 4.5)
  saver(fig_sdg1_pov_national_ur(), styler, file.path(path, "fig_sdg1_pov_national_ur.png"), width = 5.5, height = 4)

  #page 3
  saver(fig_sdg1_sp_quintile_targeting(), styler, file.path(path, "fig_sdg1_sp_quintile_targeting.png"), width = 5.5, height = 2.6)
  saver(fig_sdg1_sp_program_spending(), styler, file.path(path, "fig_sdg1_sp_program_spending.png"), width = 5.5, height = 2.75)
  saver(fig_sdg1_sp_cash_transfer(), styler, file.path(path, "fig_sdg1_sp_cash_transfer.png"), width = 5.5, height = 3.25)

  #page 4
  saver(fig_sdg1_map_cadaster_etc(), styler, file.path(path, "fig_sdg1_map_cadaster_etc.png"), width = 5.5, height = 4.5)
  saver(fig_sdg1_tenure_vs_rights(), styler, file.path(path, "fig_sdg1_tenure_vs_rights.png"), width = 2.8, height = 4)
  saver(fig_sdg1_females_land_titles(), styler, file.path(path, "fig_sdg1_females_land_titles.png"), width = 2.7, height = 4)
}
