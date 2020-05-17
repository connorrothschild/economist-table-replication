library(reactable)
library(htmltools)

data_for_table <- data %>%
  group_by(country) %>%
  summarise(
    excess_deaths = sum(excess_deaths),
    covid_deaths = sum(covid_deaths),
    perc = covid_deaths / excess_deaths
  )

bar_chart <-
  function(label,
           width = "100%",
           height = "16px",
           fill = "#00bfc4",
           background = NULL) {
    bar <-
      div(style = list(
        background = fill,
        width = width,
        height = height
      ))
    chart <-
      div(style = list(
        flexGrow = 1,
        marginLeft = "8px",
        background = background
      ),
      bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }

reactable(
  data_for_table,
  defaultSortOrder = 'desc',
  defaultSorted = 'excess_deaths',
  style = list(fontFamily = "Econ Sans, sans-serif", fontSize = "14px"),  
  defaultColDef = colDef(
    headerStyle = list(
      fontSize = "11px",
      lineHeight = "14px",
      textTransform = "uppercase",
      color = "#0c0c0c",
      fontWeight = "500",
      borderBottom = "2px solid #e9edf0",
      paddingBottom = "3px",
      verticalAlign = "bottom"
    )
  ), 
  columns = list(
    country = colDef(
      name = "Region / Country"
    ),
    covid_deaths = colDef(
      name = "COVID-19 Deaths",
      align = "left",
      cell = function(covid_deaths) {
        width <-
          paste0(covid_deaths / max(data_for_table$covid_deaths) * 100, "%")
        bar_chart(scales::comma(covid_deaths), width = width, fill = "#F15A3F")
      }
    ),
    excess_deaths = colDef(
      name = "Total Excess Deaths",
      align = "left",
      cell = function(excess_deaths) {
        width <-
          paste0(excess_deaths / max(data_for_table$excess_deaths) * 100, "%")
        bar_chart(scales::comma(excess_deaths), width = width, fill = '#3F5661')
      }
    ),
    perc = colDef(
      name = "COVID-19 as % of Total",
      align = "left",
      format = colFormat(percent = TRUE, digits = 0)
    )
  ),
)
  
