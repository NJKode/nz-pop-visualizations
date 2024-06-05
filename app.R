library(shiny)
library(leaflet)
library(hash)
library(tibble)
library(sf)
library(rsconnect)
library(rmapshaper)

region_shape_data <- read_sf(
	"./data/statsnz-regional-council-2023-generalised-SHP/regional-council-2023-generalised.shp"
) %>% rmapshaper::ms_simplify()
region_boundaries <- st_transform(region_shape_data, "+proj=longlat +datum=WGS84 +no_defs")

# horribly inefficient data wrangling
population_data <- read.csv("./data/census-population-by-region.csv")
data_2013 <- subset(population_data, Year==2013)
data_2018 <- subset(population_data, Year==2018)
data_2023 <- subset(population_data, Year==2023)

data_2013 <- merge(x=data_2013, y=region_boundaries, by.x="Region", by.y="REGC2023_1") %>% sf::st_as_sf()
data_2018 <- merge(x=data_2018, y=region_boundaries, by.x="Region", by.y="REGC2023_1") %>% sf::st_as_sf()
data_2023 <- merge(x=data_2023, y=region_boundaries, by.x="Region", by.y="REGC2023_1") %>% sf::st_as_sf()

diff_13_18 <- data_2018
diff_18_23 <- data_2023

perc_13_18 <- data_2018
perc_18_23 <- data_2023

diff_13_18$Total <- data_2018$Total - data_2013$Total
diff_18_23$Total <- data_2023$Total - data_2018$Total

perc_13_18$Total <- (((data_2018$Total / data_2013$Total) - 1) * 100) %>% round(2)
perc_18_23$Total <- (((data_2023$Total / data_2018$Total) - 1) * 100) %>% round(2)

ui <- fluidPage(
	titlePanel("New Zealand population change"),
		
	sidebarLayout(
		sidebarPanel(
			radioButtons("year", "Year", 
				choices = c("2013 -> 2018" = "13_18", "2018 -> 2023" = "18_23")),
			radioButtons("format", "Show as", 
				choices = c("Real values" = "values", "Percentage change" = "percent"))
		),

		mainPanel(
			 leafletOutput("map_nz")
		)
	)
)

pal <- colorNumeric(
	palette = "Reds",
	domain = NULL
)

labels <- function(data, is_percent = FALSE) {
	sprintf(
		"<strong>%s</strong><br/>%s%s",
		data$Region, format(data$Total, big.mark=","), if(is_percent) "%" else ""
	) %>% lapply(htmltools::HTML)
}

server <- function(input, output) {
	output$map_nz <- renderLeaflet({
		leaflet() %>%
		setView(lat = -40.9, lng = 174.8, zoom = 5) %>%
		setMaxBounds(
			lat1 = -48,
			lat2 = -32,
			lng1 = 166,
			lng2 = 180
		) %>%
		addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 7))
	})

	update_visuals <- function(heatmap_data) {
		leafletProxy("map_nz") %>%
			addPolygons(
				data = heatmap_data,
				weight=1,
				opacity=1,
				fillOpacity = 0.5,
				highlight = highlightOptions(weight = 4),
				fillColor = ~pal(heatmap_data$Total),
				label = labels(heatmap_data, input$format == "percent"),
				labelOptions = labelOptions(
					style = list("font-weight" = "normal", padding = "3px 8px"),
					textsize = "15px",
					direction = "auto"),
				layerId = heatmap_data$Region
			) %>%
			addLegend(
				title = "Population increase",
				values = heatmap_data$Total,
				position = "bottomright",
				pal = pal,
				layerId = "legend"
			)
	}
		
	observeEvent(list(input$year, input$format), {
		if (input$format == "values") {
			heatmap_data <- 
			switch(input$year,
				"13_18"=diff_13_18,
				"18_23"=diff_18_23
			)
		} else {
			heatmap_data <- 
			switch(input$year,
				"13_18"=perc_13_18,
				"18_23"=perc_18_23
			)
		}
		
		update_visuals(heatmap_data)
	})
}

shinyApp(ui = ui, server = server)
