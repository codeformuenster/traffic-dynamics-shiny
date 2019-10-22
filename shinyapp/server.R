# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING).
# If not, see <http://www.gnu.org/licenses/>.

# SERVER LOGIC FOR THE SHINY APP

# load libraries ####
# use 00_install_R_packages.R for installing missing packages
lapply(c("shiny", "datasets", "RSQLite", "dplyr", "ggplot2", "lubridate", "plotly", "brms"), 
       require, character.only = TRUE)

source("global.R")

# to see stdout logs, too
sink(stderr(), type = "output")

NullPlot <- ggplotly(
  ggplot(NULL) +
    annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = "Keine Daten vorhanden. Bitte ändern Sie Ihre Auswahl.",
      hjust = 0
    ) + theme_void()
)

# Define server logic required to plot
shinyServer(function(input, output, session) {
  
  global_vars <- reactiveValues(filter_changed = FALSE, update_visible_data = TRUE)
  
  options(warn = -1) # suppress warnings
  
  dbData <- reactiveValues(d_hour = NULL, d_year = NULL)

  # load statistical models
  load("models/bike_model_simple.RData")
  
	# open connection to database
	con <- dbConnect(SQLite(), dbname = "data/database/traffic_data.sqlite")
		
	# close connection to database once shiny session ended
	session$onSessionEnded(
		function(){ 
		if (!is.null(con)) { 
			dbDisconnect(con) 
		} 
	}) 
	
	# Return the formula text for printing as a caption
	cap <- eventReactive(global_vars$update_visible_data, {
	  if (input$tabs_data_models == "data") {
  	  paste0(names(locationChoices[locationChoices == input$location]),
  	         ": Anzahl ",
  	         names(vehicleChoices[vehicleChoices == input$vehicle]))
	  } else {
	    paste0(names(locationChoices[locationChoices == input$location]),
	           ", Fahrräder, Werktage 2017, 7:00–8:59 Uhr, Modellschätzungen")
	  }
	})
	
	output$caption <- renderText({
	  cap()
	})
	
  observe({
		# only show filtering options (i.e., time range) relevant for the current data
    start <- Sys.time()
    
  	req(con)

		dates_in_car_data <- data.frame(date = NA_character_)
		dates_in_bike_data <- data.frame(date = NA_character_)
		
		if (input$vehicle == "bikes" | input$vehicle == "both") {
		  dates_in_bike_data <-
		    dbGetQuery(conn = con,
		               paste0("SELECT date FROM bikes WHERE location LIKE ",
		                      input$location, " AND count != ''"))
		}
		
		if (input$vehicle == "cars" | input$vehicle == "both") {
		  dates_in_car_data <-
		    dbGetQuery(conn = con,
		               paste0("SELECT date FROM cars WHERE location LIKE ",
		                      sql_location_cars(input$location)[1], " ESCAPE '/' AND count != ''"))
		  # this is only considering the first "fahrspur" from the car data, even if several fahrspuren are defined
		  # should be okay, as the following only looks for available data: years and min/max date
		}
		
		years_in_data <-
		  unique(c((year(dates_in_bike_data$date)),
		           (year(dates_in_car_data$date))))
		
		updateSelectizeInput(session = session,
		                     inputId = "years",
		                     selected = isolate(input$years),
		                     choices = years_in_data)
		
		first_date <- min(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE)
		last_date <- max(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE)
		
		updateDateRangeInput(session = session,
		                     inputId = "date_range",
		                     min = first_date,
              				 	 max = last_date,
              				 	 start = first_date,
              				   end = last_date
		)
		
		cat(paste("\nupdating time filters took ",
		          Sys.time() - start,
		          "seconds\n"))
	})

  load_filtered_data_from_db <- eventReactive(global_vars$update_visible_data, ignoreInit = TRUE, {
    	start <- Sys.time()
    	
    	req(con)
    	
  		sql_table_bikes <- "bikes"
  		sql_table_cars <- "cars"
  		
  		sql_location_bikes <- input$location

    	date_filter <- 
  			paste0(" WHERE hour >= ", input$hour_range[1],
  						" AND hour <= ", input$hour_range[2])
    	
    	if (input$tabs_time == "timerange") {
  	  	date_filter <- 
  	  		paste0(date_filter,
  			" AND date >= '", input$date_range[1], "'", 
  			" AND date <= '", input$date_range[2], "'")
    	} else if (input$tabs_time == "timepoints") {
    		
    		# years
    		date_filter <- 
    			paste0(date_filter,
  			" AND year IN (")
    		# construct a list with quotes
    		for (yidx in 1:length(input$years)) {
    			if (yidx != length(input$years)) {
    				date_filter <- paste0(date_filter, "'" , input$years[yidx], "',")
    			} else {
    				# no comma after last date
    				date_filter <- paste0(date_filter, "'" , input$years[yidx], "'")
    			}
    		}
    		date_filter <- paste0(date_filter, ")")
    		
    		# months
  			date_filter <- paste0(date_filter, " AND month IN (")

	    	for (midx in 1:length(input$months)) {
    			if (midx != length(input$months)) {
    				# sprintf prints leading 0 (e.g., 03 instead of 3)
    				date_filter <- paste0(date_filter, "'" , sprintf("%02d", as.numeric(input$months[midx])), "',")
    			} else {
    				# no comma after last date
    				date_filter <- paste0(date_filter, "'" , sprintf("%02d", as.numeric(input$months[midx])), "'")
    			}
    		}
    		date_filter <- paste0(date_filter, ")")
  			
    		# weekday
  			date_filter <- paste0(date_filter, " AND weekday IN (")
    		for (wdidx in 1:length(input$weekdays)) {
    			if (wdidx != length(input$weekdays)) {
    				date_filter <- paste0(date_filter, "'" , input$weekdays[wdidx], "',")
    			} else {
    				# no comma after last date
    				date_filter <- paste0(date_filter, "'" , input$weekdays[wdidx], "'")
    			}
    		}
    		date_filter <- paste0(date_filter, ")")
    	}
  	
    # default: cars
    sql_table <- sql_table_cars
    sql_location <- sql_location_cars(input$location)
    
    if (input$vehicle == "bikes") {
    	sql_table <- sql_table_bikes
    	sql_location <- sql_location_bikes
    }
    
		sql_string <- paste0("SELECT date, hour, count, location, vehicle", 
      " FROM ", sql_table, date_filter, " AND (")
		
		sql_location_string <- ""
		for (lidx in 1:length(sql_location)) {
		  sql_location_string <- paste0(sql_location_string, "location LIKE ", sql_location[lidx], " ESCAPE '/'")
		  if (lidx < length(sql_location)) {
		    sql_location_string <- paste0(sql_location_string, " OR ")
		  }
		}
		
		sql_string <- paste0(sql_string, sql_location_string, ")")
  
		if (input$vehicle == "both") {
	  	# add bikes
			sql_table = "bikes"
  		sql_location = input$location
	  	sql_string <-	paste0(sql_string, 
			" UNION SELECT date, hour, count, location, vehicle",
    	" FROM ", sql_table, date_filter,
			" AND location LIKE ", sql_location)
		}
		
		print(sql_string)
		# example string:
		# sql_string <- "SELECT date, hour, count, location, vehicle FROM bikes WHERE hour >= 0 AND hour <= 24 AND year IN ('2017') AND month IN ('01','02','03','04','05','06','07','08','09','10','11','12') AND weekday IN ('1','2','3','4','5','6','0') AND location LIKE 'Neutor'"
		
		vehicles <- dbGetQuery(conn = con, sql_string) %>% 
			mutate(date = as.POSIXct(date)) %>%
			mutate(vehicle = factor(vehicle, levels = c("bike", "car"), 
															labels = c("Fahrrad", "Kfz")))
		
		global_vars$filter_changed <- FALSE
		
		cat(paste("\nload_filtered_data_from_db() took",
		          Sys.time() - start,
		          "seconds\n"))
		
		return(vehicles)
	})  # end reactive

  observe({
    dbData$d_hour <- load_filtered_data_from_db()
  })
  
  # observe all filters and toggle the global variable to color the refresh button
  observeEvent(c(input$vehicle, input$location, input$hour_range, input$tabs_time,
                 input$date_range, input$weekdays, input$months, input$years), {
                   global_vars$filter_changed <- TRUE
                 })
  
  output$button <- renderUI({
    if (global_vars$filter_changed) {
      actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                   style = "color: white; background-color: #86D956;")
    } else {
      actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                   style = "color: black; background-color: #FFFFFF")
    }
  })
  
  # this triggers an SQL query and the display of markers and / or the heatmap
  # via global_vars$update_visible_data (but only once for every filter change)
  observeEvent(input$update_button, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (global_vars$filter_changed) {
      global_vars$update_visible_data <- !global_vars$update_visible_data
    }
  })
  
 	aggregated_data_year <- reactive({
 	  req(dbData$d_hour)
 		start <- Sys.time()
 		vehicles_year <-
 		  dbData$d_hour %>%
 			group_by(date, vehicle) %>%
 			summarise(count_day = sum(count, na.rm = TRUE)) %>% 
 		  ungroup()
	    cat(paste("aggregated_data_year() took", Sys.time() - start, "seconds\n"))

    return(vehicles_year)
  })
 	
 	observe({
 	  dbData$d_year <- aggregated_data_year()
 	})

 	output$plotYear <- renderPlotly({
 	  
 	  req(dbData$d_hour)
 	  
  	start <- Sys.time()
  	if (is.null(dbData$d_hour) || nrow(dbData$d_hour) == 0) {
  	  p <- NullPlot
  	} else {
  	  p <- plot_ly(data = dbData$d_year,
  	               x = ~date, 
  	               y = ~count_day, 
  	               type = "scattergl",
  	               mode = "lines+markers",
  	               color = ~vehicle,
  	               name = ~vehicle,
  	               hoverinfo = "text",
  	               text = ~paste0(strftime(date, format = "%d. %m. %Y"),
  	                              ", ", vehicle,
  	                              ": ", count_day)) %>%
  	    layout(xaxis = list(title = "Datum"),
  	           yaxis = list(title = "Anzahl"),
  	           legend = list(x = 0.1, y = 0.9),
  	           showlegend = TRUE
  	    ) %>% 
  	    config(plot_ly(), displayModeBar = T, collaborate = F, displaylogo = F,locale = 'de')
  	}
  	
  	cat(paste("renderYearPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
  
  output$plotDay <- renderPlotly({
    
    req(dbData$d_hour)
    
  	start <- Sys.time()
  	if (is.null(dbData$d_hour) || nrow(dbData$d_hour) == 0) {
  	  p <- NullPlot
  	} else {
  	  p <- plot_ly(data = dbData$d_hour,
  	               x = ~hour, 
  	               y = ~count,
  	               type = "box", 
  	               alpha = 0.1,
  	               color = ~vehicle,
  	               name = ~vehicle
  	  ) %>%
  	    layout(
  	      xaxis = list(title = "Stunde"),
  	      yaxis = list(title = "Anzahl"),
  	      legend = list(x = 0.1, y = 0.9),
  	      showlegend = TRUE
  	    ) %>% 
  	    config(plot_ly(), displayModeBar = T, collaborate = F, displaylogo = F,locale = 'de')
  	}
  	
  	cat(paste("renderDayPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })

  aggregated_data_radar_year <- reactive({
 	  req(dbData$d_hour)
 		start <- Sys.time()
 		vehicles_radar_year <-
 		  dbData$d_hour %>%
 		  mutate(month_character = month(date, label = TRUE, abbr = TRUE, locale = "de_DE.UTF-8")) %>% 
 			group_by(month_character, vehicle) %>%
 			summarise(count_month = sum(count, na.rm = TRUE)) %>%
 		  ungroup()
 		
	  cat(paste("aggregated_data_radar_year() took", Sys.time() - start, "seconds\n"))

    return(vehicles_radar_year)
  })
  
  aggregated_data_radar_day <- reactive({
 	  req(dbData$d_hour)
 		start <- Sys.time()
 		vehicles_radar <-
 		  dbData$d_hour %>%
 			group_by(hour, vehicle) %>%
 			summarise(count_hour = sum(count, na.rm = TRUE)) %>%
 		  mutate(hour_character = paste0(hour, "h")) %>% 
 		  ungroup()
	    cat(paste("aggregated_data_radar_day() took", Sys.time() - start, "seconds\n"))

    return(vehicles_radar)
  })
  
  output$plotRadarYear <- renderPlotly({
    
    bikes <- aggregated_data_radar_year() %>% 
      filter(vehicle == "Fahrrad")
    
    cars <- aggregated_data_radar_year() %>% 
      filter(vehicle == "Kfz")
    
    if (nrow(bikes) == 0) {
      bikes <- data.frame(count_month = rep(NA, 12),
                          month_character = c('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun',
                                              'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
                          vehicle = rep("Fahrrad", 12))
    }

    if (nrow(cars) == 0) {
      cars <- data.frame(count_month = rep(NA, 12),
                         month_character = c('Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun',
                                             'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
                         vehicle = rep("Kfz", 12))
    }

    p <- plot_ly(
      type = 'scatterpolargl',
      mode = 'markers',
      fill = 'toself'
    ) %>% 
      add_trace(data = bikes,
        r = ~count_month,
        theta = ~month_character,
        name = ~vehicle,
        color = ~vehicle) %>%
      add_trace(data = cars,
        r = ~count_month,
        theta = ~month_character,
        name = ~vehicle,
        color = ~vehicle) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            angle = 90,
            tickangle = 90,
            range = c(0, max(bikes$count_month, cars$count_month))
          ),
          angularaxis = list(
            direction = 'clockwise'
          )
        )
      ) %>% 
      config(plot_ly(), displayModeBar = T, collaborate = F, displaylogo = F,locale = 'de')
    
    return(p)
  })
  
  output$plotRadarDay <- renderPlotly({

    bikes <- aggregated_data_radar_day() %>%
      filter(vehicle == "Fahrrad")
    
    cars <- aggregated_data_radar_day() %>%
      filter(vehicle == "Kfz")

     if (nrow(bikes) == 0) {
      bikes <- data.frame(count_hour = rep(NA, 24),
                         hour_character = c('0h', '1h', '2h', '3h', '4h', '5h', '6h', '7h',
                                            '8h', '9h', '10h', '11h', '12h', '13h', '14h', '15h',
                                            '16h', '17h', '18h', '19h', '20h', '21h', '22h', '23h'),
                         vehicle = rep("Fahrrad", 24))
    }

    if (nrow(cars) == 0) {
      cars <- data.frame(count_hour = rep(NA, 24),
                         hour_character = c('0h', '1h', '2h', '3h', '4h', '5h', '6h', '7h',
                                            '8h', '9h', '10h', '11h', '12h', '13h', '14h', '15h',
                                            '16h', '17h', '18h', '19h', '20h', '21h', '22h', '23h'),
                         vehicle = rep("Kfz", 24))
    }
        
    p <- plot_ly(
      type = 'scatterpolargl',
      mode = 'markers', 
      fill = 'toself',
      direction = 'clockwise'
    ) %>% 
      add_trace(data = bikes,
                r = ~count_hour,
                theta = ~hour_character,
                name = ~vehicle,
                color = ~vehicle) %>% 
      add_trace(data = cars,
                r = ~count_hour,
                theta =  ~hour_character,
                name = ~vehicle,
                color = ~vehicle) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            angle = 90,
            tickangle = 90,
            range = c(0, max(bikes$count_hour, cars$count_hour))
          ),
          angularaxis = list(
            direction = 'clockwise'
          )
        )
      ) %>% 
      config(plot_ly(), displayModeBar = T, collaborate = F, displaylogo = F,locale = 'de')
    
    return(p)
  })
  
  output$stringCounts <-
  	renderText({
  	  
  	  if (input$tabs_data_models == "data") {
    		# TODO: dplyr this ... (some say its easier to read :P)
  
    	  req(dbData$d_year)
    	  req(dbData$d_hour)
    	  
    		all_bikes <- sum(dbData$d_year[dbData$d_year$vehicle == "Fahrrad", ]$count_day, na.rm = TRUE)
    		mean_bikes_year <- mean(dbData$d_year[dbData$d_year$vehicle == "Fahrrad", ]$count_day, na.rm = TRUE)
    		mean_bikes_hour <- mean(dbData$d_hour[dbData$d_hour$vehicle == "Fahrrad", ]$count, na.rm = TRUE)
    		
    		all_cars <- sum(dbData$d_year[dbData$d_year$vehicle == "Kfz", ]$count_day, na.rm = TRUE)
    		mean_cars_year <- mean(dbData$d_year[dbData$d_year$vehicle == "Kfz", ]$count_day, na.rm = TRUE)
    		mean_cars_hour <- mean(dbData$d_hour[dbData$d_hour$vehicle == "Kfz", ]$count, na.rm = TRUE)
    		
    		# does not work but cuts all digits
    		# TODO: why not?
    		# options(digits = 2)
    		
    		count_string <- 
    			paste0("Im gewählten Zeitraum:\nGesamtanzahl Fahrräder: ", 
    					 prettyNum(all_bikes, big.mark = " ", decimal.mark = ","), 
    					 "\nFahrräder pro Tag (Mittelwert): ",
    					 prettyNum(mean_bikes_year, big.mark = " ", decimal.mark = ","),
    					 "\nFahrräder pro Stunde (Mittelwert): ",
    					 prettyNum(mean_bikes_hour, big.mark = " ", decimal.mark = ","),
    					 "\nGesamtanzahl Kfz: ", 
    					 prettyNum(all_cars, big.mark = " ", decimal.mark = ","), 
    					 "\nKfz pro Tag (Mittelwert): ",
    					 prettyNum(mean_cars_year, big.mark = " ", decimal.mark = ","),
    					"\nKfz pro Stunde (Mittelwert): ",
    					 prettyNum(mean_cars_hour, big.mark = " ", decimal.mark = ","),
    					"\ngeschätzter CO2-Ausstoß pro Tag: ",
    					"Fahrrad: 0, Kfz: TODO",
    					"\ngeschätzter Stickstoff-Ausstoß pro Tag: ",
    					"Fahrrad: 0, Kfz: TODO",
    					"\ngeschätzter Platzverbrauch pro bewegtem Mensch am Tag: ",
    					"\n\tFahrrad: Radwegbreite [TODO] / ", mean_bikes_year,
    					"\n\tKfz: Fahrbahnbreite [TODO] /  (", mean_cars_year, " * durchschnittliche Anzahl Personen im Auto [TODO])")
    		return(count_string)
  	  }
  	})
  
  ## model plots
  
  baldPlot <- ggplotly(
    ggplot(NULL) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = "Bald™.",
        hjust = 0
      ) + theme_void()
    )
  
  output$plotTemperature <- renderPlotly({
    if (input$select_model == "simple") {
      model <- neutor_werktage_2017_bikes_simple
    } else {
      # model <- neutor_werktage_2017_bikes_complex
      return(baldPlot)
    }
    
    pdf(NULL)
    p <- plot(
        marginal_effects(model,
                       effects = "temperature",
                       plot = FALSE)
      )[[1]] + 
      xlab("Temperatur (in °C)") +
      ylab("Anzahl Fahrräder pro Stunde") +
      theme_minimal()
    
    dev.off()
    
    return(ggplotly(p))
  })
  
  output$plotWind <- renderPlotly({
    if (input$select_model == "simple") {
      model <- neutor_werktage_2017_bikes_simple
    } else {
      # model <- neutor_werktage_2017_bikes_complex
      return(baldPlot)
    }
    
    pdf(NULL)
    p <- plot(
      marginal_effects(model,
                       effects = "windspeed",
                       plot = FALSE)
    )[[1]] + 
      xlab("Windgeschwindigkeit (km/h)") +
      ylab("Anzahl Fahrräder pro Stunde") +
      theme_minimal()
    
    dev.off()
    
    return(ggplotly(p))
  })
  
  output$plotRain <- renderPlotly({
    if (input$select_model == "simple") {
      model <- neutor_werktage_2017_bikes_simple
    } else {
      # model <- neutor_werktage_2017_bikes_complex
      return(baldPlot)
    }
    
    pdf(NULL)
    p <- plot(
      marginal_effects(model,
                       effects = "rain",
                       plot = FALSE)
    )[[1]] + 
      xlab("Regen") +
      ylab("Anzahl Fahrräder pro Stunde") +
      theme_minimal()
    
    dev.off()
    
    return(ggplotly(p))
  })
  
  output$plotMonth <- renderPlotly({
    if (input$select_model == "simple") {
      model <- neutor_werktage_2017_bikes_simple
    } else {
      # model <- neutor_werktage_2017_bikes_complex
      return(baldPlot)
    }
    
    pdf(NULL)
    p <- plot(
      marginal_effects(model,
                       effects = "month",
                       plot = FALSE)
    )[[1]] + 
      xlab("Monat") +
      ylab("Anzahl Fahrräder pro Stunde") +
      theme_minimal()
    
    dev.off()
    
    return(ggplotly(p))
  })
  
  output$plotWeekday <- renderPlotly({
    if (input$select_model == "simple") {
      model <- neutor_werktage_2017_bikes_simple
    } else {
      # model <- neutor_werktage_2017_bikes_complex
      return(baldPlot)
    }
    
    pdf(NULL)
    p <- plot(
      marginal_effects(model,
                       effects = "weekday",
                       plot = FALSE)
    )[[1]] +
      xlab("Wochentag") +
      ylab("Anzahl Fahrräder pro Stunde") +
      theme_minimal()
    
    dev.off()
    
    return(ggplotly(p))
  })
  
  
})
