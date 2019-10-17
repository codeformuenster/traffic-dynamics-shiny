# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING).
# If not, see <http://www.gnu.org/licenses/>.

# TODO for next release
# - fix month for radar plot
# - link to codeformuenster impressum
# - add proper fathom tracking
# - color the update button, see crashes code
# - update docker file to point to new data docker image

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
	cap <- reactive({
	  if (input$tabs_data_models == "data") {
  	  paste0(names(locationChoices[locationChoices == input$location]),
  	         ": Anzahl ",
  	         names(vehicleChoices[vehicleChoices == input$vehicle]),
  	         " (nicht angezeigte Daten existieren leider (noch) nicht)")
	  } else {
	    paste0(names(locationChoices[locationChoices == input$location]),
	           ", Fahrräder, Werktage 2017, 7:00–8:59 Uhr, Modellschätzungen")
	  }
	})
	
	output$caption <- renderText({
	  cap()
	})
	
  observe({
		# only show filtering option relevant for the current data
  	
  	req(con)
		
  	# TODO move the following switch to global.R to have the code only once
		sql_location_cars <- 
				switch(input$location, 
						 "'Neutor'" = "'%01080%'", 
						 "'Wolbecker.Straße'" = "'%04050%'",
						 "'Hüfferstraße'" = "'%03052%'",
						 "'Hammer.Straße'" = "'%07030%'",
						 "'Promenade'" = "'%04051%'",
						 "'Gartenstraße'" = "'%04073%'",
						 "'Warendorfer.Straße'" = "'%04061%'",
						 "'Hafenstraße'" = "'%04010%'",
						 "'Weseler.Straße'" = "'%01190%'", # TODO add only proper directions for Kfz Kolde-Ring (i.e., only Weseler Str.)
						 "'Hansaring'" = "'%03290%'"
						 )
	
			dates_in_car_data <- data.frame(date = NA_character_)
			dates_in_bike_data <- data.frame(date = NA_character_)
			
			if (input$vehicle == "bikes" | input$vehicle == "both") {
				dates_in_bike_data <-
					dbGetQuery(conn = con, 
										 paste0("SELECT DISTINCT date AS date FROM bikes WHERE location LIKE ", 
										 			 input$location, " AND count != ''"))
			}
			if (input$vehicle == "cars" | input$vehicle == "both") {
				dates_in_car_data <-
					dbGetQuery(conn = con, 
										 paste0("SELECT DISTINCT date AS date FROM cars WHERE location LIKE ", 
										 			 sql_location_cars, " AND count != ''"))
			}
			
			years_in_data <- 
				sort(unique(c(as.character(year(dates_in_bike_data$date)), 
											as.character(year(dates_in_car_data$date)))))
			months_in_data <-
				sort(unique(c(as.numeric(month(dates_in_bike_data$date)), 
											as.numeric(month(dates_in_car_data$date)))))
			
			# wday() - 1  to get Sunday == 0 and Monday == 1
			wdays_in_data <-
				sort(unique(c(as.numeric(wday(dates_in_bike_data$date) - 1),
											as.numeric(wday(dates_in_car_data$date) - 1))))
			
			updateSelectizeInput(session = session,
			                   inputId = "years",
												 selected = isolate(input$years),
			                   choices = years_in_data)
			
			updateSelectizeInput(session = session,
			                   inputId = "months",
				 		             selected = isolate(input$months),
			                   choices = monthChoices[monthChoices %in% months_in_data])
			
			updateSelectizeInput(session = session,
			                   inputId = "weekdays",
				 		             selected = isolate(input$weekdays),
			                   choices = weekdayChoices[weekdayChoices %in% wdays_in_data])
			
			updateDateRangeInput(session = session,
				 		inputId = "date_range",
				 		min = min(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE),
				 		max = max(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE),
				 		start = min(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE),
				 		end = max(c(dates_in_car_data$date, dates_in_bike_data$date), na.rm  = TRUE)
				 	)
	})
	
  load_filtered_data_from_db <- eventReactive(input$QueryBtn, ignoreNULL = FALSE, {
    	start <- Sys.time()
    	
    	req(con)
    	
  		sql_table_bikes <- "bikes"
  		sql_table_cars <- "cars"
  		
  		sql_location_bikes <- input$location
  		sql_location_cars <- 
  			switch(input$location,
  						 "'Neutor'" = "'%01080%'", 
  						 "'Wolbecker.Straße'" = "'%04050%'",
  						 "'Hüfferstraße'" = "'%03052%'",
  						 "'Hammer.Straße'" = "'%07030%'",
  						 "'Promenade'" = "'%04051%'",
  						 "'Gartenstraße'" = "'%04073%'",
  						 "'Warendorfer.Straße'" = "'%04061%'",
  						 "'Hafenstraße'" = "'%04010%'",
  						 "'Weseler.Straße'" = "'%01190%'",
  						 "'Hansaring'" = "'%03290%'",
  						 # Roxel
  						 "'roxel1'" = "'%24020%'", 
  						 "'roxel2'" = "'%24100%'", 
  						 "'roxel3'" = "'%24140%'", 
  						 "'roxel4'" = "'%24010%'", 
  						 "'roxel5'" = "'%24120%'", 
  						 "'roxel6'" = "'%24130%'", 
  						 "'roxel7'" = "'%24030%'"
  						 )

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
    sql_location <- sql_location_cars
    
    if (input$vehicle == "bikes") {
    	sql_table <- sql_table_bikes
    	sql_location <- sql_location_bikes
    }
    
		sql_string <- paste0("SELECT date, hour, count, location, vehicle", 
      " FROM ", sql_table, date_filter,
			" AND location LIKE ", sql_location)
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
		
		cat(paste("\nload_filtered_data_from_db() took",
		          Sys.time() - start,
		          "seconds\n"))
		return(vehicles)
	})  # end reactive

  observe({
    dbData$d_hour <- load_filtered_data_from_db()
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
  	    )
  	}
  	cat(paste("renderYearPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
  
  output$plotDay <- renderPlotly({
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
  	      )
  	}
  	cat(paste("renderDayPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })

  aggregated_data_radar_year <- reactive({
 	  req(dbData$d_hour)
 		start <- Sys.time()
 		vehicles_radar_year <-
 		  dbData$d_hour %>%
 			group_by(month(date), vehicle) %>%
 			summarise(count_month = sum(count, na.rm = TRUE)) %>%
 		  mutate(month_character = "TODO") %>% 
 		  ungroup()
 		
 		# print(month(vehicles_radar_year$date, label = TRUE, abbr = TRUE, locale = "de_DE"))
 		
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
                          month_character = c('Dez', 'Nov', 'Okt', 'Sep', 'Aug', 'Jul',
                                    'Jun', 'Mai', 'Apr', 'Mrz', 'Feb', 'Jan'),
                         vehicle = rep("Fahrrad", 12))
    }

    if (nrow(cars) == 0) {
      cars <- data.frame(count_month = rep(NA, 12),
                         month_character = c('Dez', 'Nov', 'Okt', 'Sep', 'Aug', 'Jul',
                                   'Jun', 'Mai', 'Apr', 'Mrz', 'Feb', 'Jan'),
                         vehicle = rep("Kfz", 12))
    }

    print(bikes)
    
    p <- plot_ly(
      type = 'scatterpolar',
      mode = 'markers',
      fill = 'toself'
    ) %>% 
      add_trace(data = bikes,
        r = ~count_month,
        theta = ~month_character,#c('Dez', 'Nov', 'Okt', 'Sep', 'Aug', 'Jul',
                  #'Jun', 'Mai', 'Apr', 'Mrz', 'Feb', 'Jan'),
        name = ~vehicle,
        color = ~vehicle) %>%
      add_trace(data = cars,
        r = ~count_month,
        theta = ~month_character,#,c('Dez', 'Nov', 'Okt', 'Sep', 'Aug', 'Jul',
                 #'Jun', 'Mai', 'Apr', 'Mrz', 'Feb', 'Jan'),
        name = ~vehicle,
        color = ~vehicle) %>%
      layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0, max(bikes$count_month, cars$count_month))
        )
      )
    )
    
    return(p)
  })
  
  output$plotRadarDay <- renderPlotly({

    bikes <- aggregated_data_radar_day() %>%
      filter(vehicle == "Fahrrad")
    
    cars <- aggregated_data_radar_day() %>%
      filter(vehicle == "Kfz")

     if (nrow(bikes) == 0) {
      bikes <- data.frame(count_hour = rep(NA, 24),
                         hour_character = c('23h', '22h', '21h', '20h', '19h', '18h', '17h', '16h',
                                  '15h', '14h', '13h', '12h', '11h', '10h', '9h',
                                  '8h', '7h', '6h', '5h', '4h', '3h', '2h', '1h', '0h'),
                         vehicle = rep("Fahrrad", 24))
    }

    if (nrow(cars) == 0) {
      cars <- data.frame(count_hour = rep(NA, 24),
                         hour_character = c('23h', '22h', '21h', '20h', '19h', '18h', '17h', '16h',
                                 '15h', '14h', '13h', '12h', '11h', '10h', '9h',
                                 '8h', '7h', '6h', '5h', '4h', '3h', '2h', '1h', '0h'),
                         vehicle = rep("Kfz", 24))
    }
        p <- plot_ly(
      type = 'scatterpolar',
      mode = 'markers', 
      fill = 'toself'
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
          range = c(0, max(bikes$count_hour, cars$count_hour))
        )
      )
      )
    
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
    if(input$select_model == "simple"){
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
    
    ggplotly(p)
  })
  
  output$plotWind <- renderPlotly({
    if(input$select_model == "simple"){
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
    
    ggplotly(p)
  })
  
  output$plotRain <- renderPlotly({
    if(input$select_model == "simple"){
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
    
    ggplotly(p)
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
    
    ggplotly(p)
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

    ggplotly(p)
  })
  
  
})
