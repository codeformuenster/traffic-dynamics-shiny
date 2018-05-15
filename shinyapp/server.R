# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING).
# If not, see <http://www.gnu.org/licenses/>.

# SERVER LOGIC FOR THE SHINY APP

# load libraries ####
# use 00_install_R_packages.R for installing missing packages
lapply(c("shiny", "datasets", "RSQLite", "dplyr", "ggplot2", "lubridate", "plotly"), 
       require, character.only = TRUE)

source("global.R")

# to see stdout logs, too
sink(stderr(), type = "output")

# Define server logic required to plot
shinyServer(function(input, output, session) {

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
  output$caption <- 
    renderText({
      paste0(names(locationChoices[locationChoices == input$location]),
             ": Anzahl ",
             names(vehicleChoices[vehicleChoices == input$vehicle]),
             " (nicht angezeigte Daten existieren leider nicht)")
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
				
				updateCheckboxGroupInput(session = session,
				                   inputId = "years",
													 inline = TRUE,
													 selected = isolate(input$years),
				                   choices = years_in_data)
				
				updateCheckboxGroupInput(session = session,
				                   inputId = "months",
													 inline = TRUE,
					 		             selected = isolate(input$months),
				                   choices = monthChoices[monthChoices %in% months_in_data])
				
				updateCheckboxGroupInput(session = session,
				                   inputId = "weekdays",
													 inline = TRUE,
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
	
  load_filtered_data_from_db <-
    reactive({
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
  			" AND strftime('%Y', date) IN (")
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
  			date_filter <- paste0(date_filter, " AND strftime('%m', date) IN (")

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
  			date_filter <- paste0(date_filter, " AND strftime('%w', date) IN (")
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
		
		vehicles <- dbGetQuery(conn = con, sql_string)

		cat(paste("\nload_filtered_data_from_db() took",
		          Sys.time() - start,
		          "seconds\n"))
		return(vehicles)
	})  # reactive

 	aggregated_data_year <- reactive({
 		start <- Sys.time()
 		vehicles_year <-
	   load_filtered_data_from_db() %>%
	      mutate(date = as.POSIXct(date)) %>% 
 				mutate(vehicle = factor(vehicle, levels = c("bike", "car"), labels = c("Fahrrad", "Kfz"))) %>% 
 				group_by(date, vehicle) %>%
 				summarise(count_day = sum(count, na.rm = TRUE))
	    cat(paste("aggregated_data_year() took", Sys.time() - start, "seconds\n"))

    return(vehicles_year)
  })

  aggregated_data_hour <- reactive({
  	start <- Sys.time()
  	vehicles_hour <- 
  		load_filtered_data_from_db() %>%
	      mutate(date = as.POSIXct(date)) %>% 
  			mutate(vehicle = factor(vehicle, levels = c("bike", "car"), labels = c("Fahrrad", "Kfz"))) %>% 
	      group_by(date, hour, vehicle) %>%
	      summarise(count_hour = sum(count, na.rm = TRUE))
  	cat(paste0("aggregated_data_hour() took ",
  	           Sys.time() - start,
  	           " seconds\n"))
  	
  	return(vehicles_hour)
  })
  
 	output$plotYear <- renderPlotly({
  	start <- Sys.time()
  	p <- plot_ly(x = ~aggregated_data_year()$date, 
  	        y = ~aggregated_data_year()$count_day, 
  	        type = "scattergl", mode = "lines+markers",
  	        color = ~aggregated_data_year()$vehicle, 
  	        name = ~aggregated_data_year()$vehicle,
  	        hoverinfo = "text",
  	        text = ~paste0(strftime(aggregated_data_year()$date, format = "%d. %m. %Y"), 
  	                      ", ", aggregated_data_year()$vehicle,
  	                      ": ", aggregated_data_year()$count_day)) %>% 
  	  layout(xaxis = list(title = "Datum"), 
  	         yaxis = list(title = "Anzahl"),
						legend = list(x = 0.1, y = 0.9)
  	  			 )
  	cat(paste("renderYearPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
  
  output$plotDay <- renderPlotly({
  	start <- Sys.time()
		  		p <-
					plot_ly(x = ~aggregated_data_hour()$hour, 
									y = ~aggregated_data_hour()$count_hour,
									type = "scattergl", 
									mode = "lines+markers",
									alpha = 0.1,
									color = ~aggregated_data_hour()$vehicle,
									name = ~names(aggregated_data_hour()$vehicle),
									hoverinfo = "text",
									text = ~paste0(strftime(aggregated_data_hour()$date, format = "%d. %m. %Y"), 
									               ", ", aggregated_data_hour()$hour, 
									               " Uhr, ", aggregated_data_hour()$vehicle,
									               ": ", aggregated_data_hour()$count_hour)) %>%
				layout(
					xaxis = list(title = "Stunde"),
					yaxis = list(title = "Anzahl"),
					legend = list(x = 0.1, y = 0.9)
				)
  	cat(paste("renderDayPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
  
  output$stringCounts <-
  	renderText({
  		# TODO: dplyr this ... (some say its easier to read :P)
  		d_year <- aggregated_data_year()
  		d_hour <- aggregated_data_hour()
  		
  		all_bikes <- sum(d_year[d_year$vehicle == "Fahrrad", ]$count_day, na.rm = TRUE)
  		mean_bikes_year <- mean(d_year[d_year$vehicle == "Fahrrad", ]$count_day, na.rm = TRUE)
  		mean_bikes_hour <- mean(d_hour[d_hour$vehicle == "Fahrrad", ]$count_hour, na.rm = TRUE)
  		
  		all_cars <- sum(d_year[d_year$vehicle == "Kfz", ]$count_day, na.rm = TRUE)
  		mean_cars_year <- mean(d_year[d_year$vehicle == "Kfz", ]$count_day, na.rm = TRUE)
  		mean_cars_hour <- mean(d_hour[d_hour$vehicle == "Kfz", ]$count_hour, na.rm = TRUE)
  		
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
  	})
})
