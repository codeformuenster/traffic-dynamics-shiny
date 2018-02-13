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
	
  # Return the formula text for printing as a caption
  output$caption <- 
    renderText({
      paste0(names(locationChoices[locationChoices == input$location]),
             ": Anzahl ",
             names(vehicleChoices[vehicleChoices == input$vehicle]),
             " (nicht angezeigte Daten fehlen noch)")
      			 # input$location, ": Anzahl ", input$vehicle)
    })

    observe({
  		# only show filtering option relevant for the current data
			
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
  						 "'Weseler.Straße'" = "'%01190%'",
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
				
				con <- dbConnect(SQLite(), dbname = "data/database/traffic_data.sqlite")
    				
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
				
				dbDisconnect(con)
				
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
    	
    	con <- dbConnect(SQLite(), dbname = "data/database/traffic_data.sqlite")
    	
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
    	
    	if (input$tabs == "timerange") {
  	  	date_filter <- 
  	  		paste0(date_filter,
  			" AND date >= '", input$date_range[1], "'", 
  			" AND date <= '", input$date_range[2], "'")
    	} else if (input$tabs == "timepoints") {
    		
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
		
		dbDisconnect(con)
		
		cat(paste("\nload_filtered_data_from_db() took",
		          Sys.time() - start,
		          "seconds\n"))
		return(vehicles)
	})  # reactive

 	aggregated_data_year <- reactive({
 		start <- Sys.time()
 		vehicles_year <-
	   load_filtered_data_from_db() %>%
	      group_by(date, vehicle) %>%
	      summarise(count_day = sum(count))
 		cat(paste("aggregated_data_year() took", Sys.time() - start, "seconds\n"))
    return(vehicles_year)
  })
 	
  aggregated_data_hour <- reactive({
  	start <- Sys.time()
  	vehicles_hour <- 
  		load_filtered_data_from_db() %>%
	      group_by(date, hour, vehicle) %>%
	      summarise(count_hour = sum(count))
  	cat(paste0("aggregated_data_hour() took ",
  	           Sys.time() - start,
  	           " seconds\n"))
  	return(vehicles_hour)
  })
  
 	output$plotYear <- renderPlot({
  	start <- Sys.time()
    p <- ggplot(data = aggregated_data_year()) +
	    geom_line(aes(x = as.POSIXct(date),
	                  y = count_day, group = vehicle,
	                  color = vehicle)) +
	  	labs(x = "Datum", y = "Anzahl", color = "Verkehrsmittel") +
	  	scale_color_manual(labels = c("bike" = "Fahrräder", "car" = "Autos"), 
	  										 values = c("bike" = "blue", "car" = "red")) +
	    theme_minimal(base_size = 18) +
	    theme(legend.position = "bottom")
  	cat(paste("renderYearPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
  
  output$plotDay <- renderPlot({
  	start <- Sys.time()
    p <-
      ggplot(data = aggregated_data_hour(), aes(x = hour, y = count_hour)) +
	    geom_line(aes(group = interaction(vehicle, date), color = vehicle),
	              alpha = 0.2) +
	    labs(x = "Stunde", y = "Anzahl", color = "Verkehrsmittel") +
	  	scale_color_manual(labels = c("bike" = "Fahrräder", "car" = "Autos"), 
	  										 values = c("bike" = "blue", "car" = "red")) +
	    theme_minimal(base_size = 18) + 
	    theme(legend.position = "bottom")
  	cat(paste("renderDayPlot() took", Sys.time() - start, "seconds\n"))
  	return(p)
  })
})
