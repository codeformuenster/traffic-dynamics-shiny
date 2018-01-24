# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING). If not, see <http://www.gnu.org/licenses/>.

# SERVER LOGIC FOR THE SHINY APP

# load libraries ####
# use 00_install_R_packages.R for installing missing packages
lapply(c("shiny", "datasets", "RSQLite", "dplyr", "ggplot2"), 
       require, character.only = TRUE)

# to see stdout logs, too
sink(stderr(), type = "output")

# Define server logic required to plot
shinyServer(function(input, output) {
	
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    paste("Alpha-Version, Daten nicht verlässlich!", input$scale)
  })

  load_filtered_data_from_db <- reactive({
  	start <- Sys.time()
  	
  	con <- dbConnect(SQLite(), dbname = "data/database/traffic_data.sqlite")
  	
  	if (input$vehicle == "bikes") {
  		sql_table = "bikes"
  		sql_location = input$location
  	} else  { 
  		# default to cars only 
  		# (for selecting both, a second SQL query is added below)
  		sql_table = "cars"
  		if (input$location == "'Wolbecker.Straße'") {
				sql_location = "'%09040%'"
  		} else if (input$location == "'Neutor'") {
  			sql_location = "'%01080%'"
  		}
  	}
  	
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
  		for (yidx in 1:length(input$months)) {
  			if (yidx != length(input$months)) {
  				date_filter <- paste0(date_filter, "'" , input$months[yidx], "',")
  			} else {
  				# no comma after last date
  				date_filter <- paste0(date_filter, "'" , input$months[yidx], "'")
  			}
  		}
  		date_filter <- paste0(date_filter, ")")
			
  		# weekday
			date_filter <- paste0(date_filter, " AND strftime('%w', date) IN (")
  		for (yidx in 1:length(input$weekdays)) {
  			if (yidx != length(input$weekdays)) {
  				date_filter <- paste0(date_filter, "'" , input$weekdays[yidx], "',")
  			} else {
  				# no comma after last date
  				date_filter <- paste0(date_filter, "'" , input$weekdays[yidx], "'")
  			}
  		}
  		date_filter <- paste0(date_filter, ")")
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
		
		cat(paste0("\nload_filtered_data_from_db() took ", Sys.time() - start, " seconds\n"))
		
		return(vehicles)
	})

 	aggregated_data_year <- reactive({
 		start <- Sys.time()
 		vehicles_year <-
	   load_filtered_data_from_db() %>%
	      group_by(date, vehicle) %>%
	      summarise(count_day = sum(count))
 		cat(paste0("aggregated_data_year() took ", Sys.time() - start, " seconds\n"))
    return(vehicles_year)
  })

  aggregated_data_hour <- reactive({
  	start <- Sys.time()
  	vehicles_hour <- 
  		load_filtered_data_from_db() %>%
	      group_by(date, hour, vehicle) %>%
	      summarise(count_hour = sum(count))
  	cat(paste0("aggregated_data_hour() took ", Sys.time() - start, " seconds\n"))
  	return(vehicles_hour)
  })
    
  output$plotYear <- renderPlot({
  	start <- Sys.time()
    p <- ggplot(data = aggregated_data_year()) +
	    geom_line(aes(x = as.POSIXct(date), y = count_day, group = vehicle, color = vehicle)) +
	  	labs(x = "Datum", y = "Anzahl", color = "Verkehrsmittel") +
	  	scale_color_manual(labels = c("bike" = "Fahrräder", "car" = "Autos"), 
	  										 values = c("bike" = "blue", "car" = "red")) +
	    theme_minimal(base_size = 18) +
	    theme(legend.position = "bottom")
  	cat(paste0("renderYearPlot() took ", Sys.time() - start, " seconds\n"))
  	return(p)
  })
  
  output$plotDay <- renderPlot({
  	start <- Sys.time()
    p <- ggplot(data = aggregated_data_hour(), aes(x = hour, y = count_hour)) +
	    geom_line(aes(group = interaction(vehicle, date), color = vehicle), alpha = 0.2) +
	    labs(x = "Stunde", y = "Anzahl", color = "Verkehrsmittel") +
	  	scale_color_manual(labels = c("bike" = "Fahrräder", "car" = "Autos"), 
	  										 values = c("bike" = "blue", "car" = "red")) +
	    theme_minimal(base_size = 18) + 
	    theme(legend.position = "bottom")
  	cat(paste0("renderDayPlot() took ", Sys.time() - start, " seconds\n"))
  	return(p)
  })
})