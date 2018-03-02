# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING). 
# If not, see <http://www.gnu.org/licenses/>.

# USER INTERFACE OF THE SHINY APP

# load libraries ####
# use 00_install_R_packages.R for installing missing packages
lapply(c("shiny", "shinycssloaders", "plotly"),
			 require, character.only = TRUE)

source("global.R")

# define type of loading animation
options("spinner.type" = 4)

shinyUI(
	fluidPage(
		title = "Anzahl Fahrzeuge in Münster", 
		fluidRow(h4(textOutput("caption"))),
		fluidRow(
			column(
			  3,
    		wellPanel(
    		  # see global.R for choices
    		 	selectInput("vehicle", 
    		 							"Verkehrsmittel:", 
    		 							choices = vehicleChoices,
    		 							selected = startVehicle),
    		 	selectInput("location", "Ort:", 
    		 							choices = locationChoices,
    		 							selected = startLocation),
    		 	sliderInput(
    		 		"hour_range",
    		 		"Uhrzeit:",
    		 		min = 0,
    		 		max = 24,
    		 		value = c(0, 24)
    		  )
  		  ),  # wellPanel
			  tabsetPanel(
			    id = "tabs",
          tabPanel(
            "Zeitspanne",
            value = "timerange", 
          	dateRangeInput(
  				 		"date_range",
  				 		"Wähle eine Zeitspanne:",
  				 		min = "2013-06-01",
  				 		max = "2017-12-31",
  				 		start = "2013-06-01",
  				 		end = "2017-12-31",
  				 		format = "d. m. 'yy",
  				 		language = "de", separator = "bis"
  				 	)
  				),
        	tabPanel(
        	  "Zeitpunkte",
        	  value = "timepoints", 
        	  checkboxGroupInput("years",
        	                     "Wähle Jahre:",
        	                     inline = TRUE,
        	                     selected = 2017,
        	                     choices = list("2013", "2014", "2015", "2016",
        	                                    "2017")),
				 		checkboxGroupInput("months",
				 		                   "Wähle Monate:",
				 		                   inline = TRUE,
				 		                   selected = seq(1,12),
				 		                   choices = monthChoices),
				 		checkboxGroupInput("weekdays",
				 		                   "Wähle Wochentage:",
				 		                   inline = TRUE,
				 		                   selected = seq(0,6),
				 		                   choices = weekdayChoices)
				  )
			  )
			),
			column(5, withSpinner(plotOutput("plotYear"))
  		),
			column(4, withSpinner(plotOutput("plotDay")))
		),
	  fluidRow(
		  column(12,
		         hr(),
		         print("Ein Projekt von "), 
		         a("Code for Münster", href = "http://codeformuenster.org"),
		         print("lizenziert unter der GPLv3"),
		         a("(mehr Infos zur Lizenz hier)",
		           href = "https://github.com/codeformuenster/traffic-dynamics#rechtliches"),
		         HTML("<br>"),
		         a("Ideen und Feedback willkommen!", href = "https://github.com/codeformuenster/traffic-dynamics-shiny/issues"),
		         HTML("<br>"),
		         print("Datenquelle: Stadt Münster (lizenziert unter Datenlizenz Deutschland - Namensnennung - Version 2.0)"),
		         a("(weitere Infos zu den Daten hier)",
		           href = "http://www.stadt-muenster.de/verkehrsplanung/verkehr-in-zahlen.html")
	    )
    )
  )
)
