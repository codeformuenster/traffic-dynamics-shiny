# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING). If not, see <http://www.gnu.org/licenses/>.

# USER INTERFACE OF THE SHINY APP

# load libraries ####
# use 00_install_R_packages.R for installing missing packages
lapply(c("shiny"),
			 require, character.only = TRUE)

shinyUI(
	fluidPage(
		title = "Anzahl Fahrzeuge auf der Wolbecker Straße",
		#sidebarLayout(
		# sidebarPanel(
		fluidRow(
		column(2,
					 wellPanel(
					 	selectInput("vehicle", "Verkehrsmittel:", c("Fahrräder" = "bikes", "Autos" = "cars", "Fahrräder & Autos" = "both")),
					 	selectInput("location", "Ort:", c("Neutor" = "'Neutor'", "Wolbecker" = "'Wolbecker.Straße'")),
					 	sliderInput(
					 		"hour_range",
					 		"Uhrzeit:",
					 		min = 0,
					 		max = 24,
					 		value = c(0, 24)
					 	)
					 ),
				 tabsetPanel(id = "tabs",
          tabPanel("Zeitspanne", value = "timerange", 
          	dateRangeInput(
					 		"date_range",
					 		"Wähle eine Zeitspanne:",
					 		min = "2015-01-01",
					 		max = "2016-12-31",
					 		start = "2016-01-01",
					 		end = "2016-12-31",
					 		format = "dd. M yyyy",
					 		language = "de", separator = "bis"
					 	)),
          	tabPanel("Zeitpunkte", value = "timepoints", 
          					 checkboxGroupInput("years", "Wähle Jahre:", inline = TRUE, selected = 2017, choices = list("2013", "2014", "2015", "2016", "2017")),
          				 		checkboxGroupInput("months", "Wähle Monate:", inline = TRUE, selected = '08', choices = c("Januar" = '01', "Februar" = '02', "März" = '03', "April" = '04', "Mai" = '05', "Juni" = '06', "Juli" = '07', "August" = '08', "September" = '09', "Oktober" = '10', "November" = '11', "Dezember" = '12')),
          				 checkboxGroupInput("weekdays", "Wähle Wochentage:", inline = TRUE, selected = '3', choices = c("Montag" = '1', "Dienstag" = '2', "Mittwoch" = '3', "Donnerstag" = '4', "Freitag" = '5', "Samstag" = '6', "Sonntag" = '0'))
  				)
  				)
					 ),
		column(5,
			h3(textOutput("caption")),
			
			plotOutput(
				"plotYear",
				click = "plot1_click",
				brush = brushOpts(id = "plot1_brush")
			)
		),
		column(5,
					 plotOutput("plotDay")
		)
		#),
	),
	fluidRow(
		column(12,
		hr(),
		print("lizenziert unter der GPLv3,"),
		a("mehr Infos hier", href = "https://github.com/codeformuenster/traffic-dynamics#rechtliches"),
		HTML("<br>"),
		print(
			"Datenquelle: Stadt Münster (lizenziert unter Datenlizenz Deutschland - Namensnennung - Version 2.0)"
		)
	)
)
)
)
