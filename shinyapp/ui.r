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
				tabsetPanel(
					id = "tabs_counts_models",
					tabPanel(
						"Zähldaten",
						value = "data",
						wellPanel(
							# see global.R for choices
							selectInput(
								"vehicle", 
								"Verkehrsmittel:", 
								choices = vehicleChoices,
								selected = startVehicle
							),
							selectInput(
								"location", "Ort:", 
								choices = locationChoices,
								selected = startLocation
							),
							sliderInput(
								"hour_range",
								"Uhrzeit:",
								min = 0,
								max = 24,
								value = c(0, 24)
							),
							actionButton("QueryBtn", "Aktualisieren", icon = icon("refresh"))
						),  # end wellPanel vehicle / location choice
						tabsetPanel(
							id = "tabs_time",
							selected = "timepoints",
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
							), # end tabPanel timerange
							tabPanel(
								"Zeitpunkte",
								value = "timepoints", 
								selectizeInput(
									"years",
                 	"Wähle Jahre:",
                 	selected = 2017,
                 	choices = list("2013", "2014", "2015", "2016", "2017"), 
									multiple = TRUE, options = list(
									  'plugins' = list('remove_button'))
								),
								selectizeInput(
									"months",
									"Wähle Monate:",
									selected = seq(1,12),
									choices = monthChoices, 
									multiple = TRUE, options = list(
									  'plugins' = list('remove_button'))
								),
								selectizeInput(
									"weekdays",
									"Wähle Wochentage:",
									selected = seq(0,6),
									choices = weekdayChoices, 
									multiple = TRUE, options = list(
									  'plugins' = list('remove_button'))
								)
							) # end tabPanel timepoints
						) # end tabsetPanel time
					), # end tabPanel counts
					tabPanel(
						"Modelle",
						value = "models",
						print("Bald™ lassen sich hier statistische Modelle auswählen.")
					) # end tabPanel models
				) # end tabsetPanel counts / models
			), # end column counts / models
			column(
				5,
				withSpinner(plotlyOutput("plotYear"))
  		),
			column(
				4,
				withSpinner(plotlyOutput("plotDay"))
			)
		), # end fluid row that contains almost everything
		fluidRow(
			column(
				12,
				verbatimTextOutput("stringCounts"),
				hr(),
				print("Ein Projekt von "), 
				a("Code for Münster", href = "http://codeformuenster.org"),
				print("in Zusammenarbeit mit der "),
				a("IG Fahrradstadt Münster.", href = "https://fahrradstadt.ms"),
				HTML("<br>"),
				print("Lizenziert unter der GPLv3"),
				a("(mehr Infos zur Lizenz hier).",
					href = "https://github.com/codeformuenster/traffic-dynamics#rechtliches"),
				HTML("<br>"),
				a("Ideen und Feedback willkommen!", 
					href = "https://github.com/codeformuenster/traffic-dynamics-shiny/issues"),
				HTML("<br>"),
				print("Datenquelle: Stadt Münster (lizenziert unter Datenlizenz Deutschland - Namensnennung - Version 2.0)"),
				HTML("<br>"),
				a("Hier gibt es die genaue Lage der Fahrrad-Zählstellen und weitere Infos zu den Daten.",
					href = "http://www.stadt-muenster.de/verkehrsplanung/verkehr-in-zahlen.html")
			) #  end footer column
    ) # end footer fluid row
  ) # end fluidPage
) # end shinyUI
