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
		fluidRow(column(12, h4(textOutput("caption", inline = TRUE)))),
    fluidRow(
			column(
				2,
				tabsetPanel(
					id = "tabs_data_models",
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
							fluidRow(
							  column(12,
							         align = "center",
							         uiOutput("button") # refresh button, see renderUI in server.R
							  )
							)
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
                 	selected = 2018,
                 	choices = list("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
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
						radioButtons("select_model",
						             "Wähle ein Modell:",
						             choices = c("einfach" = "simple", "komplex" = "complex"),
						            selected = "simple"),
						p("Neutor, Fahrräder, beide Richtungen, Werktage 2017, zwischen 7:00 und 8:59 Uhr. Linien/Punkte sind wahrscheinlichste Modellschätzungen, die mit 95% Wahrscheinlichkeit auch in den schattierten Bereichen/Fehlerbalken liegen könnten.")
					) # end tabPanel models
				) # end tabsetPanel counts / models
			), # end column counts / models
			conditionalPanel(
			  condition = "input.tabs_data_models == 'data'",
  			column(
  				5,
  				h4("Summe pro Tag:"),
  				withSpinner(plotlyOutput("plotYear")),
  				h4("Summe pro Monat:"),
  				withSpinner(plotlyOutput("plotRadarYear"))
    		),
  			column(
  				5,
  				h4("Durchschnitt pro Stunde:"),
  				withSpinner(plotlyOutput("plotDay")),
  				h4("Summe pro Stunde:"),
  				withSpinner(plotlyOutput("plotRadarDay"))
  			)
			),
			conditionalPanel(
			  condition = "input.tabs_data_models == 'models'",
			  column(
			    5,
			    withSpinner(plotlyOutput("plotTemperature")),
			    withSpinner(plotlyOutput("plotMonth")),
			    withSpinner(plotlyOutput("plotWeekday"))
			  ),
			  column(
			    4,
			    withSpinner(plotlyOutput("plotWind")),
			    withSpinner(plotlyOutput("plotRain"))
			  )
			)
		), # end fluid row that contains almost everything
		fluidRow(
		  column(12,
		         verbatimTextOutput("stringCounts")
		         )
		  ),
		fluidRow(
		  h3("Hinweise zu den Daten"),
		  p("Kfz werden an vielen Ampeln im Stadtgebiet gezählt. Wenn Du die Daten für eine bestimmte Ampelkreuzung hier",
		    " visualisiert sehen möchtest, dann", a("melde Dich gerne!", href = "mailto:muenster@codefor.de"),
		    " Da die Zählstellen für Fahrräder nicht an Ampeln liegen (wo die Kfz gezählt werden), ist die ",
		    " Vergleichbarkeit zwischen Fahrrad und Kfz bei den oben gleichzeitig gezeigten Daten für einen Ort mit Vorsicht zu genießen.",
		    " Ausgewählte Ampelkreuzungen für Kfz (jeweils nur die Fahrtrichtung, die auch die Radzählstelle misst): ",
		    " Neutor / Wilhelmstraße; Wolbecker Str. / Dortmunder Str.; Hüfferstr. / Badstr.; Hammer Str. / Geiststr.; Eisenbahnstr. Höhe ]pg[;
		    Gartenstr. / Bohlweg; Warendorfer Str. / Piusallee; Hafenstr. / Von-Steuben-Str. (alle Fahrspuren); Weseler Str. / Koldering; Hansaring / Albersloher Weg (alle Fahrspuren)",
		    p(a("Hier gibt es die genaue Lage der Fahrrad-Zählstellen und weitere Infos zu den Daten.",
		        href = "http://www.stadt-muenster.de/verkehrsplanung/verkehr-in-zahlen.html", target = "_blank")),
		    "Nicht angezeigte Daten fehlen vermutlich in der Datenquelle."),
		    strong("Keine Gewähr für Vollständigkeit oder Korrektheit der Daten!"),
		  p("Datenquelle: Stadt Münster (Daten bis 2017 lizenziert unter Datenlizenz Deutschland - Namensnennung - Version 2.0; Daten ab 2017 unter keiner genauer spezifizierten Lizenz; Fahrraddaten ab 2019 lizenziert unter Datenlizenz Deutschland - Namensnennung - Version 2.0)"),
		  h3("Über diese Seite"),
			p("Ein Projekt von ",
			  a(img(src = "cfm_logo.png", alt = "Code for Münster."), href = "http://codeformuenster.org", target = "_blank"),
			  "in Zusammenarbeit mit der ",
			  a("IG Fahrradstadt Münster.", href = "https://fahrradstadt.ms", target = "_blank")
			  ),
			p("Ideen und Feedback willkommen!",
			  a(href = "https://github.com/codeformuenster/traffic-dynamics-shiny/issues", target = "_blank", "Zum Beispiel auf github"),
			  " oder ", a(href = "mailto:muenster@codefor.de", "per e-Mail.")),
			p("Lizenziert unter der GPLv3",
			  a("(mehr Infos zur Lizenz hier).",
			  href = "https://github.com/codeformuenster/traffic-dynamics-shiny#rechtliches", target = "_blank")
			  ),
			a("Impressum & Datenschutzerklärung", href = "https://codeformuenster.org/impressum/", target = "_blank")
    ), # end footer fluid row
		
		tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

		tags$script(
		  HTML(
		    paste0("(function(f, a, t, h, o, m){",
		                        "a[h]=a[h]||function(){",
		                        "(a[h].q=a[h].q||[]).push(arguments)",
		                        "};",
		                        "o=f.createElement('script'),",
		                        "m=f.getElementsByTagName('script')[0];",
		                        "o.async=1; o.src=t; o.id='fathom-script';",
		                        "m.parentNode.insertBefore(o,m)",
		                        "})(document, window, '//fathom.codeformuenster.org/tracker.js', 'fathom');",
		                        "fathom('set', 'siteId', '", Sys.getenv("FATHOM_SITEID"), "');",
		                        "fathom('trackPageview');"
		    )
		  )
		) # end fathom script
		
  ) # end fluidPage
) # end shinyUI
