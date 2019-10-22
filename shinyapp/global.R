vehicleChoices <- 
	c("Fahrräder" = "bikes", 
		"Kfz" = "cars", 
		"Fahrräder & Kfz" = "both")

startVehicle <- "both"

locationChoices <-
	c("Neutor" = "'Neutor'", 
		"Wolbecker Straße" = "'Wolbecker.Straße'", 
		"Hüfferstraße" = "'Hüfferstraße'", 
		"Hammer Straße" = "'Hammer.Straße'",
		"Promenade / Eisenbahnstr." = "'Promenade'",
		"Gartenstraße" = "'Gartenstraße'",
		"Warendorfer Straße" = "'Warendorfer.Straße'",
		"Hafenstraße" = "'Hafenstraße'",
		"Weseler Straße / Kolde-Ring" = "'Weseler.Straße'",
		"Hansaring / Albersloher Weg" = "'Hansaring'")

startLocation <- "'Wolbecker.Straße'"

monthChoices <-
	 c("Januar" = 1, "Februar" = 2,
		"März" = 3, "April" = 4,
		"Mai" = 5, "Juni" = 6,
		"Juli" = 7, "August" = 8,
		"September" = 9, "Oktober" = 10,
		"November" = 11, "Dezember" = 12
	)

weekdayChoices <-
	c("Montag" = 1, "Dienstag" = 2,
		"Mittwoch" = 3, "Donnerstag" = 4,
		"Freitag" = 5, "Samstag" = 6,
		"Sonntag" = 0
	)

sql_location_cars <- function(location_choice) {
  location_code <-
    switch(location_choice,
           "'Neutor'" = c("'%01080/_FV1/_G%'", "'%01080/_FV2/_L%'", "'%01080/_FV3%'", "'%01080/_FV4/_R%'"),
           "'Wolbecker.Straße'" = "'%09040%'",
           "'Hüfferstraße'" = "'%01123%'", # TODO Fahrspurfilterung?
           # Hammer Straße / Geiststraße: FV1 müsste aus der Stadt kommen (Rechtsabbieger haben keine Ampel), FV2 dann von Hiltrup, FV3 Geiststraße
           "'Hammer.Straße'" = c("'%07080%/_FV1%'", "'%07080/_FV2/_G%'", "'%07080/_FV3/_L%'"),
           "'Promenade'" = "'%04051%'",
           # Gartenstr. / Bohlweg
           "'Gartenstraße'" = c("'%04070/_FV3/_G%'", "'%04070/_FV4/_R%'", "'%04070/_FV1%'", "'%04070/_FV2/_L%'"),
           # Warendorfer Straße / Piusallee: FV2 müsste aus der Stadt kommend sein (nur L und G), FV3 Piusallee (nur L und R), FV4 Warendorfer (G und R, mehr G als FV1), FV1 Friedrichstr. (G und R, weniger Verkehr als FV4)
           "'Warendorfer.Straße'" = c("'%04061/_FV4%'", "'%04061/_FV1/_R%'", "'%04061/_FV2/_G%'", "'%04061/_FV3/_L%'"),
           "'Hafenstraße'" = "'%04010%'",
           # Weseler Straße / Koldering: FV1 müsste aus der Stadt kommen; FV3 von der Autobahn, FV2 müsste dann der Ring sein
           "'Weseler.Straße'" = c("'%01190/_FV1%'", "'%01190/_FV2/_L%'", "'%01190/_FV3/_G%'"),
           "'Hansaring'" = "'%03290%'"
    )
  return(location_code)
}
