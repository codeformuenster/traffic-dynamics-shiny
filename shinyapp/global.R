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
		"Promenade / Eisenbahnstraße" = "'Promenade'",
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