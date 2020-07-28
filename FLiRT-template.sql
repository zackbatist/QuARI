CREATE TABLE `allloci` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Locus` varchar(191) DEFAULT NULL,
  `LocusType` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_allloci_Locus` (`Locus`),
  UNIQUE KEY `idx_allloci_LocusLocusType` (`Locus`,`LocusType`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `plots` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Plot` varchar(191) DEFAULT NULL,
  `Landowner` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_plots_Plot` (`Plot`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `trenches` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Plot` varchar(191) DEFAULT NULL,
  `Trench` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Reasoning` longtext,
  `Description` longtext,
  `Supervisors` varchar(191) DEFAULT NULL,
  `Excavators` varchar(191) DEFAULT NULL,
  `ExcavatedCheck` int(11) DEFAULT NULL,
  `SWGrid` varchar(191) DEFAULT NULL,
  `NEGrid` varchar(191) DEFAULT NULL,
  `DateOpened` date DEFAULT NULL,
  `DateClosed` date DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_trenches_Trenches_Year` (`Trench`,`Year`),
  UNIQUE KEY `idx_trenches_PlotTrenchYear` (`Plot`,`Trench`,`Year`),
  KEY `idx_trenches_Trench` (`Plot`),
  KEY `idx_trenches_PlotTrench` (`Plot`,`Trench`),
  CONSTRAINT `FK_trenches_Plot` FOREIGN KEY (`Plot`) REFERENCES `plots` (`Plot`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `contexts` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Plot` varchar(191) DEFAULT NULL,
  `Trench` varchar(191) DEFAULT NULL,
  `Context` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Reasoning` longtext,
  `Description` longtext,
  `Excavators` varchar(191) DEFAULT NULL,
  `ExcavatedCheck` tinyint(1) DEFAULT NULL,
  `Notes` text,
  `XFinds` varchar(191) DEFAULT '0',
  `Placeholder` tinyint(1) DEFAULT NULL,
  `DateOpened` date DEFAULT NULL,
  `DateClosed` date DEFAULT NULL,
  `LithostratigraphicUnit` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_contexts_Context` (`Context`),
  UNIQUE KEY `idx_contexts_Contexts_Trench_Plot` (`Context`,`Trench`,`Plot`),
  KEY `idx_contexts_TrenchYear` (`Trench`,`Year`),
  CONSTRAINT `FK_contexts_TrenchYear` FOREIGN KEY (`Trench`, `Year`) REFERENCES `trenches` (`Trench`, `Year`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `transects` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Year` varchar(191) DEFAULT NULL,
  `Transect` varchar(191) DEFAULT NULL,
  `Length` varchar(191) DEFAULT NULL,
  `Direction` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_transects_TransectYear` (`Transect`,`Year`),
  KEY `idx_transects_Transect` (`Transect`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `grids` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Grid` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Size` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_grids_Grid` (`Grid`),
  UNIQUE KEY `idx_grids_GridYear` (`Grid`,`Year`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `transectcollectionpoints` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(191) DEFAULT NULL,
  `Transect` varchar(191) DEFAULT NULL,
  `Type` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `X` varchar(191) DEFAULT NULL,
  `Y` varchar(191) DEFAULT NULL,
  `Z` varchar(191) DEFAULT NULL,
  `GPSPoint` varchar(191) DEFAULT NULL,
  `Vegetation` varchar(191) DEFAULT NULL,
  `Slope` varchar(191) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_transectcollectionpoints_CollectionPointIDType` (`CollectionPointID`,`Type`),
  KEY `idx_transectcollectionpoints_Transect` (`Transect`),
  KEY `idx_transectcollectionpoints_TransectYear` (`Transect`,`Year`),
  CONSTRAINT `FK_transectcollectionpoints_GridYear` FOREIGN KEY (`Transect`, `Year`) REFERENCES `transects` (`Transect`, `Year`) ON UPDATE CASCADE,
  CONSTRAINT `FK_transectcollectionpoints_Locus` FOREIGN KEY (`CollectionPointID`, `Type`) REFERENCES `allloci` (`Locus`, `LocusType`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `gridcollectionpoints` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(191) DEFAULT NULL,
  `Grid` varchar(191) DEFAULT NULL,
  `Type` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Size` varchar(191) DEFAULT NULL,
  `X` varchar(191) DEFAULT NULL,
  `Y` varchar(191) DEFAULT NULL,
  `Z` varchar(191) DEFAULT NULL,
  `Notes` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_gridcollectionpoints_GridYear` (`Grid`,`Year`),
  KEY `idx_gridcollectionpoints_Grid` (`Grid`),
  KEY `FK_gridcollectionpoints_Locus_idx` (`CollectionPointID`,`Type`),
  CONSTRAINT `FK_gridcollectionpoints_GridYear` FOREIGN KEY (`Grid`, `Year`) REFERENCES `grids` (`Grid`, `Year`) ON UPDATE CASCADE,
  CONSTRAINT `FK_gridcollectionpoints_Locus` FOREIGN KEY (`CollectionPointID`, `Type`) REFERENCES `allloci` (`Locus`, `LocusType`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `grabsamples` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(191) DEFAULT NULL,
  `Type` varchar(191) DEFAULT NULL,
  `X` varchar(191) DEFAULT NULL,
  `Y` varchar(191) DEFAULT NULL,
  `GPSPoint` varchar(191) DEFAULT NULL,
  `Notes` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Item` varchar(191) DEFAULT NULL,
  `Material` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_grabsamples_CollectionPointID` (`CollectionPointID`),
  CONSTRAINT `FK_grabsamples_Locus` FOREIGN KEY (`CollectionPointID`) REFERENCES `allloci` (`Locus`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `level2` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Locus` varchar(191) DEFAULT NULL,
  `LocusType` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  `Blank` varchar(191) DEFAULT NULL,
  `Modification` varchar(191) DEFAULT NULL,
  `Quantity` int(11) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`),
  KEY `idx_level2_Locus` (`Locus`),
  KEY `idx_level2_LocusPeriodBlankModification` (`Locus`,`Period`,`Blank`,`Modification`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `level3` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Locus` varchar(191) DEFAULT NULL,
  `LocusType` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  `Blank` varchar(191) DEFAULT NULL,
  `Modification` varchar(191) DEFAULT NULL,
  `ArtefactID` varchar(191) DEFAULT NULL,
  `WrittenOnArtefact` varchar(191) DEFAULT NULL,
  `Illustrations` varchar(191) DEFAULT NULL,
  `Photos` varchar(191) DEFAULT NULL,
  `RawMaterial` varchar(191) DEFAULT NULL,
  `Weathering` varchar(191) DEFAULT '',
  `Patination` varchar(191) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_ArtefactID` (`ArtefactID`),
  KEY `idx_level3_Locus` (`Locus`),
  KEY `idx_level3_LocusLocusType` (`Locus`,`LocusType`),
  KEY `idx_level3_Blank` (`Blank`),
  KEY `idx_level3_Modification` (`Modification`),
  KEY `idx_level3_BlankModification` (`Blank`,`Modification`),
  KEY `idx_level3_Period` (`Period`),
  KEY `idx_level3_RawMaterial` (`RawMaterial`),
  KEY `idx_level3_WeatheringIndex` (`Weathering`),
  KEY `idx_level3_Patination` (`Patination`),
  KEY `idx_level3_LocusPeriodBlankModification` (`Locus`,`Period`,`Blank`,`Modification`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `photos` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `PhotoID` varchar(191) DEFAULT NULL,
  `ArtefactID` varchar(191) DEFAULT NULL,
  `Filename` varchar(191) DEFAULT NULL,
  `Photographer` varchar(191) DEFAULT NULL,
  `Camera` varchar(191) DEFAULT NULL,
  `Date` date DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Notes` varchar(191) DEFAULT NULL,
  `Locus` varchar(191) DEFAULT NULL,
  `LocusType` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_photos_PhotoID` (`PhotoID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `illustrations` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `IllustrationID` varchar(191) NOT NULL,
  `ArtefactID` varchar(191) DEFAULT NULL,
  `Filename` varchar(191) DEFAULT NULL,
  `Illustrator` varchar(191) DEFAULT NULL,
  `Date` varchar(191) DEFAULT NULL,
  `Year` varchar(191) DEFAULT NULL,
  `Notes` varchar(191) DEFAULT NULL,
  `Locus` varchar(191) DEFAULT NULL,
  `LocusType` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `blanks_excavation` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Blank` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `blanks_survey` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Blank` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `modifications_excavation` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Modification` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `modifications_survey` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Modification` varchar(191) DEFAULT NULL,
  `Period` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `dating` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Period` varchar(191) DEFAULT NULL,
  `PeriodAbbreviation` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_dating_Period` (`Period`(191)),
  KEY `idx_dating_PeriodAbbreviation` (`PeriodAbbreviation`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `activitylog` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Log` varchar(191) DEFAULT NULL,
  `Timestamp` varchar(191) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

