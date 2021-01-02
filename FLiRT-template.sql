# ************************************************************
# Sequel Pro SQL dump
# Version 5446
#
# https://www.sequelpro.com/
# https://github.com/sequelpro/sequelpro
#
# Host: 178.128.232.127 (MySQL 5.5.5-10.1.47-MariaDB-0ubuntu0.18.04.1)
# Database: SNAP-jan2-2021-2
# Generation Time: 2021-01-02 18:10:58 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
SET NAMES utf8mb4;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table activitylog
# ------------------------------------------------------------

CREATE TABLE `activitylog` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Timestamp` varchar(255) DEFAULT NULL,
  `Log` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table blanks
# ------------------------------------------------------------

CREATE TABLE `blanks` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Blank` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table contexts
# ------------------------------------------------------------

CREATE TABLE `contexts` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Trench` varchar(255) DEFAULT NULL,
  `Context` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Reasoning` longtext,
  `Description` longtext,
  `Excavators` varchar(255) DEFAULT NULL,
  `ExcavatedCheck` int(1) DEFAULT NULL,
  `DateOpened` varchar(255) DEFAULT NULL,
  `DateClosed` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`ID`),
  KEY `idx_contexts_TrenchYear` (`Trench`,`Year`),
  KEY `idx_contexts_Context` (`Context`),
  CONSTRAINT `FK_contexts_TrenchYear` FOREIGN KEY (`Trench`, `Year`) REFERENCES `trenches` (`Trench`, `Year`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table periods
# ------------------------------------------------------------

CREATE TABLE `periods` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Period` varchar(255) DEFAULT NULL,
  `PeriodAbbreviation` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table grabsamples
# ------------------------------------------------------------

CREATE TABLE `grabsamples` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `X` varchar(255) DEFAULT NULL,
  `Y` varchar(255) DEFAULT NULL,
  `Item` varchar(255) DEFAULT NULL,
  `Material` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`ID`),
  KEY `idx_grabsamples_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table gridcollectionpoints
# ------------------------------------------------------------

CREATE TABLE `gridcollectionpoints` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(255) DEFAULT NULL,
  `Grid` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Size` varchar(255) DEFAULT NULL,
  `X` varchar(255) DEFAULT NULL,
  `Y` varchar(255) DEFAULT NULL,
  `Z` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`ID`),
  KEY `idx_gridcollectionpoints_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table illustrations
# ------------------------------------------------------------

CREATE TABLE `illustrations` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `IllustrationID` varchar(255) NOT NULL DEFAULT '',
  `ArtefactID` varchar(255) DEFAULT NULL,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Filename` varchar(255) DEFAULT NULL,
  `Illustrator` varchar(255) DEFAULT NULL,
  `Date` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`),
  KEY `idx_illustrations_IllustrationID` (`IllustrationID`(191))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;



# Dump of table level2
# ------------------------------------------------------------

CREATE TABLE `level2` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  `Blank` varchar(255) DEFAULT NULL,
  `Modification` varchar(255) DEFAULT NULL,
  `Quantity` int(11) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table level3
# ------------------------------------------------------------

CREATE TABLE `level3` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  `Blank` varchar(255) DEFAULT NULL,
  `Modification` varchar(255) DEFAULT NULL,
  `ArtefactID` varchar(255) DEFAULT NULL,
  `WrittenOnArtefact` varchar(255) DEFAULT NULL,
  `Illustrations` varchar(255) DEFAULT NULL,
  `Photos` varchar(255) DEFAULT NULL,
  `RawMaterial` varchar(255) DEFAULT NULL,
  `Weathering` varchar(255) DEFAULT '',
  `Patination` varchar(255) DEFAULT NULL,
  `Burned` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`),
  KEY `idx_level3_ArtefactID` (`ArtefactID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table modifications
# ------------------------------------------------------------

CREATE TABLE `modifications` (
  `ID` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Modification` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table photos
# ------------------------------------------------------------

CREATE TABLE `photos` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `PhotoID` varchar(255) DEFAULT NULL,
  `ArtefactID` varchar(255) DEFAULT NULL,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Filename` varchar(255) DEFAULT NULL,
  `Photographer` varchar(255) DEFAULT NULL,
  `Camera` varchar(255) DEFAULT NULL,
  `Date` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`id`),
  KEY `idx_photos_PhotoID` (`PhotoID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table transectcollectionpoints
# ------------------------------------------------------------

CREATE TABLE `transectcollectionpoints` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(255) DEFAULT NULL,
  `Transect` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `X` varchar(255) DEFAULT NULL,
  `Y` varchar(255) DEFAULT NULL,
  `Z` varchar(255) DEFAULT NULL,
  `Vegetation` varchar(255) DEFAULT NULL,
  `Slope` varchar(255) DEFAULT NULL,
  `Notes` longtext,
  PRIMARY KEY (`ID`),
  KEY `idx_transectcollectionpoints_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table trenches
# ------------------------------------------------------------

CREATE TABLE `trenches` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Trench` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Reasoning` longtext,
  `Description` longtext,
  `Supervisors` varchar(255) DEFAULT NULL,
  `Excavators` varchar(255) DEFAULT NULL,
  `DateOpened` varchar(255) DEFAULT NULL,
  `DateClosed` varchar(255) DEFAULT NULL,
  `ExcavatedCheck` int(1) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_trenches_Trenches_Year` (`Trench`,`Year`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;




/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
