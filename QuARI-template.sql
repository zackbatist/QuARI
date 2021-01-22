# ************************************************************
# Sequel Pro SQL dump
# Version 5446
#
# https://www.sequelpro.com/
# https://github.com/sequelpro/sequelpro
#
# Host: db (MySQL 5.5.5-10.3.27-MariaDB)
# Database: rshiny_batistz_quaridemo
# Generation Time: 2021-01-22 16:41:25 +0000
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

DROP TABLE IF EXISTS `activitylog`;

CREATE TABLE `activitylog` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Timestamp` varchar(255) DEFAULT NULL,
  `Log` longtext DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table contexts
# ------------------------------------------------------------

DROP TABLE IF EXISTS `contexts`;

CREATE TABLE `contexts` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Trench` varchar(255) DEFAULT NULL,
  `Context` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Reasoning` longtext DEFAULT NULL,
  `Description` longtext DEFAULT NULL,
  `Excavators` varchar(255) DEFAULT NULL,
  `ExcavatedCheck` int(1) DEFAULT NULL,
  `DateOpened` varchar(255) DEFAULT NULL,
  `DateClosed` varchar(255) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_contexts_TrenchYear` (`Trench`,`Year`),
  KEY `idx_contexts_Context` (`Context`),
  CONSTRAINT `FK_contexts_TrenchYear` FOREIGN KEY (`Trench`, `Year`) REFERENCES `trenches` (`Trench`, `Year`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `contexts` WRITE;
/*!40000 ALTER TABLE `contexts` DISABLE KEYS */;

INSERT INTO `contexts` (`ID`, `Trench`, `Context`, `Year`, `Reasoning`, `Description`, `Excavators`, `ExcavatedCheck`, `DateOpened`, `DateClosed`, `Notes`)
VALUES
	(0,'000','0000','0000',NULL,NULL,NULL,NULL,NULL,NULL,NULL),
	(1,'001','0001','2015','Context is trench opening.','Redeposited and dumped loose soil, rootings, small to very large boulders, small and large cobbles, fine sand particles. Abundant chert material, mostly white chert with small amount of red or multi-coloured chert.','GS,RS',1,'5/27/15','5/27/15',NULL),
	(2,'001','0002','2015','After finishing with surafe collecting and cleaning the resolution of 1m squared was removed, and trench sized 2x2m resolution established. Contexts will go on with numbers on every 10-15cm to resume excavation control in form of artificial spits.','Deposited and humified loose soil, rootings, small to very large boulders, small and large cobbles, fine sand particles.','GS,RS',1,'5/27/15','5/27/15',NULL),
	(3,'001','0003','2015','Emergence of a new deposit','Fine reddish deposit with considerabely less lithic material. Still containing roots. Is of colluvial origin','DM,JAC',1,'5/29/15','5/29/15',NULL),
	(4,'001','0004','2015','Emergence of a new deposit','Considerably more compact deposit, with much less lithic material. Almost no roots. ','JAC,RS',1,'5/30/15','5/30/15',NULL),
	(5,'001','0005','2015','Vertical control of excavation progress and the appearance of more diagnostic lithic artefacts at the bottom of the context 0004, or to be more precise, around contexts 0004/0005 boundary','Control spit at the boundary of different deposit compactness, contains considerable amount of lithic material','JAC',1,'2006-03-15','2006-03-15',NULL),
	(6,'002','0011','2015','To remove the surface layer of plant remains and shattered new material and get to more homogenous surface.','The context involves the whole trench area. Consists of surface vegetation and thin layer of sandy soil (?). It also includes some snail shells. Many flat pieces of shattered lithic material scattered all over the surface. Several big chunks of poterntial new material in N and SE part of trench.','DM,BF',1,'5/26/15','5/26/15',NULL),
	(7,'002','0012','2015','When the surface layer was removed this ciontext was made in order to see how much the similar sediment will go further down.','Loose sandy topsoil. Very similar to context 0011, with less plant roots and a bit larger chunks of rocks, fine gravel. More artefacts found in south part of context. The bigger rock found in east part of trench of sedimentary origin, fine grained, made of sand, quartz and thin layers of chert. (Reddish yellowish chert).','DM,BF',1,'5/26/15','5/27/15',NULL),
	(8,'002','0013','2015','It seemed that larger chunks of rock started appearing on the bottom surface of 0012, which might indicate different kind of sediment.','Bigger gravel with sandy sediment (fine-medium sand). Bigger boulders started appearing in NE corner almost immediately beneath the context surface. Digging surface was shortened in South because new context was opened in that place (Context 0014).','BS',1,'5/27/15','5/28/16',NULL),
	(9,'002','0014','2015','There were more finds (artefacts) in the southern end of the trench. However, the sediment was the same.','Sediment is the same as the last context (0013) with a lot of pieces of white chert.','BS',1,'5/28/15','5/29/15',NULL),
	(10,'003','0021','2015','Beginning of Trench 03 - NE quadrant removal of surface material with plant residue.','NE quadrant of Trench 03 - Surface split with plant material. Purely surface','SC',1,'5/26/15','5/26/15',NULL),
	(11,'003','0022','2015','Surface of Trench 03. SE quadrant removal of plant material. ','SE quadrant of Trench 03. 1m squared surface layer with plant material. Purely surface','SC',1,'5/26/15','5/26/15',NULL),
	(12,'003','0023','2015','Beginning/surface of Trench 003 - SW quadrant ','SW quadrant of surface of Trench 03 surface ','SC',1,'5/26/15','5/26/15',NULL),
	(13,'003','0024','2015','Changed context to test stratigraphic layers. We wanted to see if the red paleosoil would come down on the blonde/yellow sand.','A paleosol that contains intact shells. Context covers the southern half of the trench; divided approximately from the SW to the NE corners.','KM,BP,AAG,PO',1,'6/19/17','6/26/17',NULL),
	(14,'004','0031','2015','To collect surface finds from the NW quad of trench 004.','Collection of surface finds.','LP',1,'2006-08-15','2006-08-15',NULL),
	(15,'004','0032','2015','To collect surface finds from NE quad of Trench 004','Collection of surface finds','LP',1,'2006-08-15','2006-08-15',NULL),
	(16,'004','0033','2015','To collect finds from SW quad of Trench 004','Collection of surface finds','LP',1,'2006-08-15','2006-08-15',NULL),
	(17,'004','0034','2015','To collect finds from SE quad of trench 004.','Collection of surface finds.','LP',1,'2006-08-15','2006-08-15',NULL),
	(18,'004','0035','2015','After surface collecting of finds this context was opened in order to remove topsoil and vegetation remains.','n/a','LP',1,'2006-08-15','2006-08-15',NULL),
	(19,'005','0041','2015','Start of excavation','(0-11cm) Ephemeral colluvial soil overlying porrly sorted very cobbly silt loam; angular cobbles and cobbles fragments of mixed lithology, predominantly cryptocrystalline silicated throughout. Fine microbiotic crust present at the top of soil surface. Calcium carbonate (CaCO3) Present throughout as fine masses and fine spherical nodules; concentration of weakly cemented carbonate occurs in north-west corner of the unitl stained by secondary iron developed during ephemral exposure to subnerial weathering. Sediment size decreased with depth; artifacts found throughout. Many fine to coarse roots throughout. Context interpreted as colluvial deposit with incipient pedogenesis.','BS',1,'2006-09-15','2006-09-15',NULL),
	(20,'005','0042','2015','Localized area with increased CaCO3 and slight increase in sorting.','Laterally discontinuous poorly sorted silty clay loam intermixed with CaCo3 and restricted to the northwest corner of the unit. Increase in carbonate that occurs as fine masses and medium rounded nodules, also forming around roots. Iron staining occurs throughout. Few clay depletions throughout. Moderately sticky. Many fine to coarse roots throughout. Lower boundary is gradual and broken. Context interpreted as a transitional context to a localized area effected by subaquatic conditions.','BS',1,'2006-10-15','2006-10-15',NULL),
	(21,'005','0043','2015','Abrupt transition to a localized area of moderately sorted and compact CaCO3 dominated horizon.','Carbonate rich and moderately sorted silty clay loam with sub-angular blocky structure and restricted to the northwest corner of the unit. Very few, fine, angular (<1cm) rock fragments present throughout, predominantly local cryptocystalline silicated. Redoximorphic features present, including both Fe and Mn oxides as fine to coarse nodules and concretions occurring throughout. Iron staining present indication exposure to subaerial weathering. Few clay bodies throughout. Lower boundary very abrupt and broken. Context interpreted as localized area of CaCO3 affected by subaerial weathering due to subaquic conditions.','BS',1,'2006-11-15','2006-11-15',NULL),
	(22,'005','0044','2015','Colour change to more red in hue compared to overlying context (0043)','A well developed colluvial soil overprinting poorly sorted, brownish-red silty clay loam with sub-angular blocky structure. Many angular rocks and rock gragments present throughout, concentrated in NW, SW, SE corners, predominantly cryptocrystalline silicates. Few iron oxides as concentrations. Decreased in artifact density compared to overlying context (0043). Many fine to coarse roots. Lower boundary is aprupt and eavy. Context is interpreted as a continuation of pedogenesis associated with overlying conext 0043 or a paleosurface similar to other four upslope trenches.','BS',1,'2006-12-15','2006-12-15',NULL),
	(23,'005','0045','2015','Abrupt transition of texture of clay.','A well-sorted deposit of light greenish grey (Gley1 7/1) clay. Increase in redoximorphic features including: Fe-Mn oxides as nodules and concretions and occurring as iron root psuedomorphs (rhizoliths); Fe+3 as rigist spherical nodules (Ferrhydrite?) and iron stains throughout. Iron depletions throughout. Unit oxidizes as exposed to air (reduced matrix). Few fine roots throughout. Rounded rock (0059.x1) found at bottom of unit.','BS',1,'6/13/15','6/13/15',NULL),
	(24,'005','0046','2015','Context represents the first context after 1x1m SW quad split from original 2x2m.','(SW Quad) Carbonate rich pale yellow (2.5Y 8/4) silt clay loam. Moderatly to well sorted with sub-angular blocky structure Iron stains throughout and Fe nodules present. CaCO3 accumulations as nodules throughout.','BS',1,'6/19/15','6/19/15',NULL),
	(25,'005','0047','2015','Abrupt lower boundary; change from clay to carbonate rich silt clay loam','Moderate to well sorted carbonate rich pale yellow (2.5Y 7/4) Silt clay loam. Few iron stains throughout. Many fine roots. Carbonate coats root pores and appears as accumulations (nodules).','BS',1,'6/19/15','6/19/15',NULL),
	(26,'005','0048','2015','Aprupt change to clay.','Well sorted olive yellow clay. Redox features present as iron nodules and stains. Many crack throughout. Many fine roots.','BS',1,'6/20/15','6/20/15',NULL),
	(27,'005','0049','2015','Abrupt change from carbonate to clay','Well sorted thick (~50cm) deposit of light greenish grey clay. Heavily oxidized. Fragipans appear as laterally discontinuous cemented and rigid brownish red accumulations throughout.','BS',1,'6/21/15','6/21/15',NULL);

/*!40000 ALTER TABLE `contexts` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table grabsamples
# ------------------------------------------------------------

DROP TABLE IF EXISTS `grabsamples`;

CREATE TABLE `grabsamples` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `X` varchar(255) DEFAULT NULL,
  `Y` varchar(255) DEFAULT NULL,
  `Item` varchar(255) DEFAULT NULL,
  `Material` varchar(255) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_grabsamples_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `grabsamples` WRITE;
/*!40000 ALTER TABLE `grabsamples` DISABLE KEYS */;

INSERT INTO `grabsamples` (`ID`, `CollectionPointID`, `Year`, `X`, `Y`, `Item`, `Material`, `Notes`)
VALUES
	(1,'GS001','2013','25.345253','37.087136','Sidescraper','Chert',NULL),
	(2,'GS002','2013','25.344366236','37.08650616','5 pieces','Chert','2 M Pal flakes (linear & sidescraper), 3 MPal blades'),
	(3,'GS003','2013','25.346061308','37.087669987','Scraper','Chert',NULL),
	(4,'GS004','2013','25.345234433','37.085870979','2 pieces','Chert','Levallois flake, Mesolithic flake core'),
	(5,'GS005','2013','25.3456','37.0878','5 pieces','Chert','2 retouched Levallois blades; 2 non-diag blade-like flakes, 1 chunk'),
	(6,'GS006','2013','25.346286781','37.086710259','Retouched flake','Chert',NULL),
	(7,'GS007','2013','25.3463','37.0872','3 pieces','Chert','Retouched M Pal flake;  2 spines'),
	(8,'GS008','2013','25.345645063','37.085154159','3 pieces','Chert','retouched Levallois flake, notch/scraper on M Pal flake, non-diagnostic retouched flake'),
	(9,'GS009','2013','25.344231706','37.085722117','Notched flake','Chert',NULL),
	(10,'GS010','2013','25.3449','37.0909','Flake','Chert','M Pal flake'),
	(11,'GS011','2013','25.345605','37.086356','Hammerstone?','Emery',NULL),
	(12,'GS012','2013','25.3453','37.08798','4 pieces','3 chert, 1 obsidian','obsidian flake/denticulate, obsidian bladse/notch, chert flake/spine, non-diag flake'),
	(13,'GS013','2013','25.338871228','37.08052936','Spine','Chert','flake/spine');

/*!40000 ALTER TABLE `grabsamples` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table gridcollectionpoints
# ------------------------------------------------------------

DROP TABLE IF EXISTS `gridcollectionpoints`;

CREATE TABLE `gridcollectionpoints` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `CollectionPointID` varchar(255) DEFAULT NULL,
  `Grid` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Size` varchar(255) DEFAULT NULL,
  `X` varchar(255) DEFAULT NULL,
  `Y` varchar(255) DEFAULT NULL,
  `Z` varchar(255) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_gridcollectionpoints_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `gridcollectionpoints` WRITE;
/*!40000 ALTER TABLE `gridcollectionpoints` DISABLE KEYS */;

INSERT INTO `gridcollectionpoints` (`ID`, `CollectionPointID`, `Grid`, `Year`, `Size`, `X`, `Y`, `Z`, `Notes`)
VALUES
	(1,'G100-001','100-001','2013','Dogleash','25.345936','37.086368',NULL,NULL),
	(2,'G100-002','100-002','2013','Dogleash','25.345838','37.086307',NULL,NULL),
	(3,'G100-003','100-003','2013','Dogleash','25.34587','37.086415',NULL,NULL),
	(4,'G100-004','100-004','2013','Dogleash','25.346003','37.086304',NULL,NULL),
	(5,'G100-005.A1','100-005','2013','15x15','25.3455275','37.08588',NULL,NULL),
	(6,'G100-005.A8','100-005','2013','15x15','25.3455525','37.086',NULL,NULL),
	(7,'G100-005.A15','100-005','2013','15x15','25.3455675','37.08612',NULL,NULL),
	(8,'G100-005.H1','100-005','2013','15x15','25.3456475','37.08588',NULL,NULL),
	(9,'G100-005.H8','100-005','2013','15x15','25.3456725','37.086',NULL,NULL),
	(10,'G100-005.H15','100-005','2013','15x15','25.3456875','37.08612',NULL,NULL),
	(11,'G100-006','100-006','2013','Dogleash','25.345327','37.086198',NULL,NULL),
	(12,'G100-007','100-007','2013','Dogleash','25.345279','37.086227',NULL,NULL),
	(13,'G100-008','100-008','2013','Dogleash','25.345213','37.086248',NULL,NULL),
	(14,'G100-009','100-009','2013','Dogleash','25.345183','37.086234',NULL,NULL),
	(15,'G100-010','100-010','2013','10x10','25.345413','37.08642',NULL,NULL),
	(16,'G100-011','100-011','2013','Dogleash','25.345277','37.086223',NULL,NULL),
	(17,'G100-012','100-012','2013','10x10','25.3455485','37.08683',NULL,NULL),
	(18,'G100-012.A1','100-012','2013','10x10','25.345536','37.08675',NULL,NULL),
	(19,'G100-012.A10','100-012','2013','10x10','25.345561','37.08691',NULL,NULL),
	(20,'G100-012.E6','100-012','2013','10x10','25.3456535','37.08683',NULL,NULL),
	(21,'G100-012.J1','100-012','2013','10x10','25.345696','37.08675',NULL,NULL),
	(22,'G100-012.J10','100-012','2013','10x10','25.345721','37.08691',NULL,NULL),
	(23,'G100-013','100-013','2013','10x10','25.3454225','37.0873',NULL,NULL),
	(24,'G100-013.A1','100-013','2013','10x10','25.34541','37.08722',NULL,NULL),
	(25,'G100-013.A10','100-013','2013','10x10','25.345435','37.08738',NULL,NULL);

/*!40000 ALTER TABLE `gridcollectionpoints` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table illustrations
# ------------------------------------------------------------

DROP TABLE IF EXISTS `illustrations`;

CREATE TABLE `illustrations` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `IllustrationID` varchar(255) NOT NULL DEFAULT '',
  `ArtefactID` varchar(255) DEFAULT NULL,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Filename` varchar(255) DEFAULT NULL,
  `Illustrator` varchar(255) DEFAULT NULL,
  `Date` varchar(255) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `idx_illustrations_IllustrationID` (`IllustrationID`(191))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

LOCK TABLES `illustrations` WRITE;
/*!40000 ALTER TABLE `illustrations` DISABLE KEYS */;

INSERT INTO `illustrations` (`id`, `IllustrationID`, `ArtefactID`, `LocusType`, `Locus`, `Filename`, `Illustrator`, `Date`, `Notes`)
VALUES
	(0,'DR0000',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*!40000 ALTER TABLE `illustrations` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table level2
# ------------------------------------------------------------

DROP TABLE IF EXISTS `level2`;

CREATE TABLE `level2` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  `Blank` varchar(255) DEFAULT NULL,
  `Cortex` varchar(255) DEFAULT NULL,
  `Technique` varchar(255) DEFAULT NULL,
  `Modification` varchar(255) DEFAULT NULL,
  `Quantity` int(11) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table level3
# ------------------------------------------------------------

DROP TABLE IF EXISTS `level3`;

CREATE TABLE `level3` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `LocusType` varchar(255) DEFAULT NULL,
  `Locus` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  `Blank` varchar(255) DEFAULT NULL,
  `Cortex` varchar(255) DEFAULT NULL,
  `Technique` varchar(255) DEFAULT NULL,
  `Modification` varchar(255) DEFAULT NULL,
  `ArtefactID` varchar(255) DEFAULT NULL,
  `WrittenOnArtefact` varchar(255) DEFAULT NULL,
  `Illustrations` varchar(255) DEFAULT NULL,
  `Photos` varchar(255) DEFAULT NULL,
  `RawMaterial` varchar(255) DEFAULT NULL,
  `Weathering` varchar(255) DEFAULT NULL,
  `Patination` varchar(255) DEFAULT NULL,
  `Burned` varchar(255) DEFAULT NULL,
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `idx_level3_ArtefactID` (`ArtefactID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `level3` WRITE;
/*!40000 ALTER TABLE `level3` DISABLE KEYS */;

INSERT INTO `level3` (`id`, `LocusType`, `Locus`, `Period`, `Blank`, `Cortex`, `Technique`, `Modification`, `ArtefactID`, `WrittenOnArtefact`, `Illustrations`, `Photos`, `RawMaterial`, `Weathering`, `Patination`, `Burned`, `Notes`)
VALUES
	(1,'Context','0000',NULL,NULL,NULL,NULL,NULL,'AR000000',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*!40000 ALTER TABLE `level3` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table lookups
# ------------------------------------------------------------

DROP TABLE IF EXISTS `lookups`;

CREATE TABLE `lookups` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `Blank` varchar(255) DEFAULT NULL,
  `Cortex` varchar(255) DEFAULT NULL,
  `Technique` varchar(255) DEFAULT NULL,
  `Modification` varchar(255) DEFAULT NULL,
  `RawMaterial` varchar(255) DEFAULT NULL,
  `Weathering` varchar(255) DEFAULT NULL,
  `Patination` varchar(255) DEFAULT NULL,
  `Burned` varchar(255) DEFAULT NULL,
  `Period` varchar(255) DEFAULT NULL,
  `PeriodAbbreviation` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

LOCK TABLES `lookups` WRITE;
/*!40000 ALTER TABLE `lookups` DISABLE KEYS */;

INSERT INTO `lookups` (`id`, `Blank`, `Cortex`, `Technique`, `Modification`, `RawMaterial`, `Weathering`, `Patination`, `Burned`, `Period`, `PeriodAbbreviation`)
VALUES
	(1,'Flake','1 (0-25%)','Crested','No Modification','Type A','1','Yes','Yes','Lower Palaeolithic','LP'),
	(2,'Flake core','2 (25-90%)','Dejete','Backed','Type B','2','No','No','Middle Palaeolithic','MP'),
	(3,'Blade','3 (90-100%)','Levallois','Bec','Type C','3',NULL,NULL,'Upper Palaeolithic','UP'),
	(4,'Blade core',NULL,'Core preparation','Biface','Type D','4',NULL,NULL,'Mesolithic','Meso'),
	(5,'Blade & flake core',NULL,'Core rejuvenation','Burin','Type E','5',NULL,NULL,'Final Neolithic / Early Bronze Age','FNEBA'),
	(6,'Blade-like flake',NULL,'Percussion','Chopper','Type F',NULL,NULL,NULL,'NonDiagnostic','ND'),
	(7,'Blade-like flake core',NULL,'Pseudo Levallois','Concave','Indeterminate',NULL,NULL,NULL,'Late Neolithic / Final Neolithic','FNLN'),
	(8,'Bladelet',NULL,'Pressure-flaked','Denticulate','Missing',NULL,NULL,NULL,'Lower / Middle Palaeolithic','LMP'),
	(9,'Bladelet core',NULL,NULL,'Geometric',NULL,NULL,NULL,NULL,'Middle / Upper Palaeolithic','MUP'),
	(10,'Bladelet & flake core',NULL,NULL,'Linear',NULL,NULL,NULL,NULL,'Upper Palaeolithic / Mesolithic','UPMeso'),
	(11,'Chip',NULL,NULL,'Notched',NULL,NULL,NULL,NULL,NULL,NULL),
	(12,'Chunk',NULL,NULL,'Pick',NULL,NULL,NULL,NULL,NULL,NULL),
	(13,'Core preparation',NULL,NULL,'Piece Esq.',NULL,NULL,NULL,NULL,NULL,NULL),
	(14,'Core rejuvenation',NULL,NULL,'Piercer',NULL,NULL,NULL,NULL,NULL,NULL),
	(15,'Crested blade',NULL,NULL,'Point',NULL,NULL,NULL,NULL,NULL,NULL),
	(16,'Discoidal flake core',NULL,NULL,'Scraper',NULL,NULL,NULL,NULL,NULL,NULL),
	(17,'Levallois core preparation flake',NULL,NULL,'Snapped',NULL,NULL,NULL,NULL,NULL,NULL),
	(18,'Levallois flake & blade core',NULL,NULL,'Spine',NULL,NULL,NULL,NULL,NULL,NULL),
	(19,'p/f Blade core rejuvenation',NULL,NULL,'Tranchet',NULL,NULL,NULL,NULL,NULL,NULL),
	(20,'p/f Blade w/ rem crests',NULL,NULL,'Truncated',NULL,NULL,NULL,NULL,NULL,NULL),
	(21,'p/f Crested blade',NULL,NULL,'Uniface',NULL,NULL,NULL,NULL,NULL,NULL),
	(22,'Rejuv. flake (from blade-core)',NULL,NULL,'Combined',NULL,NULL,NULL,NULL,NULL,NULL),
	(23,'Rejuv. flake (from bladelet-core)',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*!40000 ALTER TABLE `lookups` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table photos
# ------------------------------------------------------------

DROP TABLE IF EXISTS `photos`;

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
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `idx_photos_PhotoID` (`PhotoID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `photos` WRITE;
/*!40000 ALTER TABLE `photos` DISABLE KEYS */;

INSERT INTO `photos` (`id`, `PhotoID`, `ArtefactID`, `LocusType`, `Locus`, `Filename`, `Photographer`, `Camera`, `Date`, `Notes`)
VALUES
	(0,'PH00000',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*!40000 ALTER TABLE `photos` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table transectcollectionpoints
# ------------------------------------------------------------

DROP TABLE IF EXISTS `transectcollectionpoints`;

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
  `Notes` longtext DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `idx_transectcollectionpoints_CollectionPointID` (`CollectionPointID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `transectcollectionpoints` WRITE;
/*!40000 ALTER TABLE `transectcollectionpoints` DISABLE KEYS */;

INSERT INTO `transectcollectionpoints` (`ID`, `CollectionPointID`, `Transect`, `Year`, `X`, `Y`, `Z`, `Vegetation`, `Slope`, `Notes`)
VALUES
	(1,'T01-1010','01','2013','25.345693','37.086655','156.29','<10%','<15','High density of flaked material - including microliths'),
	(2,'T01-1020','01','2013','25.34574','37.086717','152.656','50-90%','15-45','Zero visibility - point on rock amidst bushes (either side) quite representative of this immediate environment.'),
	(3,'T01-1030','01','2013','25.345699','37.086808','151.895','10-50%','15-45','Small-scale flake/blade (?) Material'),
	(4,'T01-1040','01','2013','25.34567','37.086919','144.58','>90%','15-45','Flakes of material'),
	(5,'T01-1050','01','2013','25.345672','37.086971','143.67','<10%','>45','Sample point on bedrock but culture material was naturally collected on \'shelf\'; mainly white flake material.'),
	(6,'T02-1010','02','2013','25.34581','37.086556','151.252','<10%','<15','Scatter of material 1-2m to the SW of this point. Sharp dropoff 5m to the North. Large bushes covering 10m squared to the NW. Lots of material along the 10m back to origin.'),
	(7,'T02-1020','02','2013','25.345821','37.086548','149.412','<10%','15-45',NULL),
	(8,'T02-1030','02','2013','25.345855','37.086542','143.494','50-90%','<15','Other material may be hidden under the bushes which populate this point'),
	(9,'T02-1040','02','2013','25.346119','37.086507','140.305','50-90%','<15','Absolutely no material seen between here and last point, come down to a nice collection point here. Large boulders to our E, S and W. Gradual slope to N, with lots of vegetation and frequent rocks.'),
	(10,'T02-1050','02','2013','25.346236','37.08648','139.375','50-90%','<15','Huge dropoff aproximately 5m to the N (drops approximately 15m)'),
	(11,'T02-1060','02','2013','25.346326','37.086487','136.248','<10%','>45','Point is directly on the face of the giant rocks. No cultural material, no vegetation, steep slope. No sidestep used.'),
	(12,'T03-1010','03','2013','25.34543','37.08666','141.9','50-90%','15-45',NULL),
	(13,'T03-1020','03','2013','25.34540833','37.08675833','147.2','>90%','15-45','Bush!'),
	(14,'T03-1030','03','2013','25.34538667','37.08687667','127.3','<10%','15-45',NULL),
	(15,'T03-1040','03','2013','25.34532667','37.08688333','127','>90%','15-45',NULL),
	(16,'T03-1050','03','2013','25.34538167','37.08699','142','>90%','15-45','Bit bushy.'),
	(17,'T03-1060','03','2013','25.34534833','37.08709333','136.8','>90%','15-45','Bushy.'),
	(18,'T03-1070','03','2013','25.34533833','37.08718167','134.9','10-50%','15-45',NULL),
	(19,'T04-0830','04','2013','25.3439893','37.08699541','94.815','50-90%','>45','Same as 840, slope gets steeper. Last station before houses.'),
	(20,'T04-0840','04','2013','25.34405451','37.08697337','99.14','>90%','15-45','Thick bushes, limited visibility.'),
	(21,'T04-0850','04','2013','25.34418343','37.0869877','104.908','10-50%','15-45','Clear patch with carpet of material.'),
	(22,'T04-0860','04','2013','25.34426976','37.08699248','106.591','>90%','15-45','Thick bushes, no visibility.'),
	(23,'T04-0870','04','2013','25.34436766','37.08696264','110.436','10-50%','15-45','Same as 880.');

/*!40000 ALTER TABLE `transectcollectionpoints` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table trenches
# ------------------------------------------------------------

DROP TABLE IF EXISTS `trenches`;

CREATE TABLE `trenches` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Trench` varchar(255) DEFAULT NULL,
  `Year` varchar(255) DEFAULT NULL,
  `Reasoning` longtext DEFAULT NULL,
  `Description` longtext DEFAULT NULL,
  `Supervisors` varchar(255) DEFAULT NULL,
  `Excavators` varchar(255) DEFAULT NULL,
  `DateOpened` varchar(255) DEFAULT NULL,
  `DateClosed` varchar(255) DEFAULT NULL,
  `ExcavatedCheck` int(1) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  UNIQUE KEY `idx_trenches_Trenches_Year` (`Trench`,`Year`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `trenches` WRITE;
/*!40000 ALTER TABLE `trenches` DISABLE KEYS */;

INSERT INTO `trenches` (`ID`, `Trench`, `Year`, `Reasoning`, `Description`, `Supervisors`, `Excavators`, `DateOpened`, `DateClosed`, `ExcavatedCheck`)
VALUES
	(0,'000','0000',NULL,NULL,NULL,NULL,NULL,NULL,NULL),
	(1,'001','2015','Trench established in an area believed to cap Pleistocene sediments/palaeosoil','Size 2x2 m, surrounded by larger boulders, and/or parts of bedrock emerging from the ground. Contains surface lithic collection, designated at Context 0001.','IK, SM','GS,RS','5/26/15','2007-02-15',1),
	(2,'002','2015','The trench was established where geoarchaeologists believed their might be a depth of original soil retained by a natural bedrock lip downslope to the south. It was still thought that any remnant soil would probably be culluvial in nature but that it would not have moved too far because of the bedrock lip acting as a retaining wall.','Trench was located on upper natural terrace of plot DG-A, upslope of a natural bedrock lip and terrace wall. The modern surface was covered with low vegetation and a lot of shattered lithic material both natural and artefacts. The trench is 2x2 meters.','IK','DM,BF,GS,RS','5/26/15','2007-02-15',1),
	(3,'003','2015','The trench was established where geoarchaeologists believed their might be a depth of original soil retained by a natural bedrock lip downslope to the south. It was still thought that any remnant soil would probably be culluvial in nature but that it would not have moved too far because of the bedrock lip acting as a retaining wall.','Two meter square trench located on uppermost slope of DG-A about halfway across the plot. Located due south of trenches 01, 02, and 04.','TC','SC','5/26/15','2007-02-15',1),
	(4,'004','2015','The trench was established where geoarchaeologists believed their might be a depth of original soil retained by a natural bedrock lip downslope to the south. It was still thought that any remnant soil would probably be culluvial in nature but that it would not have moved too far because of the bedrock lip acting as a retaining wall.','2x2 meter trench established on upper slope of DG-A upslope and two meters North East of trench 002 and between trenches 001 and 003. Low vegetation and artefacts on surface at start.','IK, SM','LP','2006-08-15','2007-02-15',1),
	(5,'005','2015','Trench DG-A/05 is a 2x2 m unit representing the lowest elevation of the topslope area (DG-A). The unit was selected  because of its largely flat topography, lack of vegetation, and it\'s location behind a seemingly natural accumulation of large (~1-2m) boulders, perhaps a natural sediment trap','A thick deposit of carbonate rich silts and clays affected by subaerial weathering and subaquic conditions. Trench is capped by two sequences of colluvial episodes.','JAH','BF','2006-09-15','2007-02-15',1);

/*!40000 ALTER TABLE `trenches` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
