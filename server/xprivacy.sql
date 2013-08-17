-- phpMyAdmin SQL Dump
-- version 4.0.4.2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Aug 17, 2013 at 12:44 PM
-- Server version: 5.5.31-1~dotdeb.0
-- PHP Version: 5.3.27-1~dotdeb.0

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `updates.faircode.eu`
--

-- --------------------------------------------------------

--
-- Table structure for table `xprivacy`
--

CREATE TABLE IF NOT EXISTS `xprivacy` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `android_id_md5` text NOT NULL,
  `android_sdk` int(11) NOT NULL,
  `xprivacy_version` int(11) DEFAULT NULL,
  `application_name` text NOT NULL,
  `package_name` text NOT NULL,
  `package_version` text NOT NULL,
  `restriction` text NOT NULL,
  `method` text NOT NULL,
  `restricted` bit(1) NOT NULL,
  `used` bigint(13) NOT NULL,
  `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `accessed` timestamp NULL DEFAULT NULL,
  `updates` int(11) NOT NULL DEFAULT '1',
  `fetches` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `restriction` (`android_id_md5`(40),`android_sdk`,`package_name`(60),`package_version`(40),`restriction`(40),`method`(40))
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=182515 ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
