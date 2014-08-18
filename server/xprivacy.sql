-- phpMyAdmin SQL Dump
-- version 4.2.0
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Aug 16, 2014 at 12:13 PM
-- Server version: 5.6.19-1~dotdeb.1-log
-- PHP Version: 5.5.15-1~dotdeb.1

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
`id` int(11) NOT NULL,
  `android_id_md5` text NOT NULL,
  `android_sdk` int(11) NOT NULL,
  `xprivacy_version` int(11) DEFAULT NULL,
  `package_name` text NOT NULL,
  `package_version` text NOT NULL,
  `package_version_code` int(11) NOT NULL,
  `restriction` text NOT NULL,
  `method` text NOT NULL,
  `restricted` bit(1) NOT NULL,
  `allowed` int(11) NOT NULL DEFAULT '0',
  `used` bigint(13) NOT NULL,
  `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updates` int(11) NOT NULL DEFAULT '1'
) ENGINE=MyISAM  DEFAULT CHARSET=utf8 AUTO_INCREMENT=8170557 ;

-- --------------------------------------------------------

--
-- Table structure for table `xprivacy_app`
--

CREATE TABLE IF NOT EXISTS `xprivacy_app` (
`id` int(11) NOT NULL,
  `application_name` text CHARACTER SET utf8 NOT NULL,
  `package_name` text CHARACTER SET utf8 NOT NULL,
  `package_version` text CHARACTER SET utf8 NOT NULL,
  `package_version_code` int(11) NOT NULL,
  `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=54865 ;

-- --------------------------------------------------------

--
-- Table structure for table `xprivacy_update`
--

CREATE TABLE IF NOT EXISTS `xprivacy_update` (
`id` int(11) NOT NULL,
  `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `android_id_md5` text CHARACTER SET utf8,
  `installed_version` text CHARACTER SET utf8 NOT NULL,
  `test_versions` int(11) NOT NULL,
  `current_version` text CHARACTER SET utf8 NOT NULL
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=1256 ;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `xprivacy`
--
ALTER TABLE `xprivacy`
 ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `restriction` (`android_id_md5`(50),`android_sdk`,`package_name`(100),`package_version`(50),`package_version_code`,`restriction`(20),`method`(70)), ADD KEY `package` (`package_name`(100));

--
-- Indexes for table `xprivacy_app`
--
ALTER TABLE `xprivacy_app`
 ADD PRIMARY KEY (`id`), ADD UNIQUE KEY `application` (`application_name`(100),`package_name`(100),`package_version`(50),`package_version_code`);

--
-- Indexes for table `xprivacy_update`
--
ALTER TABLE `xprivacy_update`
 ADD PRIMARY KEY (`id`), ADD KEY `android_id` (`android_id_md5`(50));

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `xprivacy`
--
ALTER TABLE `xprivacy`
MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=8170557;
--
-- AUTO_INCREMENT for table `xprivacy_app`
--
ALTER TABLE `xprivacy_app`
MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=54865;
--
-- AUTO_INCREMENT for table `xprivacy_update`
--
ALTER TABLE `xprivacy_update`
MODIFY `id` int(11) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=1256;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
