<?php
	$starttime = microtime(true);
	require_once('xprivacy.inc.php');

	$min_diff = 0.50;
	$max_confidence = 0.35;
	$max_packages = 72;

	function confidence($restricted, $not_restricted) {
		// Agresti-Coull Interval
		// http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti-Coull_Interval
		$n = $restricted + $not_restricted;
		$p = $restricted / $n;
		$z = 1.96; // 95%
		$n1 = $n + $z * $z;
		$p1 = (1 / $n1) * ($restricted + 0.5 * $z * $z);
		$ci = $z * sqrt((1 / $n1) * $p1 * (1- $p1));
		return $ci;
	}

	function log_error($message, $my_email, $data = null) {
		error_log(
			'XPrivacy message=' . $message . PHP_EOL . PHP_EOL .
			' data=' . print_r($data, true) . PHP_EOL . PHP_EOL .
			' request=' . print_r($_REQUEST, true) . PHP_EOL .
			' server=' . print_r($_SERVER, true) . PHP_EOL,
			1, $my_email);
	}

	// $_SERVER["HTTP_CONTENT_TYPE"]
	// max_input_vars
	// post_max_size
	// [suhosin]
	// suhosin.request.max_vars = 1000 # Default is 200
	// suhosin.post.max_vars = 1000 # Default is 200

	// Check if JSON request
	parse_str($_SERVER['QUERY_STRING']);
	if (!empty($format) && $format == 'json') {
		// Get data
		$ok = true;
		$body = file_get_contents('php://input');
		$data = json_decode($body);
		if (empty($body) || empty($data)) {
			log_error('json: empty request', $my_email, $data);
			header('Content-Type: application/json');
			echo json_encode(array('ok' => false, 'errno' => 101, 'error' => 'Empty request'));
			exit();
		}

		// Check XPrivacy version
		if (empty($data->xprivacy_version) || (int)$data->xprivacy_version < 219) {
			header('Content-Type: application/json');
			echo json_encode(array('ok' => false, 'errno' => 102, 'error' => 'Please upgrade to at least XPrivacy version 1.11'));
			exit();
		}

		// Connect to database
		$db = new mysqli($db_host, $db_user, $db_password, $db_database);
		if ($db->connect_errno) {
			log_error('json: database connect: ' . $db->connect_error, $my_email, $data);
			header('Content-Type: application/json');
			echo json_encode(array('ok' => false, 'errno' => 103, 'error' => 'Error connecting to database'));
			exit();
		}

		// Select character set
		$db->query("SET NAMES 'utf8'");

		// Store/update settings
		if (empty($action) || $action == 'submit') {
			header('Content-Type: application/json');

			// Validate
			if (empty($data->android_id)) {
				log_error('submit: Android ID missing', $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 202, 'error' => 'Android ID missing'));
				exit();
			}
			if (empty($data->package_name)) {
				log_error('submit: package name missing', $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 203, 'error' => 'Package name missing'));
				exit();
			}

			// Fixes
			if (empty($data->protocol_version) || $data->protocol_version <= 3)
				$data->android_id = md5($data->android_id);

			if (empty($data->application_name))
				$data->application_name = array('');
			else if (!is_array($data->application_name))
				$data->application_name = array($data->application_name);

			if (!is_array($data->package_name))
				$data->package_name = array($data->package_name);

			if (!empty($data->package_version))
				$data->package_version_name = $data->package_version; // legacy

			if (empty($data->package_version_name))
				$data->package_version_name = array('');
			else if (!is_array($data->package_version_name))
				$data->package_version_name = array($data->package_version_name);

			if (empty($data->package_version_code))
				$data->package_version_code = array();
			else if (!is_array($data->package_version_code))
				$data->package_version_code = array($data->package_version_code);

			// Validate
			if (count($data->package_name) > $max_packages) {
				log_error('submit: too many packages count=' . count($data->package_name) . '/' . $max_packages, $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 204, 'error' => 'Too many packages for application'));
				exit();
			}

			// Check restrictions
			if (!empty($data->settings)) {
				foreach ($data->settings as $restriction) {
					$found = false;

					if ($restriction->restriction == 'accounts')
						$found = true;
					else if ($restriction->restriction == 'browser')
						$found = true;
					else if ($restriction->restriction == 'calendar')
						$found = true;
					else if ($restriction->restriction == 'calling')
						$found = true;
					else if ($restriction->restriction == 'clipboard')
						$found = true;
					else if ($restriction->restriction == 'contacts')
						$found = true;
					else if ($restriction->restriction == 'dictionary')
						$found = true;
					else if ($restriction->restriction == 'email')
						$found = true;
					else if ($restriction->restriction == 'identification')
						$found = true;
					else if ($restriction->restriction == 'internet')
						$found = true;
					else if ($restriction->restriction == 'ipc')
						$found = true;
					else if ($restriction->restriction == 'location')
						$found = true;
					else if ($restriction->restriction == 'media')
						$found = true;
					else if ($restriction->restriction == 'messages')
						$found = true;
					else if ($restriction->restriction == 'network')
						$found = true;
					else if ($restriction->restriction == 'nfc')
						$found = true;
					else if ($restriction->restriction == 'notifications')
						$found = true;
					else if ($restriction->restriction == 'overlay')
						$found = true;
					else if ($restriction->restriction == 'phone')
						$found = true;
					else if ($restriction->restriction == 'sensors')
						$found = true;
					else if ($restriction->restriction == 'shell')
						$found = true;
					else if ($restriction->restriction == 'storage')
						$found = true;
					else if ($restriction->restriction == 'system')
						$found = true;
					else if ($restriction->restriction == 'view')
						$found = true;

					if (!$found) {
						if (empty($restriction->method))
							$name = $restriction->restriction;
						else
							$name = $restriction->restriction . '/' . $restriction->method;
						log_error('submit: restriction unknown: ' . $name , $my_email, $data);
					}
				}
			}

			// Process application
			foreach ($data->application_name as $application_name)
				for ($i = 0; $i < count($data->package_name); $i++) {
					$sql = "INSERT INTO xprivacy_app (application_name, package_name, package_version, package_version_code) VALUES ";
					$sql .= "('" . $db->real_escape_string($application_name) . "'";
					$sql .= ",'" . $db->real_escape_string($data->package_name[$i]) . "'";
					$sql .= ",'" . ($i < count($data->package_version_name) ? $db->real_escape_string($data->package_version_name[$i]) : '') . "'";
					$sql .= "," . ($i < count($data->package_version_code) ? (int)$data->package_version_code[$i] : 0) . ")";
					$sql .= " ON DUPLICATE KEY UPDATE";
					$sql .= " modified=CURRENT_TIMESTAMP()";
					if (!$db->query($sql)) {
						log_error('submit: insert application: ' . $db->error . ' query=' . $sql, $my_email, $data);
						$ok = false;
					}
				}

			// Process restrictions
			for ($i = 0; $i < count($data->package_name); $i++)
				foreach ($data->settings as $restriction) {
					// Fixes
					if (empty($restriction->method))
						$restriction->method = '';
					if (empty($restriction->allowed))
						$restriction->allowed = 0;

					$sql = "INSERT INTO xprivacy (android_id_md5, android_sdk, xprivacy_version,";
					$sql .= " package_name, package_version, package_version_code,";
					$sql .= " restriction, method, restricted, allowed, used) VALUES ";
					$sql .= "('" . $data->android_id . "'";
					$sql .= "," . $db->real_escape_string($data->android_sdk) . "";
					$sql .= "," . (empty($data->xprivacy_version) ? 'NULL' : (int)$data->xprivacy_version) . "";
					$sql .= ",'" . $db->real_escape_string($data->package_name[$i]) . "'";
					$sql .= ",'" . ($i < count($data->package_version_name) ? $db->real_escape_string($data->package_version_name[$i]) : '') . "'";
					$sql .= "," . ($i < count($data->package_version_code) ? (int)$data->package_version_code[$i] : 0);
					$sql .= ",'" . $db->real_escape_string($restriction->restriction) . "'";
					$sql .= ",'" . $db->real_escape_string($restriction->method) . "'";
					$sql .= "," . ($restriction->restricted ? 1 : 0);
					$sql .= "," . ((int)$restriction->allowed);
					$sql .= "," . ((int)$restriction->used) . ")";
					$sql .= " ON DUPLICATE KEY UPDATE";
					$sql .= " xprivacy_version=VALUES(xprivacy_version)";
					$sql .= ", restricted=VALUES(restricted)";
					$sql .= ", allowed=VALUES(allowed)";
					$sql .= ", used=VALUES(used)";
					$sql .= ", modified=CURRENT_TIMESTAMP()";
					$sql .= ", updates=updates+1";
					if (!$db->query($sql)) {
						log_error('submit: insert restrictions: error=' . $db->error . ' query=' . $sql, $my_email, $data);
						$ok = false;
					}
				}

			// Send reponse
			echo json_encode(array('ok' => $ok, 'errno' => ($ok ? 0 : 205), 'error' => ($ok ? '' : 'Error storing restrictions')));
			exit();
		}

		// Fetch settings
		else if (!empty($action) && $action == 'fetch') {
			header('Content-Type: application/json');

			// Check credentials
			$signature = '';
			if (openssl_sign($data->email, $signature, $private_key, OPENSSL_ALGO_SHA1))
				$signature = bin2hex($signature);

			if (empty($signature) || $signature != $data->signature) {
				//log_error('fetch: not authorized', $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 301, 'error' => 'Not authorized'));
				exit();
			}

			// Validate
			if (empty($data->package_name)) {
				log_error('fetch: package name missing', $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 303, 'error' => 'Package name missing'));
				exit();
			}

			// Fixes
			if (!is_array($data->package_name))
				$data->package_name = array($data->package_name);

			// Validate
			if (count($data->package_name) > $max_packages) {
				log_error('fetch: too many packages count=' . count($data->package_name) . '/' . $max_packages, $my_email, $data);
				echo json_encode(array('ok' => false, 'errno' => 304, 'error' => 'Too many packages for application'));
				exit();
			}

			// Get confidence interval
			$max_ci = (empty($data->confidence) ? $max_confidence : $data->confidence / 100);

			// Get requested package names
			$package_names = '';
			foreach ($data->package_name as $package_name) {
				if (!empty($package_names))
					$package_names .= ', ';
				$package_names .= "'" . $db->real_escape_string($package_name) . "'";
			}

			// Get restrictions
			$settings = Array();
			$empty = ($data->xprivacy_version >= 240);

			$sql = "SELECT restriction, method";
			$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
			$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
			$sql .= ", SUM(CASE WHEN allowed > 0 THEN 1 ELSE 0 END) AS allowed";
			$sql .= ", SUM(CASE WHEN allowed <= 0 THEN 1 ELSE 0 END) AS not_allowed";
			$sql .= " FROM xprivacy";
			$sql .= " WHERE package_name IN (" . $package_names . ")";
			$sql .= " GROUP BY restriction, method";
			$sql .= " ORDER BY restriction ASC, method ASC";
			$result = $db->query($sql);
			if ($result) {
				while (($row = $result->fetch_object())) {
					$ci = confidence($row->restricted, $row->not_restricted);
					$diff = $row->restricted / ($row->restricted + $row->not_restricted);
					$restrict = ($ci < $max_ci && $diff > $min_diff && $row->allowed <= $row->not_allowed);
					if ($restrict)
						$empty = false;

					$entry = Array();
					$entry['restriction'] = $row->restriction;
					if (!empty($row->method))
						$entry['method'] = $row->method;
					$entry['restricted'] = ($restrict ? 1 : 0);
					$entry['not_restricted'] = 0; // backward compatibility
					$settings[] = (object) $entry;
				}
				$result->close();
			} else {
				log_error('fetch: retrieve restrictions: ' . $db->error . ' query=' . $sql, $my_email, $data);
				$ok = false;
			}

			// Send reponse
			if ($ok && $empty)
				echo json_encode(array('ok' => false, 'errno' => 305, 'error' => 'No restrictions available', 'settings' => $settings));
			else
				echo json_encode(array('ok' => $ok, 'errno' => ($ok ? 0 : 306), 'error' => ($ok ? '' : 'Error retrieving restrictions'), 'settings' => $settings));
			exit();
		}

		// Update
		else if (!empty($action) && $action == 'update') {
			// Check credentials
			$signature = '';
			if (openssl_sign($data->email, $signature, $private_key, OPENSSL_ALGO_SHA1))
				$signature = bin2hex($signature);

			if (empty($signature) || $signature != $data->signature) {
				header($_SERVER['SERVER_PROTOCOL'] . ' 403 Forbidden');
				exit();
			}

			$folder = 'release';
			if (!empty($data->test_versions) && $data->test_versions)
				$folder = 'test';

			// Find latest version
			$latest = null;
			$files = glob($folder . '/XPrivacy_*.apk');
			if ($files)
				foreach ($files as $filename) {
					$version = explode('_', basename($filename, '.apk'))[1];
					if ($latest == null || version_compare($version, $latest) >= 0)
						$latest = $version;
				}

			$sql = "INSERT INTO xprivacy_update (installed_version, test_versions, current_version)";
			$sql .= " VALUES (";
			$sql .= "'" . $db->real_escape_string($data->xprivacy_version_name) . "'";
			$sql .= ", " . (int)$data->test_versions;
			$sql .= ", '" . $db->real_escape_string($latest) . "'";
			$sql .= ")";
			if (!$db->query($sql))
				log_error('update: error=' . $db->error . ' query=' . $sql, $my_email, $data);

			// Check if newer
			if ($latest == null || version_compare($latest, $data->xprivacy_version_name) <= 0)
				header($_SERVER['SERVER_PROTOCOL'] . ' 204 No Content');
			else {
				// Send latest
				$apk = $folder . '/XPrivacy_' . $latest . '.apk';
				header('Content-Type: application/octet-stream');
				header('Content-Description: File Transfer');
				header('Content-Disposition: attachment; filename=' . basename($apk));
				header('Expires: 0');
				header('Cache-Control: must-revalidate');
				header('Pragma: public');
				header('Content-Length: ' . filesize($apk));
				readfile($apk);
			}
			exit();
		}

		else {
			log_error('json: unknown action', $my_email, $data);
			echo json_encode(array('ok' => false, 'errno' => 104, 'error' => 'Unknown action: ' . $action));
			exit();
		}

		// Close database
		$db->close();

		// Failsafe
		exit();
	}
?>
<!DOCTYPE html>
<html>
	<head>
		<title>XPrivacy</title>
		<meta charset="utf-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge">
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<meta name="description" content="XPrivacy">
		<meta name="author" content="M66B">
		<link href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css" rel="stylesheet" media="screen">
		<style type="text/css">
			body { padding-left: 5px; padding-right: 5px; }
			th, tr, td { padding: 0px !important; }
			.page-header { margin-top: 0; }
			.action { margin-right: 20px; }
		</style>
	</head>
	<body>
		<div class="navbar navbar-default navbar-static-top" role="navigation">
			<div class="container">
				<div class="navbar-header">
					<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
						<span class="sr-only">Toggle navigation</span>
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
					</button>
					<a class="navbar-brand" href="/">Home</a>
				</div>
				<div class="navbar-collapse collapse">
					<ul class="nav navbar-nav">
						<li><a href="https://github.com/M66B/XPrivacy#xprivacy" target="_blank">XPrivacy</a></li>
						<li><a href="http://www.xprivacy.eu/" target="_blank">Pro license</a></li>
					</ul>
					<ul class="nav navbar-nav navbar-right">
						<li><a href="http://forum.faircode.eu/contact/" target="_blank">Contact</a></li>
						<li><a href="http://stats.pingdom.com/dcqg2snuqaf1/388316" target="_blank">Status</a></li>
						<li><a href="http://forum.xda-developers.com/showthread.php?t=2320783" target="_blank">Support</a></li>
						<li><a href="http://blog.bokhorst.biz/about/" target="_blank">About</a></li>
					</ul>
				</div>
			</div>
		</div>

		<div class="container">
<?php
			// Connect to database
			$db = new mysqli($db_host, $db_user, $db_password, $db_database);
			if ($db->connect_errno) {
				log_error('web: database connect: ' . $db->connect_error, $my_email);
				echo '<pre>Error connecting to database</pre>';
				exit();
			}

			// Select character set
			$db->query("SET NAMES 'utf8'");
?>
			<div class="page-header" style="overflow: hidden;">
				<div class="pull-right">
					<a href="http://www.blackducksoftware.com/news/releases/black-duck-announces-open-source-rookies-year-winners" target="_blank"><img style="width: 158px; height: 167px;" src="open-source-rookie-of-the-year.png" alt="" /></a>
				</div>
<?php
				$count = 0;
				$total = 0;

				// Get package count
				$sql = "SELECT COUNT(DISTINCT package_name) AS count";
				$sql .= " FROM xprivacy_app";
				$result = $db->query($sql);
				if ($result) {
					$row = $result->fetch_object();
					if ($row)
						$count = $row->count;
					$result->close();
				}
				else
					log_error('web: query count: ' . $db->error . ' query=' . $sql, $my_email);

				// Get restriction count
				$sql = "SELECT COUNT(*) AS total";
				$sql .= " FROM xprivacy";
				$result = $db->query($sql);
				if ($result) {
					$row = $result->fetch_object();
					if ($row)
						$total = $row->total;
					$result->close();
				}
				else
					log_error('web: query total: ' . $db->error . ' query=' . $sql, $my_email);

				// Display titles
				if (empty($package_name)) {
?>
					<h1>XPrivacy</h1>
					<p>Crowd sourced restrictions</p>
<?php
				} else {
					// Get application names
					$application_names = array();
					$sql = "SELECT DISTINCT application_name";
					$sql .= " FROM xprivacy_app";
					$sql .= " WHERE package_name = '" . $db->real_escape_string($package_name) . "'";
					$sql .= " ORDER BY application_name";
					$result = $db->query($sql);
					if ($result) {
						while (($row = $result->fetch_object()))
							$application_names[] = $row->application_name;
						$result->close();
					}
					else
						log_error('web: query application names: ' . $db->error . ' query=' . $sql, $my_email);

					// Get package names/versions
					$apps = $application_names;
					for ($i = 0; $i < count($apps); $i++)
						$apps[$i] = "'" . $db->real_escape_string($apps[$i]) . "'";

					$package_names = array();
					if (count($apps)) {
						// Get package names
						$sql = "SELECT DISTINCT package_name";
						$sql .= " FROM xprivacy_app";
						$sql .= " WHERE application_name IN (" . implode(',', $apps) . ")";
						$sql .= " ORDER BY package_name";
						$result = $db->query($sql);
						if ($result) {
							while (($row = $result->fetch_object()))
								$package_names[] = $row->package_name;
							$result->close();
						}
						else
							log_error('web: query package names: ' . $db->error . ' query=' . $sql, $my_email);
					}
?>
					<h2><?php echo htmlentities(implode(', ', $application_names), ENT_COMPAT, 'UTF-8'); ?></h2>

					<p style="font-size: smaller;">
<?php
					for ($i = 0; $i < count($package_names); $i++) {
						echo '<a href="/xprivacy?package_name=' . urlencode($package_names[$i]) . '">';
						echo htmlentities($package_names[$i], ENT_COMPAT, 'UTF-8') . '</a> ';
					}
?>
					</p>
<?php
				}
?>
			</div>

			<div class="page-header">
				<p>This is a voting system for
					<a href="https://github.com/M66B/XPrivacy#xprivacy">XPrivacy</a> restrictions.<br />
					Everybody using XPrivacy can submit his/her restriction settings.<br />
					With a <a href="http://www.xprivacy.eu/">Pro license</a> you can fetch submitted restriction settings.<br />
					There are currently <?php echo number_format($total, 0, '.', ','); ?> rules
					for <?php echo number_format($count, 0, '.', ',') ?> applications submitted.
				</p>
			</div>

			<div class="container">
<?php
				if (empty($package_name)) {
					// Display alphabet
					$letter = empty($_REQUEST['letter']) ? 'A' : $_REQUEST['letter'];
					echo '<p>';
					foreach (range('A', 'Z') as $alpha) {
						echo '<a href="?letter=' . $alpha . '"';
						if ($letter == $alpha)
							echo ' style="font-weight: bold;"';
						echo '>' . $alpha . '</a> ';
					}
					echo '<a href="?letter=*"';
					if ($letter == '*')
						echo ' style="font-weight: bold;"';
					echo '>other</a> ';
					echo '</p>';
				}
				else {
?>
					<div class="page-header">
						<p>
							<span class="glyphicon glyphicon-file"></span>
							<a class="action" href="#" id="details">Show details</a>
							<span class="glyphicon glyphicon-comment"></span>
							<a class="action" href="http://forum.faircode.eu/forums/forum/android/xprivacy/applications/?package_name=<?php echo urlencode($package_name); ?>" target="_blank">Discussion</a>
							<a class="action" href="https://play.google.com/store/apps/details?id=<?php echo urlencode($package_name); ?>" target="_blank"><img src="play_logo_x2.png" style="width:95px; height:20px" alt="Play store" /></a>
						</p>
					</div>
					<p style="font-size: smaller;">Rows marked with a <span style="background: lightgray;">grey background</span> will be restricted when fetched;
					<strong>bold text</strong> means data was used</p>
<?php
				}

				if (!empty($package_name)) {
					$last_version = '';
					$sql = "SELECT DISTINCT package_version";
					$sql .= " FROM xprivacy";
					$sql .= " WHERE package_name = '" . $db->real_escape_string($package_name) . "'";
					$sql .= " ORDER BY package_version";
					$result = $db->query($sql);
					if ($result) {
						echo '<p>';
						while (($row = $result->fetch_object())) {
							echo '<a href="?package_name=' . urlencode($package_name);
							echo '&amp;package_version=' . urlencode($row->package_version) . '"">';
							echo $row->package_version . '</a> ';
							if (version_compare($row->package_version, $last_version, '>'))
								$last_version = $row->package_version;
						}
						echo '</p>';
						$result->close();
					}

					if (empty($package_version))
						$package_version = $last_version;
				}
?>
				<table class="table table-hover table-condensed">
					<thead>
						<tr>
<?php
						// Display column titles
						if (empty($package_name)) {
?>
							<th>Application</th>
							<th>Package</th>
<?php
						} else {
?>
							<th style="text-align: center;">All versions<br />deny/allow <sup>*</sup></th>
							<th style="text-align: center;">Version <?php echo htmlentities($package_version, ENT_COMPAT, 'UTF-8'); ?><br />deny/allow</th>
							<th style="text-align: center;">Exceptions<br />(yes/no)</th>
							<th style="text-align: center;">CI95 &plusmn;% <sup>**</sup></th>
							<th style="display: none; text-align: center;" class="details">Used</th>
							<th>Restriction</th>
							<th style="display: none;" class="details">Method</th>
							<th style="display: none;" class="details">Last update (UTC)</th>
							<th style="display: none; text-align: center;" class="details">Updates</th>
<?php
						}
?>
						</tr>
					</thead>
					<tbody>
<?php
					$count = 0;
					$votes = 0;
					if (empty($package_name)) {
						// Display application list
						$letter = empty($_REQUEST['letter']) ? 'A' : $_REQUEST['letter'];
						$sql = "SELECT DISTINCT application_name, package_name";
						$sql .= " FROM xprivacy_app";
						if ($letter == '*')
							$sql .= " WHERE application_name REGEXP '^[^a-zA-Z]'";
						else
							$sql .= " WHERE application_name LIKE '" . ($letter == '%' ? '\\' : '') . $db->real_escape_string($letter) . "%'";
						$sql .= " ORDER BY application_name, package_name";
						$result = $db->query($sql);
						if ($result) {
							while (($row = $result->fetch_object())) {
								$count++;
								$name = (empty($row->application_name) ? '---' : $row->application_name);
								echo '<tr>';

								echo '<td><a href="?package_name=' . urlencode($row->package_name) . '">';
								echo htmlentities($name, ENT_COMPAT, 'UTF-8') . '</a></td>';

								echo '<td>' . htmlentities($row->package_name, ENT_COMPAT, 'UTF-8') . '</td>';
								echo '</tr>' . PHP_EOL;
							}
							$result->close();
						}
						else
							log_error('web: query application list: ' . $db->error . ' query=' . $sql, $my_email);
					} else {
						// Display application details
						$sql = "SELECT restriction, method";
						$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
						$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
						$sql .= ", SUM(CASE WHEN allowed > 0 THEN 1 ELSE 0 END) AS allowed";
						$sql .= ", SUM(CASE WHEN allowed <= 0 THEN 1 ELSE 0 END) AS not_allowed";
						$sql .= ", SUM(CASE WHEN restricted = 1 AND package_version = '" . $db->real_escape_string($package_version) . "' THEN 1 ELSE 0 END) AS restricted_package";
						$sql .= ", SUM(CASE WHEN restricted != 1 AND package_version = '" . $db->real_escape_string($package_version) . "' THEN 1 ELSE 0 END) AS not_restricted_package";
						$sql .= ", SUM(CASE WHEN allowed > 0 AND package_version = '" . $db->real_escape_string($package_version) . "' THEN 1 ELSE 0 END) AS allowed_package";
						$sql .= ", SUM(CASE WHEN allowed <= 0 AND package_version = '" . $db->real_escape_string($package_version) . "' THEN 1 ELSE 0 END) AS not_allowed_package";
						$sql .= ", MAX(used) AS used";
						$sql .= ", MAX(modified) AS modified";
						$sql .= ", SUM(updates) AS updates";
						$sql .= " FROM xprivacy";
						$sql .= " WHERE package_name = '" . $db->real_escape_string($package_name) . "'";
						$sql .= " GROUP BY restriction, method";
						$sql .= " ORDER BY restriction, method";
						$result = $db->query($sql);
						if ($result) {
							while (($row = $result->fetch_object())) {
								$count++;
								$votes += $row->restricted + $row->not_restricted;

								$ci = confidence($row->restricted, $row->not_restricted);
								$diff = $row->restricted / ($row->restricted + $row->not_restricted);
								$restrict = ($ci < $max_confidence && $diff > $min_diff && $row->allowed <= $row->not_allowed);

								echo '<tr style="';
								if (!empty($row->method))
									echo 'display: none;';
								if ($row->used)
									echo 'font-weight: bold;';
								if ($restrict)
									echo 'background: lightgray;';
								echo '"';
								if (!empty($row->method))
									echo ' class="details"';
								echo '>';

								echo '<td style="text-align: center;">';
								echo ($row->restricted < $row->not_restricted) ? '<span class="text-muted">' . $row->restricted . '</span>' : $row->restricted;
								echo ' / ';
								echo ($row->restricted > $row->not_restricted) ? '<span class="text-muted">' . $row->not_restricted . '</span>' : $row->not_restricted;
								echo ' <span style="font-size: smaller;">' . number_format($diff * 100, 0) . '%</span>';
								echo '</td>';

								echo '<td style="text-align: center; font-size: smaller;">';
								echo $row->restricted_package . ' / ' . $row->not_restricted_package;
								echo '</td>';

								echo '<td style="text-align: center;">';
								echo ($row->allowed . ' / ' . $row->not_allowed);
								echo '</td>';

								echo '<td style="text-align: center;">';
								echo number_format($ci * 100, 1);
								echo '</td>';

								echo '<td style="display: none; text-align: center;" class="details">' . ($row->used ? 'Yes' : '') . '</td>';

								echo '<td>' . ($row->method ? '' :
								'<a href="https://github.com/M66B/XPrivacy#' . urlencode($row->restriction) . '" target="_blank">' .
								htmlentities($row->restriction, ENT_COMPAT, 'UTF-8') . '</a>') . '</td>';

								echo '<td style="display: none;" class="details">' . htmlentities($row->method, ENT_COMPAT, 'UTF-8') . '</td>';

								echo '<td style="display: none;" class="details">' . $row->modified . '</td>';
								echo '<td style="display: none; text-align: center;" class="details">' . ($row->updates > 1 ? $row->updates : '-'). '</td>';
								echo '</tr>' . PHP_EOL;
							}
							$result->close();
						}
						else
							log_error('web: query application details: ' . $db->error . ' query=' . $sql, $my_email);
					}
?>
					</tbody>
				</table>
<?php
				if (!empty($package_name)) {
?>
					<p style="font-size: smaller;">
						* More than <?php echo number_format($min_diff * 100, 0); ?> % of the votes is required<br />
						** Calculated using a
						<a href="http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti-Coull_Interval" target="_blank">Agresti-Coull interval</a> of 95%;
						values below <?php echo number_format($max_confidence * 100, 1); ?> % are considered reliable.<br />
					</p>
<?php
				}
?>
			</div>

			<div class="container">
				<p class="text-muted">
<?php
				$elapse = round(microtime(true) - $starttime, 3);
				echo $count . ' entries ' . $elapse . ' seconds';
?>
				</p>
			</div>

			<div class="container">
				<a id="privacy_policy" href="#">Privacy policy</a>
				<p id="privacy_policy_text" style="display: none;">I will not, under any circumstances whatsoever, give out or sell your information to anyone, unless required by law.</p>
				<p class="text-muted credit">Copyright &copy; 2013&ndash;<?php echo date("Y"); ?> by <a href="http://blog.bokhorst.biz/about/" target="_blank">Marcel Bokhorst</a></p>
			</div>
<?php
		// Close database connection
		$db->close();
?>
		</div>

		<script src="//code.jquery.com/jquery-1.11.0.min.js"></script>
		<script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>
		<script>
			jQuery(document).ready(function($) {
				$('#details').click(function() {
					$('.details').toggle();
					return false;
				});
				$('#privacy_policy').click(function() {
					$('#privacy_policy_text').toggle();
					return false;
				});
			});
		</script>
	</body>
</html>
