<?php
	$starttime = microtime(true);
	require_once('xprivacy.inc.php');

	$max_confidence = 0.35;
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

	// Check if JSON request
	parse_str($_SERVER['QUERY_STRING']);
	if (!empty($format) && $format == 'json') {
		// Get data
		$ok = true;
		$body = file_get_contents('php://input');
		$data = json_decode($body);

		// Send header
		header('Content-Type: application/json');

		// Connect to database
		$db = new mysqli($db_host, $db_user, $db_password, $db_database);
		if ($db->connect_errno) {
			error_log('XPrivacy database connect: ' . $db->connect_error . PHP_EOL, 1, $my_email);
			echo json_encode(array('ok' => false, 'error' => 'Error connecting to database'));
			exit();
		}
		$db->query("SET NAMES 'utf8'");

		// Store/update settings
		if (empty($action) || $action == 'submit') {
			// Validate data
			if (empty($data->package_name)) {
				echo json_encode(array('ok' => false, 'error' => 'Package name missing'));
				exit();
			}

			// Fixes
			if (empty($data->protocol_version))
				$data->protocol_version = 0;
			if ($data->protocol_version <= 3)
				$data->android_id = md5($data->android_id);

			if (empty($data->application_name))
				$data->application_name = array('');
			else if (!is_array($data->application_name))
				$data->application_name = array($data->application_name);

			if (!is_array($data->package_name))
				$data->package_name = array($data->package_name);

			if (empty($data->package_version))
				$data->package_version = array('');
			else if (!is_array($data->package_version))
				$data->package_version = array($data->package_version);

			// Check if restrictions
			$empty = true;
			if (!empty($data->settings))
				foreach ($data->settings as $restriction)
					if ($restriction->restricted) {
						$empty = false;
						break;
					}
			if ($empty) {
				echo json_encode(array('ok' => false, 'error' => 'Restrictions missing'));
				exit();
			}

			// Process application
			foreach ($data->application_name as $application_name)
				for ($i = 0; $i < count($data->package_name); $i++) {
					$sql = "INSERT INTO xprivacy_app (application_name, package_name, package_version) VALUES ";
					$sql .= "('" . $db->real_escape_string($application_name) . "'";
					$sql .= ",'" . $db->real_escape_string($data->package_name[$i]) . "'";
					$sql .= ",'" . $db->real_escape_string($data->package_version[$i]) . "')";
					$sql .= " ON DUPLICATE KEY UPDATE";
					$sql .= " modified=CURRENT_TIMESTAMP()";
					if (!$db->query($sql)) {
						error_log('XPrivacy insert application: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
						$ok = false;
					}
				}

			// Process restrictions
			for ($i = 0; $i < count($data->package_name); $i++)
				foreach ($data->settings as $restriction) {
					if (empty($restriction->method))
						$restriction->method = '';
					$sql = "INSERT INTO xprivacy (android_id_md5, android_sdk, xprivacy_version, package_name, package_version,";
					$sql .= " restriction, method, restricted, used) VALUES ";
					$sql .= "('" . $data->android_id . "'";
					$sql .= "," . $db->real_escape_string($data->android_sdk) . "";
					$sql .= "," . (empty($data->xprivacy_version) ? 'NULL' : $db->real_escape_string($data->xprivacy_version)) . "";
					$sql .= ",'" . $db->real_escape_string($data->package_name[$i]) . "'";
					$sql .= ",'" . $db->real_escape_string($data->package_version[$i]) . "'";
					$sql .= ",'" . $db->real_escape_string($restriction->restriction) . "'";
					$sql .= ",'" . $db->real_escape_string($restriction->method) . "'";
					$sql .= "," . ($restriction->restricted ? 1 : 0);
					$sql .= "," . $db->real_escape_string($restriction->used) . ")";
					$sql .= " ON DUPLICATE KEY UPDATE";
					$sql .= " xprivacy_version=VALUES(xprivacy_version)";
					$sql .= ", restricted=VALUES(restricted)";
					$sql .= ", used=VALUES(used)";
					$sql .= ", modified=CURRENT_TIMESTAMP()";
					$sql .= ", updates=updates+1";
					if (!$db->query($sql)) {
						error_log('XPrivacy insert restrictions: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
						$ok = false;
					}
				}

			// Send reponse
			echo json_encode(array('ok' => $ok, 'error' => ($ok ? '' : 'Error storing restrictions')));
		}

		// Fetch settings
		else if (!empty($action) && $action == 'fetch') {
			// Check credentials
			$signature = '';
			if (openssl_sign($data->email, $signature, $private_key, OPENSSL_ALGO_SHA1))
				$signature = bin2hex($signature);

			if (empty($signature) || $signature != $data->signature) {
				if (!empty($data->email))
					error_log(date('c') . ' XPrivacy unauthorized: ' . $data->email . PHP_EOL, 1, $my_email);
				echo json_encode(array('ok' => false, 'error' => 'Not authorized'));
				exit();
			}
			else {
				// Validate
				if (empty($data->package_name)) {
					echo json_encode(array('ok' => false, 'error' => 'Package name missing'));
					exit();
				}

				// Fixes
				if (!is_array($data->package_name))
					$data->package_name = array($data->package_name);

				$max_ci = (empty($data->confidence) ? $max_confidence : $data->confidence / 100);

				$package_names = '';
				foreach ($data->package_name as $package_name) {
					if (!empty($package_names))
						$package_names .= ', ';
					$package_names .= "'" . $db->real_escape_string($package_name) . "'";
				}

				$settings = Array();
				$sql = "SELECT restriction, method, restricted";
				$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
				$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
				$sql .= " FROM xprivacy";
				$sql .= " WHERE package_name IN (" . $package_names . ")";
				$sql .= " GROUP BY restriction, method";
				$result = $db->query($sql);
				if ($result) {
					while (($row = $result->fetch_object())) {
						$ci = confidence($row->restricted, $row->not_restricted);
						if ($ci < $max_ci) {
							$entry = Array();
							$entry['restriction'] = $row->restriction;
							if (!empty($row->method))
								$entry['method'] = $row->method;
							$entry['restricted'] = ($row->restricted > $row->not_restricted ? 1 : 0);
							$entry['not_restricted'] = 0; // backward compatibility
							$settings[] = (object) $entry;
						}
					}
					$result->close();
				} else {
					error_log('XPrivacy fetch restrictions: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
					$ok = false;
				}

				// Send reponse
				echo json_encode(array('ok' => $ok, 'error' => ($ok ? '' : 'Error retrieving restrictions'), 'settings' => $settings));
			}
		}

		// Close database
		$db->close();

		// JSON done
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
		<link href="//netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" rel="stylesheet" media="screen">
		<style type="text/css">
			body { padding-left: 5px; padding-right: 5px; }
			th, tr, td { padding: 0px !important; }
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
					<a class="navbar-brand" href="http://updates.faircode.eu/xprivacy">XPrivacy</a>
				</div>
				<div class="navbar-collapse collapse">
					<ul class="nav navbar-nav">
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
				error_log('XPrivacy database connect: ' . $db->connect_error . PHP_EOL, 1, $my_email);
				echo '<pre>Error connecting to database</pre>';
				exit();
			}
			$db->query("SET NAMES 'utf8'");
?>
			<div class="page-header">
<?php
			$count = 0;
			$total = 0;

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
				error_log('XPrivacy query count: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);

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
				error_log('XPrivacy query total: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);

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
					error_log('XPrivacy query application names: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);

				// Get package names
				$apps = $application_names;
				for ($i = 0; $i < count($apps); $i++)
					$apps[$i] = "'" . $db->real_escape_string($apps[$i]) . "'";

				$package_names = array();
				if (count($apps)) {
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
						error_log('XPrivacy query package names: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
				}
?>
				<h2><?php echo htmlentities(implode(', ', $application_names), ENT_COMPAT, 'UTF-8'); ?></h2>
				<p style="font-size: smaller;">
<?php
				foreach ($package_names as $pname)
					echo '<a href="/xprivacy?package_name=' . urlencode($pname) . '">' . htmlentities($pname, ENT_COMPAT, 'UTF-8') . '</a> ';
?>
				</p>
				<p>
					<a href="/xprivacy">Home</a>
					-
					<a href="https://play.google.com/store/apps/details?id=<?php echo urlencode($package_name); ?>" target="_blank">Play store</a>
				</p>
<?php
			}
?>
				<p>This is a voting system for
					<a href="https://github.com/M66B/XPrivacy#xprivacy">XPrivacy</a> restrictions.<br />
					Everybody using XPrivacy can submit his/her restriction settings.<br />
					With a <a href="http://www.faircode.eu/xprivacy">Pro license</a> you can fetch submitted restriction settings.<br />
					There are currently <?php echo number_format($total, 0, '.', ','); ?> restriction settings
					for <?php echo number_format($count, 0, '.', ',') ?> applications submitted.
				</p>
			</div>

			<div class="container">
<?php
			if (empty($package_name)) {
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
				<p><a href="#" id="details">Show details</a></p>
<?php
			}
?>
				<table class="table table-hover table-condensed">
					<thead>
						<tr>
<?php
						if (empty($package_name)) {
?>
							<th>Application</th>
							<th>Package</th>
<?php
						} else {
?>
							<th style="text-align: center;">Votes<br />deny/allow</th>
							<th style="text-align: center;">CI95 &plusmn;% <sup>*</sup></th>
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
						// Get application list
						$letter = empty($_REQUEST['letter']) ? 'A' : $_REQUEST['letter'];
						$sql = "SELECT DISTINCT application_name, package_name";
						$sql .= " FROM xprivacy_app";
						if ($letter == '*')
							$sql .= " WHERE application_name REGEXP '^[^a-zA-Z]'";
						else
							$sql .= " WHERE application_name LIKE '" . ($letter == '%' ? '\\' : '') . $db->real_escape_string($letter) . "%'";
						$sql .= " ORDER BY application_name";
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
							error_log('XPrivacy query application list: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
					} else {
						// Get application details
						$sql = "SELECT restriction, method, restricted";
						$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
						$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
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

								echo '<tr style="';
								if (!empty($row->method))
									echo 'display: none;';
								if ($row->used)
									echo 'font-weight: bold;';
								if ($ci < $max_confidence && $row->restricted > $row->not_restricted)
									echo 'background: lightgray;';
								echo '"';
								if (!empty($row->method))
									echo ' class="details"';
								echo '>';

								echo '<td style="text-align: center;">';
								echo ($row->restricted < $row->not_restricted) ? '<span class="text-muted">' . $row->restricted . '</span>' : $row->restricted;
								echo ' / ';
								echo ($row->restricted > $row->not_restricted) ? '<span class="text-muted">' . $row->not_restricted . '</span>' : $row->not_restricted;
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
							error_log('XPrivacy query application details: ' . $db->error . ' query=' . $sql . PHP_EOL, 1, $my_email);
					}
?>
					</tbody>
				</table>
				<p>
					* Calculated using the
					<a href="http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti-Coull_Interval" target="_blank">Agresti-Coull interval</a>.<br />
					Values below <?php echo number_format($max_confidence * 100, 1); ?> % are considered reliable.<br />
					Rows marked with a grey background will be restricted when fetched.<br />
				</p>
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
				<p class="text-muted credit">&copy; 2013-<?php echo date("Y"); ?> <a href="http://blog.bokhorst.biz/about/" target="_blank">Marcel Bokhorst</a></p>
			</div>
<?php
		// Close database connection
		$db->close();
?>
		</div>

		<script src="http://code.jquery.com/jquery.js"></script>
		<script src="http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"></script>
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
		<!-- Piwik -->
		<script type="text/javascript">
		  var _paq = _paq || [];
		  _paq.push(["trackPageView"]);
		  _paq.push(["enableLinkTracking"]);

		  (function() {
			var u=(("https:" == document.location.protocol) ? "https" : "http") + "://piwik.bokhorst.biz/";
			_paq.push(["setTrackerUrl", u+"piwik.php"]);
			_paq.push(["setSiteId", "15"]);
			var d=document, g=d.createElement("script"), s=d.getElementsByTagName("script")[0]; g.type="text/javascript";
			g.defer=true; g.async=true; g.src=u+"piwik.js"; s.parentNode.insertBefore(g,s);
		  })();
		</script>
		<!-- End Piwik Code -->
	</body>
</html>
