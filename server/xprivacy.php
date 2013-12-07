<?php
	$starttime = microtime(true);
	require_once('xprivacy.inc.php');

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
		require_once('xprivacy.inc.php');
		$db = new mysqli($db_host, $db_user, $db_password, $db_database);
		if ($db->connect_errno) {
			echo json_encode(array('ok' => false, 'error' => 'Unable to connect db'));
			exit();
		}
		$db->query("SET NAMES 'utf8'");

		// Store/update settings
		if (empty($action) || $action == 'submit') {
			// Validate data
			if (empty($data->protocol_version) || empty($data->settings)) {
				echo json_encode(array('ok' => false, 'error' => 'No data'));
				exit();
			}

			// Fixes
			if ($data->protocol_version <= 3)
				$data->android_id = md5($data->android_id);
			if (empty($data->application_name))
				$data->application_name = '';
			if (empty($data->package_version))
				$data->package_version = '';

			// Check if restrictions
			$empty = true;
			foreach ($data->settings as $restriction)
				if ($restriction->restricted) {
					$empty = false;
					break;
				}
			if ($empty) {
				echo json_encode(array('ok' => false, 'error' => 'No restrictions'));
				exit();
			}

			// Process restrictions
			foreach ($data->settings as $restriction) {
				if (empty($restriction->method))
					$restriction->method = '';
				$sql = "INSERT INTO xprivacy (android_id_md5, android_sdk, xprivacy_version, application_name, package_name, package_version,";
				$sql .= " restriction, method, restricted, used) VALUES ";
				$sql .= "('" . $data->android_id . "'";
				$sql .= "," . $db->real_escape_string($data->android_sdk) . "";
				$sql .= "," . (empty($data->xprivacy_version) ? 'NULL' : $db->real_escape_string($data->xprivacy_version)) . "";
				$sql .= ",'" . $db->real_escape_string($data->application_name) . "'";
				$sql .= ",'" . $db->real_escape_string($data->package_name) . "'";
				$sql .= ",'" . $db->real_escape_string($data->package_version) . "'";
				$sql .= ",'" . $db->real_escape_string($restriction->restriction) . "'";
				$sql .= ",'" . $db->real_escape_string($restriction->method) . "'";
				$sql .= "," . ($restriction->restricted ? 1 : 0);
				$sql .= "," . $db->real_escape_string($restriction->used) . ")";
				$sql .= " ON DUPLICATE KEY UPDATE";
				$sql .= " xprivacy_version=VALUES(xprivacy_version)";
				$sql .= ", application_name=VALUES(application_name)";
				$sql .= ", restricted=VALUES(restricted)";
				$sql .= ", used=VALUES(used)";
				$sql .= ", modified=CURRENT_TIMESTAMP()";
				$sql .= ", updates=updates+1";
				if (!$db->query($sql)) {
					$ok = false;
					break;
				}
			}

			// Send reponse
			echo json_encode(array('ok' => $ok, 'error' => $db->error));
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
			}
			else {
				$settings = Array();
				$sql = "SELECT restriction, method, restricted";
				$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
				$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
				$sql .= " FROM xprivacy";
				$sql .= " WHERE package_name = '" . $db->real_escape_string($data->package_name) . "'";
				$sql .= " GROUP BY restriction, method";
				$result = $db->query($sql);
				if ($result) {
					while (($row = $result->fetch_object())) {
						$entry = Array();
						$entry['restriction'] = $row->restriction;
						if (!empty($row->method))
							$entry['method'] = $row->method;
						$entry['restricted'] = $row->restricted;
						$entry['not_restricted'] = $row->not_restricted;
						$settings[] = (object) $entry;
					}
					$result->close();

					// Update number of fetches
					$sql = "UPDATE xprivacy";
					$sql .= " SET fetches=fetches+1";
					$sql .= ", accessed=CURRENT_TIMESTAMP()";
					$sql .= " WHERE package_name = '" . $db->real_escape_string($data->package_name) . "'";
					$db->query($sql);
				} else
					$ok = false;

				// Send reponse
				echo json_encode(array('ok' => $ok, 'error' => $db->error, 'settings' => $settings));
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
				echo '<pre>Error connecting to database</pre>';
				exit();
			}
			//$db->query("SET NAMES 'utf8'");
?>
			<div class="page-header">
<?php
			if (empty($package_name)) {
				$count = 0;
				$sql = "SELECT COUNT(DISTINCT application_name) AS count";
				$sql .= " FROM xprivacy";
				$result = $db->query($sql);
				if ($result) {
					$row = $result->fetch_object();
					if ($row)
						$count = $row->count;
					$result->close();
				}
?>
				<h1>XPrivacy</h1>
				<p>Crowd sourced restrictions</p>
				<p>This is a voting system for
					the <a href="https://github.com/M66B/XPrivacy#xprivacy">XPrivacy</a> restrictions.<br />
					Everybody using XPrivacy can submit his/her restriction settings.<br />
					With a <a href="http://www.faircode.eu/xprivacy">Pro license</a> you can fetch submitted restriction settings.<br />
					There are currently restriction settings for <?php echo $count; ?> applications submitted.
				</p>
<?php
			} else {
				$application_name = '---';
				$sql = "SELECT DISTINCT application_name";
				$sql .= " FROM xprivacy";
				$sql .= " WHERE package_name = '" . $db->real_escape_string($package_name) . "'";
				$result = $db->query($sql);
				if ($result) {
					$row = $result->fetch_object();
					if ($row)
						$application_name = $row->application_name;
					$result->close();
				}
?>
				<h1><?php echo htmlentities($application_name, ENT_COMPAT, 'UTF-8'); ?></h1>
				<span style="font-size: smaller;"><?php echo htmlentities($package_name, ENT_COMPAT, 'UTF-8'); ?></span>
				<p>
					<a href="http://wiki.faircode.eu/index.php?title=<?php echo urlencode($package_name); ?>" target="_blank">Wiki</a>
					-
					<a href="https://play.google.com/store/apps/details?id=<?php echo urlencode($package_name); ?>" target="_blank">Play store</a>
					-
					<a href="/xprivacy">Back</a>
				</p>
<?php
			}
?>
			</div>

			<div class="container">
<?php
			if (empty($package_name)) {
				echo '<p>';
				$sql = "SELECT DISTINCT LEFT(UCASE(application_name), 1) AS letter";
				$sql .= " FROM xprivacy";
				$sql .= " ORDER BY application_name";
				$result = $db->query($sql);
				if ($result) {
					while ($row = $result->fetch_object()) {
						echo '<a href="?letter=' . urlencode($row->letter) . '">';
						if ($row->letter == '')
							echo '---';
						else {
							$ent = htmlentities($row->letter, ENT_NOQUOTES, 'UTF-8');
							echo ($ent == '' ? '???' : $ent);
						}
						echo '</a> ';
					}
					$result->close();
				}
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
							<th style="display: none; text-align: center;" class="details">Used</th>
							<th>Restriction</th>
							<th style="display: none;" class="details">Method</th>
							<th style="display: none;" class="details">Last update (UTC)</th>
							<th style="display: none;" class="details">Last access (UTC)</th>
							<th style="display: none; text-align: center;" class="details">Updates</th>
							<th style="display: none; text-align: center;" class="details">Fetches</th>
<?php
						}
?>
						</tr>
					</thead>
					<tbody>
<?php
					$count = 0;
					$votes = 0;
					$fetches = 0;
					$records = 0;
					$first = null;
					$last = null;

					if (empty($package_name)) {
						// Get application list
						$letter = isset($_REQUEST['letter']) ? $_REQUEST['letter'] :'A';
						$sql = "SELECT DISTINCT application_name, package_name";
						$sql .= " FROM xprivacy";
						if ($letter == '')
							$sql .= " WHERE application_name = ''";
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
					} else {
						// Get application details
						$sql = "SELECT restriction, method, restricted";
						$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
						$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
						$sql .= ", MAX(used) AS used";
						$sql .= ", MAX(modified) AS modified";
						$sql .= ", MAX(accessed) AS accessed";
						$sql .= ", SUM(updates) AS updates";
						$sql .= ", MAX(fetches) AS fetches";
						$sql .= " FROM xprivacy";
						$sql .= " WHERE package_name = '" . $db->real_escape_string($package_name) . "'";
						$sql .= " GROUP BY restriction, method";
						$sql .= " ORDER BY restriction, method";
						$result = $db->query($sql);
						if ($result) {
							while (($row = $result->fetch_object())) {
								$count++;
								$votes += $row->restricted + $row->not_restricted;
								echo '<tr style="';
								if ($row->used)
									echo 'font-weight: bold;';
								if (!empty($row->method))
									echo 'display: none;';
								echo '"';
								if (!empty($row->method))
									echo ' class="details"';
								echo '>';

								echo '<td style="text-align: center;">';
								echo ($row->restricted < $row->not_restricted) ? '<span class="text-muted">' . $row->restricted . '</span>' : $row->restricted;
								echo ' / ';
								echo ($row->restricted > $row->not_restricted) ? '<span class="text-muted">' . $row->not_restricted . '</span>' : $row->not_restricted;
								echo '</td>';

								echo '<td style="display: none; text-align: center;" class="details">' . ($row->used ? 'Yes' : '') . '</td>';

								echo '<td>' . ($row->method ? '' :
								'<a href="http://wiki.faircode.eu/index.php?title=' . urlencode($row->restriction) . '" target="_blank">' .
								htmlentities($row->restriction, ENT_COMPAT, 'UTF-8') . '</a>') . '</td>';

								echo '<td style="display: none;" class="details">' . htmlentities($row->method, ENT_COMPAT, 'UTF-8') . '</td>';

								echo '<td style="display: none;" class="details">' . $row->modified . '</td>';
								echo '<td style="display: none;" class="details">' . $row->accessed . '</td>';
								echo '<td style="display: none; text-align: center;" class="details">' . ($row->updates > 1 ? $row->updates : '-'). '</td>';
								echo '<td style="display: none; text-align: center;" class="details">' . ($row->fetches > 0 ? $row->fetches : '-') . '</td>';
								echo '</tr>' . PHP_EOL;
							}
							$result->close();
						}
					}
?>
					</tbody>
				</table>
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
