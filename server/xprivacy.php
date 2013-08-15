<?php
	// Check if submit
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
			echo json_encode(array('ok' => false, 'error' => $db->connect_error));
			exit();
		}

		// Store/update settings
		if (empty($action) || $action == 'submit') {
			if ($data->protocol_version <= 3)
				$data->android_id = md5($data->android_id);
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

			if (empty($signature) || $signature != $data->signature)
				echo json_encode(array('ok' => $ok, 'error' => 'Not authorized'));
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

				} else
					$ok = false;

				// Send reponse
				echo json_encode(array('ok' => $ok, 'error' => $db->error, 'settings' => $settings));
			}
		}

		// Close database
		$db->close();

		// Done
		exit();
	}
?>
<!DOCTYPE html>
<html>
	<head>
		<title>XPrivacy</title>
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<meta name="description" content="XPrivacy">
		<meta name="author" content="M66B">
		<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
		<link href="http://netdna.bootstrapcdn.com/bootstrap/3.0.0-rc1/css/bootstrap.min.css" rel="stylesheet" media="screen">
		<style type="text/css">
			body { padding-left: 5px; padding-right: 5px; }
			th, tr, td { padding: 0px !important; }
		</style>
	</head>
	<body>
		<div class="container">
			<div class="navbar">
				<div class="container">
					<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".nav-collapse">
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
					</button>
					<a class="navbar-brand" href="/xprivacy">XPrivacy</a>
					<div class="nav-collapse collapse">
						<ul class="nav navbar-nav">
							<li><a href="http://forum.xda-developers.com/showthread.php?t=2320783" target="_blank">XDA</a></li>
							<li><a href="https://github.com/M66B/XPrivacy" target="_blank">GitHub</a></li>
						</ul>
					</div>
				</div>
			</div>

			<div class="page-header">
<?php		if (empty($package_name)) { ?>
				<h1>XPrivacy</h1>
<?php		} else { ?>
				<h1><?php echo htmlentities($application_name, ENT_COMPAT, 'UTF-8'); ?></h1>
				<span style="font-size: smaller;"><?php echo htmlentities($package_name, ENT_COMPAT, 'UTF-8'); ?></span>
				<p>
					<a href="http://wiki.faircode.eu/index.php?title=<?php echo urlencode($package_name); ?>" target="_blank">Wiki</a>
					-
					<a href="https://play.google.com/store/apps/details?id=<?php echo urlencode($package_name); ?>" target="_blank">Play store</a>
					-
					<a href="/xprivacy">Back</a>
				</p>
<?php		} ?>
			</div>

			<div class="container">
				<p><a href="#" id="details">Show details</a></p>
				<table class="table table-hover table-condensed">
					<thead>
						<tr>
<?php					if (empty($package_name)) { ?>
							<th style="text-align: center;">Votes</th>
							<th>Application</th>
							<th>Package</th>
							<th style="display: none;" class="details">Versions</th>
							<th style="display: none;" class="details">Last update (UTC)</th>
<?php					} else { ?>
							<th style="text-align: center;">Votes<br />deny/allow</th>
							<th style="display: none; text-align: center;" class="details">Used</th>
							<th>Restriction</th>
							<th style="display: none;" class="details">Method</th>
<?php					} ?>
						</tr>
					</thead>
					<tbody>
<?php
	$count = 0;
	$votes = 0;
	require_once('xprivacy.inc.php');
	$db = new mysqli($db_host, $db_user, $db_password, $db_database);
	if (!$db->connect_errno) {
		if (empty($package_name)) {
			$sql = "SELECT application_name, package_name,";
			$sql .= " COUNT(DISTINCT android_id_md5) AS count";
			$sql .= ", COUNT(DISTINCT package_version) AS versions";
			$sql .= ", MAX(modified) AS modified";
			$sql .= " FROM xprivacy";
			$sql .= " WHERE method = ''";
			$sql .= " GROUP BY package_name";
			$sql .= " ORDER BY application_name";
			$result = $db->query($sql);
			if ($result) {
				while (($row = $result->fetch_object())) {
					$count++;
					$votes += $row->count;
					$name = (empty($row->application_name) ? '???' : $row->application_name);
					echo '<tr>';
					echo '<td style="text-align: center;">' . $row->count . '</td>';

					echo '<td><a href="?application_name=' . urlencode($name);
					echo '&amp;package_name=' . urlencode($row->package_name) . '">';
					echo htmlentities($name, ENT_COMPAT, 'UTF-8') . '</a></td>';

					echo '<td>' . htmlentities($row->package_name, ENT_COMPAT, 'UTF-8') . '</td>';
					echo '<td style="display: none; text-align: center;" class="details">' . htmlentities($row->versions, ENT_COMPAT, 'UTF-8') . '</td>';
					echo '<td style="display: none;" class="details">' . $row->modified . '</td>';
					echo '</tr>' . PHP_EOL;
				}
				$result->close();
			}
			else
				echo $db->error;
		} else {
			$sql = "SELECT restriction, method, restricted";
			$sql .= ", SUM(CASE WHEN restricted = 1 THEN 1 ELSE 0 END) AS restricted";
			$sql .= ", SUM(CASE WHEN restricted != 1 THEN 1 ELSE 0 END) AS not_restricted";
			$sql .= ", MAX(used) AS used";
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
					echo '</tr>' . PHP_EOL;
				}
				$result->close();
			}
			else
				echo $db->error;
		}

		// Number of users
		$users = 0;
		if (empty($package_name)) {
			$sql = "SELECT COUNT(DISTINCT android_id_md5) AS users";
			$sql .= " FROM xprivacy";
			$sql .= " WHERE method = ''";
			$result = $db->query($sql);
			if ($result) {
				if (($row = $result->fetch_object()))
					$users = $row->users;
				$result->close();
			}
		}
		else {
			$sql = "SELECT COUNT(DISTINCT android_id_md5) AS users";
			$sql .= " FROM xprivacy";
			$sql .= " WHERE method = ''";
			$sql .= " AND package_name = '" . $db->real_escape_string($package_name) . "'";
			$result = $db->query($sql);
			if ($result) {
				if (($row = $result->fetch_object()))
					$users = $row->users;
				$result->close();
			}
		}
		$db->close();
	}
?>
					</tbody>
				</table>
				<p class="text-muted"><?php echo $count; ?> rows, <?php echo $votes; ?> votes, <?php echo $users; ?> users</p>
			</div>

			<div class="container">
				<p class="text-muted credit">&copy; 2013 <a href="http://blog.bokhorst.biz/about/" target="_blank">Marcel Bokhorst</a></p>
			</div>
		</div>

		<script src="http://code.jquery.com/jquery.js"></script>
		<script src="http://netdna.bootstrapcdn.com/bootstrap/3.0.0-rc1/js/bootstrap.min.js"></script>
		<script>
			jQuery(document).ready(function($) {
			  $('#details').click(function() {
				  $('.details').toggle();
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
