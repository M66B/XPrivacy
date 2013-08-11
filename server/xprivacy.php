<?php
	// Check if submit
	parse_str($_SERVER['QUERY_STRING']);
	if (!empty($format) && $format == 'json') {
		// Get data
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

		// Store/update data
		$ok = true;
		foreach ($data->settings as $restriction) {
			if (empty($restriction->application_name))
				$restriction->application_name = '';
			if (empty($restriction->method))
				$restriction->method = '';
			$sql = "INSERT INTO xprivacy (android_id, android_sdk, application_name, package_name, package_version,";
			$sql .= " restriction, method, restricted, used) VALUES ";
			$sql .= "('" . $db->real_escape_string($data->android_id) . "'";
			$sql .= "," . $db->real_escape_string($data->android_sdk) . "";
			$sql .= ",'" . $db->real_escape_string($data->application_name) . "'";
			$sql .= ",'" . $db->real_escape_string($data->package_name) . "'";
			$sql .= ",'" . $db->real_escape_string($data->package_version) . "'";
			$sql .= ",'" . $db->real_escape_string($restriction->restriction) . "'";
			$sql .= ",'" . $db->real_escape_string($restriction->method) . "'";
			$sql .= "," . ($restriction->restricted ? 1 : 0);
			$sql .= "," . $db->real_escape_string($restriction->used) . ")";
			$sql .= " ON DUPLICATE KEY UPDATE";
			$sql .= " application_name=VALUES(application_name)";
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
				<p>
					<a href="http://wiki.faircode.eu/index.php?title=<?php echo urlencode($package_name); ?>" target="_blank">Wiki</a>
					-
					<a href="https://play.google.com/store/apps/details?id=<?php echo urlencode($package_name); ?>" target="_blank">Play store</a>
					<br />
					<span style="font-size: smaller;"><?php echo htmlentities($package_name, ENT_COMPAT, 'UTF-8'); ?></span>
				</p>
<?php		} ?>
			</div>

			<div class="container">
				<p><a href="#" id="details">Show details</a></p>
				<table class="table table-striped table-condensed">
					<thead>
						<tr>
<?php					if (empty($package_name)) { ?>
							<th>Application</th>
							<th>Package</th>
							<th>Version</th>
							<th style="text-align: center;">Votes</th>
							<th style="display: none;" class="details">Last update (UTC)</th>
<?php					} else { ?>
							<th>Restriction</th>
							<th style="display: none;" class="details">Method</th>
							<th style="text-align: center;">Votes (yes/no)</th>
							<th style="text-align: center;">Used</th>
<?php					} ?>
						</tr>
					</thead>
					<tbody>
<?php
	$count = 0;
	require_once('xprivacy.inc.php');
	$db = new mysqli($db_host, $db_user, $db_password, $db_database);
	if (!$db->connect_errno) {
		if (empty($package_name)) {
			$sql = "SELECT application_name, package_name, package_version,";
			$sql .= " COUNT(DISTINCT android_id) AS count";
			$sql .= ", MAX(modified) AS modified";
			$sql .= " FROM xprivacy";
			$sql .= " GROUP BY package_name, package_version";
			$sql .= " ORDER BY application_name";
			$result = $db->query($sql);
			if ($result) {
				while (($row = $result->fetch_object())) {
					$count++;
					echo '<tr>';
					echo '<td>' . htmlentities($row->application_name, ENT_COMPAT, 'UTF-8') . '</td>';
					echo '<td><a href="?application_name=' . urlencode($row->application_name);
					echo '&amp;package_name=' . urlencode($row->package_name) . '">';
					echo htmlentities($row->package_name, ENT_COMPAT, 'UTF-8') . '</a></td>';
					echo '<td>' . htmlentities($row->package_version, ENT_COMPAT, 'UTF-8') . '</td>';
					echo '<td style="text-align: center;">' . $row->count . '</td>';
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
			$sql .= " GROUP BY package_name, restriction, method";
			$sql .= " HAVING package_name = '" . $db->real_escape_string($package_name) . "'";
			$sql .= " ORDER BY restriction, method";
			$result = $db->query($sql);
			if ($result) {
				while (($row = $result->fetch_object())) {
					$count++;
					echo '<tr style="';
					if ($row->used)
						echo 'font-weight: bold;';
					if (!empty($row->method))
						echo 'display: none;';
					echo '"';
					if (!empty($row->method))
						echo ' class="details"';
					echo '>';
					echo '<td>' . ($row->method ? '' :
					'<a href="http://wiki.faircode.eu/index.php?title=' . urlencode($row->restriction) . '" target="_blank">' .
					htmlentities($row->restriction, ENT_COMPAT, 'UTF-8') . '</a>') . '</td>';
					echo '<td style="display: none;" class="details">' . htmlentities($row->method, ENT_COMPAT, 'UTF-8') . '</td>';
					echo '<td style="text-align: center;">';
					echo ($row->restricted < $row->not_restricted) ? '<span class="text-muted">' . $row->restricted . '</span>' : $row->restricted;
					echo ' / ';
					echo ($row->restricted > $row->not_restricted) ? '<span class="text-muted">' . $row->not_restricted . '</span>' : $row->not_restricted;
					echo '</td>';
					echo '<td style="text-align: center;">' . ($row->used ? 'Yes' : '') . '</td>';
					echo '</tr>' . PHP_EOL;
				}
				$result->close();
			}
			else
				echo $db->error;
		}
		$db->close();
	}
?>
					</tbody>
				</table>
				<p class="text-muted"><?php echo $count; ?> rows</p>
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
