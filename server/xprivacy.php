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
		<link href="http://netdna.bootstrapcdn.com/bootstrap/3.0.0-rc1/css/bootstrap.min.css" rel="stylesheet" media="screen">
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
				<h1>XPrivacy</h1>
				</div>
					<table class="table">
						<thead>
							<tr>
								<th>SDK</th>
								<th>Application</th>
								<th>Package</th>
								<th>Version</th>
								<th>Count</th>
								<th>Date</th>
							</tr>
						</thead>
						<tbody>
<?php
	require_once('xprivacy.inc.php');
	$db = new mysqli($db_host, $db_user, $db_password, $db_database);
	if (!$db->connect_errno) {
		$sql = "SELECT android_sdk, application_name, package_name, package_version,";
		$sql .= " COUNT(DISTINCT android_id) AS count,";
		$sql .= " MAX(modified) AS modified";
		$sql .= " FROM xprivacy";
		$sql .= " GROUP BY android_sdk, package_name, package_version";
		$sql .= " ORDER BY package_name";
		$result = $db->query($sql);
		if ($result) {
			while (($row = $result->fetch_object())) {
				echo '<tr>';
				echo '<td>' . $row->android_sdk . '</td>';
				echo '<td>' . $row->application_name . '</td>';
				echo '<td>' . $row->package_name . '</td>';
				echo '<td>' . $row->package_version . '</td>';
				echo '<td>' . $row->count . '</td>';
				echo '<td>' . $row->modified . '</td></tr>';
			}
			$result->close();
		}
		$db->close();
	}
?>
						</tbody>
					</table>
			</div>

			<div class="container">
				<p class="text-muted credit">&copy; 2013 <a href="http://blog.bokhorst.biz/about/" target="_blank">Marcel Bokhorst</a></p>
			</div>
		</div>

		<script src="http://code.jquery.com/jquery.js"></script>
		<script src="http://netdna.bootstrapcdn.com/bootstrap/3.0.0-rc1/js/bootstrap.min.js"></script>
	</body>
</html>
