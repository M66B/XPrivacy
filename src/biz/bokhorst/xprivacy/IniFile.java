package biz.bokhorst.xprivacy;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IniFile {

	private Map<String, String> mIni = new HashMap<String, String>();

	public IniFile(File file) throws IOException {
		String line;
		Pattern pattern = Pattern.compile("\\s*([^=]*)=(.*)");
		FileReader fr = new FileReader(file);
		BufferedReader br = new BufferedReader(fr);
		while ((line = br.readLine()) != null)
			if (!line.startsWith("#")) {
				Matcher matcher = pattern.matcher(line);
				if (matcher.matches()) {
					String key = matcher.group(1).trim();
					String value = matcher.group(2).trim();
					mIni.put(key, value);
				}
			}
		br.close();
		fr.close();
	}

	public String get(String key, String defaultvalue) {
		return mIni.get(key);
	}
}