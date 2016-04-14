package com.cscie107;

import org.apache.commons.lang.StringEscapeUtils;

import java.io.*;

/**
 * Created by pulkit on 4/12/16.
 */
public class Cleaner {
    public static void main(String[] args) throws IOException {

        File unprocessed = new File("/code/tot/Info/twitter-data/unprocessesed/");
        File processed = new File("/code/tot/Info/twitter-data/processesed");

        if (unprocessed.isDirectory()) {
            for (File f1 : unprocessed.listFiles(new FilenameFilter() {
                @Override
                public boolean accept(File dir, String name) {
                    if (name.startsWith("twitter")) {
                        return true;
                    }
                    return false;
                }
            })) {
                String destFileName = f1.getAbsolutePath().replace("unprocessesed","processesed");
                try (PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(destFileName, true)))) {
                    try (BufferedReader br = new BufferedReader(new FileReader(f1))) {
                        String line;
                        while ((line = br.readLine()) != null) {
                            String str = StringEscapeUtils.unescapeJava(line);
                            out.println(str.replaceAll("\n", ""));
                        }
                    }
                }
            }
        }
    }
}
