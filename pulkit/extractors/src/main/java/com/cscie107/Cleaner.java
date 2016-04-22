package com.cscie107;

import org.apache.commons.lang.StringEscapeUtils;
import org.json.JSONObject;

import java.io.*;

/**
 * Created by pulkit on 4/12/16.
 */
public class Cleaner {
    public static void main(String[] args) throws IOException {
        int totalLines;

        File f11 = new File("/code/tot/Info/twitter-data/unprocessesed/newlineissue.json");
        File f22 = new File("/code/tot/Info/twitter-data/unprocessesed/newlineissue-mod.json");
        try (PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(f22, true)))) {
            try (BufferedReader br = new BufferedReader(new FileReader(f11))) {
                String line;
                while ((line = br.readLine()) != null) {
                    line = getCleanedString(line);
                    try {
                        JSONObject jObject = new JSONObject(line); // json
                        //JSONObject data = jObject.getJSONObject("created_at"); // get data object

                    }catch(Exception ex){
                        System.out.println(line);
                        out.println(line);
                        //System.exit(1);
                    }
                }
            }
        }
        System.exit(1);
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

    private static String getCleanedString(String line) {
        int startIndex = line.indexOf("source");
        int endIndex = line.indexOf("truncated");
        while(startIndex != -1 && endIndex !=-1 ){
            String newLine = line.substring(0,startIndex);
            String endLine = line.substring(endIndex,line.length());
            line = newLine+endLine;
            startIndex = line.indexOf("source");
            endIndex = line.indexOf("truncated",startIndex);
        }
        return line;
    }
}
