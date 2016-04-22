package com.cscie107;

import org.json.JSONObject;

import java.io.*;

/**
 * Takes a files and attempts to read it line by line. Tries to concatenate the multiple lines untill we have a new line and then tries to parse it.
 * If there is a valid json it processes it
 */
public class Cleaner2 {
    public static void main(String[] args) throws IOException {
        int totalLines;

        File f11 = new File("/code/tot/Info/twitter-data/unprocessesed/newlineissue.json");
        File f22 = new File("/code/tot/Info/twitter-data/unprocessesed/newlineissue-mod.json");
        try (PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(f22, true)))) {
            StringBuilder concatenatedLine = new StringBuilder();
            try (BufferedReader br = new BufferedReader(new FileReader(f11))) {
                String line;
                while ((line = br.readLine()) != null) {
                    if (!line.endsWith("}")) {
                        concatenatedLine.append(line);
                        continue;
                    } else {
                        if(concatenatedLine.length() >0){
                            line = concatenatedLine.toString()+line;
                        }
                    }
                    concatenatedLine.setLength(0);
                    try {
                        JSONObject jObject = new JSONObject(line); // json
                        //JSONObject data = jObject.getJSONObject("created_at"); // get data object
                        out.println(line);

                    } catch (Exception ex) {
                        line = getCleanedString(line);
                        try {
                            JSONObject jObject = new JSONObject(line); // json
                            //JSONObject data = jObject.getJSONObject("created_at"); // get data object
                            out.println(line);

                        } catch (Exception ex1) {

                        }
                        System.out.println(line);
                        //System.exit(1);
                    }
                }
            }
        }
    }

    private static String getCleanedString(String line) {
        int startIndex = line.indexOf("source");
        int endIndex = line.indexOf("truncated");
        while (startIndex != -1 && endIndex != -1) {
            String newLine = line.substring(0, startIndex);
            String endLine = line.substring(endIndex, line.length());
            line = newLine + endLine;
            startIndex = line.indexOf("source");
            endIndex = line.indexOf("truncated", startIndex);
        }
        return line;
    }
}
