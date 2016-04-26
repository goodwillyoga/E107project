package com.cscie107;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.exception.ExceptionUtils;

import java.io.*;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Created by pulkit on 4/22/16.
 */
public class DateManipulator {

    static SimpleDateFormat dateFormater = new SimpleDateFormat("M/dd/yyyy");
    static SimpleDateFormat timeFormater = new SimpleDateFormat("h:mma");

    //Wed Apr 06 11:54:00 PDT 2016
    static SimpleDateFormat sdf2 = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy");

    static Map<String,String> stockYearHigh = new HashMap<>();
    static Map<String,String> stockYearLow = new HashMap<>();

    static {
        try {
            File actualExtractedFile = new File("/code/tot/Info/twitter-data/unprocessesed/stocks.csv");
            Reader reader = new FileReader(actualExtractedFile);
            CSVParser parser = new CSVParser(reader, CSVFormat.DEFAULT);
            for (final CSVRecord record : parser) {
                String symbol = record.get(0);
                String yearHigh = record.get(7);
                String yearLow = record.get(8);
                if(yearHigh !=null && !yearHigh.equals("N/A") && !yearHigh.equals("null")){
                    stockYearHigh.put(symbol,yearHigh);
                }
                if(yearLow !=null && !yearLow.equals("N/A") && !yearLow.equals("null")){
                    stockYearLow.put(symbol,yearLow);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
    public static void main(String[] args) throws Exception {
        timeFormater.setTimeZone(TimeZone.getTimeZone("America/New_York"));
        dateFormater.setTimeZone(TimeZone.getTimeZone("America/New_York"));


        File unprocessed = new File("/code/tot/Info/twitter-data/temp/");
        File processed = new File("/code/tot/Info/twitter-data/processesed");
        if (unprocessed.isDirectory()) {
            for (File f1 : unprocessed.listFiles(new FilenameFilter() {
                @Override
                public boolean accept(File dir, String name) {
                    if (name.startsWith("stocks.csv")) {
                        return true;
                    }
                    return false;
                }
            })) {
                System.out.println("processing file "+f1);
                String destFileName = f1.getAbsolutePath().replace("temp","processesed");
                try (PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(destFileName, true)))) {
                    Reader reader = new FileReader(f1);
                    final CSVParser parser = new CSVParser(reader, CSVFormat.DEFAULT);
                    try {
                        for (final CSVRecord record : parser) {
                            String symbol = record.get(0);
                            String price = record.get(1);
                            String lastTradeDate = record.get(2);
                            String lastTradeDateTime = record.get(3);
                            Date dt =  sdf2.parse(lastTradeDateTime);
                            lastTradeDate = dateFormater.format(dt);
                            lastTradeDateTime = timeFormater.format(dt);
                            String dayHigh = record.get(4);
                            String dayLow = record.get(5);
                            String volume = record.get(6);
                            String yearHigh = stockYearHigh.get(symbol);
                            String yearLow = stockYearLow.get(symbol);
                            String open = record.get(9);
                            String change = record.get(10);
                            String previousClose = record.get(11);

                            out.println(symbol+","+price+","+lastTradeDate+","+lastTradeDateTime+","+dayHigh+","+dayLow+","+volume+","+yearHigh+","+yearLow+","+open+","+previousClose);

                        }
                    } catch (Exception ex){
                        ex.printStackTrace();
                    }

                    finally {
                        parser.close();
                        reader.close();
                    }
                }
            }
        }
    }
}
