package com.cscie107;

import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

/**
 * Created by pulkit on 4/5/16.
 */
public class MonitoredStocks {
    static String[] stocks;
    static Properties extractorProperties = new Properties();
    static{
        try {
            extractorProperties.load(MonitoredStocks.class.getResourceAsStream("/extractor.properties"));
            String delimitedStocks = extractorProperties.getProperty("monitored_stocks");
            delimitedStocks = delimitedStocks.replaceAll("\"","");
            stocks = delimitedStocks.split(",");
        } catch (IOException e) {
            System.err.println("Error loading the resource file"+e.getMessage());
        }
    }

    public static String[] getStocks(){
        return stocks;
    }
}
