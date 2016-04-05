package com.cscie107;

import yahoofinance.Stock;
import yahoofinance.quotes.stock.StockQuote;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Created by pulkit on 4/5/16.
 */
public class Main {
    static SimpleDateFormat sdf = new SimpleDateFormat("MM-dd-yyyy");
    private static ScheduledExecutorService scheduler =
            Executors.newScheduledThreadPool(1);

    public static void main(String[] args) {
        String fileName = "stocks.csv";
        MonitoredStocks.getStocks();
        StockPriceExtractor spe = new StockPriceExtractor();
        scheduler.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                Map<String, Stock> stocks = spe.extractPrices(MonitoredStocks.getStocks());
                writeLiveStockData(stocks,fileName);
            }
        },0,15, TimeUnit.MINUTES);

    }

    private static void writeLiveStockData(Map<String, Stock> stocks,String fileName) {
        try(PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName, true)))) {
            for(Map.Entry<String,Stock> current : stocks.entrySet()){
                StockQuote quote = current.getValue().getQuote();
                if(quote.getLastTradeTime() !=null) {
                    out.println(current.getKey() + "," + quote.getPrice() + "," + sdf.format(quote.getLastTradeTime().getTime()) + "," + quote.getLastTradeTime().getTime() + "," + quote.getDayHigh() + "," + quote.getDayLow() + ","
                            + quote.getVolume() + "," + quote.getChangeFromYearHigh() + "," + quote.getChangeFromYearLow() + "," + quote.getOpen() + "," + quote.getChange() + ","
                            + quote.getPreviousClose());
                }
            }
        }catch (IOException e) {
            System.err.println("Unable to write to file"+e.getMessage());
        }
    }
}
