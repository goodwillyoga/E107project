package com.cscie107;

import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.text.StrBuilder;
import yahoofinance.Stock;
import yahoofinance.quotes.stock.StockQuote;

import java.io.*;
import java.nio.channels.FileChannel;
import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.net.URLDecoder;

/**
 * Created by pulkit on 4/5/16.
 */
public class Main {
    static SimpleDateFormat sdf = new SimpleDateFormat("MM-dd-yyyy");
    private static ScheduledExecutorService scheduler =
            Executors.newScheduledThreadPool(1);

    public static void main(String[] args) throws InterruptedException, IOException {
       String stocksFileName = "stocks.csv";
        String tweetsFileName = "twitter.json";
        String[] monitoredStocks = MonitoredStocks.getStocks();
        StockPriceExtractor spe = new StockPriceExtractor();
        StockTweetExtractor ste = new StockTweetExtractor(args[0], args[1], args[2], args[3],monitoredStocks);
        ste.setup();
        BlockingQueue<String> messages = ste.getMessageQueue();
        Thread t1 = new Thread(new Runnable() {
            @Override
            public void run() {
                FileOutputStream outputFile = null;
                try {
                    FileWriter fileWritter = new FileWriter(new File(tweetsFileName), true);
                    BufferedWriter bufferWritter = new BufferedWriter(fileWritter);
                    int count = 0;
                    while (true) {
                        String msg = null;
                        try {
                            msg = messages.take();
                            System.out.println(msg);
                            bufferWritter.write(StringEscapeUtils.unescapeJava(msg));
                            if (count == 200) {
                                count=0;
                                bufferWritter.flush();
                            } else {
                                count++;
                            }
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        //System.out.println(sbr.toString());
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });
        t1.start();
        scheduler.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                Map<String, Stock> stocks = spe.extractPrices(MonitoredStocks.getStocks());
                writeLiveStockData(stocks,stocksFileName);
            }
        },0,15, TimeUnit.MINUTES);

    }

    private static void writeLiveStockData(Map<String, Stock> stocks, String fileName) {
        try (PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName, true)))) {
            for (Map.Entry<String, Stock> current : stocks.entrySet()) {
                StockQuote quote = current.getValue().getQuote();
                if (quote.getLastTradeTime() != null) {
                    out.println(current.getKey() + "," + quote.getPrice() + "," + sdf.format(quote.getLastTradeTime().getTime()) + "," + quote.getLastTradeTime().getTime() + "," + quote.getDayHigh() + "," + quote.getDayLow() + ","
                            + quote.getVolume() + "," + quote.getChangeFromYearHigh() + "," + quote.getChangeFromYearLow() + "," + quote.getOpen() + "," + quote.getChange() + ","
                            + quote.getPreviousClose());
                }
            }
        } catch (IOException e) {
            System.err.println("Unable to write to file" + e.getMessage());
        }
    }
}
