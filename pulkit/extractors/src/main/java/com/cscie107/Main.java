package com.cscie107;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.concurrent.BlockingQueue;

/**
 * Created by pulkit on 4/5/16.
 */
public class Main {
    static SimpleDateFormat sdf = new SimpleDateFormat("MM-dd-yyyy");

    public static void main(String[] args) throws InterruptedException, IOException {
        String tweetsFileName = "twitter.json";
        String[] monitoredStocks = MonitoredStocks.getStocks();
        StockTweetExtractor ste = new StockTweetExtractor("p2x0SjCGtWUu6NQO9xDrJrfNR",
                "V8b9SpJnHAuzzDTkaD5ty8k4qcR8jykyof4CpuNm05Aye375wk",
                "17918415-3Fn11Si5hu2AL0SEfQPdMMYn5nPBGvPHOROBSIFkC",
                "3Eyt1WMz6HGVRgHE8n6N0p7P0ghfSJ58RIsxXoGMWdfXu",monitoredStocks);
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
                            bufferWritter.write(msg);
                            if (count == 200) {
                                count = 0;
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
    }
}
