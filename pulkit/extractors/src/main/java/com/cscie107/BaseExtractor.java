package com.cscie107;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * Created by pulkit on 4/6/16.
 */
public abstract class BaseExtractor {
    private BlockingQueue<String> msgQueue = new LinkedBlockingQueue<>(50000);
    private String[] monitoredStocks;

    public BaseExtractor(String[] monitoredStocks) {
        this.monitoredStocks = monitoredStocks;
    }

    public abstract void setup();

    public BlockingQueue<String> getMessageQueue() {
        return msgQueue;
    }
    protected String[] getMonitoredStocks(){
        return monitoredStocks;
    }
    public abstract void stop();
}
