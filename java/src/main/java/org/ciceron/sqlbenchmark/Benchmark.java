/*
 * SQL Benchmark
 * Copyright (C) 2018 Stephane Carrez
 * Written by Stephane Carrez (Stephane.Carrez@gmail.com)
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.ciceron.sqlbenchmark;

import com.mysql.jdbc.jdbc2.optional.MysqlDataSource;
import org.postgresql.ds.PGSimpleDataSource;
import org.sqlite.SQLiteConfig;
import org.sqlite.SQLiteDataSource;
import org.sqlite.SQLiteOpenMode;

import javax.sql.DataSource;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

public abstract class Benchmark {

    public static String getConfigPath(String name) {
        return "config/" + mDriverName + "-" + name;
    }

    static class Result {
        long count;
        long time;

        Result(long count, long time) {
            this.count = count;
            this.time = time;
        }
    }

    private static final TreeMap<String, Result> mResults = new TreeMap<String, Result>();
    private static String mDriverName;
    static DataSource mDataSource;
    static Connection mConnection;
    static final Properties mConfig = new Properties();
    static long mRepeat;
    private static long mBaseRepeat = 10;
    private String mTitle;
    private final int mFactor;

    public static void loadConfiguration(String path) throws IOException {
        FileInputStream input = new FileInputStream(path);
        mConfig.load(input);
        input.close();
    }

    public static void setBaseRepeat(long value) {
        mBaseRepeat = value;
    }

    public static boolean setDatabase(String driver) throws SQLException {
        String config = (String) mConfig.get(driver + ".database");

        mDriverName = driver;
        if ("mysql".equals(driver)) {

            // Load the MySQL driver.
            try {
                Class.forName("com.mysql.jdbc.Driver");
            } catch (Exception ex) {
                System.err.println("Loading MySQL-Driver failed!");
            }

            MysqlDataSource ds = new MysqlDataSource();
            ds.setURL(config);
            mDataSource = ds;
            mConnection = mDataSource.getConnection();
            mConnection.setAutoCommit(false);
            return true;
        }

        if ("postgresql".equals(driver)) {
            PGSimpleDataSource ds = new PGSimpleDataSource();

            ds.setURL(config);
            mDataSource = ds;
            mConnection = mDataSource.getConnection();
            mConnection.setAutoCommit(false);
            return true;
        }

        if ("sqlite".equals(driver)) {
            SQLiteDataSource ds = new SQLiteDataSource();
            SQLiteConfig sqliteConfig = new SQLiteConfig();
            sqliteConfig.setJournalMode(SQLiteConfig.JournalMode.WAL);
            sqliteConfig.setEncoding(SQLiteConfig.Encoding.UTF_8);
            sqliteConfig.setBusyTimeout(5000);
            sqliteConfig.setTransactionMode(SQLiteConfig.TransactionMode.EXCLUSIVE);
            sqliteConfig.setOpenMode(SQLiteOpenMode.READWRITE);
            sqliteConfig.setOpenMode(SQLiteOpenMode.CREATE);
            sqliteConfig.setOpenMode(SQLiteOpenMode.FULLMUTEX);

            ds.setConfig(sqliteConfig);
            ds.setUrl(config);
            mDataSource = ds;
            mConnection = mDataSource.getConnection();
            mConnection.setAutoCommit(false);
            return true;
        }
        return false;
    }

    static void readProcessInfo() {
        int thread_count = 0;
        int rss_size = 0;
        int hwm_size = 0;
        int user_time = 0;
        int sys_time = 0;
        try {
            File f = new File("/proc/self/status");

            BufferedReader b = new BufferedReader(new FileReader(f));

            String line;
            while ((line = b.readLine()) != null) {
                String[] items = line.split("\\s+");
                if (line.startsWith("Threads:") && items.length > 0) {
                    thread_count = Integer.parseInt(items[1]);
                } else if (line.startsWith("VmRSS:")) {
                    rss_size = Integer.parseInt(items[1]);
                } else if (line.startsWith("VmHWM:")) {
                    hwm_size = Integer.parseInt(items[1]);
                }
            }
        } catch (IOException e) {
            // Ignore since /proc/self/status is available only under GNU/Linux.
        }
        try {
            File f = new File("/proc/self/stat");

            BufferedReader b = new BufferedReader(new FileReader(f));

            String line;
            while ((line = b.readLine()) != null) {
                String[] items = line.split("\\s+");
                if (items.length > 14) {
                    user_time = 10 * Integer.parseInt(items[13]);
                    sys_time = 10 * Integer.parseInt(items[14]);
                    break;
                }
            }
        } catch (IOException e) {
            // Ignore since /proc/self/stat is available only under GNU/Linux.
        }
        System.out.println("<benchmark language='Java' driver='" + mDriverName
                + "' threads='" + thread_count + "' rss_size='" + rss_size
                + "' peek_rss_size='" + hwm_size + "' user_time='" + user_time
                + "' sys_time='" + sys_time + "'>");
    }

    static String formatTime(long time) {
        long us = time / 1000;
        long ms = us / 1000;
        if (ms > 1000) {
            long sec = ms / 1000;
            ms = ms % 1000;
            return String.format("%d.%03d s", sec, ms);
        } else if (ms > 100) {
            us = us % 1000;
            return String.format("%d.%d ms", ms, us / 100);
        } else if (ms > 10) {
            us = us % 1000;
            return String.format("%d.%02d ms", ms, us / 10);
        } else if (ms > 1) {
            us = us % 1000;
            return String.format("%d.%03d ms", ms, us);
        } else {
            return String.format("%d us", us);
        }
    }

    static void printReport(String output) throws IOException {
        if (output.length() > 0) {
            PrintStream o = new PrintStream(new File(output));
            System.setOut(o);
        }
        readProcessInfo();
        System.out.println("<measures title='SQL Benchmark'>");
        for (Map.Entry<String, Result> result : mResults.entrySet()) {
            System.out.println("<time count='" + result.getValue().count + "' time='"
            + formatTime(result.getValue().time / result.getValue().count) + "' total='"
            + formatTime(result.getValue().time) + "' title='" + result.getKey() + "' />");
        }
        System.out.println("</measures>");
        System.out.println("</benchmark>");
    }

    public static String getDriverName() {
        return mDriverName;
    }

    Benchmark() {
        mFactor = 100;
    }

    Benchmark(String title) {
        mTitle = title;
        mFactor = 100;
    }

    Benchmark(String title, int factor) {
        mTitle = title;
        mFactor = factor;
    }

    public Benchmark Set(String name) {
        mTitle = name;
        return this;
    }

    public String getName() {
        return mTitle;
    }

    public abstract void execute() throws SQLException;

    public void run() {

        long start = System.nanoTime();
        mRepeat = mFactor * mBaseRepeat;
        try {
            execute();
        } catch (SQLException ex) {
            System.err.println("Test " + getName() + " failed:");
            System.err.println("SQLException: " + ex.getMessage());
            System.err.println("SQLState: " + ex.getSQLState());
            System.err.println("VendorError: " + ex.getErrorCode());
            ex.printStackTrace();
        }
        long end = System.nanoTime();
        mResults.put(mTitle, new Result(mRepeat, end - start));
    }
}
