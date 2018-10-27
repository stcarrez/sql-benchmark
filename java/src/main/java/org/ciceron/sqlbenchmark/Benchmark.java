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

import javax.sql.DataSource;
import java.io.FileInputStream;
import java.io.IOException;
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

    static void loadConfiguration(String path) throws IOException {
        FileInputStream input = new FileInputStream(path);
        mConfig.load(input);
        input.close();
    }

    static boolean setDatabase(String driver) throws SQLException {
        String config = (String) mConfig.get(driver + ".database");

        mDriverName = driver;
        if ("mysql".equals(driver)) {

            // Load the MySQL driver.
            try {
                Class.forName("com.mysql.jdbc.Driver").newInstance();
            } catch (Exception ex) {
                System.err.println("Loading MySQL-Driver failed!");
            }

            MysqlDataSource ds = new MysqlDataSource();
            ds.setURL(config);
            mDataSource = ds;
            mConnection = mDataSource.getConnection();
            return true;
        }
        return false;
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

    static void printReport() {
        System.out.println("<measures title='SQL Benchmark'>");
        for (Map.Entry<String, Result> result : mResults.entrySet()) {
            System.out.println("<time count='" + result.getValue().count + "' time='"
            + formatTime(result.getValue().time / result.getValue().count) + "' total='"
            + formatTime(result.getValue().time) + "' title='" + result.getKey() + "' />");
        }
        System.out.println("</measures>");
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

    public abstract void execute() throws SQLException;

    public void run() {

        long start = System.nanoTime();
        mRepeat = mFactor * mBaseRepeat;
        try {
            execute();
        } catch (SQLException ex) {

        }
        long end = System.nanoTime();
        mResults.put(mTitle, new Result(mRepeat, end - start));
    }
}
