/*
 * Main SQL Benchmark
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

import java.io.IOException;
import java.sql.SQLException;

public class Main {

    private static void Usage() {
        System.err.println("Usage: java -jar sql-benchmark.jar [-sqlite|-mysql|-postgresql] [-repeat N] "
                + "[-o file]");
        System.exit(2);
    }

    public static void main(String[] args) {
        String driver = "sqlite";
        String output = "";

        long repeat = 10;
        for (int i = 0; i < args.length; i++) {
            if ("-mysql".equals(args[i])) {
                driver = "mysql";
            } else if ("-sqlite".equals(args[i])) {
                driver = "sqlite";
            } else if ("-postgresql".equals(args[i])) {
                driver = "postgresql";
            } else if ("-repeat".equals(args[i])) {
                i++;
                if (i == args.length) {
                    System.err.println("Missing argument to -repeat option");
                    Usage();
                }
                try {
                    repeat = Long.parseLong(args[i]);
                } catch (NumberFormatException ex) {
                    System.err.println("Repeat count is not a number");
                    Usage();
                }
            } else if ("-o".equals(args[i])) {
                i++;
                if (i == args.length) {
                    System.err.println("Missing argument to -repeat option");
                    Usage();
                }
                output = args[i];

            } else {
                Usage();
            }
        }

        // Load the configuration file.
        try {
            Benchmark.loadConfiguration("sqlbench.properties");
        } catch (IOException ex) {
            System.err.println("Cannot load sqlbench.properties file");
            System.exit(1);
        }

        try {
            if (!Benchmark.setDatabase(driver)) {
                System.err.println("Cannot configure the database for driver: " + driver);
                System.exit(1);
            }
            Benchmark.setBaseRepeat(repeat);

        } catch (SQLException ex) {
            System.err.println("SQLException: " + ex.getMessage());
            System.err.println("SQLState: " + ex.getSQLState());
            System.err.println("VendorError: " + ex.getErrorCode());
            System.exit(1);
        }
        try {
            Benchmark[] tests = Simple.Create();

            for (Benchmark t : tests) {
                try {
                    t.run();

                } catch (Exception ex) {
                    System.err.println("Test " + t.getName() + " failed:");

                }
            }

        } catch (Exception e) {
            System.err.println(e);
        }

        try {
            Benchmark.printReport(output);
        } catch (IOException ex) {
            System.err.println(ex);
            System.exit(1);
        }
    }
}
