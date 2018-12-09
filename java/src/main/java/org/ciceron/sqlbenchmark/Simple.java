/*
 * Simple SQL Benchmark
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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class Simple {

    private static String createSQL;

    public static Benchmark[] Create() throws IOException {

        byte[] encoded = Files.readAllBytes(Paths.get(Benchmark.getConfigPath("create-table.sql")));
        createSQL = new String(encoded, StandardCharsets.UTF_8);

        if ("sqlite".equals(Benchmark.getDriverName())) {
            return new Benchmark[]{
                    new Select_Static(),
                    new Connect_Select_Static(),
                    new Drop_Create(),
                    new Insert(),
                    new Select_Table(1),
                    new Select_Table(10),
                    new Select_Table(100),
                    new Select_Table(500),
                    new Select_Table(1000)
            };
        }
        if ("postgresql".equals(Benchmark.getDriverName())) {
            return new Benchmark[] {
                    new Select_Static(),
                    new Connect_Select_Static(),
                    new Drop_Create(),
                    new Insert(),
                    new Select_Table(1),
                    new Select_Table(10),
                    new Select_Table(100),
                    new Select_Table(500),
                    new Select_Table(1000)
            };
        }
        return new Benchmark[] {
                new Do_Static(),
                new Select_Static(),
                new Connect_Select_Static(),
                new Drop_Create(),
                new Insert(),
                new Select_Table(1),
                new Select_Table(10),
                new Select_Table(100),
                new Select_Table(500),
                new Select_Table(1000)
        };
    }

    private static class Do_Static extends Benchmark {
        Do_Static() {
            super("DO 1");
        }

        @Override
        public void execute() throws SQLException {
            Statement stmt = mConnection.createStatement();

            for (int i = 0; i < mRepeat; i++) {
                stmt.execute("DO 1");
            }
            stmt.close();
        }
    }

    private static class Select_Static extends Benchmark {
        Select_Static() {
            super("SELECT 1");
        }

        @Override
        public void execute() throws SQLException {
            Statement stmt = mConnection.createStatement();

            for (int i = 0; i < mRepeat; i++) {
                stmt.execute("SELECT 1");
            }
            stmt.close();
        }
    }

    private static class Connect_Select_Static extends Benchmark {
        Connect_Select_Static() {
            super("CONNECT; SELECT 1; CLOSE");
        }

        @Override
        public void execute() throws SQLException {

            for (int i = 0; i < mRepeat; i++) {
                Connection conn = mDataSource.getConnection();
                Statement stmt = conn.createStatement();
                stmt.execute("SELECT 1");
                stmt.close();
                conn.close();
            }
        }
    }

    private static class Drop_Create extends Benchmark {
        Drop_Create() {
            super("DROP table; CREATE table", 1);
        }

        @Override
        public void execute() throws SQLException {

            for (int i = 0; i < mRepeat; i++) {
                Statement dropStmt = null;
                try {
                    dropStmt = mConnection.createStatement();
                    dropStmt.execute("DROP TABLE IF EXISTS test_simple");
                    mConnection.commit();
                } catch (SQLException ex) {

                }
                if (dropStmt != null) {
                    dropStmt.close();
                }
                Statement createStmt = mConnection.createStatement();
                createStmt.execute(createSQL);
                createStmt.close();
                mConnection.commit();
            }
        }
    }

    private static class Insert extends Benchmark {
        Insert() {
            super("INSERT INTO table", 10);
        }

        @Override
        public void execute() throws SQLException {
            PreparedStatement insertStmt
                    = mConnection.prepareStatement("INSERT INTO test_simple (value) VALUES (1)");

            for (int i = 0; i < mRepeat; i++) {
                insertStmt.execute();
            }
            insertStmt.close();
	    mConnection.commit();
        }
    }

    private static class Select_Table extends Benchmark {
        private final int mExpectCount;

        Select_Table(int count) {
            super("SELECT * FROM table LIMIT " + count);
            mExpectCount = count;
        }

        @Override
        public void execute() throws SQLException {
            PreparedStatement stmt
                    = mConnection.prepareStatement("SELECT * FROM test_simple LIMIT " + mExpectCount);

            for (int i = 0; i < mRepeat; i++) {
                if (stmt.execute()) {
                    ResultSet rs = stmt.getResultSet();
                    int count = 0;
                    while (rs.next()) {
                        count++;
                    }
                    rs.close();
                    if (count != mExpectCount) {
                        throw new SQLException("Invalid result count: " + count);
                    }
                } else {
                    throw new SQLException("No result");
                }
            }
            stmt.close();
        }
    }

}
