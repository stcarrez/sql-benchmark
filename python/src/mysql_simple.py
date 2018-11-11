#!/usr/bin/env python3
"""
Mysql Benchmark tests
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import MySQLdb
import mysql_benchmark

class DoStatic(mysql_benchmark.MysqlBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "DO 1"

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        stmt = db.cursor()
    
        for i in range(0, repeat):
            stmt.execute("DO 1")
        stmt.close()

class SelectStatic(mysql_benchmark.MysqlBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "SELECT 1"

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        stmt = db.cursor()
    
        for i in range(0, repeat):
            stmt.execute("SELECT 1")
        stmt.close()

class ConnectSelectStatic(mysql_benchmark.MysqlBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "CONNECT; SELECT 1; CLOSE"

    def execute(self):
        repeat = self.repeat()
    
        for i in range(0, repeat):
            db = self.newConnection()
            stmt = db.cursor()
            stmt.execute("SELECT 1")
            stmt.close()
            db.close()

class DropCreate(mysql_benchmark.MysqlBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "DROP table; CREATE table"
        self.repeat_factor = 1
        with open('config/mysql-create-table.sql') as f:
            self.create_sql = f.read()

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        drop_stmt = db.cursor()
        create_stmt = db.cursor()
    
        for i in range(0, repeat):
            try:
                drop_stmt.execute("DROP TABLE test_simple")
                db.commit()
            except:
                pass

            create_stmt.execute(self.create_sql)
            db.commit()

        drop_stmt.close()
        create_stmt.close()

class Insert(mysql_benchmark.MysqlBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "INSERT INTO table"
        self.repeat_factor = 10

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        stmt = db.cursor()

        for i in range(0, repeat):
            stmt.execute("INSERT INTO test_simple (value) VALUES (1)")

        stmt.close()
        db.commit()

class SelectTable(mysql_benchmark.MysqlBenchmark):
    def __init__(self, count):
        super().__init__()
        self.title = "SELECT * FROM table LIMIT " + str(count)
        self.sql = "SELECT * FROM test_simple LIMIT " + str(count)
        self.expect_count = count

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        stmt = db.cursor()

        for i in range(0, repeat):
            stmt.execute(self.sql)
            row_count = 0
            for row in stmt:
                row_count = row_count + 1

            if row_count != self.expect_count:
                raise Exception('Invalid result count:' + str(row_count))

        stmt.close()

def create():
    s = SelectStatic()
    return [DoStatic(), SelectStatic(), ConnectSelectStatic(), DropCreate(), Insert(),
            SelectTable(1), SelectTable(10), SelectTable(100), SelectTable(500), SelectTable(1000)]

