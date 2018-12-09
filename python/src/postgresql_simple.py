#!/usr/bin/env python3
"""
PostgreSQL Benchmark tests
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import psycopg2
import postgresql_benchmark

class SelectStatic(postgresql_benchmark.PostgreSQLBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "SELECT 1"

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
        stmt = db.cursor()
    
        for i in range(0, repeat):
            stmt.execute("SELECT 1")

class ConnectSelectStatic(postgresql_benchmark.PostgreSQLBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "CONNECT; SELECT 1; CLOSE"

    def execute(self):
        repeat = self.repeat()
    
        for i in range(0, repeat):
            db = self.newConnection()
            stmt = db.cursor()
            stmt.execute("SELECT 1")
            db.close()

class DropCreate(postgresql_benchmark.PostgreSQLBenchmark):
    def __init__(self):
        super().__init__()
        self.title = "DROP table; CREATE table"
        self.repeat_factor = 1
        with open('config/postgresql-create-table.sql') as f:
            self.create_sql = f.read()

    def execute(self):
        repeat = self.repeat()
        db = self.connection()
    
        for i in range(0, repeat):
            stmt = db.cursor()
            try:
                stmt.execute("DROP TABLE test_simple")
                db.commit()
            except:
                db.rollback()

            stmt.execute(self.create_sql)
            db.commit()

class Insert(postgresql_benchmark.PostgreSQLBenchmark):
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
        db.commit()

class SelectTable(postgresql_benchmark.PostgreSQLBenchmark):
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

def create():
    s = SelectStatic()
    return [SelectStatic(), ConnectSelectStatic(), DropCreate(), Insert(),
            SelectTable(1), SelectTable(10), SelectTable(100), SelectTable(500), SelectTable(1000)]

