#!/usr/bin/env python3
"""
SQLite Benchmark class
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import benchmark
import sqlite3

"""
SQLite Benchmark
"""
class SQLiteBenchmark(benchmark.Benchmark):
    _databaseUri = ''
    _database = ''

    @staticmethod
    def setup():
        database = benchmark.Benchmark._config.get('sqlite.database')
        if not database:
            raise Exception("Missing 'sqlite.database' configuration")
        if database.startswith("jdbc:sqlite:"):
            database = database[12:]
        benchmark.Benchmark._driver = 'sqlite'
        SQLiteBenchmark._databaseUri = database
        SQLiteBenchmark._database = sqlite3.connect(database)

    def connection(self):
        return SQLiteBenchmark._database

    def newConnection(self):
        return sqlite3.connect(SQLiteBenchmark._databaseUri)



