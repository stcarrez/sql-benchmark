#!/usr/bin/env python3
"""
PostgreSQL Benchmark class
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import benchmark
import psycopg2

"""
PostgreSQL Benchmark
"""
class PostgreSQLBenchmark(benchmark.Benchmark):
    _databaseUri = ''
    _database = ''

    @staticmethod
    def setup():
        database = benchmark.Benchmark._config.get('postgresql.database')
        if not database:
            raise Exception("Missing 'postgresql.database' configuration")
        if database.startswith("jdbc:sqlite:"):
            database = database[12:]
        benchmark.Benchmark._driver = 'postgresql'
        PostgreSQLBenchmark._databaseUri = database
        PostgreSQLBenchmark._database = psycopg2.connect(database)

    def connection(self):
        return PostgreSQLBenchmark._database

    def newConnection(self):
        return psycopg2.connect(PostgreSQLBenchmark._databaseUri)



