#!/usr/bin/env python3
"""
SQL Benchmark tests
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import argparse
import benchmark
import sqlite_benchmark
import sqlite_simple
import postgresql_benchmark
import postgresql_simple
import mysql_benchmark
import mysql_simple
import sys

parser = argparse.ArgumentParser(description='SQL Benchmark')
parser.add_argument('-repeat', dest='repeat', help='Repeat counter', type=int)
parser.add_argument('-sqlite', help='Run the SQLite benchmarks', action="store_true")
parser.add_argument('-postgresql', help='Run the PostgreSQL benchmarks', action="store_true")
parser.add_argument('-mysql', help='Run the Mysql benchmarks', action="store_true")
parser.add_argument('-o', dest='output', help='Write the result in the file')

if __name__ == '__main__':
    
    result = parser.parse_args()
    benchmark.Benchmark.read_config("sqlbench.properties")
    if result.repeat:
        benchmark.Benchmark._repeat_base = result.repeat

    if result.sqlite:
        sqlite_benchmark.SQLiteBenchmark.setup()
        tests = sqlite_simple.create()

    if result.postgresql:
        postgresql_benchmark.PostgreSQLBenchmark.setup()
        tests = postgresql_simple.create()

    if result.mysql:
        mysql_benchmark.MysqlBenchmark.setup()
        tests = mysql_simple.create()

    if not tests:
        print("Missing -sqlite, -postgresql or -mysql option", file=sys.stderr)
        sys.exit(1)

    for test in tests:
        test.run()

    if result.output:
        sys.stdout = open(result.output, 'w')

    benchmark.Benchmark.print_report()
