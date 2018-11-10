#!/usr/bin/env python3
"""
Mysql Benchmark class
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import benchmark
import MySQLdb
import urllib.parse

"""
Mysql Benchmark
"""
class MysqlBenchmark(benchmark.Benchmark):
    _databaseUri = ''
    _database = ''
    _hostname = 'localhost'
    _port = 3306
    _dbname = ''
    _username = ''
    _password = ''

    @staticmethod
    def setup():
        database = benchmark.Benchmark._config.get('mysql.database')
        if not database:
            raise Exception("Missing 'mysql.database' configuration")
        if database.startswith("jdbc:mysql:"):
            database = database[11:]
        l = urllib.parse.urlparse(database)
        benchmark.Benchmark._driver = l.scheme
        MysqlBenchmark._hostname = l.hostname
        MysqlBenchmark._port = l.port
        MysqlBenchmark._dbname = l.path[1:]
        
        query = urllib.parse.parse_qs(l.query)
        value = query.get('user')
        if value:
            MysqlBenchmark._username = value[0]

        value = query.get('password')
        if value:
            MysqlBenchmark._password = value[0]

        MysqlBenchmark._database = MySQLdb.connect(host=MysqlBenchmark._hostname,
          port=MysqlBenchmark._port,
          user=MysqlBenchmark._username,
          passwd=MysqlBenchmark._password,
          db=MysqlBenchmark._dbname)

    def connection(self):
        return MysqlBenchmark._database

    def newConnection(self):
        return MySQLdb.connect(host=MysqlBenchmark._hostname,
          port=MysqlBenchmark._port,
          user=MysqlBenchmark._username,
          passwd=MysqlBenchmark._password,
          db=MysqlBenchmark._dbname)



