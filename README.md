# Overview

The *sql-benchmark* provides simple benchmark of several database drivers in several languages.
Benchmarks are intended to be simple so that they can be implemented easily for several
languages and SQL databases.  Some of the goals are:

* Evaluate the performance of a database driver,
* Compare the performance of different languages when connecting to a database,
* Have a rough comparison on simple SQL queries on different databases.

# Languages

* [Java Benchmark](https://github.com/stcarrez/sql-benchmark/tree/master/java)
* [Ada Benchmark](https://github.com/stcarrez/sql-benchmark/tree/master/ado)
* [Python Benchmark](https://github.com/stcarrez/sql-benchmark/tree/master/python)

# Databases

Three SQL databases are supported:

* SQLite,
* MySQL,
* PostgreSQL

Before running the SQL benchmark of MySQL and PostgreSQL, you must create the
`sqlbench` database and give access to the `sqlbench` user.

The SQLite database is created automatically.

## Postgresql setup

To create manually the database, you can proceed to the following steps:

1. Create the 'sqlbench' user and configure the password
(enter 'sqlbench' for the password or update the configuration sqlbench.properties file):

`
sudo -u postgres createuser sqlbench --pwprompt
`

2. Create the 'sqlbench' database in Postgresql

`
sudo -u postgres createdb -O sqlbench sqlbench
`

# Results

## CONNECT; SELECT 1; CLOSE

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  272.1 us     |  5.509 ms     |  22.73 us     |
| Java                  |  944.4 us     |  9.926 ms     |  197.9 us     |

## DO 1

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  33.46 us     |               |               |
| Java                  |  63.05 us     |               |               |

## DROP table; CREATE table

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  509.8 ms     |  75.13 ms     |  219.6 us     |
| Java                  |  542.5 ms     |  68.13 ms     |  2.920 ms     |

## INSERT INTO table

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  52.40 ms     |  11.71 ms     |  73.83 us     |
| Java                  |  49.98 ms     |  12.03 ms     |  146.3 us     |

## SELECT * FROM table LIMIT 1

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  25.93 us     |  125.4 us     |  27.63 us     |
| Java                  |  60.28 us     |  87.43 us     |  4.969 us     |

## SELECT * FROM table LIMIT 10

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  41.82 us     |  132.5 us     |  28.97 us     |
| Java                  |  66.07 us     |  113.4 us     |  5.098 us     |

## SELECT * FROM table LIMIT 100

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  48.02 us     |  205.8 us     |  40.56 us     |
| Java                  |  66.92 us     |  209.6 us     |  15.48 us     |

## SELECT * FROM table LIMIT 500

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  96.71 us     |  346.4 us     |  86.12 us     |
| Java                  |  157.2 us     |  572.8 us     |  63.20 us     |

## SELECT * FROM table LIMIT 1000

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  144.9 us     |  483.8 us     |  147.4 us     |
| Java                  |  242.7 us     |  838.1 us     |  123.8 us     |

## SELECT 1

|                       | mysql         | postgresql    | sqlite        |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  36.51 us     |  75.34 us     |  9.514 us     |
| Java                  |  63.43 us     |  106.1 us     |  5.483 us     |
