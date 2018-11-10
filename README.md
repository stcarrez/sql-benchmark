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

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  21.93 us     |  331.5 us     |  6.220 ms     |
| Java                  |  194.4 us     |  853.4 us     |  11.69 ms     |
| Python                |  45.04 us     |  256.3 us     |  1.503 ms     |

## DO 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |               |  33.32 us     |               |
| Java                  |               |  70.34 us     |               |
| Python                |               |               |               |

## DROP table; CREATE table

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  222.0 us     |  498.8 ms     |  68.58 ms     |
| Java                  |  413.8 us     |  535.3 ms     |  70.16 ms     |
| Python                |  1.452 ms     |  464.5 us     |  248.8 us     |

## INSERT INTO table

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  73.25 us     |  48.65 ms     |  11.82 ms     |
| Java                  |  19.43 us     |  48.52 ms     |  12.10 ms     |
| Python                |  7.481 us     |  206.7 us     |  24.21 us     |

## SELECT * FROM table LIMIT 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  28.30 us     |  38.61 us     |  152.3 us     |
| Java                  |  4.839 us     |  57.37 us     |  108.6 us     |
| Python                |  3.813 us     |  34.89 us     |  57.17 us     |

## SELECT * FROM table LIMIT 10

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  29.05 us     |  31.32 us     |  169.9 us     |
| Java                  |  5.124 us     |  65.35 us     |  125.6 us     |
| Python                |  8.785 us     |  49.59 us     |  67.11 us     |

## SELECT * FROM table LIMIT 100

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  38.11 us     |  41.10 us     |  215.3 us     |
| Java                  |  15.53 us     |  81.90 us     |  239.3 us     |
| Python                |  61.34 us     |  92.83 us     |  99.48 us     |

## SELECT * FROM table LIMIT 500

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  79.16 us     |  82.77 us     |  381.6 us     |
| Java                  |  63.18 us     |  143.8 us     |  634.6 us     |
| Python                |  301.4 us     |  278.7 us     |  250.8 us     |

## SELECT * FROM table LIMIT 1000

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  128.9 us     |  148.7 us     |  504.5 us     |
| Java                  |  122.2 us     |  226.6 us     |  896.2 us     |
| Python                |  598.8 us     |  522.5 us     |  440.0 us     |

## SELECT 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  9.477 us     |  45.89 us     |  119.8 us     |
| Java                  |  6.437 us     |  59.68 us     |  136.8 us     |
| Python                |  1.654 us     |  30.16 us     |  54.28 us     |
