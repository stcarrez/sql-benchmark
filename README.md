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
* MySQL/MariaDB,
* PostgreSQL

Before running the SQL benchmark of MySQL/MariaDB and PostgreSQL, you must create the
`sqlbench` database and give access to the `sqlbench` user.

The SQLite database is created automatically.

## MySQL/MariaDB setup

1. Create the 'sqlbench' database in MySQL/MariaDB

```
mysql -u root
mysql> create database sqlbench;
```

2. Create the 'sqlbench' user:
```
mysql> create user 'sqlbench'@'localhost' identified by 'sqlbench';
```

3. Give the access rights:
```
mysql> grant select, insert, update, delete,
       create, drop, create temporary tables, execute,
       show view on sqlbench.* to sqlbench@'localhost';
mysql> flush privileges;
```

## Postgresql setup

To create manually the database, you can proceed to the following steps:

1. Create the 'sqlbench' user and configure the password
(enter 'sqlbench' for the password or update the configuration sqlbench.properties file):

```
sudo -u postgres createuser sqlbench --pwprompt
```

2. Create the 'sqlbench' database in Postgresql

```
sudo -u postgres createdb -O sqlbench sqlbench
```

# Running

The script `run-all.sh` can be used to run all the benchmark and produce the results.
Before running it, make sure you have built the Ada and Java benchmark programs as
well as the Ada aggregator tool.  To build, run the following commands.

```
cd ado
./configure
make
cd ../java
mvn compile assembly:single
cd ../tools
./configure
make
cd ..
```

Then, simply run the script:

```
./run-all.sh
```

# Results

![Time](https://github.com/stcarrez/sql-benchmark/wiki/images/time.png)
![Memory](https://github.com/stcarrez/sql-benchmark/wiki/images/memory.png)
![SQLite](https://github.com/stcarrez/sql-benchmark/wiki/images/sqlite.png)
![MySQL](https://github.com/stcarrez/sql-benchmark/wiki/images/mysql.png)
![PostgreSQL](https://github.com/stcarrez/sql-benchmark/wiki/images/postgresql.png)


## CONNECT; SELECT 1; CLOSE

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  23.68 us     |  311.0 us     |  5.541 ms     |
| Java                  |  187.7 us     |  895.8 us     |  10.80 ms     |
| Python                |  42.00 us     |  398.5 us     |  6.071 ms     |

## DO 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |               |  19.04 us     |               |
| Java                  |               |  53.72 us     |               |
| Python                |               |  33.03 us     |               |

## DROP table; CREATE table

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  216.9 us     |  496.2 ms     |  42.64 ms     |
| Java                  |  1.698 ms     |  504.9 ms     |  45.03 ms     |
| Python                |  122.5 ms     |  498.6 ms     |  41.24 ms     |

## INSERT INTO table

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  73.59 us     |  118.5 us     |  160.6 us     |
| Java                  |  96.20 us     |  241.4 us     |  119.3 us     |
| Python                |  3.684 us     |  290.7 us     |  119.1 us     |

## SELECT * FROM table LIMIT 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  25.63 us     |  75.60 us     |  105.5 us     |
| Java                  |  5.096 us     |  97.38 us     |  105.6 us     |
| Python                |  3.457 us     |  47.31 us     |  148.7 us     |

## SELECT * FROM table LIMIT 10

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  27.60 us     |  61.30 us     |  99.43 us     |
| Java                  |  5.115 us     |  101.7 us     |  92.56 us     |
| Python                |  8.766 us     |  51.76 us     |  128.0 us     |

## SELECT * FROM table LIMIT 100

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  37.41 us     |  131.1 us     |  161.6 us     |
| Java                  |  15.53 us     |  174.5 us     |  223.8 us     |
| Python                |  60.43 us     |  102.0 us     |  236.6 us     |

## SELECT * FROM table LIMIT 500

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  78.14 us     |  328.6 us     |  305.7 us     |
| Java                  |  62.12 us     |  462.2 us     |  616.2 us     |
| Python                |  297.8 us     |  300.2 us     |  462.2 us     |

## SELECT * FROM table LIMIT 1000

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  132.0 us     |  544.9 us     |  456.6 us     |
| Java                  |  121.8 us     |  728.2 us     |  871.6 us     |
| Python                |  605.6 us     |  551.6 us     |  730.3 us     |

## SELECT 1

|                       | sqlite        | mysql         | postgresql    |
|-----------------------|---------------|---------------|---------------|
| Ada                   |  9.501 us     |  35.60 us     |  87.55 us     |
| Java                  |  5.629 us     |  70.61 us     |  104.4 us     |
| Python                |  1.530 us     |  78.13 us     |  89.66 us     |
