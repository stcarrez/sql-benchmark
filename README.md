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
