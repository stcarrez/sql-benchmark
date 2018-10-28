# Overview

The *sql-benchmark* provides simple benchmark of several database drivers in several languages.
Benchmarks are intended to be simple so that they can be implemented easily for several
languages and SQL databases.  Some of the goals are:

* Evaluate the performance of a database driver,
* Compare the performance of different languages when connecting to a database,
* Have a rough comparison on simple SQL queries on different databases.

# Languages

* [Java Benchmark](https://github.com/stcarrez/sql-benchmark/java/README.md)
* [Ada Benchmark](https://github.com/stcarrez/sql-benchmark/ado/README.md)

# Databases

Three SQL databases are supported:

* SQLite,
* MySQL,
* PostgreSQL

Before running the SQL benchmark of MySQL and PostgreSQL, you must create the
`sqlbench` database and give access to the `sqlbench` user.

The SQLite database is created automatically.
