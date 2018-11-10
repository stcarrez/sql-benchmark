## Requisites

You must use Python3 to run the benchmarks and install the PostgreSQL support.

```
sudo apt-get install python3-psycopg2
```

```
sudo apt-get install python3-mysqldb
```

## Run

MySQL
```
python3 src -mysql -r 100
```

PostgreSQL
```
python3 src -postgresql -r 100
```

SQLite
```
python3 src -sqlite -r 100
```

