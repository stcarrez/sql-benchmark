## Requisites

You must use Python3 to run the benchmarks and install the PostgreSQL support.

```
sudo apt-get install python3-psycopg2
```

## Run

PostgreSQL
```
python3 src -postgresql -r 100
```

SQLite
```
python3 src -sqlite -r 100
```

