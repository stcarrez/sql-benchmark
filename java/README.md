## Build

To build the Java program benchmark, you must use Maven

```
mvn compile assembly:single
```

## Run

MySQL
```
java -jar target/sql-benchmark-1.0.jar -mysql -repeat 100
```

PostgreSQL
```
java -jar target/sql-benchmark-1.0.jar -postgresql -repeat 100
```

SQLite
```
java -jar target/sql-benchmark-1.0.jar -sqlite -repeat 100
```
