## Build

To build the Ada program benchmark, you will need the following Ada projects:

* ADO           (https://github.com/stcarrez/ada-ado)
* Ada Util      (https://github.com/stcarrez/ada-util)

Compile and install them before running the following commands:

```
./configure
make
```

## Run

MySQL
```
bin/sqlbench -mysql -repeat 100
```

PostgreSQL
```
bin/sqlbench -postgresql -repeat 100
```

SQLite
```
bin/sqlbench -sqlite -repeat 100
```
