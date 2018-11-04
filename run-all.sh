#!/bin/sh

REPEAT=100
LANGUAGES="ado java"
DATABASES="sqlite mysql postgresql"

# Check SQL benchmark programs are built.
for L in $LANGUAGES; do
  case $L in
    ado)
      if test ! -f ado/bin/sqlbench; then
         echo "Please, build Ada SQL benchmark by running:"
         echo "cd ado && ./configure && make"
         exit 2
      fi
      ;;

    java)
      if test ! -f java/target/sql-benchmark-1.0.jar; then
         echo "Please, build Java SQL benchmark by running:"
         echo "cd java && mvn compile assembly:single"
         exit 2
      fi
      ;;

    *)
      echo "Invalid language $L"
      exit 2;
      ;;
  esac  
done

if test ! -f tools/bin/tool-main; then
  echo "Please, build the SQL benchmark aggrgation tool:"
  echo "cd tools && ./configure && make"
  exit 2
fi

mkdir -p results

DIR=`pwd`
FAIL=0
FILES=""
for D in $DATABASES; do

  for L in $LANGUAGES; do
  
     echo "Running SQL benchmark on $D for $L"
     case $L in
       ado)
         cd $DIR/ado && bin/sqlbench -$D -repeat $REPEAT -o ../results/$L-$D.xml
         if test $? -ne 0; then
            echo "Execution of SQL benchmark on $D for $L failed"
            FAIL=1
         fi
         FILES="$FILES results/$L-$D.xml"
         ;;

       java)
         cd $DIR/java && java -jar target/sql-benchmark-1.0.jar -$D -repeat $REPEAT -o ../results/$L-$D.xml
         if test $? -ne 0; then
            echo "Execution of SQL benchmark on $D for $L failed"
            FAIL=1
         fi
         FILES="$FILES results/$L-$D.xml"
         ;;

       *)
         ;;
     esac
  done

done

echo "Building the results"
cd $DIR && tools/bin/tool-main $FILES

