-----------------------------------------------------------------------
--  tool-data -- Perf data representation
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Util.Beans.Objects;
private with Util.Serialize.Mappers.Record_Mapper;

pragma No_Recursion;

package Tool.Data is

   --  Type representing a number of times a benchmark test is executed.
   type Count_Type is new Natural;

   --  Type representing the number of rows associated with the benchmark.
   type Row_Count_Type is new Natural;

   --  Type representing a driver index.
   type Driver_Type is new Positive;

   --  Type representing a language index.
   type Language_Type is new Positive;

   --  Type representing a database index.
   type Database_Type is new Positive;

   type Driver_Result is record
      Index        : Driver_Type   := Driver_Type'First;
      Language     : Language_Type := Language_Type'First;
      Database     : Database_Type := Database_Type'First;
      Count        : Count_Type  := 0;
      Thread_Count : Natural     := 0;
      Rss_Size     : Natural     := 0;
      Peek_Rss     : Natural     := 0;
      User_Time    : Natural     := 0;
      Sys_Time     : Natural     := 0;
   end record;

   type Result_Type is record
      Count : Count_Type;
      Time  : Duration;
   end record;

   package Result_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Driver_Type,
                                 Element_Type => Result_Type,
                                 "="          => "=");

   type Perf_Result is record
      Value   : Row_Count_Type := 0;
      Results : Result_Vectors.Vector;
   end record;

   package Perf_Result_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Row_Count_Type,
                                      Element_Type => Perf_Result,
                                      "<"          => "<",
                                      "="          => "=");
   subtype Perf_Map is Perf_Result_Maps.Map;
   subtype Perf_Cursor is Perf_Result_Maps.Cursor;

   package Benchmark_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Perf_Map,
                                                 "<"          => "<",
                                                 "="          => Perf_Result_Maps."=");
   subtype Benchmark_Map is Benchmark_Maps.Map;
   subtype Benchmark_Cursor is Benchmark_Maps.Cursor;

   procedure Read (Path : in String);

   procedure Save (Path      : in String;
                   Databases : in String;
                   Languages : in String) with
     Pre => Databases'Length > 0 and Languages'Length > 0;

   procedure Save_Memory (Path      : in String;
                          Languages : in String) with
     Pre => Languages'Length > 0;

   procedure Save_Excel (Path : in String);

private

   package UBO renames Util.Beans.Objects;

   package Driver_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Driver_Result,
                                                 "<" => "<");
   subtype Driver_Map is Driver_Maps.Map;
   subtype Driver_Cursor is Driver_Maps.Cursor;

   package Language_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Language_Type,
                                            Element_Type => String);
   subtype Language_Vector is Language_Vectors.Vector;
   subtype Language_Cursor is Language_Vectors.Cursor;

   package Database_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Database_Type,
                                            Element_Type => String);
   subtype Database_Vector is Database_Vectors.Vector;
   subtype Database_Cursor is Database_Vectors.Cursor;

   --  Array of database index.
   type Database_Array_Index is array (Positive range <>) of Database_Type;

   --  Array of language index.
   type Language_Array_Index is array (Positive range <>) of Language_Type;

   type Benchmark_Fields is (FIELD_DRIVER,
                             FIELD_LANGUAGE,
                             FIELD_THREADS,
                             FIELD_RSS_SIZE,
                             FIELD_PEEK_RSS_SIZE,
                             FIELD_USER_TIME,
                             FIELD_SYS_TIME,
                             FIELD_COUNT,
                             FIELD_TIME,
                             FIELD_TITLE,
                             FIELD_MEASURES,
                             FIELD_TOTAL);

   type Benchmark_Info is record
      Drivers        : Driver_Map;
      Databases      : Database_Vector;
      Languages      : Language_Vector;
      Benchmarks     : Benchmark_Map;
      Thread_Count   : Natural := 0;
      Rss_Size       : Natural := 0;
      Peek_Rss_Size  : Natural := 0;
      User_Time      : Natural := 0;
      Sys_Time       : Natural := 0;
      Count          : Count_Type := 0;
      Driver         : UBO.Object;
      Language       : UBO.Object;
      Title          : UBO.Object;
      Time           : Duration := 0.0;
      Driver_Index   : Driver_Type := Driver_Type'First;
      Language_Index : Language_Type;
      Database_Index : Database_Type;
   end record;
   type Benchmark_Info_Access is access all Benchmark_Info;

   procedure Set_Member (Benchmark : in out Benchmark_Info;
                         Field     : in Benchmark_Fields;
                         Value     : in UBO.Object);

   package Benchmark_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Benchmark_Info,
                                               Element_Type_Access => Benchmark_Info_Access,
                                               Fields              => Benchmark_Fields,
                                               Set_Member          => Set_Member);

end Tool.Data;
