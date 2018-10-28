-----------------------------------------------------------------------
--  sqlbench -- SQL Benchmark
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
with ADO.Sessions;
with ADO.Sessions.Factory;
private with Util.Measures;
private with Util.Properties;
private with Ada.Containers.Indefinite_Vectors;
package Sqlbench is

   Benchmark_Error : exception;

   type Context_Type is tagged limited private;

   type Repeat_Type is new Positive range 1 .. 1_000_000;

   subtype Repeat_Factor_Type is Repeat_Type range 1 .. 100;

   type Benchmark_Handler is access not null procedure (Context : in out Context_Type);

   --  Register a benchmark handler under the given name.
   procedure Register (Context : in out Context_Type;
                       Handler : in Benchmark_Handler;
                       Title   : in String;
                       Factor  : in Repeat_Factor_Type := 100)
     with Pre => Title'Length > 0;

   --  Get the database session to make SQL requests on the database.
   function Get_Session (Context : in Context_Type) return ADO.Sessions.Master_Session;

   --  Get a benchmark configuration parameter.
   function Get_Parameter (Context : in Context_Type;
                           Name    : in String) return String
     with Pre => Name'Length > 0;

   --  Get a SQL configuration file path that depends on the database driver.
   --  The file is of the form: <config-directory>/<database-driver>-<name>
   function Get_Config_Path (Context : in Context_Type;
                             Name    : in String) return String
     with Pre => Name'Length > 0;

   --  Get the database driver name.
   function Get_Driver_Name (Context : in Context_Type) return String
     with Post => Get_Driver_Name'Result'Length > 0;

private

   type Benchmark_Test (Len : Natural) is record
      Handler : Benchmark_Handler;
      Title   : String (1 .. Len);
      Factor  : Repeat_Factor_Type := 1;
   end record;

   package Benchmark_Test_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Benchmark_Test,
                                            "="          => "=");

   subtype Benchmark_Vector is Benchmark_Test_Vectors.Vector;
   subtype Benchmark_Cursor is Benchmark_Test_Vectors.Cursor;

   type Context_Type is tagged limited record
      Perf    : Util.Measures.Measure_Set;
      Repeat  : Repeat_Type := 1;
      Session : ADO.Sessions.Master_Session;
      Factory : ADO.Sessions.Factory.Session_Factory;
      Tests   : Benchmark_Vector;
      Config  : Util.Properties.Manager;
   end record;

end Sqlbench;
