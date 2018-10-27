-----------------------------------------------------------------------
--  sqlbench-main -- Main SQL Bencharmk
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
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;

with Util.Log.Loggers;
with Util.Measures;

with ADO;
with ADO.Drivers;
with ADO.Sessions;
with ADO.Sessions.Factory;

with Sqlbench.Simple;

procedure Sqlbench.Main is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Sqlbench.Main");

   Context : Sqlbench.Context_Type;
begin
   Util.Log.Loggers.Initialize ("sqlbench.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("sqlbench.properties");

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Context.Factory.Create (ADO.Drivers.Get_Config ("ado.database"));
   Context.Session := Context.Factory.Get_Master_Session;
   Simple.Register (Context);

   for Test of Context.Tests loop
      Context.Repeat := 10 * Test.Factor;
      declare
         T : Util.Measures.Stamp;
      begin
         Test.Handler (Context);
         Util.Measures.Report (Context.Perf, T, Test.Title, Positive (Context.Repeat));
      end;
   end loop;

   Util.Measures.Write (Context.Perf, "SQL Benchmark", Ada.Text_IO.Standard_Output);

exception
   when E : ADO.Drivers.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Sqlbench.Main;
