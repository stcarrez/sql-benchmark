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
with Util.Strings;
with Util.Files;

with ADO;
with ADO.Drivers;
with ADO.Sessions;
with ADO.Sessions.Factory;

with Sqlbench.Simple;

procedure Sqlbench.Main is

   procedure Read_Line (Line : in String);

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Sqlbench.Main");

   Thread_Count : Natural := 0;
   Rss_Size     : Natural := 0;
   Hwm_Size     : Natural := 0;

   procedure Read_Line (Line : in String) is
      Pos  : Natural := Util.Strings.Index (Line, ASCII.HT);
      Last : Natural := Util.Strings.Rindex (Line, ' ');
   begin
      Log.Debug ("{0}", Line);

      if Pos = 0 then
         Pos := Util.Strings.Index (Line, ' ');
      end if;
      if Last = 0 then
         Last := Line'Last;
      end if;
      if Pos > 0 then
         if Util.Strings.Starts_With (Line, "Threads:") then
            Thread_Count := Natural'Value (Line (Pos + 1 .. Last));
         elsif Util.Strings.Starts_With (Line, "VmRSS:") then
            Rss_Size := Natural'Value (Line (Pos + 1 .. Last));
         elsif Util.Strings.Starts_With (Line, "VmHWM:") then
            Hwm_Size := Natural'Value (Line (Pos + 1 .. Last));
         end if;
      end if;

   exception
      when Constraint_Error =>
         null;
   end Read_Line;

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

   begin
      Util.Files.Read_File (Path => "/proc/self/status", Process => Read_Line'Access);

   exception
      when others =>
         null;
   end;
   Ada.Text_IO.Put ("<benchmark language='Ada' driver='");
   Ada.Text_IO.Put (Context.Get_Driver_Name);
   Ada.Text_IO.Put ("' threads='");
   Ada.Text_IO.Put (Util.Strings.Image (Thread_Count));
   Ada.Text_IO.Put ("' rss_size='");
   Ada.Text_IO.Put (Util.Strings.Image (Rss_Size));
   Ada.Text_IO.Put ("' peek_rss_size='");
   Ada.Text_IO.Put (Util.Strings.Image (Hwm_Size));
   Ada.Text_IO.Put_Line ("'>");
   Util.Measures.Write (Context.Perf, "SQL Benchmark", Ada.Text_IO.Standard_Output);
   Ada.Text_IO.Put_Line ("</benchmark>");

exception
   when E : ADO.Drivers.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Sqlbench.Main;
