-----------------------------------------------------------------------
--  sqlbench-main -- Main SQL Bencharmk
--  Copyright (C) 2018, 2019 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Log.Loggers;
with Util.Measures;
with Util.Strings;
with Util.Files;

with ADO;
with ADO.Configs;
with ADO.Drivers;
with ADO.Connections;
with ADO.Sessions;
with ADO.Sessions.Factory;

with Sqlbench.Simple;

procedure Sqlbench.Main is

   use Ada.Strings.Unbounded;

   procedure Read_Line (Line : in String);
   procedure Read_Stat_Line (Line : in String);

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Sqlbench.Main");

   Thread_Count : Natural := 0;
   Rss_Size     : Natural := 0;
   Hwm_Size     : Natural := 0;
   User_Time    : Natural := 0;
   Sys_Time     : Natural := 0;

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

   procedure Read_Stat_Line (Line : in String) is
      Pos  : Natural := Line'First;
      Next : Natural;
   begin
      Log.Debug ("{0}", Line);

      for I in 1 .. 13 loop
         Pos := Util.Strings.Index (Line, ' ', Pos + 1);
         exit when Pos = 0;
      end loop;
      Next := Util.Strings.Index (Line, ' ', Pos + 1);
      User_Time := 10 * Natural'Value (Line (Pos + 1 .. Next - 1));
      Pos := Next;
      Next := Util.Strings.Index (Line, ' ', Pos + 1);
      Sys_Time := 10 * Natural'Value (Line (Pos + 1 .. Next - 1));

   exception
      when Constraint_Error =>
         null;
   end Read_Stat_Line;

   Driver      : Unbounded_String;
   Context     : Sqlbench.Context_Type;
   Repeat      : Sqlbench.Repeat_Type := 100;
   Output      : Unbounded_String;
   Output_File : Ada.Text_IO.File_Type;
   Arg_Pos     : Positive := 1;
   Arg_Count   : constant Natural := Ada.Command_Line.Argument_Count;
begin
   Util.Log.Loggers.Initialize ("sqlbench.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("sqlbench.properties");

   while Arg_Pos <= Arg_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (Arg_Pos);
      begin
         if Arg = "-sqlite" then
            Driver := To_Unbounded_String ("sqlite");
         elsif Arg = "-mysql" then
            Driver := To_Unbounded_String ("mysql");
         elsif Arg = "-postgresql" then
            Driver := To_Unbounded_String ("postgresql");
         elsif Arg = "-repeat" and Arg_Pos + 1 <= Arg_Count then
            Arg_Pos := Arg_Pos + 1;
            Repeat := Repeat_Type'Value (Ada.Command_Line.Argument (Arg_Pos));
         elsif Arg = "-o" and Arg_Pos + 1 <= Arg_Count then
            Arg_Pos := Arg_Pos + 1;
            Output := To_Unbounded_String (Ada.Command_Line.Argument (Arg_Pos));
         else
            raise Constraint_Error;
         end if;

      exception
         when Constraint_Error =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Usage: sql-bench [-sqlite] [-mysql] [-postgresql] "
                                  & "[-repeat count] [-o output]");

            Ada.Command_Line.Set_Exit_Status (2);
            return;
      end;
      Arg_Pos := Arg_Pos + 1;
   end loop;

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Context.Factory.Create (ADO.Configs.Get_Config (To_String (Driver) & ".database"));
   Context.Session := Context.Factory.Get_Master_Session;
   Simple.Register (Context);

   for Test of Context.Tests loop
      Context.Repeat := Repeat * Test.Factor;

      declare
         T : Util.Measures.Stamp;
      begin
         Test.Handler (Context);
         Util.Measures.Report (Context.Perf, T, Test.Title, Positive (Context.Repeat));

      exception
         when others =>
            null;
      end;

   end loop;

   begin
      Util.Files.Read_File (Path => "/proc/self/status", Process => Read_Line'Access);

   exception
      when others =>
         null;
   end;

   begin
      Util.Files.Read_File (Path => "/proc/self/stat", Process => Read_Stat_Line'Access);

   exception
      when others =>
         null;
   end;
   if Length (Output) > 0 then
      Ada.Text_IO.Create (Output_File, Ada.Text_IO.Out_File, To_String (Output));
      Ada.Text_IO.Set_Output (Output_File);
   end if;
   Ada.Text_IO.Put ("<benchmark language='Ada' driver='");
   Ada.Text_IO.Put (Context.Get_Driver_Name);
   Ada.Text_IO.Put ("' threads='");
   Ada.Text_IO.Put (Util.Strings.Image (Thread_Count));
   Ada.Text_IO.Put ("' rss_size='");
   Ada.Text_IO.Put (Util.Strings.Image (Rss_Size));
   Ada.Text_IO.Put ("' peek_rss_size='");
   Ada.Text_IO.Put (Util.Strings.Image (Hwm_Size));
   Ada.Text_IO.Put ("' user_time='");
   Ada.Text_IO.Put (Util.Strings.Image (User_Time));
   Ada.Text_IO.Put ("' sys_time='");
   Ada.Text_IO.Put (Util.Strings.Image (Sys_Time));
   Ada.Text_IO.Put_Line ("'>");
   Util.Measures.Write (Context.Perf, "SQL Benchmark",
                        (if Length (Output) > 0 then
                            Output_File else Ada.Text_IO.Standard_Output));
   Ada.Text_IO.Put_Line ("</benchmark>");
   Ada.Text_IO.Flush;

exception
   when E : ADO.Connections.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Sqlbench.Main;
