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
with Util.Files;
package body Sqlbench is

   --  ------------------------------
   --  Register a benchmark handler under the given name.
   --  ------------------------------
   procedure Register (Context : in out Context_Type;
                       Handler : in Benchmark_Handler;
                       Title   : in String;
                       Factor  : in Repeat_Factor_Type := 100) is
   begin
      Context.Tests.Append (Benchmark_Test '(Len     => Title'Length,
                                             Handler => Handler,
                                             Title   => Title,
                                             Factor  => Factor));
   end Register;

   --  ------------------------------
   --  Get the database session to make SQL requests on the database.
   --  ------------------------------
   function Get_Session (Context : in Context_Type) return ADO.Sessions.Master_Session is
   begin
      return Context.Session;
   end Get_Session;

   --  ------------------------------
   --  Get a benchmark configuration parameter.
   --  ------------------------------
   function Get_Parameter (Context : in Context_Type;
                           Name    : in String) return String is
   begin
      return Context.Config.Get (Name, "");
   end Get_Parameter;

   --  ------------------------------
   --  Get a SQL configuration file path that depends on the database driver.
   --  The file is of the form: <config-directory>/<database-driver>-<name>
   --  ------------------------------
   function Get_Config_Path (Context : in Context_Type;
                             Name    : in String) return String is
   begin
      return Util.Files.Compose ("config", Context.Get_Driver_Name & "-" & Name);
   end Get_Config_Path;

   --  ------------------------------
   --  Get the database driver name.
   --  ------------------------------
   function Get_Driver_Name (Context : in Context_Type) return String is
   begin
      return Context.Session.Get_Driver.Get_Driver_Name;
   end Get_Driver_Name;

end Sqlbench;
