-----------------------------------------------------------------------
--  tool-main -- Main tool program
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
with Ada.Command_Line;

with Util.Log.Loggers;
with Tool.Data;
procedure Tool.Main is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Tool.Main");

begin
   Util.Log.Loggers.Initialize ("tool.properties");

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      Tool.Data.Read (Ada.Command_Line.Argument (I));
   end loop;

   Tool.Data.Save ("result.dat", "sqlite,mysql,postgresql", "Ada,Python,Java");
   Tool.Data.Save_Memory ("memory.dat", "Ada,Python,Java");
   Tool.Data.Save_Excel ("result.xls");

exception
   when E : others =>
      Log.Error (Message => "Internal error:",
                 E       => E,
                 Trace   => True);
end Tool.Main;
