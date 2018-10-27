-----------------------------------------------------------------------
--  sqlbench-simple -- Simple SQL benchmark
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
with Ada.Strings.Unbounded;
with ADO.Statements;
with ADO.Parameters;
with Util.Files;
package body Sqlbench.Simple is

   use Ada.Strings.Unbounded;

   procedure Do_Static (Context : in out Context_Type);

   procedure Select_Static (Context : in out Context_Type);

   procedure Connect_Select_Static (Context : in out Context_Type);

   procedure Drop_Create (Context : in out Context_Type);

   procedure Insert (Context : in out Context_Type);

   procedure Select_Table_1 (Context : in out Context_Type);

   procedure Select_Table_10 (Context : in out Context_Type);

   procedure Select_Table_100 (Context : in out Context_Type);

   Create_SQL : Ada.Strings.Unbounded.Unbounded_String;

   procedure Register (Tests : in out Context_Type) is
   begin
      Tests.Register (Do_Static'Access, "DO 1");
      Tests.Register (Select_Static'Access, "SELECT 1");
      Tests.Register (Connect_Select_Static'Access, "CONNECT; SELECT 1; CLOSE");
      Tests.Register (Drop_Create'Access, "DROP table; CREATE table", 1);
      Tests.Register (Insert'Access, "INSERT INTO table", 10);
      Tests.Register (Select_Table_1'Access, "SELECT * FROM table LIMIT 1");
      Tests.Register (Select_Table_10'Access, "SELECT * FROM table LIMIT 10");
      Tests.Register (Select_Table_100'Access, "SELECT * FROM table LIMIT 100");
      Util.Files.Read_File (Tests.Get_Config_Path ("create-table.sql"), Create_SQL);
   end Register;

   procedure Do_Static (Context : in out Context_Type) is
      Stmt  : ADO.Statements.Query_Statement := Context.Session.Create_Statement ("DO 1");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
      end loop;
   end Do_Static;

   procedure Select_Static (Context : in out Context_Type) is
      Stmt  : ADO.Statements.Query_Statement := Context.Session.Create_Statement ("SELECT 1");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
      end loop;
   end Select_Static;

   procedure Connect_Select_Static (Context : in out Context_Type) is
   begin
      for I in 1 .. Context.Repeat loop
         declare
            DB    : constant ADO.Sessions.Session := Context.Factory.Get_Session;
            Stmt  : ADO.Statements.Query_Statement := DB.Create_Statement ("SELECT 1");
         begin
            Stmt.Execute;
         end;
      end loop;
   end Connect_Select_Static;

   procedure Drop_Create (Context : in out Context_Type) is
      Drop_Stmt    : ADO.Statements.Query_Statement
        := Context.Session.Create_Statement ("DROP TABLE test_simple");
      Create_Stmt  : ADO.Statements.Query_Statement
        := Context.Session.Create_Statement (To_String (Create_SQL));
   begin
      for I in 1 .. Context.Repeat loop
         begin
            Drop_Stmt.Execute;
         exception
            when ADO.Statements.SQL_Error =>
               null;
         end;
         Create_Stmt.Execute;
      end loop;
   end Drop_Create;

   procedure Insert (Context : in out Context_Type) is
      Stmt    : ADO.Statements.Query_Statement
        := Context.Session.Create_Statement ("INSERT INTO test_simple (value) VALUES (1)");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
      end loop;
   end Insert;

   procedure Select_Table_1 (Context : in out Context_Type) is
      Name  : constant String := Context.Get_Parameter ("table");
      DB    : constant ADO.Sessions.Master_Session := Context.Get_Session;
      Stmt  : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT * FROM test_simple LIMIT 1");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
         while Stmt.Has_Elements loop
            Stmt.Next;
         end loop;
      end loop;
   end Select_Table_1;

   procedure Select_Table_10 (Context : in out Context_Type) is
      Name  : constant String := Context.Get_Parameter ("table");
      DB    : constant ADO.Sessions.Master_Session := Context.Get_Session;
      Stmt  : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT * FROM test_simple LIMIT 10");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
         while Stmt.Has_Elements loop
            Stmt.Next;
         end loop;
      end loop;
   end Select_Table_10;

   procedure Select_Table_100 (Context : in out Context_Type) is
      Name  : constant String := Context.Get_Parameter ("table");
      DB    : constant ADO.Sessions.Master_Session := Context.Get_Session;
      Stmt  : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT * FROM test_simple LIMIT 100");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
         while Stmt.Has_Elements loop
            Stmt.Next;
         end loop;
      end loop;
   end Select_Table_100;

end Sqlbench.Simple;
