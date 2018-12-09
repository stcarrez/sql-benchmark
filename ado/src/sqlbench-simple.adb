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
with Util.Files;
package body Sqlbench.Simple is

   use Ada.Strings.Unbounded;

   generic
      LIMIT : Positive;
   procedure Select_Table_N (Context : in out Context_Type);

   procedure Select_Table_N (Context : in out Context_Type) is
      DB    : constant ADO.Sessions.Master_Session := Context.Get_Session;
      Count : Natural;
      Stmt  : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT * FROM test_simple LIMIT " & Positive'Image (LIMIT));
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
         Count := 0;
         while Stmt.Has_Elements loop
            Count := Count + 1;
            Stmt.Next;
         end loop;
         if Count /= LIMIT then
            raise Benchmark_Error with "Invalid result count:" & Natural'Image (Count);
         end if;
      end loop;
   end Select_Table_N;

   procedure Do_Static (Context : in out Context_Type);

   procedure Select_Static (Context : in out Context_Type);

   procedure Connect_Select_Static (Context : in out Context_Type);

   procedure Drop_Create (Context : in out Context_Type);

   procedure Insert (Context : in out Context_Type);

   procedure Select_Table_1 is new Select_Table_N (1);

   procedure Select_Table_10 is new Select_Table_N (10);

   procedure Select_Table_100 is new Select_Table_N (100);

   procedure Select_Table_500 is new Select_Table_N (500);

   procedure Select_Table_1000 is new Select_Table_N (1000);

   Create_SQL : Ada.Strings.Unbounded.Unbounded_String;

   procedure Register (Tests : in out Context_Type) is
      Driver : constant String := Tests.Get_Driver_Name;
   begin
      if Driver /= "sqlite" and Driver /= "postgresql" then
         Tests.Register (Do_Static'Access, "DO 1");
      end if;
      Tests.Register (Select_Static'Access, "SELECT 1");
      Tests.Register (Connect_Select_Static'Access, "CONNECT; SELECT 1; CLOSE");
      Tests.Register (Drop_Create'Access, "DROP table; CREATE table", 1);
      Tests.Register (Insert'Access, "INSERT INTO table", 10);
      Tests.Register (Select_Table_1'Access, "SELECT * FROM table LIMIT 1");
      Tests.Register (Select_Table_10'Access, "SELECT * FROM table LIMIT 10");
      Tests.Register (Select_Table_100'Access, "SELECT * FROM table LIMIT 100");
      Tests.Register (Select_Table_500'Access, "SELECT * FROM table LIMIT 500");
      Tests.Register (Select_Table_1000'Access, "SELECT * FROM table LIMIT 1000");
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
            Context.Session.Commit;
         exception
            when ADO.Statements.SQL_Error =>
               Context.Session.Rollback;
         end;
         Context.Session.Begin_Transaction;
         Create_Stmt.Execute;
         Context.Session.Commit;
         Context.Session.Begin_Transaction;
      end loop;
   end Drop_Create;

   procedure Insert (Context : in out Context_Type) is
      Stmt    : ADO.Statements.Query_Statement
        := Context.Session.Create_Statement ("INSERT INTO test_simple (value) VALUES (1)");
   begin
      for I in 1 .. Context.Repeat loop
         Stmt.Execute;
      end loop;
      Context.Session.Commit;
   end Insert;

end Sqlbench.Simple;
