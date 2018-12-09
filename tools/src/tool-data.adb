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
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Util.Strings;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;
with Util.Strings.Tokenizers;
with Excel_Out;
package body Tool.Data is

   use type Ada.Containers.Count_Type;
   use type Ada.Text_IO.Positive_Count;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Tool.Data");

   function Parse_Time (Value : in String) return Duration;
   function Get_Title (Value : in String) return String;
   function Get_Value (Value : in String) return Row_Count_Type;
   procedure Collect_Result (Into   : in out Perf_Result;
                             Driver : in Driver_Type;
                             Result : in Result_Type);
   procedure Add_Driver (Benchmark : in out Benchmark_Info);
   function Format (Value : in Duration) return String;
   function Format_Us (Value : in Duration) return String;

   Empty_Perf        : Perf_Map;
   Empty_Perf_Result : Perf_Result;

   function Parse_Time (Value : in String) return Duration is
      Pos    : constant Natural := Util.Strings.Index (Value, ' ');
      Result : Duration;
   begin
      Result := Duration'Value (Value (Value'First .. Pos - 1));
      if Value (Pos + 1 .. Value'Last) = "ns" then
         Result := Result / 1_000_000_000;
      elsif Value (Pos + 1 .. Value'Last) = "us" then
         Result := Result / 1_000_000;
      elsif Value (Pos + 1 .. Value'Last) = "ms" then
         Result := Result / 1_000;
      end if;
      return Result;
   end Parse_Time;

   function Get_Title (Value : in String) return String is
      Pos : constant Natural := Util.Strings.Rindex (Value, ' ');
   begin
      if Pos = 0 or Value'Length < String '("SELECT * FROM")'Length then
         return Value;
      end if;
      for C of Value (Pos + 1 .. Value'Last) loop
         if not (C in '0' .. '9') then
            return Value;
         end if;
      end loop;
      return Value (Value'First .. Pos - 1);
   end Get_Title;

   function Get_Value (Value : in String) return Row_Count_Type is
      Pos : constant Natural := Util.Strings.Rindex (Value, ' ');
   begin
      if Value'Length < String '("SELECT * FROM")'Length then
         return 0;
      else
         return Row_Count_Type'Value (Value (Pos + 1 .. Value'Last));
      end if;

   exception
      when Constraint_Error =>
         return 0;
   end Get_Value;

   function Format (Value : in Duration) return String is
   begin
      if Value < 0.000_001 then
         return Duration'Image (Value * 1_000_000_000) (1 .. 6) & " ns";
      elsif Value < 0.001 then
         return Duration'Image (Value * 1_000_000) (1 .. 6) & " us";
      elsif Value < 1.0 then
         return Duration'Image (Value * 1_000) (1 .. 6) & " ms";
      else
         return Duration'Image (Value) (1 .. 6) & " s";
      end if;
   end Format;

   function Format_Us (Value : in Duration) return String is
      Result : constant String := Duration'Image (Value * 1_000_000);
      Pos    : Positive := Result'Last;
   begin
      --  Drop leading zeros.
      while Pos > Result'First and then Result (Pos) = '0' and then Result (Pos - 1) /= '.' loop
         Pos := Pos - 1;
      end loop;
      return Result (Result'First .. Pos);
   end Format_Us;

   procedure Collect_Result (Into   : in out Perf_Result;
                             Driver : in Driver_Type;
                             Result : in Result_Type) is
      procedure Update (Item : in out Result_Type);

      procedure Update (Item : in out Result_Type) is
      begin
         Item.Count := Item.Count + Result.Count;
         Item.Time := Item.Time + Result.Time;
      end Update;
   begin
      while Into.Results.Length < Ada.Containers.Count_Type (Driver) loop
         Into.Results.Append (Result_Type '(Count => 0, Time => 0.0));
      end loop;
      Into.Results.Update_Element (Driver, Update'Access);
   end Collect_Result;

   procedure Add_Driver (Benchmark : in out Benchmark_Info) is

      procedure Update_Driver (Key : in String;
                               Driver : in out Driver_Result);

      Database   : constant String := UBO.To_String (Benchmark.Driver);
      Language   : constant String := UBO.To_String (Benchmark.Language);
      Pos        : Driver_Cursor := Benchmark.Drivers.Find (Language & " " & Database);
      New_Driver : Driver_Result;

      procedure Update_Driver (Key : in String;
                               Driver : in out Driver_Result) is
         pragma Unreferenced (Key);
      begin
         Driver.Count := Driver.Count + 1;
         Driver.Rss_Size := Driver.Rss_Size + Benchmark.Rss_Size;
         Driver.Peek_Rss := Driver.Peek_Rss + Benchmark.Peek_Rss_Size;
         Driver.Thread_Count := Driver.Thread_Count + Benchmark.Thread_Count;
         Driver.User_Time := Driver.User_Time + Benchmark.User_Time;
         Driver.Sys_Time := Driver.Sys_Time + Benchmark.Sys_Time;
         Driver.Language := Benchmark.Language_Index;
         Driver.Database := Benchmark.Database_Index;
         Driver.Index := Benchmark.Driver_Index;
      end Update_Driver;

   begin
      Log.Debug ("Adding driver {0} {1}", Language, Database);
      if UBO.Is_Null (Benchmark.Driver) or else UBO.Is_Null (Benchmark.Language) then
         return;
      end if;
      if not Driver_Maps.Has_Element (Pos) then
         New_Driver.Index := Driver_Type (Benchmark.Drivers.Length + 1);
         Benchmark.Drivers.Insert (Language & " " & Database, New_Driver);
         Pos := Benchmark.Drivers.Find (Language & " " & Database);
      end if;
      Benchmark.Database_Index := Benchmark.Databases.Find_Index (Database);
      Benchmark.Language_Index := Benchmark.Languages.Find_Index (Language);
      Benchmark.Driver_Index := Driver_Maps.Element (Pos).Index;
      Benchmark.Drivers.Update_Element (Pos, Update_Driver'Access);
   end Add_Driver;

   procedure Set_Member (Benchmark : in out Benchmark_Info;
                         Field     : in Benchmark_Fields;
                         Value     : in UBO.Object) is
   begin
      case Field is
         when FIELD_DRIVER =>
            if not Benchmark.Databases.Contains (UBO.To_String (Value)) then
               Benchmark.Databases.Append (UBO.To_String (Value));
            end if;
            Benchmark.Driver := Value;

         when FIELD_LANGUAGE =>
            if not Benchmark.Languages.Contains (UBO.To_String (Value)) then
               Benchmark.Languages.Append (UBO.To_String (Value));
            end if;
            Benchmark.Language := Value;

         when FIELD_THREADS =>
            Benchmark.Thread_Count := UBO.To_Integer (Value);

         when FIELD_RSS_SIZE =>
            Benchmark.Rss_Size := UBO.To_Integer (Value);

         when FIELD_PEEK_RSS_SIZE =>
            Benchmark.Peek_Rss_Size := UBO.To_Integer (Value);

         when FIELD_USER_TIME =>
            Benchmark.User_Time := UBO.To_Integer (Value);

         when FIELD_SYS_TIME =>
            Benchmark.Sys_Time := UBO.To_Integer (Value);

         when FIELD_MEASURES =>
            Add_Driver (Benchmark);

         when FIELD_COUNT =>
            Benchmark.Count := Count_Type (UBO.To_Integer (Value));

         when FIELD_TITLE =>
            Benchmark.Title := Value;

         when FIELD_TOTAL =>
            Benchmark.Time := Parse_Time (UBO.To_String (Value));

         when FIELD_TIME =>
            declare

               procedure Update (Key  : in String;
                                 Item : in out Perf_Map);
               procedure Update_Perf_Result (Key  : in Row_Count_Type;
                                             Item : in out Perf_Result);

               Main_Title : constant String := UBO.To_String (Benchmark.Title);
               Title      : constant String := Get_Title (Main_Title);
               Value      : constant Row_Count_Type := Get_Value (Main_Title);
               Pos        : Benchmark_Cursor := Benchmark.Benchmarks.Find (Title);
               Result     : Result_Type;

               procedure Update_Perf_Result (Key  : in Row_Count_Type;
                                             Item : in out Perf_Result) is
                  pragma Unreferenced (Key);
               begin
                  Collect_Result (Item, Benchmark.Driver_Index, Result);
               end Update_Perf_Result;

               procedure Update (Key  : in String;
                                 Item : in out Perf_Map) is
                  pragma Unreferenced (Key);
                  Pos : Perf_Cursor := Item.Find (Value);
               begin
                  if not Perf_Result_Maps.Has_Element (Pos) then
                     Item.Insert (Value, Empty_Perf_Result);
                     Pos := Item.Find (Value);
                  end if;
                  Item.Update_Element (Pos, Update_Perf_Result'Access);
               end Update;

            begin
               Result.Count := Benchmark.Count;
               Result.Time  := Benchmark.Time;
               if not Benchmark_Maps.Has_Element (Pos) then
                  Benchmark.Benchmarks.Insert (Title, Empty_Perf);
                  Pos := Benchmark.Benchmarks.Find (Title);
               end if;
               Benchmark.Benchmarks.Update_Element (Pos, Update'Access);
            end;

      end case;
   end Set_Member;

   Mapping   : aliased Benchmark_Mapper.Mapper;
   Benchmark : aliased Benchmark_Info;

   procedure Read (Path : in String) is
      Mapper : Util.Serialize.Mappers.Processing;
      Reader : Util.Serialize.IO.XML.Parser;
   begin
      Benchmark.User_Time := 0;
      Benchmark.Sys_Time  := 0;
      Benchmark.Thread_Count := 0;
      Benchmark.Rss_Size := 0;
      Benchmark.Peek_Rss_Size := 0;
      Benchmark.Language := UBO.Null_Object;
      Benchmark.Driver   := UBO.Null_Object;
      Mapper.Add_Mapping ("benchmark", Mapping'Access);
      Benchmark_Mapper.Set_Context (Mapper, Benchmark'Access);
      Reader.Parse (Path, Mapper);
   end Read;

   procedure Save_Memory (Path      : in String;
                          Languages : in String) is

      procedure Process_Language (Token : in String;
                                  Done  : out Boolean);

      File : Ada.Text_IO.File_Type;

      procedure Process_Language (Token : in String;
                                  Done  : out Boolean) is
         L : constant Language_Type := Benchmark.Languages.Find_Index (Token);
      begin
         Done := False;
         for D in Benchmark.Drivers.Iterate loop
            declare
               Driver : constant Driver_Result := Driver_Maps.Element (D);
            begin
               if Driver.Language = L then
                  Ada.Text_IO.Put (File, Benchmark.Databases.Element (Driver.Database));
                  Ada.Text_IO.Set_Col (File, 20);
                  Ada.Text_IO.Put (File, Benchmark.Languages.Element (L));
                  Ada.Text_IO.Set_Col (File, 30);
                  Ada.Text_IO.Put (File, Natural'Image (Driver.User_Time));
                  Ada.Text_IO.Set_Col (File, 40);
                  Ada.Text_IO.Put (File, Natural'Image (Driver.Sys_Time));
                  Ada.Text_IO.Set_Col (File, 60);
                  Ada.Text_IO.Put (File, Natural'Image (Driver.Peek_Rss));
                  Ada.Text_IO.Set_Col (File, 70);
                  Ada.Text_IO.Put (File, Natural'Image (Driver.Thread_Count));
                  Ada.Text_IO.New_Line (File);
               end if;
            end;
         end loop;
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.New_Line (File);
      end Process_Language;

   begin
      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Languages,
                                              Pattern => ",",
                                              Process => Process_Language'Access);
      Ada.Text_IO.Close (File);
   end Save_Memory;

   procedure Save (Path      : in String;
                   Databases : in String;
                   Languages : in String) is

      procedure Process_Database (Token : in String;
                                  Done  : out Boolean);
      procedure Process_Language (Token : in String;
                                  Done  : out Boolean);

      DB_Count   : constant Natural := Ada.Strings.Fixed.Count (Databases, ",") + 1;
      DB_List    : Database_Array_Index (1 .. DB_Count);
      Lang_Count : constant Natural := Ada.Strings.Fixed.Count (Languages, ",") + 1;
      Lang_List  : Language_Array_Index (1 .. Lang_Count);
      Pos        : Positive := 1;
      Col        : Ada.Text_IO.Positive_Count;
      File       : Ada.Text_IO.File_Type;

      procedure Process_Database (Token : in String;
                                  Done  : out Boolean) is
      begin
         DB_List (Pos) := Benchmark.Databases.Find_Index (Token);
         Pos := Pos + 1;
         Done := False;
      end Process_Database;

      procedure Process_Language (Token : in String;
                                  Done  : out Boolean) is
      begin
         Lang_List (Pos) := Benchmark.Languages.Find_Index (Token);
         Pos := Pos + 1;
         Done := False;
      end Process_Language;

   begin
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Databases,
                                              Pattern => ",",
                                              Process => Process_Database'Access);
      Pos := 1;
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Languages,
                                              Pattern => ",",
                                              Process => Process_Language'Access);

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Path);

      --  Print performance results.
      Ada.Text_IO.Put (File, "# order is ");
      Ada.Text_IO.Put (File, Databases);
      Ada.Text_IO.Put (File, " and ");
      Ada.Text_IO.Put (File, Languages);
      Ada.Text_IO.New_Line (File);

      for C in Benchmark.Benchmarks.Iterate loop
         for P in Benchmark_Maps.Element (C).Iterate loop
            if Perf_Result_Maps.Key (P) > 0 then
               Ada.Text_IO.Put (File, Row_Count_Type'Image (Perf_Result_Maps.Key (P)));
               for DB_Index of DB_List loop
                  for Lang_Index of Lang_List loop
                     declare
                        Database : constant String := Benchmark.Databases.Element (DB_Index);
                        Language : constant String := Benchmark.Languages.Element (Lang_Index);
                        Key      : constant String := Language & " " & Database;
                        Driver   : constant Driver_Cursor := Benchmark.Drivers.Find (Key);
                        R        : Result_Type;
                        Index    : Driver_Type;
                     begin
                        if Driver_Maps.Has_Element (Driver) then
                           Index := Driver_Maps.Element (Driver).Index;
                           if Perf_Result_Maps.Element (P).Results.Last_Index >= Index then
                              R := Perf_Result_Maps.Element (P).Results.Element (Index);
                              if R.Count > 0 then
                                 Ada.Text_IO.Put
                                   (File, Format_Us (R.Time / Positive (R.Count)));
                              else
                                 Ada.Text_IO.Put (File, " 0");
                              end if;
                           else
                              Ada.Text_IO.Put (File, " 0");
                           end if;
                        else
                           Ada.Text_IO.Put (File, " 0");
                        end if;
                     end;
                  end loop;
               end loop;
               Ada.Text_IO.New_Line (File);
            end if;
         end loop;
      end loop;

      --  Print results grouped by benchmark.
      for C in Benchmark.Benchmarks.Iterate loop
         for P in Benchmark_Maps.Element (C).Iterate loop
            declare
               Row_Count : constant Row_Count_Type := Perf_Result_Maps.Key (P);
            begin
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put ("## ");
               if Row_Count > 0 then
                  Ada.Text_IO.Put (Benchmark_Maps.Key (C) & Row_Count_Type'Image (Row_Count));
               else
                  Ada.Text_IO.Put (Benchmark_Maps.Key (C));
               end if;

               Ada.Text_IO.New_Line;
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put ("| ");
               Col := 25;
               for DB_Index of DB_List loop
                  Ada.Text_IO.Set_Col (Col);
                  Ada.Text_IO.Put ("| ");
                  Ada.Text_IO.Put (Benchmark.Databases.Element (DB_Index));
                  Col := Col + 16;
               end loop;

               Ada.Text_IO.Set_Col (Col);
               Ada.Text_IO.Put_Line ("|");

               Ada.Text_IO.Put ("|-----------------------|");
               for DB_Index of DB_List loop
                  Ada.Text_IO.Put ("---------------|");
               end loop;
               Ada.Text_IO.New_Line;

               for L in 1 .. Benchmark.Languages.Last_Index loop
                  declare
                     Language : constant String := Benchmark.Languages.Element (L);
                  begin
                     Ada.Text_IO.Put ("| ");
                     Ada.Text_IO.Put (Language);
                     Col := 25;
                     for DB_Index of DB_List loop
                        declare
                           Database : constant String := Benchmark.Databases.Element (DB_Index);
                           Key      : constant String := Language & " " & Database;
                           Driver   : constant Driver_Cursor := Benchmark.Drivers.Find (Key);
                           R        : Result_Type;
                           Index    : Driver_Type;
                        begin
                           Ada.Text_IO.Set_Col (Col);
                           Ada.Text_IO.Put ("| ");
                           if Driver_Maps.Has_Element (Driver) then
                              Index := Driver_Maps.Element (Driver).Index;
                              if Perf_Result_Maps.Element (P).Results.Last_Index >= Index then
                                 R := Perf_Result_Maps.Element (P).Results.Element (Index);
                                 if R.Count > 0 then
                                    Ada.Text_IO.Put
                                      (Format (R.Time / Positive (R.Count)));
                                 end if;
                              end if;
                           end if;
                        end;
                        Col := Col + 16;
                     end loop;
                     Ada.Text_IO.Set_Col (Col);
                     Ada.Text_IO.Put_Line ("|");
                  end;
               end loop;
            end;
         end loop;
      end loop;
   end Save;

   procedure Save_Excel (Path : in String) is
      File         : Excel_Out.Excel_Out_File;
      Row          : Positive := 1;
      Col          : Positive := 1;
      Font_Title   : Excel_Out.Font_type;
      Font_Sub     : Excel_Out.Font_type;
      Font_Cell    : Excel_Out.Font_type;
      Fmt_Title    : Excel_Out.Format_type;
      Fmt_Value    : Excel_Out.Format_type;
      Fmt_Lang     : Excel_Out.Format_type;
      Fmt_Database : Excel_Out.Format_type;
   begin
      File.Create (Path);
      File.Header ("Driver SQL Benchmark");
      File.Footer ("sql-benchmark");
      File.Margins (1.2, 1.1, 0.9, 0.8);
      File.Page_Setup (scaling_percents        => 100,
                       fit_width_with_n_pages  => 0,
                       orientation             => Excel_Out.portrait,
                       scale_or_fit            => Excel_Out.fit);

      File.Write_column_width (1, 15);
      File.Write_column_width (2, 20);
      File.Write_column_width (3, 20);
      File.Write_column_width (4, 20);

      File.Define_font ("Calibri", 14, Font_Title, Excel_Out.bold);
      File.Define_font ("Calibri", 12, Font_Sub, Excel_Out.bold);
      File.Define_font ("Calibri", 12, Font_Cell, Excel_Out.regular);
      File.Define_format (font          => Font_Title,
                          number_format => Excel_Out.general,
                          cell_format   => Fmt_Title);

      File.Define_format (font          => Font_Cell,
                          number_format => Excel_Out.general,
                          border        => Excel_Out.box,
                          cell_format   => Fmt_Value);
      File.Define_format (font          => Font_Sub,
                          number_format => Excel_Out.general,
                          cell_format   => Fmt_Lang,
                          border        => Excel_Out.box);
      File.Define_format (font          => Font_Sub,
                          number_format => Excel_Out.general,
                          cell_format   => Fmt_Database,
                          border        => Excel_Out.box);

      for C in Benchmark.Benchmarks.Iterate loop
         for P in Benchmark_Maps.Element (C).Iterate loop
            declare
               Row_Count : constant Row_Count_Type := Perf_Result_Maps.Key (P);
            begin
               Row := Row + 2;
               File.Use_format (Fmt_Title);
               if Row_Count > 0 then
                  File.Write (Row, 2, Benchmark_Maps.Key (C) & Row_Count_Type'Image (Row_Count));
               else
                  File.Write (Row, 2, Benchmark_Maps.Key (C));
               end if;

               File.Use_format (Fmt_Database);
               Row := Row + 1;

               Col := 2;
               for Database of Benchmark.Databases loop
                  File.Write (Row, Col, Database);
                  Col := Col + 1;
               end loop;

               for L in 1 .. Benchmark.Languages.Last_Index loop
                  declare
                     Language : constant String := Benchmark.Languages.Element (L);
                  begin
                     Row := Row + 1;
                     File.Use_format (Fmt_Lang);
                     File.Write (Row, 1, Language);
                     File.Use_format (Fmt_Value);
                     Col := 2;
                     for D in 1 .. Benchmark.Databases.Last_Index loop
                        declare
                           Database : constant String := Benchmark.Databases.Element (D);
                           Key      : constant String := Language & " " & Database;
                           Driver   : constant Driver_Cursor := Benchmark.Drivers.Find (Key);
                           R        : Result_Type;
                           Index    : Driver_Type;
                        begin
                           if Driver_Maps.Has_Element (Driver) then
                              Index := Driver_Maps.Element (Driver).Index;
                              if Perf_Result_Maps.Element (P).Results.Last_Index >= Index then
                                 R := Perf_Result_Maps.Element (P).Results.Element (Index);
                                 if R.Count > 0 then
                                    File.Write (Row, Col, Format (R.Time / Positive (R.Count)));
                                 end if;
                              end if;
                           end if;
                        end;
                        Col := Col + 1;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
      end loop;
      File.Close;
   end Save_Excel;

begin
   Mapping.Add_Mapping ("@driver", FIELD_DRIVER);
   Mapping.Add_Mapping ("@language", FIELD_LANGUAGE);
   Mapping.Add_Mapping ("@threads", FIELD_THREADS);
   Mapping.Add_Mapping ("@rss_size", FIELD_RSS_SIZE);
   Mapping.Add_Mapping ("@peek_rss_size", FIELD_PEEK_RSS_SIZE);
   Mapping.Add_Mapping ("@user_time", FIELD_USER_TIME);
   Mapping.Add_Mapping ("@sys_time", FIELD_SYS_TIME);
   Mapping.Add_Mapping ("measures/@title", FIELD_MEASURES);
   Mapping.Add_Mapping ("measures/time/@count", FIELD_COUNT);
   Mapping.Add_Mapping ("measures/time/@total", FIELD_TOTAL);
   Mapping.Add_Mapping ("measures/time/@title", FIELD_TITLE);
   Mapping.Add_Mapping ("measures/time", FIELD_TIME);
end Tool.Data;
