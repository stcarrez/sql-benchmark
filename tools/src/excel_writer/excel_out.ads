-------------------------------------------------------------------------------------
--
-- EXCEL_OUT - A low level package for writing Microsoft Excel (*) files
--
-- Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.
--
-- Version / date / download info: see the version, reference, web strings
--   defined at the end of the public part of this package.

-- Legal licensing note:

--  Copyright (c) 2009 .. 2018 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

-- Derived from ExcelOut by Frank Schoonjans in Modula-2 - thanks!

-- (*) All Trademarks mentioned are properties of their respective owners.
-------------------------------------------------------------------------------------
--
--  Follow these steps to create an Excel spreadsheet stream:
--
--  1. Create
--
--  2. Optional settings, before any data output:
--     | Write_default_column_width
--     | Write_column_width for specific columns
--     | Write_default_row_height
--     | Write_row_height for specific rows
--     | Define_font, then Define_format
--
--  3. | Write(xl, row, column, data): row by row, column by column
--     | Put(xl, data)               : same, but column is auto-incremented
--     | New_Line(xl),...            : other "Text_IO"-like (full list below)
--     | Use_format, influences the format of data written next
--
--  4. Close
--
--  5. (Excel_Out_String only) function Contents returns the full .xls
--------------------------------------------------------------------------

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

package Excel_Out is

  -----------------------------------------------------------------
  -- The abstract Excel output stream root type.                 --
  -- From this package, you can use the following derived types: --
  --    * Excel_Out_File    : output in a file                   --
  --    * Excel_Out_String  : output in a string                 --
  -- Of course you can define your own derived types.            --
  -----------------------------------------------------------------

  type Excel_Out_Stream is abstract tagged private;

  ----------------------------------
  -- (2) Before any cell content: --
  ----------------------------------

  -- * Page layout for printing
  procedure Header(xl : Excel_Out_Stream; page_header_string: String);
  procedure Footer(xl : Excel_Out_Stream; page_footer_string: String);
  --
  procedure Left_Margin(xl : Excel_Out_Stream; inches: Long_Float);
  procedure Right_Margin(xl : Excel_Out_Stream; inches: Long_Float);
  procedure Top_Margin(xl : Excel_Out_Stream; inches: Long_Float);
  procedure Bottom_Margin(xl : Excel_Out_Stream; inches: Long_Float);
  procedure Margins(xl : Excel_Out_Stream; left, right, top, bottom: Long_Float);
  --
  procedure Print_Row_Column_Headers(xl : Excel_Out_Stream);
  procedure Print_Gridlines(xl : Excel_Out_Stream);
  --
  type Orientation_choice is (landscape, portrait);
  type Scale_or_fit_choice is (scale, fit);
  procedure Page_Setup(
    xl                     : Excel_Out_Stream;
    scaling_percents       : Positive:= 100;
    fit_width_with_n_pages : Natural:= 1; -- 0: as many as possible
    fit_height_with_n_pages: Natural:= 1; -- 0: as many as possible
    orientation            : Orientation_choice:= portrait;
    scale_or_fit           : Scale_or_fit_choice:= scale
  );

  -- * The column width unit is as it appears in Excel when you resize a column.
  --     It is the width of a '0' in a standard font.
  procedure Write_default_column_width(xl : in out Excel_Out_Stream; width : Positive);
  procedure Write_column_width(xl : in out Excel_Out_Stream; column: Positive; width: Natural);
  procedure Write_column_width(
    xl            : in out Excel_Out_Stream;
    first_column,
    last_column   : Positive;
    width         : Natural
  );

  -- * The row height unit is in font points, as appearing when you
  --     resize a row in Excel. A zero height means the row is hidden.
  procedure Write_default_row_height(xl: Excel_Out_Stream; height: Positive);
  procedure Write_row_height(xl : Excel_Out_Stream; row: Positive; height : Natural);

  ----------------------
  -- Formatting cells --
  ----------------------
  -- A cell format is, as you can see in the format dialog
  -- in Excel, a combination of:
  --   - a number format
  --   - a set of alignements
  --   - a font
  --   - and other optional things to come here...
  -- Formats are user-defined except one which is predefined: Default_format

  type Format_type is private;

  function Default_format(xl: Excel_Out_Stream) return Format_type;
  -- What you get when creating a new sheet in Excel: Default_font,...

  -- * Number format
  type Number_format_type is private;

  -- Built-in number formats
  general          : constant Number_format_type;
  decimal_0        : constant Number_format_type;
  decimal_2        : constant Number_format_type;
  decimal_0_thousands_separator: constant Number_format_type;  -- 1'234'000
  decimal_2_thousands_separator: constant Number_format_type;  -- 1'234'000.00
  percent_0        : constant Number_format_type;   --  3%, 0%, -4%
  percent_2        : constant Number_format_type;
  percent_0_plus   : constant Number_format_type;   -- +3%, 0%, -4%
  percent_2_plus   : constant Number_format_type;
  scientific       : constant Number_format_type;
  dd_mm_yyyy       : constant Number_format_type;
  dd_mm_yyyy_hh_mm : constant Number_format_type;
  hh_mm            : constant Number_format_type;
  hh_mm_ss         : constant Number_format_type;

  procedure Define_number_format(
    xl           : in out Excel_Out_Stream;
    format       :    out Number_format_type;
    format_string: in     String
  );

  -- * Fonts are user-defined, one is predefined: Default_font
  type Font_type is private;

  function Default_font(xl: Excel_Out_Stream) return Font_type;
  -- Arial 10, regular, "automatic" color

  type Color_type is
    (automatic,
     black, white, red, green, blue, yellow, magenta, cyan,
     dark_red, dark_green, dark_blue, olive, purple, teal, silver, grey);

  type Font_style is private;

  -- For combining font styles (e.g.: bold & underlined):
  function "&"(a,b: Font_style) return Font_style;

  regular     : constant Font_style;
  italic      : constant Font_style;
  bold        : constant Font_style;
  bold_italic : constant Font_style;
  underlined  : constant Font_style;
  struck_out  : constant Font_style;
  shadowed    : constant Font_style;
  condensed   : constant Font_style;
  extended    : constant Font_style;

  procedure Define_font(
    xl           : in out Excel_Out_Stream;
    font_name    :        String;
    height       :        Positive;
    font         :    out Font_type;
    -- optional:
    style        :        Font_style:= regular;
    color        :        Color_type:= automatic
  );

  type Horizontal_alignment is (
    general_alignment, to_left, centred, to_right, filled,
    justified, centred_across_selection, -- (BIFF4-BIFF8)
    distributed -- (BIFF8, Excel 10.0 ("XP") and later only)
  );

  type Vertical_alignment is (
    top_alignment, centred, bottom_alignment,
    justified, -- (BIFF5-BIFF8)
    distributed -- (BIFF8, Excel 10.0 ("XP") and later only)
  );

  type Text_orientation is (
    normal,
    stacked,     -- vertical, top to bottom
    rotated_90,  -- vertical, rotated 90 degrees counterclockwise
    rotated_270  -- vertical, rotated 90 degrees clockwise
  );

  type Cell_border is private;

  -- Operator for combining borders (e.g.: left & top):
  function "&"(a,b: Cell_border) return Cell_border;

  no_border : constant Cell_border;
  left      : constant Cell_border;
  right     : constant Cell_border;
  top       : constant Cell_border;
  bottom    : constant Cell_border;
  box       : constant Cell_border;

  procedure Define_format(
    xl               : in out Excel_Out_Stream;
    font             : in     Font_type;          -- Default_font(xl), or given by Define_font
    number_format    : in     Number_format_type; -- built-in, or given by Define_number_format
    cell_format      :    out Format_type;
    -- Optional parameters --
    horizontal_align : in     Horizontal_alignment:= general_alignment;
    border           : in     Cell_border:= no_border;
    shaded           : in     Boolean:= False;    -- Add a dotted background pattern
    background_color : in     Color_type:= automatic;
    wrap_text        : in     Boolean:= False;
    vertical_align   : in     Vertical_alignment:= bottom_alignment;
    text_orient      : in     Text_orientation:= normal
  );

  ------------------------
  -- (3) Cell contents: --
  ------------------------

  -- NB: you need to write with ascending row index and with ascending
  --     column index within a row; otherwise Excel issues a protest

  procedure Write(xl: in out Excel_Out_Stream; r,c : Positive; num : Long_Float);
  procedure Write(xl: in out Excel_Out_Stream; r,c : Positive; num : Integer);
  procedure Write(xl: in out Excel_Out_Stream; r,c : Positive; str : String);
  procedure Write(xl: in out Excel_Out_Stream; r,c : Positive; str : Unbounded_String);
  procedure Write(xl: in out Excel_Out_Stream; r,c : Positive; date: Time);

  -- "Ada.Text_IO" - like output.
  -- No need to specify row & column each time.
  -- Write 'Put(x, content)' where x is an Excel_Out_Stream just
  -- as if x was a File_Type, and vice-versa.
  --
  procedure Put(xl: in out Excel_Out_Stream; num : Long_Float);
  procedure Put(xl    : in out Excel_Out_Stream;
                num   : in Integer;
                width : in Ada.Text_IO.Field := 0; -- ignored
                base  : in Ada.Text_IO.Number_Base := 10
            );
  procedure Put(xl: in out Excel_Out_Stream; str : String);
  procedure Put(xl: in out Excel_Out_Stream; str : Unbounded_String);
  procedure Put(xl: in out Excel_Out_Stream; date: Time);
  --
  procedure Put_Line(xl: in out Excel_Out_Stream; num : Long_Float);
  procedure Put_Line(xl: in out Excel_Out_Stream; num : Integer);
  procedure Put_Line(xl: in out Excel_Out_Stream; str : String);
  procedure Put_Line(xl: in out Excel_Out_Stream; str : Unbounded_String);
  procedure Put_Line(xl: in out Excel_Out_Stream; date: Time);
  --
  procedure New_Line(xl: in out Excel_Out_Stream; Spacing : Positive := 1);

  -- Get current column and row. The next Put will put contents in that cell.
  --
  function Col(xl: in Excel_Out_Stream) return Positive;    -- Text_IO naming
  function Column(xl: in Excel_Out_Stream) return Positive; -- Excel naming
  function Line(xl: in Excel_Out_Stream) return Positive;   -- Text_IO naming
  function Row(xl: in Excel_Out_Stream) return Positive;    -- Excel naming

  -- Relative / absolute jumps
  procedure Jump(xl: in out Excel_Out_Stream; rows, columns: Natural);
  procedure Jump_to(xl: in out Excel_Out_Stream; row, column: Positive);
  procedure Next(xl: in out Excel_Out_Stream; columns: Natural:= 1);  -- Jump 0 or more cells right
  procedure Next_Row(xl: in out Excel_Out_Stream; rows: Natural:= 1); -- Jump 0 or more cells down
  --
  -- Merge a certain amount of cells with the last one,
  -- right to that cell, on the same row.
  procedure Merge(xl: in out Excel_Out_Stream; cells: Positive);

  procedure Write_cell_comment(xl: Excel_Out_Stream; row, column: Positive; text: String);
  procedure Write_cell_comment_at_cursor(xl: Excel_Out_Stream; text: String);

  -- Cells written after Use_format will be using the given format,
  -- defined by Define_format.
  procedure Use_format(
    xl           : in out Excel_Out_Stream;
    format       : in     Format_type
  );
  procedure Use_default_format(xl: in out Excel_Out_Stream);

  -- The Freeze Pane methods can be called anytime before Close
  procedure Freeze_Panes(xl: in out Excel_Out_Stream; row, column: Positive);
  procedure Freeze_Panes_at_cursor(xl: in out Excel_Out_Stream);
  procedure Freeze_Top_Row(xl: in out Excel_Out_Stream);
  procedure Freeze_First_Column(xl: in out Excel_Out_Stream);

  --  Zoom level. Example: for 85%, call with parameters 85, 100.
  procedure Zoom_level(xl: in out Excel_Out_Stream; numerator, denominator: Positive);

  --  Set_Index and Index are not directly useful for Excel_Out users.
  --  They are private indeed, but they must be visible (RM 3.9.3(10)).

  -- Set the index on the stream
  procedure Set_Index (xl: in out Excel_Out_Stream;
                       to: Ada.Streams.Stream_IO.Positive_Count)
  is abstract;

  -- Return the index of the stream
  function Index (xl: Excel_Out_Stream) return Ada.Streams.Stream_IO.Count
  is abstract;

  Excel_stream_not_created,
  Excel_stream_not_closed,
  Decreasing_row_index,
  Decreasing_column_index,
  Row_out_of_range,
  Column_out_of_range : exception;

  type Excel_type is (
    BIFF2,    --  Excel 2.1, 2,2
    BIFF3,    --  Excel 3.0
    BIFF4     --  Excel 4.0
    --  BIFF5,    --  Excel 5.0 to 7.0
    --  BIFF8     --  Excel 8.0 (97) to 11.0 (2003) - UTF-16 support
  );

  Default_Excel_type: constant Excel_type:= BIFF4;

  --  Assumed encoding for types Character and String (8-bit):

  type Encoding_type is (
    Windows_CP_874,  --  Thai
    Windows_CP_932,  --  Japanese Shift-JIS
    Windows_CP_936,  --  Chinese Simplified GBK
    Windows_CP_949,  --  Korean (Wansung)
    Windows_CP_950,  --  Chinese Traditional BIG5
    Windows_CP_1250, --  Latin II (Central European)
    Windows_CP_1251, --  Cyrillic
    Windows_CP_1252, --  Latin I, superset of ISO 8859-1
    Windows_CP_1253, --  Greek
    Windows_CP_1254, --  Turkish
    Windows_CP_1255, --  Hebrew
    Windows_CP_1256, --  Arabic
    Windows_CP_1257, --  Baltic
    Windows_CP_1258, --  Vietnamese
    Windows_CP_1361, --  Korean (Johab)
    Apple_Roman
  );

  Default_encoding: constant Encoding_type:= Windows_CP_1252;

  -----------------------------------------------------------------
  -- Here, the derived stream types pre-defined in this package. --
  -----------------------------------------------------------------
  -- * Output to a file:

  type Excel_Out_File is new Excel_Out_Stream with private;

  procedure Create(
    xl           : in out Excel_Out_File;
    file_name    :        String;
    excel_format :        Excel_type    := Default_Excel_type;
    encoding     :        Encoding_type := Default_encoding
  );

  procedure Close(xl : in out Excel_Out_File);

  function Is_Open(xl : in Excel_Out_File) return Boolean;

  -- * Output to a string (to be compressed, packaged, transmitted, ... ):

  type Excel_Out_String is new Excel_Out_Stream with private;

  procedure Create(
    xl           : in out Excel_Out_String;
    excel_format :        Excel_type    := Default_Excel_type;
    encoding     :        Encoding_type := Default_encoding
  );

  procedure Close(xl : in out Excel_Out_String);

  function Contents(xl: Excel_Out_String) return String;

  ----------------------------------------------------------------
  --  Information about this package - e.g. for an "about" box  --
  ----------------------------------------------------------------

  version   : constant String:= "17";
  reference : constant String:= "18-Oct-2018";
  web       : constant String:= "http://excel-writer.sf.net/";
  --  hopefully the latest version is at that URL...  -----^

private

  ----------------------------------------
  -- Raw Streams, with 'Read and 'Write --
  ----------------------------------------

  type XL_Raw_Stream_Class is access all Ada.Streams.Root_Stream_Type'Class;

  type Font_type is new Natural;
  type Number_format_type is new Natural;
  type Format_type is new Natural;

  subtype XF_Range is Integer range 0..62; -- after 62 we'd need to use an IXFE (5.62)
  max_font  : constant:= 62;
  max_format: constant:= 62;

  -- Theoretically, we would not need to memorize the XF informations
  -- and just give the XF identifier given with Format_type, but some
  -- versions of Excel with some locales mix up the font and numerical format
  -- when giving 0 for the cell attributes (see Cell_attributes, 2.5.13)
  --   Added Mar-2011.

  type XF_Info is record
    font: Font_type;
    numb: Number_format_type;
  end record;

  type XF_Definition is array(XF_Range) of XF_Info;

  -- Built-in number formats
  general          : constant Number_format_type:= 0;
  decimal_0        : constant Number_format_type:= 1;
  decimal_2        : constant Number_format_type:= 2;
  decimal_0_thousands_separator: constant Number_format_type:= 3;  -- 1'234'000
  decimal_2_thousands_separator: constant Number_format_type:= 4;  -- 1'234'000.00
  no_currency_0       : constant Number_format_type:= 5;
  no_currency_red_0   : constant Number_format_type:= 6;
  no_currency_2       : constant Number_format_type:= 7;
  no_currency_red_2   : constant Number_format_type:= 8;
  currency_0       : constant Number_format_type:=  9; -- 5 in BIFF2, BIFF3 (sob!)
  currency_red_0   : constant Number_format_type:= 10; -- 6 in BIFF2, BIFF3...
  currency_2       : constant Number_format_type:= 11;
  currency_red_2   : constant Number_format_type:= 12;
  percent_0        : constant Number_format_type:= 13;  --  3%, 0%, -4%
  percent_2        : constant Number_format_type:= 14;
  scientific       : constant Number_format_type:= 15;
  fraction_1       : constant Number_format_type:= 16;
  fraction_2       : constant Number_format_type:= 17;
  dd_mm_yyyy       : constant Number_format_type:= 18; -- 14 in BIFF3, 12 in BIFF2 (re-sob!)
  dd_mmm_yy        : constant Number_format_type:= 19; -- 15 in BIFF3, 13 in BIFF2...
  dd_mmm           : constant Number_format_type:= 20;
  mmm_yy           : constant Number_format_type:= 21;
  h_mm_AM_PM       : constant Number_format_type:= 22;
  h_mm_ss_AM_PM    : constant Number_format_type:= 23;
  hh_mm            : constant Number_format_type:= 24;
  hh_mm_ss         : constant Number_format_type:= 25;
  dd_mm_yyyy_hh_mm : constant Number_format_type:= 26;
  -- End of Excel built-in formats
  last_built_in : constant Number_format_type:= dd_mm_yyyy_hh_mm;

  percent_0_plus   : constant Number_format_type:= 27; -- +3%, 0%, -4%
  percent_2_plus   : constant Number_format_type:= 28;
  date_iso         : constant Number_format_type:= 29; -- ISO 8601 format: 2014-03-16
  date_h_m_iso     : constant Number_format_type:= 30; -- date, hour, minutes
  date_h_m_s_iso   : constant Number_format_type:= 31; -- date, hour, minutes, seconds
  -- End of our custom formats
  last_custom   : constant Number_format_type:= date_h_m_s_iso;

  type Col_width_set is array(1..256) of Boolean;

  -- We have a concrete type as hidden ancestor of the Excel_Out_Stream root
  -- type. A variable of that type is initialized with default values and
  -- can help re-initialize a Excel_Out_Stream when re-used several times.
  -- See the Reset procedure in body.
  -- The abstract Excel_Out_Stream could have default values, but using a
  -- variable of this type to reset values is not Ada compliant (LRM:3.9.3(8))
  --
  type Excel_Out_Pre_Root_Type is tagged record
    xl_stream  : XL_Raw_Stream_Class;
    format     : Excel_type    := Default_Excel_type;
    encoding   : Encoding_type := Default_encoding;
    dimrecpos  : Ada.Streams.Stream_IO.Positive_Count;
    maxcolumn  : Positive:= 1;
    maxrow     : Positive:= 1;
    fonts      : Integer:= -1; -- [-1..max_font]
    xfs        : Integer:= -1; -- [-1..XF_Range'Last]
    xf_in_use  : XF_Range:= 0;
    xf_def     : XF_Definition;
    number_fmt : Number_format_type:= last_custom;
    def_font   : Font_type;
    def_fmt    : Format_type; -- Default format; used for "Normal" style
    cma_fmt    : Format_type; -- Format used for defining "Comma" style
    ccy_fmt    : Format_type; -- Format used for defining "Currency" style
    pct_fmt    : Format_type; -- Format used for defining "Percent" style
    is_created : Boolean:= False;
    is_closed  : Boolean:= False;
    curr_row   : Positive:= 1;
    curr_col   : Positive:= 1;
    frz_panes  : Boolean:= False;
    freeze_row : Positive;
    freeze_col : Positive;
    zoom_num   : Positive:= 100;
    zoom_den   : Positive:= 100;
    defcolwdth : Natural:= 0; -- 0 = not set; 1/256 of the width of the zero character
    std_col_width: Col_width_set:= (others => True);
  end record;

  type Excel_Out_Stream is abstract new Excel_Out_Pre_Root_Type with null record;

  type Font_style_single is
    (bold_single,
     italic_single,
     underlined_single,
     struck_out_single,
     outlined_single,
     shadowed_single,
     condensed_single,
     extended_single);

  type Font_style is array(Font_style_single) of Boolean;

  regular     : constant Font_style:= (others => False);
  italic      : constant Font_style:= (italic_single => True, others => False);
  bold        : constant Font_style:= (bold_single => True, others => False);
  bold_italic : constant Font_style:= bold or italic;
  underlined  : constant Font_style:= (underlined_single => True, others => False);
  struck_out  : constant Font_style:= (struck_out_single => True, others => False);
  shadowed    : constant Font_style:= (shadowed_single => True, others => False);
  condensed   : constant Font_style:= (condensed_single => True, others => False);
  extended    : constant Font_style:= (extended_single => True, others => False);

  type Cell_border_single is
    (left_single,
     right_single,
     top_single,
     bottom_single);

  type Cell_border is array(Cell_border_single) of Boolean;

  no_border : constant Cell_border:= (others => False);
  left      : constant Cell_border:= (left_single => True, others => False);
  right     : constant Cell_border:= (right_single => True, others => False);
  top       : constant Cell_border:= (top_single => True, others => False);
  bottom    : constant Cell_border:= (bottom_single => True, others => False);
  box       : constant Cell_border:= (others => True);

  ----------------------
  -- Output to a file --
  ----------------------

  type XL_file_acc is
    access Ada.Streams.Stream_IO.File_Type;

  type Excel_Out_File is new Excel_Out_Stream with record
    xl_file   : XL_file_acc:= null; -- access to the "physical" Excel file
  end record;

  -- Set the index on the file
  procedure Set_Index (xl: in out Excel_Out_File;
                       To: Ada.Streams.Stream_IO.Positive_Count);

  -- Return the index of the file
  function Index (xl: Excel_Out_File) return Ada.Streams.Stream_IO.Count;

  ------------------------
  -- Output to a string --
  ------------------------
  -- Code reused from Zip_Streams

  --- *** We define here a complete in-memory stream:
  type Unbounded_Stream is new Ada.Streams.Root_Stream_Type with
    record
      Unb : Ada.Strings.Unbounded.Unbounded_String;
      Loc : Integer := 1;
    end record;

  -- Read data from the stream.
  procedure Read
    (Stream : in out Unbounded_Stream;
     Item   : out Ada.Streams.Stream_Element_Array;
     Last   : out Ada.Streams.Stream_Element_Offset);

  -- write data to the stream, starting from the current index.
  -- Data will be overwritten from index is already available.
  procedure Write
    (Stream : in out Unbounded_Stream;
     Item   : Ada.Streams.Stream_Element_Array);

  -- Set the index on the stream
  procedure Set_Index (S : access Unbounded_Stream; To : Positive);

  -- returns the index of the stream
  function Index (S: access Unbounded_Stream) return Integer;

  --- ***

  type Unbounded_Stream_Acc is access Unbounded_Stream;

  type Excel_Out_String is new Excel_Out_Stream with record
    xl_memory: Unbounded_Stream_Acc;
  end record;

  -- Set the index on the Excel string stream
  procedure Set_Index (xl: in out Excel_Out_String;
                       To: Ada.Streams.Stream_IO.Positive_Count);

  -- Return the index of the Excel string stream
  function Index (xl: Excel_Out_String) return Ada.Streams.Stream_IO.Count;

end Excel_Out;
