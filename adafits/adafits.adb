--------------------------------------------------------------------
--  AdaFITS: a set of Ada 2012 bindings to the CFITSIO library
--  Copyright (C) 2013 Maurizio Tomasi
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2 of the
--  License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
--  02110-1301, USA.
----------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

package body AdaFITS is

   function Get_Fits_Ptr (File : Fits_File)
                          return System.Address is
   begin
      return File.Ptr;
   end Get_Fits_Ptr;

   package C renames Interfaces.C;
   package C_Ext renames Interfaces.C.Extensions;
   package C_Strings renames Interfaces.C.Strings;
   package UStr renames Ada.Strings.Unbounded;

   type String_Array is array (Positive range <>) of UStr.Unbounded_String;
   type Asciiz_Array is array (Positive range <>)
     of C_Strings.chars_ptr;
   pragma Convention (C, Asciiz_Array);

   ----------------------------------------------------------------------

   function Get_FITS_IO_Mode_Code
     (Mode : IO_Mode)
      return C.int;

   ----------------------------------------------------------------------

   function Get_FITS_IO_Mode_Code
     (Mode : IO_Mode)
      return C.int is

      Result : C.int;

   begin
      case Mode is
         when READ_ONLY => Result := 0;
         when READ_WRITE => Result := 1;
         when others => Result := -1;
      end case;

      return Result;
   end Get_FITS_IO_Mode_Code;

   ----------------------------------------------------------------------

   function Get_CFITSIO_Version
     return Interfaces.C.C_float is

      procedure Fitsio_Get_Version
        (Version : out Interfaces.C.C_float);
      pragma Import (C, Fitsio_Get_Version, "ffvers");

      Version : Interfaces.C.C_float;

   begin
      Fitsio_Get_Version (Version);
      return Version;
   end Get_CFITSIO_Version;

   ----------------------------------------------------------------------

   function Get_FITSIO_Error_String
      return String is

      function Fits_Read_Errmsg
        (Error_Text : access C.char)
         return C.int;
      pragma Import (C, Fits_Read_Errmsg, "ffgmsg");

      Error_Text : C.char_array (1 .. FLEN_ERRMSG);
      More : C.int;
      Result : UStr.Unbounded_String := UStr.To_Unbounded_String ("");

   begin
      loop
         More := Fits_Read_Errmsg (Error_Text (Error_Text'First)'Access);
         exit when More = 0 and C.To_Ada (Error_Text) = "";
         UStr.Append (Result,
                      UStr.To_Unbounded_String (C.To_Ada (Error_Text)));
      end loop;
      return UStr.To_String (Result);
   end Get_FITSIO_Error_String;

   ----------------------------------------------------------------------

   function Open_File
     (File_Name : in String;
      Mode : IO_Mode)
      return Fits_File is

      function Fits_Open_File
        (File : access System.Address;
         File_Name : C.char_array;
         Mode_Code : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Open_File, "ffopen");

      Status : aliased C.int := 0;
   begin
      return File : Fits_File do
         if Fits_Open_File (File.Ptr'Access,
                            C.To_C (File_Name),
                            Get_FITS_IO_Mode_Code (Mode),
                            Status'Access) > 0 then
            raise AdaFITS_Error with Get_FITSIO_Error_String;
         end if;
      end return;
   end Open_File;

   function Open_Data
     (File_Name : in String;
      Mode : IO_Mode)
      return Fits_File is

      function Fits_Open_Data
        (File : access System.Address;
         File_Name : C.char_array;
         Mode_Code : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Open_Data, "ffdopn");

      Status : aliased C.int := 0;

   begin
      return File : Fits_File do
         if Fits_Open_Data (File.Ptr'Access,
                            C.To_C (File_Name),
                            Get_FITS_IO_Mode_Code (Mode),
                            Status'Access) > 0 then
            raise AdaFITS_Error with Get_FITSIO_Error_String;
         end if;
      end return;
   end Open_Data;

   function Open_Table
     (File_Name : in String;
      Mode : IO_Mode)
      return Fits_File is

      function Fits_Open_Table
        (File : access System.Address;
         File_Name : C.char_array;
         Mode_Code : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Open_Table, "fftopn");

      Status : aliased C.int := 0;

   begin
      return File : Fits_File do
         if Fits_Open_Table (File.Ptr'Access,
                             C.To_C (File_Name),
                             Get_FITS_IO_Mode_Code (Mode),
                             Status'Access) > 0 then
            raise AdaFITS_Error with Get_FITSIO_Error_String;
         end if;
      end return;
   end Open_Table;

   function Open_Image
     (File_Name : in String;
      Mode : IO_Mode)
      return Fits_File is

      function Fits_Open_Image
        (File : access System.Address;
         File_Name : C.char_array;
         Mode_Code : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Open_Image, "ffiopn");

      Status : aliased C.int := 0;

   begin
      return File : Fits_File do
         if Fits_Open_Image (File.Ptr'Access,
                             C.To_C (File_Name),
                             Get_FITS_IO_Mode_Code (Mode),
                             Status'Access) > 0 then
            raise AdaFITS_Error with Get_FITSIO_Error_String;
         end if;
      end return;
   end Open_Image;

   function Create_File
     (File_Name : in String;
      Overwrite : Boolean := False)
      return Fits_File is

      function Fits_Create_File
        (File : access System.Address;
         File_Name : C.char_array;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Create_File, "ffinit");

      Status : aliased C.int := 0;

      File_Name_To_Use : UStr.Unbounded_String;

   begin
      if Overwrite and then File_Name (File_Name'First) /= '!' then
         File_Name_To_Use := UStr.To_Unbounded_String ('!' & File_Name);
      else
         File_Name_To_Use := UStr.To_Unbounded_String (File_Name);
      end if;

      return File : Fits_File do
         if Fits_Create_File (File.Ptr'Access,
                              C.To_C (UStr.To_String (File_Name_To_Use)),
                              Status'Access) > 0 then
            raise AdaFITS_Error with Get_FITSIO_Error_String;
         end if;
      end return;
   end Create_File;

   procedure Close_File
     (File : Fits_File) is

      function Fits_Close_File
        (File : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Close_File, "ffclos");

      Status : aliased C.int := 0;

   begin
      if Fits_Close_File (File.Ptr, Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Close_File;

   procedure Flush_Buffers (File : Fits_File) is

      function Fits_Flush_Buffer
        (File : System.Address;
         Dummy : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Flush_Buffer, "ffflsh");

      Status : aliased C.int := 0;

   begin
      if Fits_Flush_Buffer (File.Ptr, 0, Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Flush_Buffers;

   procedure Flush_File (File : Fits_File) is

      function Fits_Flush_File
        (File : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Flush_File, "ffflus");

      Status : aliased C.int := 0;

   begin
      if Fits_Flush_File (File.Ptr, Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Flush_File;

   procedure Delete_File
     (File : Fits_File) is

      function Fits_Delete_File
        (File : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Delete_File, "ffdelt");

      Status : aliased C.int := 0;

   begin
      if Fits_Delete_File (File.Ptr, Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Delete_File;

   function Get_File_Name
     (File : Fits_File)
      return String is

      function Fits_File_Name
        (File : System.Address;
         Dest : access C.char;
         Status : access C.int)
      return C.int;
      pragma Import (C, Fits_File_Name, "ffflnm");

      Name : C.char_array (1 .. FLEN_FILENAME);
      Status : aliased C.int := 0;

   begin
      if Fits_File_Name (File.Ptr,
                         Name (Name'First)'Access,
                         Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return C.To_Ada (Name);
   end Get_File_Name;

   procedure Create_Table
     (File : Fits_File;
      Extension_Name : String;
      Columns : Column_Def_Array;
      Type_Of_Table : Table_Type := BINARY_TABLE;
      Num_Of_Elements : Natural := 0) is

      function Fits_Create_Table (File : System.Address;
                                  Table_Type : C.int;
                                  Naxis2 : C_Ext.long_long;
                                  TFields : C.int;
                                  TType : Asciiz_Array;
                                  TForm : Asciiz_Array;
                                  TUnit : Asciiz_Array;
                                  Extname : C.char_array;
                                  Status : access C.int)
                                  return C.int;
      pragma Import (C, Fits_Create_Table, "ffcrtb");

      procedure Free_Strings;

      TType : Asciiz_Array (Positive range Columns'Range);
      TForm : Asciiz_Array (Positive range Columns'Range);
      TUnit : Asciiz_Array (Positive range Columns'Range);

      Table_Type_Code : C.int;

      Status : aliased C.int := 0;

      procedure Free_Strings is
      begin
         for I in TType'Range loop
            C_Strings.Free (TType (I));
            C_Strings.Free (TForm (I));
            C_Strings.Free (TUnit (I));
         end loop;
      end Free_Strings;

   begin
      for I in Columns'Range loop
         TType (I) := C_Strings.New_String (UStr.To_String (Columns (I).Name));
         TForm (I) := C_Strings.New_String (UStr.To_String
                                            (Columns (I).Format));
         TUnit (I) := C_Strings.New_String (UStr.To_String (Columns (I).Unit));
      end loop;

      case Type_Of_Table is
         when ASCII_TABLE => Table_Type_Code := 1;
         when BINARY_TABLE => Table_Type_Code := 2;
      end case;

      if Fits_Create_Table (File => File.Ptr,
                            Table_Type => Table_Type_Code,
                            Naxis2 => C_Ext.long_long (Num_Of_Elements),
                            TFields => Columns'Length,
                            TType => TType,
                            TForm => TForm,
                            TUnit => TUnit,
                            Extname => C.To_C (Extension_Name),
                            Status => Status'Access) > 0 then
         Free_Strings;
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      Free_Strings;
   end Create_Table;

   function Get_Number_Of_HDUs
     (File : Fits_File)
      return Natural is

      function Fits_Get_Num_Hdus
        (File : System.Address;
         Num_Of_HDUs : access C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_Num_Hdus, "ffthdu");

      Status : aliased C.int := 0;

      Num_Of_HDUs : aliased C.int;

   begin
      if Fits_Get_Num_Hdus (File.Ptr,
                            Num_Of_HDUs'Access,
                            Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Natural (Num_Of_HDUs);
   end Get_Number_Of_HDUs;

   function Get_Current_HDU_Index (File : Fits_File)
                                   return Positive is

      function Fits_Get_HDU_Num
        (File : System.Address;
         Index : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_HDU_Num, "ffghdn");

      Num : aliased C.int;

   begin
      return Positive (Fits_Get_HDU_Num (File.Ptr, Num'Access));
   end Get_Current_HDU_Index;

   function Get_Current_HDU_Type (File : Fits_File)
                                  return HDU_Type is

      function Fits_Get_HDU_Type
        (File : System.Address;
         HDU_Type : access C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_HDU_Type, "ffghdt");

      Type_Code : aliased C.int;
      Status : aliased C.int := 0;

   begin
      if Fits_Get_HDU_Type (File.Ptr,
                            Type_Code'Access,
                            Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      case Type_Code is
         when 0 => return IMAGE;
         when 1 => return ASCII_TABLE;
         when 2 => return BINARY_TABLE;
         when others => raise AdaFITS_Error with
              "Unknown HDU type: " & C.int'Image (Type_Code);
      end case;
   end Get_Current_HDU_Type;

   function Get_Number_Of_Rows_For_Optimal_IO
     (File : Fits_File)
      return Positive is

      function Fits_Get_Rowsize
        (File : System.Address;
         Num_Of_Rows : access C.long;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_Rowsize, "ffgrsz");

      Status : aliased C.int := 0;
      Num_Of_Rows : aliased C.long;

   begin
      if Fits_Get_Rowsize (File.Ptr,
                           Num_Of_Rows'Access,
                           Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Positive (Num_Of_Rows);
   end Get_Number_Of_Rows_For_Optimal_IO;


   function Get_Number_Of_Rows
     (File : Fits_File)
      return Natural is

      function Fits_Get_Num_Rows
        (File : System.Address;
         Num_Of_Rows : access C.long;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_Num_Rows, "ffgnrw");

      Status : aliased C.int := 0;
      Num_Of_Rows : aliased C.long;

   begin
      if Fits_Get_Num_Rows (File.Ptr,
                            Num_Of_Rows'Access,
                            Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Natural (Num_Of_Rows);
   end Get_Number_Of_Rows;

   function Get_Number_Of_Columns
     (File : Fits_File)
      return Natural is

      function Fits_Get_Num_Columns
        (File : System.Address;
         Num_Of_Columns : access C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Get_Num_Columns, "ffgncl");

      Status : aliased C.int := 0;
      Num_Of_Columns : aliased C.int;

   begin
      if Fits_Get_Num_Columns (File.Ptr,
                               Num_Of_Columns'Access,
                               Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Natural (Num_Of_Columns);
   end Get_Number_Of_Columns;

   procedure Move_To_HDU
     (File : Fits_File;
      Number : Positive) is

      function Fits_Movabs_Hdu
        (File : System.Address;
         HDU_Num : C.int;
         HDU_Type : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Movabs_Hdu, "ffmahd");

      Status : aliased C.int := 0;

   begin
      if Fits_Movabs_Hdu (File.Ptr,
                          C.int (Number),
                          System.Null_Address,
                          Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Move_To_HDU;

   procedure Move_To_HDU
     (File : Fits_File;
      Name : String;
      Required_Type : HDU_Type := ANY;
      Required_Version : Natural := 0) is

      function Fits_Movnam_Hdu
        (File : System.Address;
         HDU_Type : C.int;
         HDU_Name : C.char_array;
         Extver : C.int;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Movnam_Hdu, "ffmnhd");

      Type_Code : C.int;
      Status : aliased C.int := 0;

   begin
      case Required_Type is
         when IMAGE => Type_Code := 0;
         when ASCII_TABLE => Type_Code := 1;
         when BINARY_TABLE => Type_Code := 2;
         when ANY => Type_Code := -1;
      end case;
      if Fits_Movnam_Hdu (File.Ptr,
                          Type_Code,
                          C.To_C (Name),
                          C.int (Required_Version),
                          Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Move_To_HDU;

   function Get_Key_As_String
     (File : Fits_File;
      Key : String)
      return String is

      function Fits_Read_Keystr
        (File : System.Address;
         Key_name : C.char_array;
         Value : access C.char;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Read_Keystr, "ffgkys");

      Result : C.char_array (1 .. FLEN_VALUE);
      Status : aliased C.int := 0;

   begin
      if Fits_Read_Keystr (File.Ptr,
                           C.To_C (Key),
                           Result (Result'First)'Access,
                           System.Null_Address,
                           Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return C.To_Ada (Result);
   end Get_Key_As_String;

   function Get_Key_As_Long
     (File : Fits_File;
      Key : String)
      return C.long is

      function Fits_Read_Key_Lng
        (File : System.Address;
         Key_Name : C.char_array;
         Value : access C.long;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Read_Key_Lng, "ffgkyj");

      Result : aliased C.long;
      Status : aliased C.int := 0;

   begin
      if Fits_Read_Key_Lng (File.Ptr,
                            C.To_C (Key),
                            Result'Access,
                            System.Null_Address,
                            Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Result;
   end Get_Key_As_Long;

   function Get_Key_As_Double
     (File : Fits_File;
      Key : String)
      return C.double is

      function Fits_Read_Key_Dbl
        (File : System.Address;
         Key_Name : C.char_array;
         Value : access C.double;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Read_Key_Dbl, "ffgkyd");

      Status : aliased C.int := 0;
      Value : aliased C.double;

   begin
      if Fits_Read_Key_Dbl (File.Ptr,
                            C.To_C (Key),
                            Value'Access,
                            System.Null_Address,
                            Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;

      return Value;
   end Get_Key_As_Double;

   procedure Set_Key
     (File : Fits_File;
      Key : String;
      Value : String;
      Comment : String := "") is

      function Fits_Update_Key_Str
        (File : System.Address;
         Key_Name : C.char_array;
         Value : C.char_array;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Update_Key_Str, "ffukys");

      Status : aliased C.int := 0;

   begin
      if Fits_Update_Key_Str (File.Ptr,
                              C.To_C (Key),
                              C.To_C (Value),
                              System.Null_Address,
                              Status'Access) > 0 then
         raise AdaFITS_Error  with Get_FITSIO_Error_String;
      end if;
   end Set_Key;

   procedure Set_Key_As_Long
     (File : Fits_File;
      Key : String;
      Value : Interfaces.C.long;
      Comment : String := "") is

      function Fits_Update_Key_Lng
        (File : System.Address;
         Key_Name : C.char_array;
         Value : C.long;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Update_Key_Lng, "ffukyj");

      Status : aliased C.int := 0;

   begin
      if Fits_Update_Key_Lng (File.Ptr,
                              C.To_C (Key),
                              Value,
                              System.Null_Address,
                              Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Set_Key_As_Long;

   procedure Set_Key_As_Double
     (File : Fits_File;
      Key : String;
      Value : Interfaces.C.double;
      Decimals : Interfaces.C.int;
      Comment : String := "") is

      function Fits_Update_Key_Dbl
        (File : System.Address;
         Key_Name : C.char_array;
         Value : C.double;
         Decimals : C.int;
         Comment : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Update_Key_Dbl, "ffukyd");

      Status : aliased C.int := 0;

   begin
      if Fits_Update_Key_Dbl (File.Ptr,
                              C.To_C (Key),
                              Value,
                              Decimals,
                              System.Null_Address,
                              Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Set_Key_As_Double;

   procedure Write_Comment
     (File : Fits_File;
      Comment : String) is

      function Fits_Write_Comment
        (File : System.Address;
         Comment : C.char_array;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Write_Comment, "ffpcom");

      Status : aliased C.int := 0;

   begin
      if Fits_Write_Comment (File.Ptr,
                             C.To_C (Comment),
                             Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Write_Comment;

   procedure Write_History
     (File : Fits_File;
      History : String) is

      function Fits_Write_History
        (File : System.Address;
         History : C.char_array;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Write_History, "ffphis");

      Status : aliased C.int := 0;

   begin
      if Fits_Write_History (File.Ptr,
                             C.To_C (History),
                             Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Write_History;

   procedure Write_Current_Date
     (File : Fits_File) is

      function Fits_Write_Date
        (File : System.Address;
         Status : access C.int)
         return C.int;
      pragma Import (C, Fits_Write_Date, "ffpdat");

      Status : aliased C.int := 0;

   begin
      if Fits_Write_Date (File.Ptr, Status'Access) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Write_Current_Date;

end AdaFITS;
