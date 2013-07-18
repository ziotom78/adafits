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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with System;
with Interfaces.C;

package AdaFITS is

   AdaFITS_Error : exception;

   type Fits_File is private;

   CFITSIO_TYPE_BIT            : constant := 1;
   CFITSIO_TYPE_BYTE           : constant := 11;
   CFITSIO_TYPE_LOGICAL        : constant := 14;
   CFITSIO_TYPE_STRING         : constant := 16;
   CFITSIO_TYPE_SHORT          : constant := 21;
   CFITSIO_TYPE_LONG           : constant := 41;
   CFITSIO_TYPE_LONG_LONG      : constant := 81;
   CFITSIO_TYPE_FLOAT          : constant := 42;
   CFITSIO_TYPE_DOUBLE         : constant := 82;
   CFITSIO_TYPE_COMPLEX        : constant := 83;
   CFITSIO_TYPE_DOUBLE_COMPLEX : constant := 163;
   CFITSIO_TYPE_INT            : constant := 31;
   CFITSIO_TYPE_SIGNED_BYTE    : constant := 12;
   CFITSIO_TYPE_UNSIGNED_INT   : constant := 30;
   CFITSIO_TYPE_UNSIGNED_SHORT : constant := 20;
   CFITSIO_TYPE_UNSIGNED_LONG  : constant := 40;

   FLOAT_NULL_VALUE    : constant := -9.11912E-36;
   DOUBLE_NULL_VALUE   : constant := -9.1191291391491E-36;

   function Get_Fits_Ptr (File : Fits_File)
                          return System.Address;

   type IO_Mode is (READ_ONLY, READ_WRITE);
   type Table_Type is (ASCII_TABLE, BINARY_TABLE);
   type HDU_Type is (IMAGE, ASCII_TABLE, BINARY_TABLE, ANY);

   type Column_Def is record
      Name    : Ada.Strings.Unbounded.Unbounded_String; --  Name of the column
      Format  : Ada.Strings.Unbounded.Unbounded_String; --  Column format
      Unit    : Ada.Strings.Unbounded.Unbounded_String; --  Measure unit
   end record;
   type Column_Def_Array is array (Positive range <>) of Column_Def;

   type UInt32_Array is array (Positive range <>)
     of Interfaces.C.unsigned;
   pragma Convention (C, UInt32_Array);
   type UInt32_Array_Ptr is access UInt32_Array;
   procedure Free_UInt32_Array is
      new Ada.Unchecked_Deallocation (UInt32_Array, UInt32_Array_Ptr);

   --  Return the version number of the CFITSIO library
   function Get_CFITSIO_Version return Interfaces.C.C_float;

   --  Return an error string which corresponds to the CFITSIO error code
   function Get_FITSIO_Error_String return String;

   --  Open a FITS file for reading/writing
   function Open_File (File_Name : in String;
                       Mode : IO_Mode) return Fits_File;
   --  Open a FITS file for reading/writing and move to the first
   --  HDU containing tabular/image data
   function Open_Data (File_Name : in String;
                       Mode : IO_Mode) return Fits_File;
   --  Open a FITS file for reading/writing and move to the first
   --  HDU containing tabular data
   function Open_Table (File_Name : in String;
                        Mode : IO_Mode) return Fits_File;
   --  Open a FITS file for reading/writing and move to the first
   --  HDU containing image data
   function Open_Image (File_Name : in String;
                        Mode : IO_Mode) return Fits_File;

   --  Create a new FITS file and optionally stop if the file already exists
   function Create_File (File_Name : in String;
                         Overwrite : Boolean := False) return Fits_File;

   --  Close a FITS file that has been previously opened
   procedure Close_File (File : in out Fits_File);

   --  Flush any internal buffer (faster flush)
   procedure Flush_Buffers (File : in out Fits_File);

   --  Flush any buffer, close the file and reopen it (safer flush)
   procedure Flush_File (File : in out Fits_File);

   --  Delete a FITS file that is still open
   procedure Delete_File (File : in out Fits_File);

   --  Return the name of the file associated with the FITS object
   function Get_File_Name (File : in out Fits_File)
                           return String;

   --  Create the header of an ASCII/binary table in the current HDU
   procedure Create_Table (File : in out Fits_File;
                           Extension_Name : String;
                           Columns : Column_Def_Array;
                           Type_Of_Table : Table_Type := BINARY_TABLE;
                           Num_Of_Elements : Natural := 0);

   --  Return the number of HDUs currently present in the FITS file
   function Get_Number_Of_HDUs (File : in out Fits_File)
                                return Natural;

   --  Return the number of HDUs currently present in the FITS file
   function Get_Current_HDU_Index (File : in out Fits_File)
                                   return Positive;

   --  Return the type of the current HDU
   function Get_Current_HDU_Type (File : in out Fits_File)
                                  return HDU_Type;

   --  Return the best number of rows to read/write at one time
   function Get_Number_Of_Rows_For_Optimal_IO (File : in out Fits_File)
                                               return Positive;

   --  Return the number of rows in the current HDU
   function Get_Number_Of_Rows (File : in out Fits_File)
                                return Natural;

   --  Return the number of columns in the current HDU
   function Get_Number_Of_Columns (File : in out Fits_File)
                                   return Natural;

   --  Move to the specified HDU (the index is 1-based)
   procedure Move_To_HDU (File : in out Fits_File;
                          Number : Positive);

   --  Move to the HDU with the given name (EXTNAME)
   procedure Move_To_HDU (File : in out Fits_File;
                          Name : String;
                          Required_Type : HDU_Type := ANY;
                          Required_Version : Natural := 0);

   --  Return the value of a key in the current HDU as a string
   function Get_Key_As_String (File : in out Fits_File;
                               Key : String)
                               return String;

   --  Return the value of a key in the current HDU converted to an integer
   function Get_Key_As_Long (File : in out Fits_File;
                             Key : String)
                             return Interfaces.C.long;

   --  Return the value of a key in the current HDU converted to a double
   function Get_Key_As_Double (File : in out Fits_File;
                               Key : String)
                               return Interfaces.C.double;

   --  Set (update) the value of a key in the current HDU
   procedure Set_Key (File : in out Fits_File;
                      Key : String;
                      Value : String;
                      Comment : String := "");

   --  Set (update) the value of a key in the current HDU
   procedure Set_Key_As_Long (File : in out Fits_File;
                              Key : String;
                              Value : Interfaces.C.long;
                              Comment : String := "");

   --  Set (update) the value of a key in the current HDU
   procedure Set_Key_As_Double (File : in out Fits_File;
                                Key : String;
                                Value : Interfaces.C.double;
                                Decimals : Interfaces.C.int;
                                Comment : String := "");

   --  Append a COMMENT field to the end of the current HDU
   procedure Write_Comment (File : in out Fits_File;
                            Comment : String);

   --  Append a HISTORY field to the end of the current HDU
   procedure Write_History (File : in out Fits_File;
                            History : String);

   --  Append the current date and time to the current HDU
   procedure Write_Current_Date (File : in out Fits_File);

private

   type Fits_File is record
      Ptr : System.Address := System.Null_Address;
   end record;

   FLEN_FILENAME : constant := 1025; --  Max length of a filename
   FLEN_KEYWORD  : constant := 72;   --  Max length of a keyword
   FLEN_CARD     : constant := 81;   --  Length of a FITS header card
   FLEN_VALUE    : constant := 71;   --  Max length of a keyword value string
   FLEN_COMMENT  : constant := 73;   --  Max length of a keyword comment string
   FLEN_ERRMSG   : constant := 81;   --  Max length of a CFITSIO error message
   FLEN_STATUS   : constant := 31;   --  Max length of a CFITSIO status string

end AdaFITS;
