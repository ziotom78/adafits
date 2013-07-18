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
with Interfaces.C.Extensions;
with AdaFITS;

package body AdaFITS.Generic_Column_Routines is

   package C renames Interfaces.C;
   package C_Ext renames Interfaces.C.Extensions;

   type Pointer is access all Element;
   type Const_Pointer is access constant Element;

   procedure Read_Column
     (File : in out AdaFITS.Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Element_Array;
      Null_Value : Element) is

      function Fits_Read_Col
        (File : System.Address;
         Data_Type : C.int;
         Column_Number : C.int;
         First_Row : C_Ext.long_long;
         First_Elem : C_Ext.long_long;
         Num_Of_Elements : C_Ext.long_long;
         Null_Value : Pointer;
         Vector : Pointer;
         Any_null : out C.int;
         Status : in out C.int)
         return C.int;
      pragma Import (C, Fits_Read_Col, "ffgcv");

      Aliased_Null : aliased Element := Null_Value;
      Any_Null_Flag : C.int;
      Status : C.int := 0;

   begin
      if Fits_Read_Col (File.Ptr,
                        Data_Type => CFITSIO_Data_Type,
                        Column_Number => C.int (Column_Number),
                        First_Row => C_Ext.long_long (First_Row),
                        First_Elem => C_Ext.long_long (First_Element),
                        Num_Of_Elements => C_Ext.long_long (Num_Of_Elements),
                        Null_Value => Aliased_Null'Unchecked_Access,
                        Vector => Destination (Destination'First)'Unchecked_Access,
                        Any_null => Any_Null_Flag,
                        Status => Status) > 0 then
         raise AdaFITS_Error with AdaFITS.Get_FITSIO_Error_String;
      end if;
   end Read_Column;

   procedure Write_Column
     (File : in out AdaFITS.Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Element_Array) is

      function Fits_Write_Col
        (File : System.Address;
         Data_Type : C.int;
         Column_Number : C.int;
         First_Row : C_Ext.long_long;
         First_Elem : C_Ext.long_long;
         Num_Of_Elements : C_Ext.long_long;
         Vector : Const_Pointer;
         Status : in out C.int)
         return C.int;
      pragma Import (C, Fits_Write_Col, "ffpcl");

      Status : C.int := 0;

   begin
      if Fits_Write_Col (File.Ptr,
                         Data_Type => CFITSIO_Data_Type,
                         Column_Number => C.int (Column_Number),
                         First_Row => C_Ext.long_long (First_Row),
                         First_Elem => C_Ext.long_long (First_Element),
                         Num_Of_Elements => C_Ext.long_long (Elements'Length),
                         Vector => Elements (Elements'First)'Unchecked_Access,
                         Status => Status) > 0 then
         raise AdaFITS_Error with Get_FITSIO_Error_String;
      end if;
   end Write_Column;

end AdaFITS.Generic_Column_Routines;
