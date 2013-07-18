--------------------------------------------------------------------
--  AdaFITS: a set of Ada 2005 bindings to the CFITSIO library
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

with Interfaces.C;
with AdaFITS;
with AdaFITS.Generic_Column_Routines;

package AdaFITS.Column_Routines is

   ----------------------------------------------------------------------

   subtype Short is Interfaces.C.short;
   type Short_Array is array (Positive range <>)
     of aliased Interfaces.C.short;
   pragma Convention (C, Short_Array);

   package Short_Column is new AdaFITS.Generic_Column_Routines
     (Element => Interfaces.C.Short,
      Element_Array => Short_Array,
      CFITSIO_Data_Type => AdaFITS.CFITSIO_TYPE_SHORT);

   procedure Read_Column_Of_Shorts
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Short_Array;
      Null_Value : Interfaces.C.short := Interfaces.C.short (0))
      renames Short_Column.Read_Column;

   procedure Write_Column_Of_Shorts
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Short_Array)
      renames Short_Column.Write_Column;
   ----------------------------------------------------------------------

   subtype Long is Interfaces.C.long;
   type Long_Array is array (Positive range <>)
     of aliased Interfaces.C.long;
   pragma Convention (C, Long_Array);

   package Long_Column is new AdaFITS.Generic_Column_Routines
     (Element => Interfaces.C.Long,
      Element_Array => Long_Array,
      CFITSIO_Data_Type => AdaFITS.CFITSIO_TYPE_LONG);

   procedure Read_Column_Of_Longs
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Long_Array;
      Null_Value : Interfaces.C.long := Interfaces.C.long (0))
      renames Long_Column.Read_Column;

   procedure Write_Column_Of_Longs
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Long_Array)
      renames Long_Column.Write_Column;

   ----------------------------------------------------------------------

   subtype Float is Interfaces.C.C_float;
   type Float_Array is array (Positive range <>)
     of aliased Interfaces.C.C_float;
   pragma Convention (C, Float_Array);

   package Float_Column is new AdaFITS.Generic_Column_Routines
     (Element => Interfaces.C.C_float,
      Element_Array => Float_Array,
      CFITSIO_Data_Type => AdaFITS.CFITSIO_TYPE_FLOAT);

   procedure Read_Column_Of_Floats
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Float_Array;
      Null_Value : Interfaces.C.C_float := FLOAT_NULL_VALUE)
      renames Float_Column.Read_Column;

   procedure Write_Column_Of_Floats
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Float_Array)
      renames Float_Column.Write_Column;

   ----------------------------------------------------------------------

   subtype Double is Interfaces.C.double;
   type Double_Array is array (Positive range <>)
     of aliased Interfaces.C.double;
   pragma Convention (C, Double_Array);

   package Double_Column is new AdaFITS.Generic_Column_Routines
     (Element => Interfaces.C.double,
      Element_Array => Double_Array,
      CFITSIO_Data_Type => AdaFITS.CFITSIO_TYPE_DOUBLE);

   procedure Read_Column_Of_Doubles
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Double_Array;
      Null_Value : Interfaces.C.double := DOUBLE_NULL_VALUE)
      renames Double_Column.Read_Column;

   procedure Write_Column_Of_Doubles
     (File : in out Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Double_Array)
      renames Double_Column.Write_Column;

end AdaFITS.Column_Routines;
