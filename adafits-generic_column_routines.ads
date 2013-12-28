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

with AdaFITS;
with Interfaces.C;

generic

   type Element is private;
   type Element_Array is array (Positive range <>) of aliased Element;

   CFITSIO_Data_Type : Interfaces.C.int;

package AdaFITS.Generic_Column_Routines is

   procedure Read_Column
     (File : in out AdaFITS.Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Num_Of_Elements : Natural;
      Destination : out Element_Array;
      Null_Value : Element);

   procedure Write_Column
     (File : in out AdaFITS.Fits_File;
      Column_Number : Positive;
      First_Row : Positive;
      First_Element : Positive;
      Elements : Element_Array);

end AdaFITS.Generic_Column_Routines;
