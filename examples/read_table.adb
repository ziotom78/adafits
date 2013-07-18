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

with Ada.Text_IO;
with Ada.Exceptions;
with AdaFITS; use AdaFITS;
with AdaFITS.Column_Routines; use AdaFITS.Column_Routines;

procedure Read_Table is

   File : Fits_File;
   Num_Of_Rows : Natural;

begin
   File := Open_Table (File_Name => "../test_data/tables.fits",
		       Mode => AdaFITS.READ_ONLY);
   
   Move_To_HDU (File, 4);
   
   Num_Of_Rows := Get_Number_Of_Rows (File);
   declare
      Values : Double_Array (1 .. Num_Of_Rows);
   begin
      Read_Column_Of_Doubles (File,
			      Column_Number => 2,
			      First_Row => 1,
			      First_Element => 1,
			      Num_Of_Elements => Values'Length,
			      Destination => Values);

      for I in Values'Range loop
	 Ada.Text_IO.Put_Line (Natural'Image (I) & ": " &
			       Double'Image (Values (I)));
      end loop;
   end;

   Close_File (File);

exception
   when E : AdaFITS_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end Read_Table;
