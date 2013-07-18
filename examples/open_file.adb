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

with Ada.Text_IO;
with Ada.Exceptions;
with AdaFITS; use AdaFITS;

procedure Open_File is
   
   File : Fits_File;
   
begin
   File := Open_Table (File_Name => "test.fits",
		       Mode => READ_ONLY);
   --  Do whatever you want with "File"
   Close_File (File);
exception
   when E : AdaFITS_Error =>
      Ada.Text_IO.Put_Line ("Error reading the file: "
			      & Ada.Exceptions.Exception_Message (E));
end Open_File;
