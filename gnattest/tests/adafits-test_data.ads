--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;

package AdaFITS.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   function Unbounded_Str
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function Ada_Str
     (S : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   Tables_File : Fits_File; -- Read-only file containing tables
   Image_File : Fits_File; -- Read-only file containing images
   Mem_File : Fits_File; -- Use this when you want to read/write

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

end AdaFITS.Test_Data;
