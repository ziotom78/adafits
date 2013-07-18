--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;
with Ada.Exceptions;
with Interfaces.C; use Interfaces.C;

package AdaFITS.Column_Routines.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   Tables_File : Fits_File; -- Read-only file containing tables
   Mem_File : Fits_File;    -- Memory file containing one table

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

end AdaFITS.Column_Routines.Test_Data;
