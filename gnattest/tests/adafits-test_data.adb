--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body AdaFITS.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);

      Tables_File_Name : constant String := "../../test_data/tables.fits";
      Image_File_Name : constant String := "../../test_data/image.fits";

      Columns : constant Column_Def_Array (1 .. 1) :=
        (1 => (Name => Unbounded_Str ("TEST"),
               Format => Unbounded_Str ("1D"),
               Unit => Unbounded_Str ("")));

   begin
      Tables_File := Open_File (Tables_File_Name, READ_ONLY);
      Image_File := Open_File (Image_File_Name, READ_ONLY);

      Mem_File := Create_File ("mem://", False);
      Create_Table (Mem_File, "TEST", Columns);
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      Close_File (Tables_File);
      Close_File (Image_File);

      Close_File (Mem_File);
   end Tear_Down;

end AdaFITS.Test_Data;
