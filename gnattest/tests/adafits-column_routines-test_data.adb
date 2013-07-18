--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Strings.Unbounded;

package body AdaFITS.Column_Routines.Test_Data is

   function Unbounded_Str
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);

      Tables_File_Name : constant String := "../../test_data/tables.fits";
      Columns : constant Column_Def_Array (1 .. 4) :=
        ((Name => Unbounded_Str ("SHORT"),
          Format => Unbounded_Str ("1I"),
          Unit => Unbounded_Str ("")),
         (Name => Unbounded_Str ("LONG"),
          Format => Unbounded_Str ("1J"),
          Unit => Unbounded_Str ("")),
         (Name => Unbounded_Str ("FLOAT"),
          Format => Unbounded_Str ("1E"),
          Unit => Unbounded_Str ("")),
         (Name => Unbounded_Str ("DOUBLE"),
          Format => Unbounded_Str ("1D"),
          Unit => Unbounded_Str (""))
        );

   begin
      Tables_File := Open_File (Tables_File_Name, READ_ONLY);

      Mem_File := Create_File ("mem://", False);
      Create_Table (Mem_File, "TEST", Columns);
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      Close_File (Tables_File);
      Close_File (Mem_File);
   end Tear_Down;

end AdaFITS.Column_Routines.Test_Data;
