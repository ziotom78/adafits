--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into AdaFITS.Column_Routines.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body AdaFITS.Column_Routines.Test_Data.Tests is


--  begin read only
   procedure Test_Read_Column_Of_Shorts (Gnattest_T : in out Test);
   procedure Test_Read_Column_Of_Shorts_f4decb (Gnattest_T : in out Test) renames Test_Read_Column_Of_Shorts;
--  id:2.1/f4decb0df94bf466/Read_Column_Of_Shorts/1/0/
   procedure Test_Read_Column_Of_Shorts (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:19:4:Read_Column_Of_Shorts
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Short_Array (1 .. 9) :=
        (16384, 16385, 16386, 16387, 16388, 16389, 16390, 16391, 16392);

      Vector : Short_Array (Ref_Vector'Range);

   begin

      Move_To_HDU (Tables_File, 3);
      Read_Column_Of_Shorts (Tables_File, 3, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Vector (I) = Ref_Vector (I),
            "The two vectors differ at position " & Positive'Image (I)
            & ": " & short'Image (Ref_Vector (I))
            & " (reference) viz. " &  short'Image (Vector (I)) );
      end loop;

--  begin read only
   end Test_Read_Column_Of_Shorts;
--  end read only


--  begin read only
   procedure Test_Write_Column_Of_Shorts (Gnattest_T : in out Test);
   procedure Test_Write_Column_Of_Shorts_03f2ee (Gnattest_T : in out Test) renames Test_Write_Column_Of_Shorts;
--  id:2.1/03f2eea5a9d5a573/Write_Column_Of_Shorts/1/0/
   procedure Test_Write_Column_Of_Shorts (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:29:4:Write_Column_Of_Shorts
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Short_Array (1 .. 3) := (1, 2, 3);
      Vector     : Short_Array (Ref_Vector'Range);

   begin

      Write_Column_Of_Shorts (Mem_File, 2, 1, 1, Ref_Vector);

      Flush_File (Mem_File);

      Read_Column_Of_Shorts (Mem_File, 2, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Vector (I) = Ref_Vector (I),
            "The two vectors differ at position " & Positive'Image (I));
      end loop;

--  begin read only
   end Test_Write_Column_Of_Shorts;
--  end read only


--  begin read only
   procedure Test_Read_Column_Of_Longs (Gnattest_T : in out Test);
   procedure Test_Read_Column_Of_Longs_2ccc00 (Gnattest_T : in out Test) renames Test_Read_Column_Of_Longs;
--  id:2.1/2ccc00840fc31ae9/Read_Column_Of_Longs/1/0/
   procedure Test_Read_Column_Of_Longs (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:48:4:Read_Column_Of_Longs
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Long_Array (1 .. 9) :=
        (1073741824, 1073741825, 1073741826, 1073741827,
         1073741828, 1073741829, 1073741830, 1073741831,
         1073741832);

      Vector : Long_Array (Ref_Vector'Range);

   begin

      Move_To_HDU (Tables_File, 3);
      Read_Column_Of_Longs (Tables_File, 5, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Vector (I) = Ref_Vector (I),
            "The two vectors differ at position " & Positive'Image (I)
            & ": " & long'Image (Ref_Vector (I))
            & " (reference) viz. " &  long'Image (Vector (I)) );
      end loop;

--  begin read only
   end Test_Read_Column_Of_Longs;
--  end read only


--  begin read only
   procedure Test_Write_Column_Of_Longs (Gnattest_T : in out Test);
   procedure Test_Write_Column_Of_Longs_03887b (Gnattest_T : in out Test) renames Test_Write_Column_Of_Longs;
--  id:2.1/03887b9baafc6b72/Write_Column_Of_Longs/1/0/
   procedure Test_Write_Column_Of_Longs (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:58:4:Write_Column_Of_Longs
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Long_Array (1 .. 3) := (1, 2, 3);
      Vector     : Long_Array (Ref_Vector'Range);

   begin

      Write_Column_Of_Longs (Mem_File, 2, 1, 1, Ref_Vector);

      Flush_File (Mem_File);

      Read_Column_Of_Longs (Mem_File, 2, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Vector (I) = Ref_Vector (I),
            "The two vectors differ at position " & Positive'Image (I));
      end loop;

--  begin read only
   end Test_Write_Column_Of_Longs;
--  end read only


--  begin read only
   procedure Test_Read_Column_Of_Floats (Gnattest_T : in out Test);
   procedure Test_Read_Column_Of_Floats_44c589 (Gnattest_T : in out Test) renames Test_Read_Column_Of_Floats;
--  id:2.1/44c589c8fd998585/Read_Column_Of_Floats/1/0/
   procedure Test_Read_Column_Of_Floats (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:78:4:Read_Column_Of_Floats
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Float_Array (1 .. 3) :=
        (1.0000, 2.71828, 1.234567875E+06);

      Vector : Float_Array (Ref_Vector'Range);

   begin

      Move_To_HDU (Tables_File, 4);
      Read_Column_Of_Floats (Tables_File, 1, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Abs (Vector (I) - Ref_Vector (I)) < 1.0E-5,
            "The two vectors differ at position " & Positive'Image (I)
            & ": " & C_float'Image (Ref_Vector (I))
            & " (reference) viz. " &  C_float'Image (Vector (I)) );
      end loop;

--  begin read only
   end Test_Read_Column_Of_Floats;
--  end read only


--  begin read only
   procedure Test_Write_Column_Of_Floats (Gnattest_T : in out Test);
   procedure Test_Write_Column_Of_Floats_9b7842 (Gnattest_T : in out Test) renames Test_Write_Column_Of_Floats;
--  id:2.1/9b7842e8d8374ddd/Write_Column_Of_Floats/1/0/
   procedure Test_Write_Column_Of_Floats (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:88:4:Write_Column_Of_Floats
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Float_Array (1 .. 3) := (1.1, 2.2, 3.3);
      Vector     : Float_Array (Ref_Vector'Range);

   begin

      Write_Column_Of_Floats (Mem_File, 3, 1, 1, Ref_Vector);

      Flush_File (Mem_File);

      Read_Column_Of_Floats (Mem_File, 3, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Abs (Vector (I) - Ref_Vector (I)) < Interfaces.C.C_float'Epsilon,
            "The two vectors differ at position " & Positive'Image (I));
      end loop;

--  begin read only
   end Test_Write_Column_Of_Floats;
--  end read only


--  begin read only
   procedure Test_Read_Column_Of_Doubles (Gnattest_T : in out Test);
   procedure Test_Read_Column_Of_Doubles_3f0bcf (Gnattest_T : in out Test) renames Test_Read_Column_Of_Doubles;
--  id:2.1/3f0bcf1329235060/Read_Column_Of_Doubles/1/0/
   procedure Test_Read_Column_Of_Doubles (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:108:4:Read_Column_Of_Doubles
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Double_Array (1 .. 3) :=
        (1.0, 3.14159265358979, 1.23456789876543E+51);

      Vector : Double_Array (Ref_Vector'Range);

   begin

      Move_To_HDU (Tables_File, 4);
      Read_Column_Of_Doubles (Tables_File, 2, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Abs (Vector (I) - Ref_Vector (I)) < 1.0E-5,
            "The two vectors differ at position " & Positive'Image (I)
            & ": " & double'Image (Ref_Vector (I))
            & " (reference) viz. " &  double'Image (Vector (I)) );
      end loop;

--  begin read only
   end Test_Read_Column_Of_Doubles;
--  end read only


--  begin read only
   procedure Test_Write_Column_Of_Doubles (Gnattest_T : in out Test);
   procedure Test_Write_Column_Of_Doubles_0cb6b5 (Gnattest_T : in out Test) renames Test_Write_Column_Of_Doubles;
--  id:2.1/0cb6b504f3122fd0/Write_Column_Of_Doubles/1/0/
   procedure Test_Write_Column_Of_Doubles (Gnattest_T : in out Test) is
   --  adafits-column_routines.ads:118:4:Write_Column_Of_Doubles
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Vector : constant Double_Array (1 .. 3) := (1.1, 2.2, 3.3);
      Vector     : Double_Array (Ref_Vector'Range);

   begin

      Write_Column_Of_Doubles (Mem_File, 4, 1, 1, Ref_Vector);

      Flush_File (Mem_File);

      Read_Column_Of_Doubles (Mem_File, 4, 1, 1, Vector'Length, Vector);

      for I in Vector'Range loop
         AUnit.Assertions.Assert
           (Abs (Vector (I) - Ref_Vector (I)) < Interfaces.C.double'Epsilon,
            "The two vectors differ at position " & Positive'Image (I));
      end loop;

--  begin read only
   end Test_Write_Column_Of_Doubles;
--  end read only

end AdaFITS.Column_Routines.Test_Data.Tests;
