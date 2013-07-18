--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into AdaFITS.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body AdaFITS.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Fits_Ptr (Gnattest_T : in out Test);
   procedure Test_Get_Fits_Ptr_5425cc (Gnattest_T : in out Test) renames Test_Get_Fits_Ptr;
--  id:2.1/5425ccb1c2cc4219/Get_Fits_Ptr/1/0/
   procedure Test_Get_Fits_Ptr (Gnattest_T : in out Test) is
   --  adafits.ads:32:4:Get_Fits_Ptr
--  end read only

      pragma Unreferenced (Gnattest_T);

      File : Fits_File;

   begin

      AUnit.Assertions.Assert
        (System."=" (Get_Fits_Ptr (File), System.Null_Address),
         "Get_Fits_Ptr returns garbage");

--  begin read only
   end Test_Get_Fits_Ptr;
--  end read only


--  begin read only
   procedure Test_Get_CFITSIO_Version (Gnattest_T : in out Test);
   procedure Test_Get_CFITSIO_Version_b3cd14 (Gnattest_T : in out Test) renames Test_Get_CFITSIO_Version;
--  id:2.1/b3cd14eeaf146ea0/Get_CFITSIO_Version/1/0/
   procedure Test_Get_CFITSIO_Version (Gnattest_T : in out Test) is
   --  adafits.ads:57:4:Get_CFITSIO_Version
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Interfaces.C.">" (Get_CFITSIO_Version, 0.0),
         "Get_CFITSIO_Version returns strange version numbers");

--  begin read only
   end Test_Get_CFITSIO_Version;
--  end read only


--  begin read only
   procedure Test_Get_FITSIO_Error_String (Gnattest_T : in out Test);
   procedure Test_Get_FITSIO_Error_String_673fd4 (Gnattest_T : in out Test) renames Test_Get_FITSIO_Error_String;
--  id:2.1/673fd4fc4ae42255/Get_FITSIO_Error_String/1/0/
   procedure Test_Get_FITSIO_Error_String (Gnattest_T : in out Test) is
   --  adafits.ads:60:4:Get_FITSIO_Error_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Err_Str : String := Get_FITSIO_Error_String;

   begin

      AUnit.Assertions.Assert
        (Err_Str'Length = 0,
         "Get_FITSIO_Error_String does not return valid error messages");

--  begin read only
   end Test_Get_FITSIO_Error_String;
--  end read only


--  begin read only
   procedure Test_Open_File (Gnattest_T : in out Test);
   procedure Test_Open_File_9339c4 (Gnattest_T : in out Test) renames Test_Open_File;
--  id:2.1/9339c47a9be32b9d/Open_File/1/0/
   procedure Test_Open_File (Gnattest_T : in out Test) is
   --  adafits.ads:63:4:Open_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : constant String := "../../test_data/tables.fits";
      File : Fits_File;

   begin

      File := Open_File (File_Name, READ_ONLY);
      AUnit.Assertions.Assert
        (System."/=" (File.Ptr, System.Null_Address),
         "Open_File cannot open files");
      Close_File (File);

      begin
         File := Open_File ("This does not exist", READ_ONLY);
         AUnit.Assertions.Assert (False,
                                  "Open_File does not raise an exception "
                                  & "when the file does not exist");
      exception
         when AdaFITS_Error => null;
      end;

--  begin read only
   end Test_Open_File;
--  end read only


--  begin read only
   procedure Test_Open_Data (Gnattest_T : in out Test);
   procedure Test_Open_Data_3d0d2e (Gnattest_T : in out Test) renames Test_Open_Data;
--  id:2.1/3d0d2ef596610942/Open_Data/1/0/
   procedure Test_Open_Data (Gnattest_T : in out Test) is
   --  adafits.ads:67:4:Open_Data
--  end read only

      pragma Unreferenced (Gnattest_T);


      File_Name : constant String := "../../test_data/tables.fits";
      File : Fits_File;

   begin

      File := Open_Data (File_Name, READ_ONLY);
      AUnit.Assertions.Assert
        (System."/=" (File.Ptr, System.Null_Address),
         "Open_Data cannot open files");
      Close_File (File);

--  begin read only
   end Test_Open_Data;
--  end read only


--  begin read only
   procedure Test_Open_Table (Gnattest_T : in out Test);
   procedure Test_Open_Table_d799e3 (Gnattest_T : in out Test) renames Test_Open_Table;
--  id:2.1/d799e3451a637f6c/Open_Table/1/0/
   procedure Test_Open_Table (Gnattest_T : in out Test) is
   --  adafits.ads:71:4:Open_Table
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : constant String := "../../test_data/tables.fits";
      File : Fits_File;

   begin

      File := Open_Table (File_Name, READ_ONLY);
      AUnit.Assertions.Assert
        (System."/=" (File.Ptr, System.Null_Address),
         "Open_Table cannot open files");
      Close_File (File);

--  begin read only
   end Test_Open_Table;
--  end read only


--  begin read only
   procedure Test_Open_Image (Gnattest_T : in out Test);
   procedure Test_Open_Image_52da17 (Gnattest_T : in out Test) renames Test_Open_Image;
--  id:2.1/52da179a42de2b9b/Open_Image/1/0/
   procedure Test_Open_Image (Gnattest_T : in out Test) is
   --  adafits.ads:75:4:Open_Image
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : constant String := "../../test_data/image.fits";
      File : Fits_File;

   begin

      File := Open_Image (File_Name, READ_ONLY);
      AUnit.Assertions.Assert
        (System."/=" (File.Ptr, System.Null_Address),
         "Open_Image cannot open files");
      Close_File (File);

--  begin read only
   end Test_Open_Image;
--  end read only


--  begin read only
   procedure Test_Create_File (Gnattest_T : in out Test);
   procedure Test_Create_File_68b26a (Gnattest_T : in out Test) renames Test_Create_File;
--  id:2.1/68b26ab889615461/Create_File/1/0/
   procedure Test_Create_File (Gnattest_T : in out Test) is
   --  adafits.ads:79:4:Create_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : String := "mem://";
      File : Fits_File;

   begin
      File := Create_File (File_Name);
   exception
      when E : AdaFITS_Error => AUnit.Assertions.Assert
           (False,
            "Create_File is not able to really create files: "
            & Ada.Exceptions.Exception_Message (E));
--  begin read only
   end Test_Create_File;
--  end read only


--  begin read only
   procedure Test_Close_File (Gnattest_T : in out Test);
   procedure Test_Close_File_dfb32f (Gnattest_T : in out Test) renames Test_Close_File;
--  id:2.1/dfb32fd7df1fca9d/Close_File/1/0/
   procedure Test_Close_File (Gnattest_T : in out Test) is
   --  adafits.ads:83:4:Close_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : String := "mem://";
      File : Fits_File;
      Columns : Column_Def_Array (1 .. 1) :=
        (1 => (Name =>   Unbounded_Str ("TEST"),
               Format => Unbounded_Str ("1J"),
               Unit =>   Unbounded_Str ("")));

   begin
      File := Create_File (File_Name);
      Create_Table (File, "TESTTBL", Columns);

      begin
         Close_File (File);
      exception
         when E : AdaFITS_Error => AUnit.Assertions.Assert
              (False,
               "Close_File does not work: "
               & Ada.Exceptions.Exception_Message (E));
      end;
--  begin read only
   end Test_Close_File;
--  end read only


--  begin read only
   procedure Test_Flush_Buffers (Gnattest_T : in out Test);
   procedure Test_Flush_Buffers_0f66b1 (Gnattest_T : in out Test) renames Test_Flush_Buffers;
--  id:2.1/0f66b16375d2b80b/Flush_Buffers/1/0/
   procedure Test_Flush_Buffers (Gnattest_T : in out Test) is
   --  adafits.ads:86:4:Flush_Buffers
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Simply test that the function works and raises no exceptions
      Flush_Buffers (Mem_File);

--  begin read only
   end Test_Flush_Buffers;
--  end read only


--  begin read only
   procedure Test_Flush_File (Gnattest_T : in out Test);
   procedure Test_Flush_File_892147 (Gnattest_T : in out Test) renames Test_Flush_File;
--  id:2.1/89214733b9d5acba/Flush_File/1/0/
   procedure Test_Flush_File (Gnattest_T : in out Test) is
   --  adafits.ads:89:4:Flush_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Simply test that the function works and raises no exceptions
      Flush_File (Mem_File);

--  begin read only
   end Test_Flush_File;
--  end read only


--  begin read only
   procedure Test_Delete_File (Gnattest_T : in out Test);
   procedure Test_Delete_File_8a263a (Gnattest_T : in out Test) renames Test_Delete_File;
--  id:2.1/8a263ae2f38b28e3/Delete_File/1/0/
   procedure Test_Delete_File (Gnattest_T : in out Test) is
   --  adafits.ads:92:4:Delete_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : String := "mem://";
      File : Fits_File;
      Columns : Column_Def_Array (1 .. 1) :=
        (1 => (Name =>   Unbounded_Str ("TEST"),
               Format => Unbounded_Str ("1J"),
               Unit =>   Unbounded_Str ("")));

   begin
      File := Create_File (File_Name);
      Create_Table (File, "TESTTBL", Columns);

      begin
         Delete_File (File);
      exception
         when E : AdaFITS_Error => AUnit.Assertions.Assert
              (False,
               "Delete_File does not work: "
               & Ada.Exceptions.Exception_Message (E));
      end;

--  begin read only
   end Test_Delete_File;
--  end read only


--  begin read only
   procedure Test_Get_File_Name (Gnattest_T : in out Test);
   procedure Test_Get_File_Name_23871c (Gnattest_T : in out Test) renames Test_Get_File_Name;
--  id:2.1/23871cde9f911682/Get_File_Name/1/0/
   procedure Test_Get_File_Name (Gnattest_T : in out Test) is
   --  adafits.ads:95:4:Get_File_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : constant String := "../../test_data/tables.fits";
      File : Fits_File;

   begin

      File := Open_File (File_Name, READ_ONLY);

      declare
         Test_Value : String := Get_File_Name (File);

      begin
         AUnit.Assertions.Assert
           (Test_Value = File_Name,
            "Get_File_Name has returned """
           & Test_Value & """ instead of """ & File_Name & """");
      end;

      Close_File (File);

--  begin read only
   end Test_Get_File_Name;
--  end read only


--  begin read only
   procedure Test_Create_Table (Gnattest_T : in out Test);
   procedure Test_Create_Table_9d79b8 (Gnattest_T : in out Test) renames Test_Create_Table;
--  id:2.1/9d79b84d9245c80d/Create_Table/1/0/
   procedure Test_Create_Table (Gnattest_T : in out Test) is
   --  adafits.ads:99:4:Create_Table
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : String := "mem://";
      File : Fits_File;
      Columns : Column_Def_Array (1 .. 2) :=
        ((Name =>   Unbounded_Str ("TEST1"),
          Format => Unbounded_Str ("1J"),
          Unit =>   Unbounded_Str ("Unit1")),
        (Name =>   Unbounded_Str ("TEST2"),
          Format => Unbounded_Str ("1D"),
          Unit =>   Unbounded_Str ("Unit2")));

   begin
      File := Create_File (File_Name);
      Create_Table (File, "TESTTBL", Columns);

      AUnit.Assertions.Assert
        (Get_Number_Of_Columns (File) = Columns'Length,
         "Create_Table has created a wrong number of columns");

      Close_File (File);

--  begin read only
   end Test_Create_Table;
--  end read only


--  begin read only
   procedure Test_Get_Number_Of_HDUs (Gnattest_T : in out Test);
   procedure Test_Get_Number_Of_HDUs_641e13 (Gnattest_T : in out Test) renames Test_Get_Number_Of_HDUs;
--  id:2.1/641e13153a62b589/Get_Number_Of_HDUs/1/0/
   procedure Test_Get_Number_Of_HDUs (Gnattest_T : in out Test) is
   --  adafits.ads:106:4:Get_Number_Of_HDUs
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      declare
         Num : Natural := Get_Number_Of_HDUs (Tables_File);
      begin
         AUnit.Assertions.Assert
           (Num = 6,
            "Get_Number_Of_HDUs reported the wrong number of HDUs ("
            & Natural'Image (Num) & ")");
      end;

--  begin read only
   end Test_Get_Number_Of_HDUs;
--  end read only


--  begin read only
   procedure Test_Get_Current_HDU_Index (Gnattest_T : in out Test);
   procedure Test_Get_Current_HDU_Index_14e641 (Gnattest_T : in out Test) renames Test_Get_Current_HDU_Index;
--  id:2.1/14e641bd971c693b/Get_Current_HDU_Index/1/0/
   procedure Test_Get_Current_HDU_Index (Gnattest_T : in out Test) is
   --  adafits.ads:110:4:Get_Current_HDU_Index
--  end read only

      pragma Unreferenced (Gnattest_T);

      Num : Positive;

   begin

      Move_To_HDU (Tables_File, 3);
      Num := Get_Current_HDU_Index (Tables_File);
      AUnit.Assertions.Assert
        (Num = 3,
         "Get_Current_HDU_Index reported the wrong index ("
         & Positive'Image (Num) & ")");

--  begin read only
   end Test_Get_Current_HDU_Index;
--  end read only


--  begin read only
   procedure Test_Get_Current_HDU_Type (Gnattest_T : in out Test);
   procedure Test_Get_Current_HDU_Type_4414d6 (Gnattest_T : in out Test) renames Test_Get_Current_HDU_Type;
--  id:2.1/4414d6343e7a695a/Get_Current_HDU_Type/1/0/
   procedure Test_Get_Current_HDU_Type (Gnattest_T : in out Test) is
   --  adafits.ads:114:4:Get_Current_HDU_Type
--  end read only

   begin

      Move_To_HDU (Tables_File, 1);

      AUnit.Assertions.Assert
        (Get_Current_HDU_Type (Tables_File) = IMAGE,
         "Get_Current_HDU_Type reported a wrong type for HDU 1");

      Move_To_HDU (Tables_File, 3);

      AUnit.Assertions.Assert
        (Get_Current_HDU_Type (Tables_File) = BINARY_TABLE,
         "Get_Current_HDU_Type reported a wrong type for HDU 3");

--  begin read only
   end Test_Get_Current_HDU_Type;
--  end read only


--  begin read only
   procedure Test_Get_Number_Of_Rows_For_Optimal_IO (Gnattest_T : in out Test);
   procedure Test_Get_Number_Of_Rows_For_Optimal_IO_a024b3 (Gnattest_T : in out Test) renames Test_Get_Number_Of_Rows_For_Optimal_IO;
--  id:2.1/a024b3e2ee864de7/Get_Number_Of_Rows_For_Optimal_IO/1/0/
   procedure Test_Get_Number_Of_Rows_For_Optimal_IO (Gnattest_T : in out Test) is
   --  adafits.ads:118:4:Get_Number_Of_Rows_For_Optimal_IO
--  end read only

      pragma Unreferenced (Gnattest_T);

      Num : Positive;

   begin

      Move_To_HDU (Tables_File, 3);
      -- Just check that the function does not assert --- if it does,
      -- then the most likely cause is a value returned by the CFITSIO
      -- function which falls outside the constraints of the Ada
      -- "Positive" type.
      Num := Get_Number_Of_Rows_For_Optimal_IO (Tables_File);

--  begin read only
   end Test_Get_Number_Of_Rows_For_Optimal_IO;
--  end read only


--  begin read only
   procedure Test_Get_Number_Of_Rows (Gnattest_T : in out Test);
   procedure Test_Get_Number_Of_Rows_4bebf2 (Gnattest_T : in out Test) renames Test_Get_Number_Of_Rows;
--  id:2.1/4bebf23686ec0392/Get_Number_Of_Rows/1/0/
   procedure Test_Get_Number_Of_Rows (Gnattest_T : in out Test) is
   --  adafits.ads:122:4:Get_Number_Of_Rows
--  end read only

      pragma Unreferenced (Gnattest_T);

      -- Associate each ADU with the number of rows in it
      Match_Table : constant array (2 .. 6) of Natural :=
        (2 => 8,
         3 => 9,
         4 => 3,
         5 => 2,
         6 => 9);

   begin

      for HDU_Number in Match_Table'Range loop
         Move_To_HDU (Tables_File, HDU_Number);

         AUnit.Assertions.Assert
           (Get_Number_Of_Rows (Tables_File) = Match_Table (HDU_Number),
            "The number of rows for HDU" & Natural'Image (HDU_Number) &
              " returned by Get_Number_Of_Rows is wrong");
      end loop;

--  begin read only
   end Test_Get_Number_Of_Rows;
--  end read only


--  begin read only
   procedure Test_Get_Number_Of_Columns (Gnattest_T : in out Test);
   procedure Test_Get_Number_Of_Columns_0a4a93 (Gnattest_T : in out Test) renames Test_Get_Number_Of_Columns;
--  id:2.1/0a4a9323a30af0b5/Get_Number_Of_Columns/1/0/
   procedure Test_Get_Number_Of_Columns (Gnattest_T : in out Test) is
   --  adafits.ads:126:4:Get_Number_Of_Columns
--  end read only

      pragma Unreferenced (Gnattest_T);

      -- Associate each ADU with the number of columns in it
      Match_Table : constant array (2 .. 6) of Natural :=
        (2 => 2,
         3 => 7,
         4 => 2,
         5 => 2,
         6 => 1);

   begin

      for HDU_Number in Match_Table'Range loop
         Move_To_HDU (Tables_File, HDU_Number);

         AUnit.Assertions.Assert
           (Get_Number_Of_Columns (Tables_File) = Match_Table (HDU_Number),
            "The number of rows for HDU" & Natural'Image (HDU_Number) &
              " returned by Get_Number_Of_Columns is wrong");
      end loop;

--  begin read only
   end Test_Get_Number_Of_Columns;
--  end read only


--  begin read only
   procedure Test_1_Move_To_HDU (Gnattest_T : in out Test);
   procedure Test_Move_To_HDU_3b3350 (Gnattest_T : in out Test) renames Test_1_Move_To_HDU;
--  id:2.1/3b33505b69d01aae/Move_To_HDU/1/0/
   procedure Test_1_Move_To_HDU (Gnattest_T : in out Test) is
   --  adafits.ads:130:4:Move_To_HDU
--  end read only

      pragma Unreferenced (Gnattest_T);

      Num_Of_HDUs : constant Natural := Get_Number_Of_HDUs (Tables_File);

   begin

      for HDU_Idx in 1 .. Num_Of_HDUs loop
         Move_To_HDU (Tables_File, HDU_Idx);

         AUnit.Assertions.Assert
           (Get_Current_HDU_Index (Tables_File) = HDU_Idx,
            "Get_Current_HDU_Index reported a wrong index for HDU"
            & Natural'Image (HDU_Idx));

      end loop;

--  begin read only
   end Test_1_Move_To_HDU;
--  end read only


--  begin read only
   procedure Test_2_Move_To_HDU (Gnattest_T : in out Test);
   procedure Test_Move_To_HDU_59e2af (Gnattest_T : in out Test) renames Test_2_Move_To_HDU;
--  id:2.1/59e2affe3932bb46/Move_To_HDU/0/0/
   procedure Test_2_Move_To_HDU (Gnattest_T : in out Test) is
   --  adafits.ads:134:4:Move_To_HDU
--  end read only

      pragma Unreferenced (Gnattest_T);

      Matching_HDU_Names : constant array (2 .. 4)
        of Ada.Strings.Unbounded.Unbounded_String :=
        (2 => Unbounded_Str ("LOGICAL_TABLE"),
         3 => Unbounded_Str ("INT_TABLE"),
         4 => Unbounded_Str ("REAL_TABLE")); -- This is the file's fault!

   begin

      for HDU_Idx in Matching_HDU_Names'Range loop
         Move_To_HDU (Tables_File, Ada_Str (Matching_HDU_Names (HDU_Idx)));

         AUnit.Assertions.Assert
           (Get_Current_HDU_Index (Tables_File) = HDU_Idx,
            "Moving to HDU named """ &
              Ada_Str (Matching_HDU_Names (HDU_Idx))
           & """ does not move to HDU number" & Natural'Image (HDU_Idx));
      end loop;

--  begin read only
   end Test_2_Move_To_HDU;
--  end read only


--  begin read only
   procedure Test_Get_Key_As_String (Gnattest_T : in out Test);
   procedure Test_Get_Key_As_String_fe8d01 (Gnattest_T : in out Test) renames Test_Get_Key_As_String;
--  id:2.1/fe8d01b26ddbbcb4/Get_Key_As_String/1/0/
   procedure Test_Get_Key_As_String (Gnattest_T : in out Test) is
   --  adafits.ads:140:4:Get_Key_As_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Move_To_HDU (Tables_File, 5);

      AUnit.Assertions.Assert
        (Get_Key_As_String (Tables_File, "TUNIT1") = "CMPLX1",
         "Get_Key_As_String returns wrong values");

--  begin read only
   end Test_Get_Key_As_String;
--  end read only


--  begin read only
   procedure Test_Get_Key_As_Long (Gnattest_T : in out Test);
   procedure Test_Get_Key_As_Long_e36bd5 (Gnattest_T : in out Test) renames Test_Get_Key_As_Long;
--  id:2.1/e36bd5e8b7e4e791/Get_Key_As_Long/1/0/
   procedure Test_Get_Key_As_Long (Gnattest_T : in out Test) is
   --  adafits.ads:145:4:Get_Key_As_Long
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Move_To_HDU (Tables_File, 5);

      AUnit.Assertions.Assert
        (Interfaces.C."=" (Get_Key_As_Long (Tables_File, "NAXIS1"), 24),
         "Get_Key_As_Long returns wrong values");

--  begin read only
   end Test_Get_Key_As_Long;
--  end read only


--  begin read only
   procedure Test_Get_Key_As_Double (Gnattest_T : in out Test);
   procedure Test_Get_Key_As_Double_837a15 (Gnattest_T : in out Test) renames Test_Get_Key_As_Double;
--  id:2.1/837a15bbea44b4a9/Get_Key_As_Double/1/0/
   procedure Test_Get_Key_As_Double (Gnattest_T : in out Test) is
   --  adafits.ads:150:4:Get_Key_As_Double
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Move_To_HDU (Tables_File, 3);

      AUnit.Assertions.Assert
        (Interfaces.C."=" (Get_Key_As_Double (Tables_File, "TZERO1"),
         Interfaces.C.double (-128.0)),
         "Get_Key_As_Double returns wrong values");

--  begin read only
   end Test_Get_Key_As_Double;
--  end read only


--  begin read only
   procedure Test_Set_Key (Gnattest_T : in out Test);
   procedure Test_Set_Key_3d5aa8 (Gnattest_T : in out Test) renames Test_Set_Key;
--  id:2.1/3d5aa85a2ff53940/Set_Key/1/0/
   procedure Test_Set_Key (Gnattest_T : in out Test) is
   --  adafits.ads:155:4:Set_Key
--  end read only

      pragma Unreferenced (Gnattest_T);

      File_Name : String := "test_set_key.fits";
   begin
      Set_Key (Mem_File, "SETKEYST", "Hello");

      Flush_File (Mem_File);

      AUnit.Assertions.Assert
        (Get_Key_As_String (Mem_File, "SETKEYST") = "Hello",
         "Get_Key_As_String returned a wrong value");

      declare
         S : Ada.Strings.Unbounded.Unbounded_String;
      begin
         S := Unbounded_Str (Get_Key_As_String (Mem_File,
                             "THISDOESNOTEXIST"));
         AUnit.Assertions.Assert
           (False,
            "Get_Key_As_String did not raise an exception");
      exception
         when AdaFITS_Error => null;
      end;
--  begin read only
   end Test_Set_Key;
--  end read only


--  begin read only
   procedure Test_Set_Key_As_Long (Gnattest_T : in out Test);
   procedure Test_Set_Key_As_Long_33c898 (Gnattest_T : in out Test) renames Test_Set_Key_As_Long;
--  id:2.1/33c8989135717278/Set_Key_As_Long/1/0/
   procedure Test_Set_Key_As_Long (Gnattest_T : in out Test) is
   --  adafits.ads:161:4:Set_Key_As_Long
--  end read only

      pragma Unreferenced (Gnattest_T);

      Value : Interfaces.C.long;

   begin
      Set_Key_As_Long (Mem_File, "SETKEYL", 12345);

      Flush_File (Mem_File);

      Value := Get_Key_As_Long (Mem_File, "SETKEYL");

      AUnit.Assertions.Assert
        (Interfaces.C."=" (Value, 12345),
         "Set_Key_As_Long set a wrong value: "
         & Interfaces.C.long'Image (Value) & " instead of 12345");

--  begin read only
   end Test_Set_Key_As_Long;
--  end read only


--  begin read only
   procedure Test_Set_Key_As_Double (Gnattest_T : in out Test);
   procedure Test_Set_Key_As_Double_a99af8 (Gnattest_T : in out Test) renames Test_Set_Key_As_Double;
--  id:2.1/a99af80a3237afaf/Set_Key_As_Double/1/0/
   procedure Test_Set_Key_As_Double (Gnattest_T : in out Test) is
   --  adafits.ads:167:4:Set_Key_As_Double
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Value : constant Interfaces.C.double := 12345.6;
      Value : Interfaces.C.double;

   begin
      Set_Key_As_Double (Mem_File, "SETKEYD", Ref_Value, 8);

      Flush_File (Mem_File);

      Value := Get_Key_As_Double (Mem_File, "SETKEYD");
      AUnit.Assertions.Assert
        (Abs (Value - Ref_Value) < 0.01,
         "Set_Key_As_Double set a wrong value:"
         & Interfaces.C.double'Image (Value) & " instead of "
         & Interfaces.C.double'Image (Ref_Value));

      declare
         S : Interfaces.C.double;
      begin
         S := Get_Key_As_Double (Mem_File, "THISDOESNOTEXIST");
         AUnit.Assertions.Assert
           (False,
            "Get_Key_As_Double did not raise an exception");
      exception
         when AdaFITS_Error => null;
      end;

--  begin read only
   end Test_Set_Key_As_Double;
--  end read only


--  begin read only
   procedure Test_Write_Comment (Gnattest_T : in out Test);
   procedure Test_Write_Comment_3802f7 (Gnattest_T : in out Test) renames Test_Write_Comment;
--  id:2.1/3802f786c49a1078/Write_Comment/1/0/
   procedure Test_Write_Comment (Gnattest_T : in out Test) is
   --  adafits.ads:174:4:Write_Comment
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Simply test that the function raises no exceptions
      Write_Comment (Mem_File, "This is a comment");

--  begin read only
   end Test_Write_Comment;
--  end read only


--  begin read only
   procedure Test_Write_History (Gnattest_T : in out Test);
   procedure Test_Write_History_43c582 (Gnattest_T : in out Test) renames Test_Write_History;
--  id:2.1/43c5824654aac20b/Write_History/1/0/
   procedure Test_Write_History (Gnattest_T : in out Test) is
   --  adafits.ads:178:4:Write_History
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Simply test that the function raises no exceptions
      Write_History (Mem_File, "This is a history comment");

--  begin read only
   end Test_Write_History;
--  end read only


--  begin read only
   procedure Test_Write_Current_Date (Gnattest_T : in out Test);
   procedure Test_Write_Current_Date_7f7af2 (Gnattest_T : in out Test) renames Test_Write_Current_Date;
--  id:2.1/7f7af2e641aa8aad/Write_Current_Date/1/0/
   procedure Test_Write_Current_Date (Gnattest_T : in out Test) is
   --  adafits.ads:182:4:Write_Current_Date
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  Simply test that the function raises no exceptions
      Write_Current_Date (Mem_File);

--  begin read only
   end Test_Write_Current_Date;
--  end read only


--  begin read only
   --  procedure Test_File_Name (Gnattest_T : in out Test);
   --  procedure Test_File_Name_96f3c7 (Gnattest_T : in out Test) renames Test_File_Name;
--  id:2.1/96f3c7023b66ad0b/File_Name/1/1/
   --  procedure Test_File_Name (Gnattest_T : in out Test) is
--  end read only
--
--        pragma Unreferenced (Gnattest_T);
--
--        File_Name : String := "../../test_data/table.fits";
--        File : Fits_File;
--
--     begin
--
--        File := Open_File (File_Name, READ_ONLY);
--
--        AUnit.Assertions.Assert
--          (File_Name = Get_File_Name (File),
--           "Test not implemented.");
--
--  begin read only
   --  end Test_File_Name;
--  end read only

end AdaFITS.Test_Data.Tests;
