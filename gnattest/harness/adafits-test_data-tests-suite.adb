--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body AdaFITS.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.AdaFITS.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_Get_Fits_Ptr_5425cc : aliased Runner_1.Test_Case;
   Case_2_1_Test_Get_CFITSIO_Version_b3cd14 : aliased Runner_1.Test_Case;
   Case_3_1_Test_Get_FITSIO_Error_String_673fd4 : aliased Runner_1.Test_Case;
   Case_4_1_Test_Open_File_9339c4 : aliased Runner_1.Test_Case;
   Case_5_1_Test_Open_Data_3d0d2e : aliased Runner_1.Test_Case;
   Case_6_1_Test_Open_Table_d799e3 : aliased Runner_1.Test_Case;
   Case_7_1_Test_Open_Image_52da17 : aliased Runner_1.Test_Case;
   Case_8_1_Test_Create_File_68b26a : aliased Runner_1.Test_Case;
   Case_9_1_Test_Close_File_dfb32f : aliased Runner_1.Test_Case;
   Case_10_1_Test_Flush_Buffers_0f66b1 : aliased Runner_1.Test_Case;
   Case_11_1_Test_Flush_File_892147 : aliased Runner_1.Test_Case;
   Case_12_1_Test_Delete_File_8a263a : aliased Runner_1.Test_Case;
   Case_13_1_Test_Get_File_Name_23871c : aliased Runner_1.Test_Case;
   Case_14_1_Test_Create_Table_9d79b8 : aliased Runner_1.Test_Case;
   Case_15_1_Test_Get_Number_Of_HDUs_641e13 : aliased Runner_1.Test_Case;
   Case_16_1_Test_Get_Current_HDU_Index_14e641 : aliased Runner_1.Test_Case;
   Case_17_1_Test_Get_Current_HDU_Type_4414d6 : aliased Runner_1.Test_Case;
   Case_18_1_Test_Get_Number_Of_Rows_For_Optimal_IO_a024b3 : aliased Runner_1.Test_Case;
   Case_19_1_Test_Get_Number_Of_Rows_4bebf2 : aliased Runner_1.Test_Case;
   Case_20_1_Test_Get_Number_Of_Columns_0a4a93 : aliased Runner_1.Test_Case;
   Case_21_1_Test_Move_To_HDU_3b3350 : aliased Runner_1.Test_Case;
   Case_22_1_Test_Move_To_HDU_59e2af : aliased Runner_1.Test_Case;
   Case_23_1_Test_Get_Key_As_String_fe8d01 : aliased Runner_1.Test_Case;
   Case_24_1_Test_Get_Key_As_Long_e36bd5 : aliased Runner_1.Test_Case;
   Case_25_1_Test_Get_Key_As_Double_837a15 : aliased Runner_1.Test_Case;
   Case_26_1_Test_Set_Key_3d5aa8 : aliased Runner_1.Test_Case;
   Case_27_1_Test_Set_Key_As_Long_33c898 : aliased Runner_1.Test_Case;
   Case_28_1_Test_Set_Key_As_Double_a99af8 : aliased Runner_1.Test_Case;
   Case_29_1_Test_Write_Comment_3802f7 : aliased Runner_1.Test_Case;
   Case_30_1_Test_Write_History_43c582 : aliased Runner_1.Test_Case;
   Case_31_1_Test_Write_Current_Date_7f7af2 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_Get_Fits_Ptr_5425cc,
         "adafits.ads:32:4:",
         Test_Get_Fits_Ptr_5425cc'Access);
      Runner_1.Create
        (Case_2_1_Test_Get_CFITSIO_Version_b3cd14,
         "adafits.ads:57:4:",
         Test_Get_CFITSIO_Version_b3cd14'Access);
      Runner_1.Create
        (Case_3_1_Test_Get_FITSIO_Error_String_673fd4,
         "adafits.ads:60:4:",
         Test_Get_FITSIO_Error_String_673fd4'Access);
      Runner_1.Create
        (Case_4_1_Test_Open_File_9339c4,
         "adafits.ads:63:4:",
         Test_Open_File_9339c4'Access);
      Runner_1.Create
        (Case_5_1_Test_Open_Data_3d0d2e,
         "adafits.ads:67:4:",
         Test_Open_Data_3d0d2e'Access);
      Runner_1.Create
        (Case_6_1_Test_Open_Table_d799e3,
         "adafits.ads:71:4:",
         Test_Open_Table_d799e3'Access);
      Runner_1.Create
        (Case_7_1_Test_Open_Image_52da17,
         "adafits.ads:75:4:",
         Test_Open_Image_52da17'Access);
      Runner_1.Create
        (Case_8_1_Test_Create_File_68b26a,
         "adafits.ads:79:4:",
         Test_Create_File_68b26a'Access);
      Runner_1.Create
        (Case_9_1_Test_Close_File_dfb32f,
         "adafits.ads:83:4:",
         Test_Close_File_dfb32f'Access);
      Runner_1.Create
        (Case_10_1_Test_Flush_Buffers_0f66b1,
         "adafits.ads:86:4:",
         Test_Flush_Buffers_0f66b1'Access);
      Runner_1.Create
        (Case_11_1_Test_Flush_File_892147,
         "adafits.ads:89:4:",
         Test_Flush_File_892147'Access);
      Runner_1.Create
        (Case_12_1_Test_Delete_File_8a263a,
         "adafits.ads:92:4:",
         Test_Delete_File_8a263a'Access);
      Runner_1.Create
        (Case_13_1_Test_Get_File_Name_23871c,
         "adafits.ads:95:4:",
         Test_Get_File_Name_23871c'Access);
      Runner_1.Create
        (Case_14_1_Test_Create_Table_9d79b8,
         "adafits.ads:99:4:",
         Test_Create_Table_9d79b8'Access);
      Runner_1.Create
        (Case_15_1_Test_Get_Number_Of_HDUs_641e13,
         "adafits.ads:106:4:",
         Test_Get_Number_Of_HDUs_641e13'Access);
      Runner_1.Create
        (Case_16_1_Test_Get_Current_HDU_Index_14e641,
         "adafits.ads:110:4:",
         Test_Get_Current_HDU_Index_14e641'Access);
      Runner_1.Create
        (Case_17_1_Test_Get_Current_HDU_Type_4414d6,
         "adafits.ads:114:4:",
         Test_Get_Current_HDU_Type_4414d6'Access);
      Runner_1.Create
        (Case_18_1_Test_Get_Number_Of_Rows_For_Optimal_IO_a024b3,
         "adafits.ads:118:4:",
         Test_Get_Number_Of_Rows_For_Optimal_IO_a024b3'Access);
      Runner_1.Create
        (Case_19_1_Test_Get_Number_Of_Rows_4bebf2,
         "adafits.ads:122:4:",
         Test_Get_Number_Of_Rows_4bebf2'Access);
      Runner_1.Create
        (Case_20_1_Test_Get_Number_Of_Columns_0a4a93,
         "adafits.ads:126:4:",
         Test_Get_Number_Of_Columns_0a4a93'Access);
      Runner_1.Create
        (Case_21_1_Test_Move_To_HDU_3b3350,
         "adafits.ads:130:4:",
         Test_Move_To_HDU_3b3350'Access);
      Runner_1.Create
        (Case_22_1_Test_Move_To_HDU_59e2af,
         "adafits.ads:134:4:",
         Test_Move_To_HDU_59e2af'Access);
      Runner_1.Create
        (Case_23_1_Test_Get_Key_As_String_fe8d01,
         "adafits.ads:140:4:",
         Test_Get_Key_As_String_fe8d01'Access);
      Runner_1.Create
        (Case_24_1_Test_Get_Key_As_Long_e36bd5,
         "adafits.ads:145:4:",
         Test_Get_Key_As_Long_e36bd5'Access);
      Runner_1.Create
        (Case_25_1_Test_Get_Key_As_Double_837a15,
         "adafits.ads:150:4:",
         Test_Get_Key_As_Double_837a15'Access);
      Runner_1.Create
        (Case_26_1_Test_Set_Key_3d5aa8,
         "adafits.ads:155:4:",
         Test_Set_Key_3d5aa8'Access);
      Runner_1.Create
        (Case_27_1_Test_Set_Key_As_Long_33c898,
         "adafits.ads:161:4:",
         Test_Set_Key_As_Long_33c898'Access);
      Runner_1.Create
        (Case_28_1_Test_Set_Key_As_Double_a99af8,
         "adafits.ads:167:4:",
         Test_Set_Key_As_Double_a99af8'Access);
      Runner_1.Create
        (Case_29_1_Test_Write_Comment_3802f7,
         "adafits.ads:174:4:",
         Test_Write_Comment_3802f7'Access);
      Runner_1.Create
        (Case_30_1_Test_Write_History_43c582,
         "adafits.ads:178:4:",
         Test_Write_History_43c582'Access);
      Runner_1.Create
        (Case_31_1_Test_Write_Current_Date_7f7af2,
         "adafits.ads:182:4:",
         Test_Write_Current_Date_7f7af2'Access);

      Result.Add_Test (Case_1_1_Test_Get_Fits_Ptr_5425cc'Access);
      Result.Add_Test (Case_2_1_Test_Get_CFITSIO_Version_b3cd14'Access);
      Result.Add_Test (Case_3_1_Test_Get_FITSIO_Error_String_673fd4'Access);
      Result.Add_Test (Case_4_1_Test_Open_File_9339c4'Access);
      Result.Add_Test (Case_5_1_Test_Open_Data_3d0d2e'Access);
      Result.Add_Test (Case_6_1_Test_Open_Table_d799e3'Access);
      Result.Add_Test (Case_7_1_Test_Open_Image_52da17'Access);
      Result.Add_Test (Case_8_1_Test_Create_File_68b26a'Access);
      Result.Add_Test (Case_9_1_Test_Close_File_dfb32f'Access);
      Result.Add_Test (Case_10_1_Test_Flush_Buffers_0f66b1'Access);
      Result.Add_Test (Case_11_1_Test_Flush_File_892147'Access);
      Result.Add_Test (Case_12_1_Test_Delete_File_8a263a'Access);
      Result.Add_Test (Case_13_1_Test_Get_File_Name_23871c'Access);
      Result.Add_Test (Case_14_1_Test_Create_Table_9d79b8'Access);
      Result.Add_Test (Case_15_1_Test_Get_Number_Of_HDUs_641e13'Access);
      Result.Add_Test (Case_16_1_Test_Get_Current_HDU_Index_14e641'Access);
      Result.Add_Test (Case_17_1_Test_Get_Current_HDU_Type_4414d6'Access);
      Result.Add_Test (Case_18_1_Test_Get_Number_Of_Rows_For_Optimal_IO_a024b3'Access);
      Result.Add_Test (Case_19_1_Test_Get_Number_Of_Rows_4bebf2'Access);
      Result.Add_Test (Case_20_1_Test_Get_Number_Of_Columns_0a4a93'Access);
      Result.Add_Test (Case_21_1_Test_Move_To_HDU_3b3350'Access);
      Result.Add_Test (Case_22_1_Test_Move_To_HDU_59e2af'Access);
      Result.Add_Test (Case_23_1_Test_Get_Key_As_String_fe8d01'Access);
      Result.Add_Test (Case_24_1_Test_Get_Key_As_Long_e36bd5'Access);
      Result.Add_Test (Case_25_1_Test_Get_Key_As_Double_837a15'Access);
      Result.Add_Test (Case_26_1_Test_Set_Key_3d5aa8'Access);
      Result.Add_Test (Case_27_1_Test_Set_Key_As_Long_33c898'Access);
      Result.Add_Test (Case_28_1_Test_Set_Key_As_Double_a99af8'Access);
      Result.Add_Test (Case_29_1_Test_Write_Comment_3802f7'Access);
      Result.Add_Test (Case_30_1_Test_Write_History_43c582'Access);
      Result.Add_Test (Case_31_1_Test_Write_Current_Date_7f7af2'Access);

      return Result'Access;

   end Suite;

end AdaFITS.Test_Data.Tests.Suite;
--  end read only
