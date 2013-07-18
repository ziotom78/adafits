--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with AUnit.Test_Caller;
with Gnattest_Generated;

package body AdaFITS.Column_Routines.Test_Data.Tests.Suite is

   package Runner_1 is new AUnit.Test_Caller
     (GNATtest_Generated.GNATtest_Standard.AdaFITS.Column_Routines.Test_Data.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Case_1_1_Test_Read_Column_Of_Shorts_f4decb : aliased Runner_1.Test_Case;
   Case_2_1_Test_Write_Column_Of_Shorts_03f2ee : aliased Runner_1.Test_Case;
   Case_3_1_Test_Read_Column_Of_Longs_2ccc00 : aliased Runner_1.Test_Case;
   Case_4_1_Test_Write_Column_Of_Longs_03887b : aliased Runner_1.Test_Case;
   Case_5_1_Test_Read_Column_Of_Floats_44c589 : aliased Runner_1.Test_Case;
   Case_6_1_Test_Write_Column_Of_Floats_9b7842 : aliased Runner_1.Test_Case;
   Case_7_1_Test_Read_Column_Of_Doubles_3f0bcf : aliased Runner_1.Test_Case;
   Case_8_1_Test_Write_Column_Of_Doubles_0cb6b5 : aliased Runner_1.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Runner_1.Create
        (Case_1_1_Test_Read_Column_Of_Shorts_f4decb,
         "adafits-column_routines.ads:19:4:",
         Test_Read_Column_Of_Shorts_f4decb'Access);
      Runner_1.Create
        (Case_2_1_Test_Write_Column_Of_Shorts_03f2ee,
         "adafits-column_routines.ads:29:4:",
         Test_Write_Column_Of_Shorts_03f2ee'Access);
      Runner_1.Create
        (Case_3_1_Test_Read_Column_Of_Longs_2ccc00,
         "adafits-column_routines.ads:48:4:",
         Test_Read_Column_Of_Longs_2ccc00'Access);
      Runner_1.Create
        (Case_4_1_Test_Write_Column_Of_Longs_03887b,
         "adafits-column_routines.ads:58:4:",
         Test_Write_Column_Of_Longs_03887b'Access);
      Runner_1.Create
        (Case_5_1_Test_Read_Column_Of_Floats_44c589,
         "adafits-column_routines.ads:78:4:",
         Test_Read_Column_Of_Floats_44c589'Access);
      Runner_1.Create
        (Case_6_1_Test_Write_Column_Of_Floats_9b7842,
         "adafits-column_routines.ads:88:4:",
         Test_Write_Column_Of_Floats_9b7842'Access);
      Runner_1.Create
        (Case_7_1_Test_Read_Column_Of_Doubles_3f0bcf,
         "adafits-column_routines.ads:108:4:",
         Test_Read_Column_Of_Doubles_3f0bcf'Access);
      Runner_1.Create
        (Case_8_1_Test_Write_Column_Of_Doubles_0cb6b5,
         "adafits-column_routines.ads:118:4:",
         Test_Write_Column_Of_Doubles_0cb6b5'Access);

      Result.Add_Test (Case_1_1_Test_Read_Column_Of_Shorts_f4decb'Access);
      Result.Add_Test (Case_2_1_Test_Write_Column_Of_Shorts_03f2ee'Access);
      Result.Add_Test (Case_3_1_Test_Read_Column_Of_Longs_2ccc00'Access);
      Result.Add_Test (Case_4_1_Test_Write_Column_Of_Longs_03887b'Access);
      Result.Add_Test (Case_5_1_Test_Read_Column_Of_Floats_44c589'Access);
      Result.Add_Test (Case_6_1_Test_Write_Column_Of_Floats_9b7842'Access);
      Result.Add_Test (Case_7_1_Test_Read_Column_Of_Doubles_3f0bcf'Access);
      Result.Add_Test (Case_8_1_Test_Write_Column_Of_Doubles_0cb6b5'Access);

      return Result'Access;

   end Suite;

end AdaFITS.Column_Routines.Test_Data.Tests.Suite;
--  end read only
