------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

package body MAG3110 is

   function To_Axis_Data is new Ada.Unchecked_Conversion (UInt16, Axis_Data);

   function Read_Register (This : MAG3110_Magnetometer'Class;
                           Addr : Register_Addresss) return UInt8;

   procedure Write_Register (This : MAG3110_Magnetometer'Class;
                             Addr : Register_Addresss;
                             Val  : UInt8);

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register (This : MAG3110_Magnetometer'Class;
                           Addr : Register_Addresss) return UInt8
   is
      Data   : I2C_Data (1 .. 1);
      Status : I2C_Status;
   begin
      This.Port.Mem_Read (Addr          => Device_Address,
                          Mem_Addr      => UInt16 (Addr),
                          Mem_Addr_Size => Memory_Size_8b,
                          Data          => Data,
                          Status        => Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
      return Data (Data'First);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (This : MAG3110_Magnetometer'Class;
                             Addr : Register_Addresss;
                             Val  : UInt8)
   is
      Status : I2C_Status;
   begin
      This.Port.Mem_Write (Addr          => Device_Address,
                           Mem_Addr      => UInt16 (Addr),
                           Mem_Addr_Size => Memory_Size_8b,
                           Data          => (1 => Val),
                           Status        => Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end Write_Register;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This                : in out MAG3110_Magnetometer;
                        Output_Rate         : in Data_Rate := 0;
                        Oversampling        : in Oversampling_Ratio := Oversample_16;
                        Fast_Read           : in Boolean := False)
   is
      CTRL1 : CTRL_REG1_Register;
      CTRL2 : CTRL_REG2_Register;
   begin

      --  Enter standby mode to be able to set configuration
      CTRL1.Active       := False;
      This.Write_Register (CTRL_REG1, To_UInt8 (CTRL1));

      CTRL2.Reset        := False;
      CTRL2.Raw          := False;
      CTRL2.Auto_Reset   := True;
      This.Write_Register (CTRL_REG2, To_UInt8 (CTRL2));

      CTRL1.Active       := True;
      CTRL1.Data_Rate    := UInt3 (Output_Rate);
      CTRL1.Oversampling := Oversampling_Ratio'Pos (Oversampling);
      CTRL1.F_Read       := Fast_Read;
      This.Write_Register (CTRL_REG1, To_UInt8 (CTRL1));

   end Configure;

   ---------------------
   -- Check_Device_Id --
   ---------------------

   function Check_Device_Id (This : MAG3110_Magnetometer) return Boolean is
   begin
      return Read_Register (This, Who_Am_I) = Device_Id;
   end Check_Device_Id;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (This : MAG3110_Magnetometer) return All_Axes_Data is
      function Convert (MSB, LSB : UInt8) return Axis_Data;

      -------------
      -- Convert --
      -------------

      function Convert (MSB, LSB : UInt8) return Axis_Data is
         Tmp : UInt16;
      begin
         Tmp := UInt16 (Shift_Left (MSB, 8));
         Tmp := Tmp or Uint16 (LSB);
         return To_Axis_Data (Tmp);
      end Convert;

      Status : I2C_Status;
      Data   : I2C_Data (1 .. 7);
      Ret    : All_Axes_Data;
   begin
      This.Port.Mem_Read (Addr          => Device_Address,
                          Mem_Addr      => UInt16 (DR_STATUS),
                          Mem_Addr_Size => Memory_Size_8b,
                          Data          => Data,
                          Status        => Status);

      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;

      Ret.X := Convert (Data (2), Data (3));
      Ret.Y := Convert (Data (4), Data (5));
      Ret.Z := Convert (Data (6), Data (7));
      return Ret;
   end Read_Data;

end MAG3110;
