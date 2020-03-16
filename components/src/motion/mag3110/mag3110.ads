------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with HAL;        use HAL;
with HAL.I2C;    use HAL.I2C;

private with Ada.Unchecked_Conversion;

package MAG3110 is

   type MAG3110_Magnetometer (Port : not null Any_I2C_Port)
   is tagged limited private;

   function Check_Device_Id (This : MAG3110_Magnetometer) return Boolean;
   --  Return False if device ID in incorrect or cannot be read

   type Axis_Data is range -2 ** 15 .. 2 ** 15 - 1
     with Size => 16;
   -- The datasheet says these values will never be outside -20_000 .. 20_000
   -- but the registers are full 16-bit signed integers.

   type All_Axes_Data is record
      X, Y, Z : Axis_Data;
   end record;

   type Data_Rate is range 0 .. 2 ** 3 - 1;
   type Oversampling_Ratio is (Oversample_16, Oversample_32,
                               Oversample_64, Oversample_128);

   procedure Configure (This         : in out MAG3110_Magnetometer;
                        Output_Rate  : in Data_Rate := 0;
                        Oversampling : in Oversampling_Ratio := Oversample_16;
                        Fast_Read    : in Boolean := False);


   function Read_Data (This : MAG3110_Magnetometer) return All_Axes_Data;

   -- TODO read/write offset
   --      read die temp

private
   type MAG3110_Magnetometer (Port : not null Any_I2C_Port) is tagged limited
     null record;

   type Register_Addresss is new UInt8;

   Device_Id  : constant := 16#C4#;

   Device_Address : constant I2C_Address := 16#1D#;

   DR_STATUS   : constant Register_Addresss := 16#00#;
   OUT_X_MSB   : constant Register_Addresss := 16#01#;
   OUT_X_LSB   : constant Register_Addresss := 16#02#;
   OUT_Y_MSB   : constant Register_Addresss := 16#03#;
   OUT_Y_LSB   : constant Register_Addresss := 16#04#;
   OUT_Z_MSB   : constant Register_Addresss := 16#05#;
   OUT_Z_LSB   : constant Register_Addresss := 16#06#;
   Who_Am_I    : constant Register_Addresss := 16#07#;
   SYSMOD      : constant Register_Addresss := 16#08#;
   OFF_X_MSB   : constant Register_Addresss := 16#09#;
   OFF_X_LSB   : constant Register_Addresss := 16#0A#;
   OFF_Y_MSB   : constant Register_Addresss := 16#0B#;
   OFF_Y_LSB   : constant Register_Addresss := 16#0C#;
   OFF_Z_MSB   : constant Register_Addresss := 16#0D#;
   OFF_Z_LSB   : constant Register_Addresss := 16#0E#;
   DIE_TEMP    : constant Register_Addresss := 16#0F#;
   CTRL_REG1   : constant Register_Addresss := 16#10#;
   CTRL_REG2   : constant Register_Addresss := 16#11#;

   type CTRL_REG1_Register is record
      Active       : Boolean := False;
      Trigger      : Boolean := False;
      F_Read       : Boolean := False;
      Oversampling : UInt2   := 0;
      Data_Rate    : UInt3   := 0;
   end record;

   for CTRL_REG1_Register use record
      Active       at 0 range 0 .. 0;
      Trigger      at 0 range 1 .. 1;
      F_Read       at 0 range 2 .. 2;
      Oversampling at 0 range 3 .. 4;
      Data_Rate    at 0 range 5 .. 7;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
     (CTRL_REG1_Register, UInt8);
   function To_Reg is new Ada.Unchecked_Conversion
     (UInt8, CTRL_REG1_Register);

   type CTRL_REG2_Register is record
      Reset      : Boolean := False;
      Raw        : Boolean := False;
      Auto_Reset : Boolean := False;
   end record;

   for CTRL_REG2_Register use record
      Reset      at 0 range 4 .. 4;
      Raw        at 0 range 5 .. 5;
      Auto_Reset at 0 range 7 .. 7;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
     (CTRL_REG2_Register, UInt8);
   function To_Reg is new Ada.Unchecked_Conversion
     (UInt8, CTRL_REG2_Register);

end MAG3110;
