with Ada.Streams.Stream_IO;
with HAL;

package body Linux.SPI is
    use Ada.Streams.Stream_IO;
    use HAL.SPI;

    function Data_Size (This : SPI_Port) return SPI_Data_Size is
        pragma Unreferenced (This);
    begin
        return Data_Size_8b;
    end Data_Size;

    procedure Transmit
        (This    : in out SPI_Port;
         Data    : SPI_Data_8b;
         Status  : out SPI_Status;
         Timeout : Natural := 1000) is
        pragma Unreferenced (Timeout);
        Output_File   : File_Type;
        Output_Stream : Stream_Access;
    begin
        Open (Output_File, Out_File, This.Device_File.all);
        Output_Stream := Stream (Output_File);
        for Word of Data loop
            HAL.UInt8'Write (Output_Stream, Word);
        end loop;
        Close (Output_File);
        Status := Ok;
    end Transmit;

    procedure Transmit
        (This   : in out SPI_Port;
         Data   : SPI_Data_16b;
         Status : out SPI_Status;
         Timeout : Natural := 1000) is
        pragma Unreferenced (Timeout);
        Output_File   : File_Type;
        Output_Stream : Stream_Access;
    begin
        Open (Output_File, Out_File, This.Device_File.all);
        Output_Stream := Stream (Output_File);
        for Word of Data loop
            HAL.UInt16'Write (Output_Stream, Word);
        end loop;
        Close (Output_File);
        Status := Ok;
    end Transmit;

    procedure Receive
        (This    : in out SPI_Port;
         Data    : out SPI_Data_8b;
         Status  : out SPI_Status;
         Timeout : Natural := 1000) is
        pragma Unreferenced (Timeout);
        Input_File   : File_Type;
        Input_Stream : Stream_Access;
    begin
        Open (Input_File, In_File, This.Device_File.all);
        Input_Stream := Stream (Input_File);
        for Word of Data loop
            HAL.UInt8'Read (Input_Stream, Word);
        end loop;
        Close (Input_File);
        Status := Ok;
    end Receive;
    
    procedure Receive
        (This    : in out SPI_Port;
         Data    : out HAL.SPI.SPI_Data_16b;
         Status  : out HAL.SPI.SPI_Status;
         Timeout : Natural := 1000) is
        pragma Unreferenced (Timeout);
        Input_File   : File_Type;
        Input_Stream : Stream_Access;
    begin
        Open (Input_File, In_File, This.Device_File.all);
        Input_Stream := Stream (Input_File);
        for Word of Data loop
            HAL.UInt16'Read (Input_Stream, Word);
        end loop;
        Close (Input_File);
        Status := Ok;
    end Receive;
end Linux.SPI;
