with HAL.SPI;

package Linux.SPI is
    type SPI_Port (Device_File : access String) is
        limited new HAL.SPI.SPI_Port with null record;

    overriding
    function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size;

    overriding
    procedure Transmit
        (This    : in out SPI_Port;
         Data    : HAL.SPI.SPI_Data_8b;
         Status  : out HAL.SPI.SPI_Status;
         Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

    overriding
    procedure Receive
        (This    : in out SPI_Port;
         Data    : out HAL.SPI.SPI_Data_8b;
         Status  : out HAL.SPI.SPI_Status;
         Timeout : Natural := 1000);
    
    overriding
    procedure Receive
        (This    : in out SPI_Port;
         Data    : out HAL.SPI.SPI_Data_16b;
         Status  : out HAL.SPI.SPI_Status;
         Timeout : Natural := 1000);
end Linux.SPI;
