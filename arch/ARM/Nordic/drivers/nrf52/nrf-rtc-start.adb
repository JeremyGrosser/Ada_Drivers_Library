separate(nRF.RTC)
procedure Start (This : Real_Time_Counter) is
begin
   This.Periph.TASKS_START.TASKS_START := True;
end Start;