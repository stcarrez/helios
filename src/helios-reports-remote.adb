-----------------------------------------------------------------------
--  helios-reports-remote -- Send reports to a remote server using REST API
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

package body Helios.Reports.Remote is

   task body Report_Task_Type is
      R      : Remote_Report_Access;
      Data   : Helios.Datas.Report_Queue_Type;
   begin
      select
         accept Start (Report : in Remote_Report_Access) do
            R := Report;
         end Start;
      end select;
      loop
         R.Queue.Dequeue (Data);
      end loop;
   end Report_Task_Type;

   procedure Start (Report : in Remote_Report_Access) is
   begin
      Report.Reporter := new Report_Task_Type;
      Report.Reporter.Start (Report);
   end Start;

   --  The timer handler executed when the timer deadline has passed.
   overriding
   procedure Time_Handler (Report : in out Remote_Report_Type;
                           Event  : in out Util.Events.Timers.Timer_Ref'Class) is
      use type Ada.Real_Time.Time_Span;
   begin
      Report.Queue.Enqueue (Helios.Datas.Get_Report);
      if Report.Period /= Ada.Real_Time.Time_Span_Zero then
         Event.Repeat (Report.Period);
      end if;
   end Time_Handler;

end Helios.Reports.Remote;
