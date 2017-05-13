-----------------------------------------------------------------------
--  helios-monitor -- Helios monitor
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
with Helios.Tools.Files;
package body Helios.Monitor.CPU is

   --  ------------------------------
   --  Start the agent and build the definition tree.
   --  ------------------------------
   overriding
   procedure Start (Agent : in out Agent_Type) is
   begin
      Agent.User_Time := Agent.Create_Definition ("user");
      Agent.Nice_Time := Agent.Create_Definition ("nice");
      Agent.Sys_Time := Agent.Create_Definition ("system");
      Agent.Idle_Time := Agent.Create_Definition ("idle");
      Agent.Iowait_Time := Agent.Create_Definition ("iowait");
      Agent.Irq_Time := Agent.Create_Definition ("irq");
      Agent.Softirq_Time := Agent.Create_Definition ("softirq");
      Agent.Steal_Time := Agent.Create_Definition ("steal");
      Agent.Guest_Time := Agent.Create_Definition ("guest");
      Agent.Guest_Nice_Time := Agent.Create_Definition ("guest_nice");
      Agent.Ctx_Count := Agent.Create_Definition ("context");
      Agent.Processes_Count := Agent.Create_Definition ("processes");
      Agent.Blocked_Count := Agent.Create_Definition ("procs_blocked");
      Agent.Running_Count := Agent.Create_Definition ("procs_running");
      Agent.Softirq_Count := Agent.Create_Definition ("softirq_count");
      Agent.Intr_Count := Agent.Create_Definition ("intr");
   end Start;

   --  ------------------------------
   --  Collect the values in the snapshot.
   --  ------------------------------
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Snapshot_Type) is
      Line : Helios.Tools.Files.File_Extractor;
   begin
      Line.Open ("/proc/stat");
      loop
         Line.Read;
         exit when Line.Is_Eof;
         if Line.Is_Field ("cpu") then
            Set_Value (Values, Agent.User_Time, Line.Get_Value (2));
            Set_Value (Values, Agent.Nice_Time, Line.Get_Value (3));
            Set_Value (Values, Agent.Sys_Time, Line.Get_Value (4));
            Set_Value (Values, Agent.Idle_Time, Line.Get_Value (5));
            Set_Value (Values, Agent.Iowait_Time, Line.Get_Value (6));
            Set_Value (Values, Agent.Irq_Time, Line.Get_Value (7));
            Set_Value (Values, Agent.Softirq_Time, Line.Get_Value (8));
            Set_Value (Values, Agent.Steal_Time, Line.Get_Value (9));
            Set_Value (Values, Agent.Guest_Time, Line.Get_Value (10));
            Set_Value (Values, Agent.Guest_Nice_Time, Line.Get_Value (11));

         elsif Line.Is_Field ("ctxt") then
            Set_Value (Values, Agent.Ctx_Count, Line.Get_Value (2));

         elsif Line.Is_Field ("processes") then
            Set_Value (Values, Agent.Processes_Count, Line.Get_Value (2));

         elsif Line.Is_Field ("softirq") then
            Set_Value (Values, Agent.Softirq_Count, Line.Get_Value (2));

         elsif Line.Is_Field ("procs_running") then
            Set_Value (Values, Agent.Running_Count, Line.Get_Value (2));

         elsif Line.Is_Field ("procs_blocked") then
            Set_Value (Values, Agent.Blocked_Count, Line.Get_Value (2));

         elsif Line.Is_Field ("intr") then
            Set_Value (Values, Agent.Intr_Count, Line.Get_Value (2));

         end if;
      end loop;
   end Collect;

end Helios.Monitor.CPU;
