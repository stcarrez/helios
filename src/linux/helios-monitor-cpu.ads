-----------------------------------------------------------------------
--  helios-monitor-cpu -- Linux CPU monitor
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
package Helios.Monitor.CPU is

   type Agent_Type is new Helios.Monitor.Agent_Type with record
      User_Time       : Definition_Type_Access;
      Nice_Time       : Definition_Type_Access;
      Sys_Time        : Definition_Type_Access;
      Idle_Time       : Definition_Type_Access;
      Iowait_Time     : Definition_Type_Access;
      Irq_Time        : Definition_Type_Access;
      Softirq_Time    : Definition_Type_Access;
      Steal_Time      : Definition_Type_Access;
      Guest_Time      : Definition_Type_Access;
      Guest_Nice_Time : Definition_Type_Access;
      Ctx_Count       : Definition_Type_Access;
      Softirq_Count   : Definition_Type_Access;
      Processes_Count : Definition_Type_Access;
      Running_Count   : Definition_Type_Access;
      Blocked_Count   : Definition_Type_Access;
      Intr_Count      : Definition_Type_Access;
   end record;

   --  Start the agent and build the definition tree.
   overriding
   procedure Start (Agent : in out Agent_Type);

   --  Collect the values in the snapshot.
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Snapshot_Type);

end Helios.Monitor.CPU;
