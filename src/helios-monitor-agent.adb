-----------------------------------------------------------------------
--  helios-monitor-agent -- Helios monitor agent
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
with Helios.Monitor.CPU;
with Helios.Monitor.Ifnet;
with Helios.Monitor.Disks;

package body Helios.Monitor.Agent is

   Cpu_Mon     : aliased Helios.Monitor.CPU.Agent_Type;
   Ifnet_Mon   : aliased Helios.Monitor.Ifnet.Agent_Type;
   Disk_Mon    : aliased Helios.Monitor.Disks.Agent_Type;

   --  ------------------------------
   --  Configure the agent plugins.
   --  ------------------------------
   procedure Configure (Runtime : in out Runtime_Type;
                        Config  : in Util.Properties.Manager) is
      procedure Process (Name  : in String;
                         Value : in Util.Properties.Value);

      procedure Configure (Name   : in String;
                           Config : in Util.Properties.Manager) is
         Agent : Agent_Type_Access;
      begin
         if Name = "helios" then
            Runtime.Report_Period := Get_Period (Config, "report_period", REPORT_PERIOD);
         elsif Name = "ifnet" then
            Agent := Ifnet_Mon'Access;
         elsif Name = "cpu" then
            Agent := Cpu_Mon'Access;
         elsif Name = "disks" then
            Agent := Disk_Mon'Access;
         end if;
         if Agent /= null then
            Register (Agent, Name, Config);
         end if;
      end Configure;

      --  Identify monitor plugins and configure them.
      procedure Process (Name  : in String;
                         Value : in Util.Properties.Value) is
      begin
         if Util.Properties.Is_Manager (Value) then
            Configure (Name, Util.Properties.To_Manager (Value));
         end if;
      end Process;

      procedure Build_Queue (Agent : in out Agent_Type'Class) is
         use type Ada.Real_Time.Time_Span;

         Count : Natural := 1 + (Runtime.Report_Period / Agent.Period);
      begin
         Helios.Datas.Initialize (Agent.Data, Agent.Node, Count);
      end Build_Queue;

      procedure Start_Timer (Agent : in out Agent_Type'Class) is
         Timer : Util.Events.Timers.Timer_Ref;
      begin
         Runtime.Timers.Set_Timer (Agent'Unchecked_Access, Timer, Agent.Period);
      end Start_Timer;

   begin
      Config.Iterate (Process'Access);
      Iterate (Build_Queue'Access);
      Iterate (Start_Timer'Access);
   end Configure;

   --  ------------------------------
   --  Run the monitoring agent main loop.
   --  ------------------------------
   procedure Run (Runtime : in out Runtime_Type) is
      Deadline : Ada.Real_Time.Time;
   begin
      while not Runtime.Stop loop
         Runtime.Timers.Process (Deadline);
         delay until Deadline;
      end loop;
   end Run;

end Helios.Monitor.Agent;
