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
         Timer : Util.Events.Timers.Timer_Ref;
      begin
         if Name = "ifnet" then
            Agent := Ifnet_Mon'Access;
         elsif Name = "cpu" then
            Agent := Cpu_Mon'Access;
         elsif Name = "disks" then
            Agent := Disk_Mon'Access;
         end if;
         if Agent /= null then
            Register (Agent, Name, Config);
            Runtime.Timers.Set_Timer (Agent.all'Access, Timer, Agent.Period);
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

   begin
      Config.Iterate (Process'Access);
   end Configure;

end Helios.Monitor.Agent;
