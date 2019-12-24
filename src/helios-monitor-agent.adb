-----------------------------------------------------------------------
--  helios-monitor-agent -- Helios monitor agent
--  Copyright (C) 2017, 2019 Stephane Carrez
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
with Helios.Monitor.Sysfile;

package body Helios.Monitor.Agent is

   function Get_Agent (Name   : in String;
                       Config : in Util.Properties.Manager) return Agent_Type_Access;

   Cpu_Mon     : aliased Helios.Monitor.CPU.Agent_Type;
   Ifnet_Mon   : aliased Helios.Monitor.Ifnet.Agent_Type;
   Disk_Mon    : aliased Helios.Monitor.Disks.Agent_Type;

   --  ------------------------------
   --  Get the monitoring agent given its name and configuration.
   --  ------------------------------
   function Get_Agent (Name   : in String;
                       Config : in Util.Properties.Manager) return Agent_Type_Access is
   begin
      if Name = "ifnet" then
         return Ifnet_Mon'Access;
      elsif Name = "cpu" then
         return Cpu_Mon'Access;
      elsif Name = "disks" then
         return Disk_Mon'Access;
      end if;
      if Config.Get ("plugin", "") = "sysfile" then
         return new Helios.Monitor.Sysfile.Agent_Type;
      else
         return null;
      end if;
   end Get_Agent;

   --  ------------------------------
   --  Configure the agent plugins.
   --  ------------------------------
   procedure Configure (Runtime : in out Runtime_Type;
                        Config  : in Util.Properties.Manager) is
      use type Ada.Real_Time.Time_Span;

      procedure Process (Name  : in String;
                         Value : in Util.Properties.Value);
      procedure Configure (Name   : in String;
                           Config : in Util.Properties.Manager);
      procedure Build_Queue (Agent : in out Agent_Type'Class);
      procedure Start_Timer (Agent : in out Agent_Type'Class);

      procedure Configure (Name   : in String;
                           Config : in Util.Properties.Manager) is
         Agent : constant Agent_Type_Access := Get_Agent (Name, Config);
      begin
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
         Count : constant Natural := 1 + (Runtime.Report_Period / Agent.Period);
      begin
         Helios.Datas.Initialize (Agent.Data, Agent.Node, Count);
      end Build_Queue;

      Start_Delay  : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (100);
      Spread_Delay : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (100);

      procedure Start_Timer (Agent : in out Agent_Type'Class) is
         Timer : Util.Events.Timers.Timer_Ref;
      begin
         Runtime.Timers.Set_Timer (Agent'Unchecked_Access, Timer, Start_Delay);
         Start_Delay := Start_Delay + Spread_Delay;
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
      use Ada.Real_Time;
      Deadline  : Ada.Real_Time.Time;
      Stop_Time : constant Ada.Real_Time.Time
        := Ada.Real_Time.Clock + Ada.Real_Time.Seconds (300);
   begin
      while not Runtime.Stop loop
         Runtime.Timers.Process (Deadline);
         delay until Deadline;
         exit when Stop_Time < Ada.Real_Time.Clock;
      end loop;
   end Run;

end Helios.Monitor.Agent;
