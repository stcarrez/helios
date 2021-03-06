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

package Helios.Monitor.Agent is

   --  Default period to report snapshots (in seconds).
   REPORT_PERIOD : constant Natural := 300;

   type Runtime_Type is limited record
      Timers        : Util.Events.Timers.Timer_List;
      Report_Period : Ada.Real_Time.Time_Span;
      Stop          : Boolean := False;
   end record;

   --  Configure the agent plugins.
   procedure Configure (Runtime : in out Runtime_Type;
                        Config  : in Util.Properties.Manager);

   --  Run the monitoring agent main loop.
   procedure Run (Runtime : in out Runtime_Type);

end Helios.Monitor.Agent;
