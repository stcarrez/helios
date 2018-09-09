-----------------------------------------------------------------------
--  helios-commands-info -- Helios information commands
--  Copyright (C) 2017, 2018 Stephane Carrez
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
with Ada.Real_Time;
with Util.Events.Timers;
with Helios.Datas;
with Helios.Monitor.Agent;
with Helios.Schemas;
with Helios.Reports.Files;
package body Helios.Commands.Info is

   use type Ada.Real_Time.Time_Span;

   --  Execute a information command to report information about the agent and monitoring.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      type Info_Report is new Helios.Reports.Files.File_Report_Type with null record;
      --  The timer handler executed when the timer deadline has passed.
      overriding
      procedure Time_Handler (Report : in out Info_Report;
                              Event  : in out Util.Events.Timers.Timer_Ref'Class);

      --  The timer handler executed when the timer deadline has passed.
      overriding
      procedure Time_Handler (Report : in out Info_Report;
                              Event  : in out Util.Events.Timers.Timer_Ref'Class) is
      begin
         Helios.Reports.Files.File_Report_Type (Report).Time_Handler (Event);
         Context.Runtime.Stop := True;
      end Time_Handler;

      Timer  : Util.Events.Timers.Timer_Ref;
      Report : aliased Info_Report;
   begin
      Report.Path := Ada.Strings.Unbounded.To_Unbounded_String ("report.json");
      Load (Context);
      Monitor.Agent.Configure (Context.Runtime, Context.Config);
      Context.Runtime.Timers.Set_Timer (Report'Unchecked_Access, Timer,
                                        Context.Runtime.Report_Period + Ada.Real_Time.Seconds (1));
      Monitor.Agent.Run (Context.Runtime);
   end Execute;

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in out Command_Type;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

end Helios.Commands.Info;
