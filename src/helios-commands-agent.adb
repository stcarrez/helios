-----------------------------------------------------------------------
--  helios-commands-agent -- Helios agent commands
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
with Ada.Real_Time;
with Ada.Text_IO;
with Util.Log;
with Util.Properties;
with Util.Events.Timers;
with Helios.Monitor.Agent;
with Helios.Reports.Files;
package body Helios.Commands.Agent is

   use Ada.Strings.Unbounded;
   use type Ada.Real_Time.Time_Span;

   procedure Setup_Report (Runtime : in out Helios.Monitor.Agent.Runtime_Type;
                           Config  : in Util.Properties.Manager);

   File_Report : aliased Helios.Reports.Files.File_Report_Type;

   procedure Setup_Report (Runtime : in out Helios.Monitor.Agent.Runtime_Type;
                           Config  : in Util.Properties.Manager) is
      Mode  : constant String := Config.Get ("mode", "file");
      Timer : Util.Events.Timers.Timer_Ref;
   begin
      Runtime.Report_Period := Helios.Monitor.Get_Period (Config, "period", 300);
      if Mode = "file" then
         File_Report.Period := Runtime.Report_Period;
         File_Report.Path := To_Unbounded_String (Config.Get ("pattern",
                                                  "report-%F-%H-%M-%S.json"));
         Runtime.Timers.Set_Timer (File_Report'Access, Timer, File_Report.Period);
      end if;
   end Setup_Report;

   --  ------------------------------
   --  Execute a information command to report information about the agent and monitoring.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      if Args.Get_Count /= 0 then
         Helios.Commands.Driver.Usage (Args);
      else
         Load (Context);
         Setup_Report (Context.Runtime, Context.Config.Get ("report"));
         Monitor.Agent.Configure (Context.Runtime, Context.Config);
         Monitor.Agent.Run (Context.Runtime);
      end if;

   exception
      when Util.Properties.NO_PROPERTY =>
         Command.Log (Util.Log.ERROR_LEVEL, Name, "Missing report configuration");
         raise Error;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command, Context);
   begin
      Ada.Text_IO.Put_Line ("agent: start the monitoring agent");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Usage: agent");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("  The agent command is the main command to run the agent and");
      Ada.Text_IO.Put_Line ("  to monitor the system components according to the configuration.");
   end Help;

end Helios.Commands.Agent;
