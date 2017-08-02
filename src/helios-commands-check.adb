-----------------------------------------------------------------------
--  helios-commands-check -- Helios check commands
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
with Util.Events.Timers;
with Helios.Monitor.Agent;
with Helios.Reports.Files;
package body Helios.Commands.Check is

   use type Ada.Real_Time.Time_Span;

   --  ------------------------------
   --  Execute a information command to report information about the agent and monitoring.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Command, Name);

      type Info_Report is new Helios.Reports.Files.File_Report_Type with null record;

      --  The timer handler executed when the timer deadline has passed.
      overriding
      procedure Time_Handler (Report : in out Info_Report;
                              Event  : in out Util.Events.Timers.Timer_Ref'Class);

      --  ------------------------------
      --  The timer handler executed when the timer deadline has passed.
      --  ------------------------------
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
      if Args.Get_Count /= 1 then
         Helios.Commands.Driver.Usage (Args);
      else
         Report.Path := Ada.Strings.Unbounded.To_Unbounded_String (Args.Get_Argument (1));
         Load (Context);
         Monitor.Agent.Configure (Context.Runtime, Context.Config);
         Context.Runtime.Timers.Set_Timer (Report'Unchecked_Access, Timer,
                                           Context.Runtime.Report_Period + Ada.Real_Time.Seconds (1));
         Monitor.Agent.Run (Context.Runtime);
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command, Context);
   begin
      Ada.Text_IO.Put_Line ("check: check the configuration and collect results in a file");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Usage: check {result-file}");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("  The check command reads the configuration files, ");
      Ada.Text_IO.Put_Line ("  configures the plugin agents and collects the data to generate");
      Ada.Text_IO.Put_Line ("  a JSON file that contains the results.  The JSON file can be");
      Ada.Text_IO.Put_Line ("  checked to verify the values that were collected.");
   end Help;

end Helios.Commands.Check;
