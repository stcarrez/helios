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
with Helios.Monitor.Agent;
package body Helios.Commands.Agent is

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
   begin
      if Args.Get_Count /= 1 then
         Helios.Commands.Driver.Usage (Args);
      else
         Load (Context);
         Monitor.Agent.Configure (Context.Runtime, Context.Config);
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
      Ada.Text_IO.Put_Line ("agent: start the monitoring agent");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Usage: agent");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("  The agent command is the main command to run the agent and");
      Ada.Text_IO.Put_Line ("  to monitor the system components according to the configuration.");
   end Help;

end Helios.Commands.Agent;
