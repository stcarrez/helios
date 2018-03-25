-----------------------------------------------------------------------
--  helios-commands -- Helios commands
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
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Util.Log.Loggers;
with Helios.Commands.Info;
with Helios.Commands.Check;
with Helios.Commands.Agent;
with Helios.Commands.Register;
package body Helios.Commands is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Commands");

   Help_Command     : aliased Helios.Commands.Drivers.Help_Command_Type;
   Info_Command     : aliased Helios.Commands.Info.Command_Type;
   Check_Command    : aliased Helios.Commands.Check.Command_Type;
   Agent_Command    : aliased Helios.Commands.Agent.Command_Type;
   Register_Command : aliased Helios.Commands.Register.Command_Type;

   --  ------------------------------
   --  Initialize the commands.
   --  ------------------------------
   procedure Initialize is
   begin
      Driver.Set_Description ("helios - monitoring agent");
      Driver.Set_Usage ("[-v] [-d] [-c config] <command> [<args>]" & ASCII.LF &
                          "where:" & ASCII.LF &
                          "  -v           Verbose execution mode" & ASCII.LF &
                          "  -d           Debug execution mode" & ASCII.LF &
                          "  -c config    Use the configuration file");
      Driver.Add_Command ("help", Help_Command'Access);
      Driver.Add_Command ("info", Info_Command'Access);
      Driver.Add_Command ("check", Check_Command'Access);
      Driver.Add_Command ("agent", Agent_Command'Access);
      Driver.Add_Command ("register", Register_Command'Access);
   end Initialize;

   --  ------------------------------
   --  Load the configuration context from the configuration file.
   --  ------------------------------
   procedure Load (Context : in out Context_Type) is
      Path : constant String := Ada.Strings.Unbounded.To_String (Context.Config_Path);
   begin
      Context.Config.Load_Properties (Path);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Configuration file '{0}' does not exist.", Path);
         Log.Error ("Use the '-c config' option to specify a configuration file.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         raise Error;

   end Load;

end Helios.Commands;
