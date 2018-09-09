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
with Ada.Strings.Unbounded;
with Util.Commands.Drivers;
with Util.Commands.Parsers.GNAT_Parser;
with Util.Properties;
with Helios.Monitor.Agent;
package Helios.Commands is

   Error : exception;

   subtype Argument_List is Util.Commands.Argument_List;

   type Context_Type is limited private;

   --  Initialize the commands.
   procedure Initialize;

   --  Print the command usage.
   procedure Usage (Args : in Argument_List'Class;
                    Name : in String := "");

   --  Execute the command with its arguments.
   procedure Execute (Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out Context_Type);

   --  Load the configuration context from the configuration file.
   procedure Load (Context : in out Context_Type);

   --  Set the path of the configuration file to load.
   procedure Set_Configuration (Context : in out Context_Type;
                                Path    : in String);

private

   type Context_Type is limited record
      Config_Path : Ada.Strings.Unbounded.Unbounded_String;
      Config      : Util.Properties.Manager;
      Server      : Util.Properties.Manager;
      Runtime     : Helios.Monitor.Agent.Runtime_Type;
   end record;

   --  Save the server connection configuration.
   procedure Save_Server_Configuration (Context : in out Context_Type);

end Helios.Commands;
