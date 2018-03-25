-----------------------------------------------------------------------
--  helios-commands-register -- Command to register the agent in hyperion
--  Copyright (C) 2018 Stephane Carrez
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
with GNAT.Command_Line;
with GNAT.Strings;
with Helios.Commands.Drivers;
package Helios.Commands.Register is

   type Command_Type is new Helios.Commands.Drivers.Command_Type with private;

   --  Execute a information command to report information about the agent and monitoring.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration);

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type);

private

   type Command_Type is new Helios.Commands.Drivers.Command_Type with record
      Client_Id     : aliased GNAT.Strings.String_Access;
      Client_Secret : aliased GNAT.Strings.String_Access;
      Name          : aliased GNAT.Strings.String_Access;
      Key           : aliased GNAT.Strings.String_Access;
      Server        : aliased GNAT.Strings.String_Access;
      IP            : aliased GNAT.Strings.String_Access;
      Port          : aliased Integer := 0;
   end record;

end Helios.Commands.Register;
