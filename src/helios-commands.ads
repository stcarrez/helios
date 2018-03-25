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

   type Context_Type is limited record
      Config_Path : Ada.Strings.Unbounded.Unbounded_String;
      Config      : Util.Properties.Manager;
      Runtime     : Helios.Monitor.Agent.Runtime_Type;
   end record;

   package Drivers is
     new Util.Commands.Drivers (Context_Type  => Context_Type,
                                Config_Parser => Util.Commands.Parsers.GNAT_Parser.Config_Parser,
                                Driver_Name   => "helios");

   Driver : Drivers.Driver_Type;

   --  Initialize the commands.
   procedure Initialize;

   --  Load the configuration context from the configuration file.
   procedure Load (Context : in out Context_Type);

end Helios.Commands;
