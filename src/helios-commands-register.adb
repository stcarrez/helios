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
with Ada.Real_Time;
with Util.Events.Timers;
with Swagger.Clients;
with Swagger.Credentials.OAuth;
with Helios.Datas;
with Helios.Monitor.Agent;
with Helios.Reports.Files;
with Helios.Rest.Clients;
with Helios.Rest.Models;
package body Helios.Commands.Register is

   use type Ada.Real_Time.Time_Span;

   function Get_Name (Command : in Command_Type) return Swagger.UString is
   begin
      return Swagger.To_UString (Command.Name.all);
   end Get_Name;

   function Get_Ip (Command : in Command_Type) return Swagger.UString is
   begin
      return Swagger.To_UString (Command.IP.all);
   end Get_Ip;

   function Get_Key (Command : in Command_Type) return Swagger.UString is
   begin
      return Swagger.To_UString (Command.Key.all);
   end Get_Key;

   --  Execute a information command to report information about the agent and monitoring.
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is


      Client : Helios.Rest.Clients.Client_Type;
      Agent  : Helios.Rest.Models.Agent_Type;
      Cred   : aliased Swagger.Credentials.OAuth.OAuth2_Credential_Type;
   begin
      if Args.Get_Count /= 2 then
         Helios.Commands.Driver.Usage (Args);
      else
         Load (Context);
         Cred.Set_Application_Identifier (Command.Client_Id.all);
         Cred.Set_Application_Secret (Command.Client_Secret.all);
         Client.Set_Server (Args.Get_Argument (1));
         Client.Register_Agent (Name      => Get_Name (Command),
                                Ip        => Get_Ip (Command),
                                Agent_Key => Get_Key (Command),
                                Result    => Agent);
      end if;
   end Execute;

   --  Setup the command before parsing the arguments and executing it.
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration) is
      package GC renames GNAT.Command_Line;
   begin
      GC.Define_Switch (Config, Command.Client_Id'Access,
                        "", "--client-id", "Define the helios client identifier");
      GC.Define_Switch (Config, Command.Client_Secret'Access,
                        "", "--client-secret", "Define the helios client secret");
      GC.Define_Switch (Config, Command.Server'Access,
                        "-s:", "--server", "Server hostname or IP address");
      GC.Define_Switch (Config, Command.Port'Access,
                        "-p:", "--port", "Server TCP/IP port");
      GC.Define_Switch (Config, Command.IP'Access,
                        "", "--client-ip", "IP address of the host to use");
   end Setup;

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

end Helios.Commands.Register;
