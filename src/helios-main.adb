-----------------------------------------------------------------------
--  helios-main -- Helios agent main procedure
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
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;

with Util.Log.loggers;
with Util.Properties;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with Util.Serialize.IO.JSON;
with Util.Commands;
with Helios.Monitor.CPU;
with Helios.Monitor.Ifnet;
with Helios.Monitor.Disks;
with Helios.Reports;
with Helios.Schemas;
with Helios.Datas;
with Helios.Reports.Files;
with Helios.Commands;
procedure Helios.Main is

   use GNAT.Command_Line;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Main");

   Log_Config  : Util.Properties.Manager;
   Debug       : Boolean := False;
   Verbose     : Boolean := False;
   First       : Natural := 0;
   All_Args    : Util.Commands.Default_Argument_List (0);
   Config      : Ada.Strings.Unbounded.Unbounded_String;
   Ctx         : Helios.Commands.Context_Type;
   Mon         : Helios.Monitor.CPU.Agent_Type;
   Ifnet_Mon   : Helios.Monitor.Ifnet.Agent_Type;
   Disk_Mon    : Helios.Monitor.Disks.Agent_Type;
begin
   Log_Config.Set ("log4j.rootCategory", "DEBUG,console");
   Log_Config.Set ("log4j.appender.console", "Console");
   Log_Config.Set ("log4j.appender.console.level", "ERROR");
   Log_Config.Set ("log4j.appender.console.layout", "level-message");
   Log_Config.Set ("log4j.appender.stdout", "Console");
   Log_Config.Set ("log4j.appender.stdout.level", "INFO");
   Log_Config.Set ("log4j.appender.stdout.layout", "message");
   Log_Config.Set ("log4j.logger.Util", "FATAL");
   Log_Config.Set ("log4j.logger.Helios", "FATAL");
   Util.Log.Loggers.Initialize (Log_Config);

   Helios.Commands.Initialize;
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");
   --  Parse the command line
   loop
      case Getopt ("* v d c:") is
         when ASCII.NUL =>
            exit;

         when 'c' =>
            Config := Ada.Strings.Unbounded.To_Unbounded_String (Parameter);

         when 'd' =>
            Debug := True;

         when 'v' =>
            Verbose := True;

         when '*' =>
            exit;

         when others =>
            null;
      end case;
      First := First + 1;
   end loop;
   if Verbose or Debug then
      Log_Config.Set ("log4j.appender.console.level", "INFO");
      Log_Config.Set ("log4j.logger.Util", "WARN");
      Log_Config.Set ("log4j.logger.Bbox", "ERR");
   end if;
   if Debug then
      Log_Config.Set ("log4j.appender.console.level", "DEBUG");
   end if;
   Util.Log.Loggers.Initialize (Log_Config);

   if First >= Ada.Command_Line.Argument_Count then
      Ada.Text_IO.Put_Line ("Missing command name to execute.");
      Helios.Commands.Driver.Usage (All_Args);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Helios.Monitor.Register (Mon, "cpu");
   Helios.Monitor.Register (Ifnet_Mon, "ifnet");
   Helios.Monitor.Register (Disk_Mon, "disks");
   declare
      Cmd_Name : constant String := Full_Switch;
      Args     : Util.Commands.Default_Argument_List (First + 1);
   begin
      Helios.Commands.Driver.Execute (Cmd_Name, Args, Ctx);
   end;
end Helios.Main;
