-----------------------------------------------------------------------
--  helios-commands-info -- Helios information commands
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

with Helios.Datas;
with Helios.Monitor;
with Helios.Schemas;
with Helios.Reports.Files;
package body Helios.Commands.Info is

   --  Execute a information command to report information about the agent and monitoring.
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      Data        : Helios.Datas.Snapshot_Queue_Type (Max_Count => 1000);
   begin
      for I in Data.Data'Range loop
         Helios.Datas.Initialize (Data.Data (I));
         Helios.Monitor.Collect_All (Data.Data (I));
         Data.Count := Data.Count + 1;
         --      Helios.Reports.Write_Snapshot (Stream, Data, Helios.Schemas.Get_Root);
         --  Helios.Reports.Files.Save_Snapshot ("result.json", Data, Helios.Schemas.Get_Root);
         delay 1.0;
      end loop;
      Helios.Reports.Files.Save_Snapshot ("result.json", Data, Helios.Schemas.Get_Root);

   end Execute;

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
   begin
      null;
   end Help;

end Helios.Commands.Info;
