-----------------------------------------------------------------------
--  helios-monitor-sysfile -- Generic system file monitor
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
with Helios.Tools.Files;
package body Helios.Monitor.Sysfile is

   --  ------------------------------
   --  Start the agent and build the definition tree.
   --  ------------------------------
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager) is
      Values : constant String := Config.Get ("values");
   begin
      Agent.Path := Config.Get ("path");
      Agent.Value := Agent.Create_Definition ("value", Values);
   end Start;

   --  ------------------------------
   --  Collect the values in the snapshot.
   --  ------------------------------
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type) is
      Line : Helios.Tools.Files.File_Extractor;
      Path : constant String := Ada.Strings.Unbounded.To_String (Agent.Path);
   begin
      Line.Open (Path);
      Line.Read;
      if not Line.Is_Eof then
         Values.Set_Value (Agent.Value, Line.Get_Value (1));
      end if;
   end Collect;

end Helios.Monitor.Sysfile;
