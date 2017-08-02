-----------------------------------------------------------------------
--  helios-monitor-disks -- Linux disks monitor
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
with Util.Strings.Transforms;
with Helios.Tools.Files;
package body Helios.Monitor.Disks is

   use Util.Strings.Transforms;

   --  ------------------------------
   --  Start the agent and build the definition tree.
   --  ------------------------------
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager) is
      Values : constant String := Config.Get ("values");
   begin
      Make_Disk (Agent, "sda", Values);
      Make_Disk (Agent, "sdb", Values);
      Make_Disk (Agent, "sdc", Values);
      Make_Disk (Agent, "sdd", Values);
   end Start;

   --  ------------------------------
   --  Collect the values in the snapshot.
   --  ------------------------------
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type) is
      use type Schemas.Definition_Type_Access;

      Line : Helios.Tools.Files.File_Extractor;
      Node : Schemas.Definition_Type_Access;
      Disk : Disk_Definition_Type_Access;
   begin
      Line.Open ("/proc/diskstats");
      Line.Name_Pos := 3;
      loop
         Line.Read;
         exit when Line.Is_Eof;
         Node := Agent.Find_Definition (Line.Get_Value (3));
         if Node /= null then
            Disk := Disk_Definition_Type'Class (Node.all)'Access;
            for I in Disk.Stats'Range loop
               Values.Set_Value (Disk.Stats (I), Line.Get_Value (4 + Stat_Type'Pos (I)));
            end loop;
         end if;
      end loop;
   end Collect;

   --  ------------------------------
   --  Make a new disk definition for the given disk name.
   --  ------------------------------
   procedure Make_Disk (Agent  : in out Agent_Type;
                        Name   : in String;
                        Filter : in String) is
      Disk : constant Disk_Definition_Type_Access
        := new Disk_Definition_Type (Len => Name'Length);
   begin
      Disk.Name := Name;
      Agent.Add_Definition (Disk.all'Access);
      for I in Disk.Stats'Range loop
         Disk.Stats (I) := Schemas.Create_Definition (Disk.all'Access,
                                                      To_Lower_Case (Stat_Type'Image (I)),
                                                      Filter);
      end loop;
   end Make_Disk;

end Helios.Monitor.Disks;
