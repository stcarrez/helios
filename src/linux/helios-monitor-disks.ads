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
package Helios.Monitor.Disks is

   type Stat_Type is (READS, READ_MERGED, SECTOR_READ, READ_TIME,
                      WRITES, WRITE_MERGED, SECTOR_WRITE, WRITE_TIME);

   type Stat_Definition_Array is array (Stat_Type) of Schemas.Definition_Type_Access;

   type Disk_Definition_Type is new Schemas.Definition_Type with record
      Stats : Stat_Definition_Array;
   end record;
   type Disk_Definition_Type_Access is access all Disk_Definition_Type'Class;

   type Agent_Type is new Helios.Monitor.Agent_Type with null record;

   --  Start the agent and build the definition tree.
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager);

   --  Collect the values in the snapshot.
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type);

   --  Make a new disk definition for the given disk name.
   procedure Make_Disk (Agent : in out Agent_Type;
                        Name  : in String);

end Helios.Monitor.Disks;
