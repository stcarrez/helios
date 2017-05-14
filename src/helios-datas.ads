-----------------------------------------------------------------------
--  helios-datas -- Helios monitoring data and snapshots
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
with Ada.Calendar;
with Ada.Real_Time;
with Ada.Finalization;
with Helios.Schemas;

--  == Data Representation ==
--  The monitored data is collected into snapshots and snapshots are stored in a queue
--  before being flushed.
package Helios.Datas is

   subtype Value_Index is Schemas.Value_Index;
   subtype Value_Array_Index is Value_Index range 1 .. Value_Index'Last;

   type Value_Array is array (Value_Array_Index range <>) of Uint64;
   type Value_Array_Access is access all Value_Array;

   type Snapshot_Type is tagged limited record
      Time          : Ada.Calendar.Time;
      Start_Time    : Ada.Real_Time.Time;
      End_Time      : Ada.Real_Time.Time;
      Values        : Value_Array_Access;
   end record;

   type Snapshot_Array is array (Positive range <>) of Snapshot_Type;

   type Snapshot_Queue_Type (Max_Count : Positive) is limited record
      Count : Natural := 0;
      Data  : Snapshot_Array (1 .. Max_Count);
   end record;

   --  Initialize the snapshot.
   procedure Initialize (Data : in out Snapshot_Type);

   --  Set the value in the snapshot.
   procedure Set_Value (Into  : in out Snapshot_Type;
                        Def   : in Schemas.Definition_Type_Access;
                        Value : in Uint64);

end Helios.Datas;
