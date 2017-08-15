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
with Helios.Schemas;

--  == Data Representation ==
--  The monitored data is collected into snapshots and snapshots are stored in a queue
--  before being flushed.
package Helios.Datas is

   subtype Definition_Type_Access is Schemas.Definition_Type_Access;
   subtype Value_Index is Schemas.Value_Index;
   subtype Value_Array_Index is Value_Index range 1 .. Value_Index'Last;

   type Value_Array is array (Value_Array_Index range <>) of Uint64;
   type Value_Array_Access is access all Value_Array;

   type Snapshot_Type;
   type Snapshot_Type_Access is access all Snapshot_Type;

   type Snapshot_Index is new Natural;
   type Report_Index is new Natural;

   type Snapshot_Type is tagged limited private;

   --  type Snapshot_Array is array (Positive range <>) of aliased Snapshot_Type;

   type Snapshot_Queue_Type is limited private;

   --  Initialize the snapshot queue for the schema.
   procedure Initialize (Queue  : in out Snapshot_Queue_Type;
                         Schema : in Helios.Schemas.Definition_Type_Access;
                         Count  : in Positive);

   --  Set the value in the snapshot.
   procedure Set_Value (Into  : in out Snapshot_Type;
                        Def   : in Schemas.Definition_Type_Access;
                        Value : in Uint64);

   --  Iterate over the values in the snapshot and collected for the definition node.
   procedure Iterate (Data    : in Snapshot_Type;
                      Node    : in Definition_Type_Access;
                      Process : not null access procedure (Value : in Uint64));

   procedure Iterate (Data    : in Snapshot_Type;
                      Node    : in Definition_Type_Access;
                      Process_Snapshot : not null access procedure (D : in Snapshot_Type;
                                                                    N : in Definition_Type_Access);
                      Process_Values  : not null access procedure (D : in Snapshot_Type;
                                                                   N : in Definition_Type_Access));

   --  Prepare the snapshot queue to collect new values.
   procedure Prepare (Queue    : in out Snapshot_Queue_Type;
                      Snapshot : out Snapshot_Type_Access);

   type Snapshot_Report_Type is limited record
      S : Natural;
   end record;

   type Report_Queue_Type is record
      Snapshot : Snapshot_Type_Access;
   end record;

   function Get_Report return Report_Queue_Type;

private

   type Snapshot_Type is tagged limited record
      Schema        : Helios.Schemas.Definition_Type_Access;
      Next          : Snapshot_Type_Access;
      Time          : Ada.Calendar.Time;
      Start_Time    : Ada.Real_Time.Time;
      End_Time      : Ada.Real_Time.Time;
      Offset        : Value_Index := 0;
      Values        : Value_Array_Access;
   end record;

   type Snapshot_Array is array (Positive range <>) of aliased Snapshot_Type;

   type Snapshot_Queue_Type is limited record
      Count     : Natural := 0;
      Read_Pos  : Natural := 0;
      Write_Pos : Natural := 0;
      Schema    : Helios.Schemas.Definition_Type_Access;
      First     : Snapshot_Type_Access;
      Current   : Snapshot_Type_Access;
   end record;
   type Snapshot_Queue_Access is access all Snapshot_Queue_Type;

end Helios.Datas;
