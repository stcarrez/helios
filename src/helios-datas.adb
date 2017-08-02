-----------------------------------------------------------------------
--  helios-monitor -- Helios monitor
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

package body Helios.Datas is

   use type Schemas.Definition_Type_Access;
   use type Schemas.Value_Index;

   --  ------------------------------
   --  Initialize the snapshot.
   --  ------------------------------
   procedure Initialize (Data : in out Snapshot_Type) is
   begin
      Data.Values := new Value_Array (1 .. Schemas.Get_Count);
   end Initialize;

   --  ------------------------------
   --  Initialize the snapshot queue for the schema.
   --  ------------------------------
   procedure Initialize (Queue  : in out Snapshot_Queue_Type;
                         Schema : in Helios.Schemas.Definition_Type_Access;
                         Count  : in Positive) is
   begin
      Queue.Schema := Schema;
      Queue.Count  := Count;
   end Initialize;

   --  ------------------------------
   --  Set the value in the snapshot.
   --  ------------------------------
   procedure Set_Value (Into  : in out Snapshot_Type;
                        Def   : in Schemas.Definition_Type_Access;
                        Value : in Uint64) is
   begin
      if Def /= null and then Def.Index > 0 then
         Into.Values (Def.Index + Into.Offset) := Value;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Prepare the snapshot queue to collect new values.
   --  ------------------------------
   procedure Prepare (Queue    : in out Snapshot_Queue_Type;
                      Snapshot : out Snapshot_Type_Access) is
   begin
      Snapshot := Queue.Current;
      Snapshot.Offset := Snapshot.Offset + Queue.Schema.Index;
      if Snapshot.Offset > Snapshot.Values'Last then
         null; --  Freeze (Snapshot);
      end if;
   end Prepare;

end Helios.Datas;
