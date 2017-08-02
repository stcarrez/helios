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
with Util.Log.Loggers;
package body Helios.Datas is

   use type Schemas.Definition_Type_Access;
   use type Schemas.Value_Index;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Datas");

   Reports : Report_Queue_Type;

   --  ------------------------------
   --  Initialize the snapshot.
   --  ------------------------------
   procedure Initialize (Data : in out Snapshot_Type) is
   begin
      Data.Values := new Value_Array (1 .. Schemas.Get_Count);
   end Initialize;

   function Allocate (Queue : in Snapshot_Queue_Type) return Snapshot_Type_Access is
      Result : Snapshot_Type_Access := new Snapshot_Type;
      Count  : Value_Array_Index := Queue.Schema.Index * Value_Array_Index (Queue.Count);
   begin
      Log.Info ("Allocate snapshot with {0} values", Value_Array_Index'Image (Count));
      Result.Schema := Queue.Schema;
      Result.Offset := 0;
      Result.Values := new Value_Array (1 .. Count);
      Result.Start_Time := Ada.Real_Time.Clock;
      return Result;
   end Allocate;

   --  ------------------------------
   --  Initialize the snapshot queue for the schema.
   --  ------------------------------
   procedure Initialize (Queue  : in out Snapshot_Queue_Type;
                         Schema : in Helios.Schemas.Definition_Type_Access;
                         Count  : in Positive) is
   begin
      Log.Info ("Initialize schema queue {0} with {1} values",
                Schema.Name, Positive'Image (Count));
      Queue.Schema := Schema;
      Queue.Count  := Count;
      Queue.Current := Allocate (Queue);
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
      if Snapshot.Offset >= Snapshot.Values'Last then
         Snapshot.Next := Reports.Snapshot;
         Reports.Snapshot := Snapshot;
         Queue.Current := Allocate (Queue);
         Snapshot.End_Time := Queue.Current.Start_Time;
         Snapshot := Queue.Current;
      end if;
   end Prepare;

   function Get_Report return Report_Queue_Type is
   begin
      return Reports;
   end Get_Report;

end Helios.Datas;
