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

   function Allocate (Queue : in Snapshot_Queue_Type) return Snapshot_Type_Access;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Datas");

   Reports : Report_Queue_Type;

   function Allocate (Queue : in Snapshot_Queue_Type) return Snapshot_Type_Access is
      Result : constant Snapshot_Type_Access := new Snapshot_Type;
      Count  : constant Value_Array_Index := Queue.Schema.Index * Value_Array_Index (Queue.Count);
   begin
      Log.Info ("Allocate snapshot with {0} values", Value_Array_Index'Image (Count));
      Result.Schema := Queue.Schema;
      Result.Offset := 0;
      Result.Values := new Value_Array (1 .. Count);
      Result.Values.all := (others => 0);
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
      Log.Info ("Initialize schema queue {0} with{1} samples of{2} values",
                Schema.Name, Positive'Image (Count),
                Value_Array_Index'Image (Schema.Index));
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
   --  Iterate over the values in the snapshot and collected for the definition node.
   --  ------------------------------
   procedure Iterate (Data    : in Helios.Datas.Snapshot_Type;
                      Node    : in Helios.Schemas.Definition_Type_Access;
                      Process : not null access procedure (Value : in Uint64)) is
      Count      : constant Helios.Datas.Value_Array_Index := Node.Index;
      Pos        : Helios.Datas.Value_Array_Index := Node.Index;
   begin
      while Pos < Data.Offset loop
         Process (Data.Values (Pos));
         Pos := Pos + Count;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Iterate over the values in the snapshot and collected for the definition node.
   --  ------------------------------
   procedure Iterate (Data    : in Helios.Datas.Snapshot_Type;
                      Node    : in Helios.Schemas.Definition_Type_Access;
                      Process_Snapshot : not null access procedure (D : in Snapshot_Type;
                                                                    N : in Definition_Type_Access);
                      Process_Values  : not null access procedure (D : in Snapshot_Type;
                                                                   N : in Definition_Type_Access)) is
      Child : Helios.Schemas.Definition_Type_Access;
   begin
      Child := Node.Child;
      while Child /= null loop
         if Child.Child /= null then
            Process_Snapshot (Data, Child);
         elsif Child.Index > 0 then
            Process_Values (Data, Child);
         end if;
         Child := Child.Next;
      end loop;
   end Iterate;

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
