-----------------------------------------------------------------------
--  helios-monitor -- Helios monitor
--  Copyright (C) 2017, 2019 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Util.Log.Loggers;
package body Helios.Datas is

   use type Schemas.Definition_Type_Access;
   use type Schemas.Value_Index;

   function Allocate (Queue : in Snapshot_Queue_Type) return Snapshot_Type_Access;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Datas");

   Reports : Report_Queue_Type;

   function Allocate (Queue : in Snapshot_Queue_Type) return Snapshot_Type_Access is
      Snapshot : constant Snapshot_Type_Access := new Snapshot_Type;
      Count    : constant Value_Array_Index
        := Queue.Schema.Index * Value_Array_Index (Queue.Count);
   begin
      Log.Info ("Allocate snapshot with {0} values", Value_Array_Index'Image (Count));
      Snapshot.Schema := Queue.Schema;
      Snapshot.Offset := 0;
      Snapshot.Count  := Queue.Schema.Index;
      Snapshot.Values := new Value_Array (1 .. Count);
      Snapshot.Values.all := (others => 0);
      Snapshot.Start_Time := Ada.Real_Time.Clock;
      return Snapshot;
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
   --  Get the snapshot start time.
   --  ------------------------------
   function Get_Start_Time (Data : in Snapshot_Type) return Ada.Real_Time.Time is
   begin
      return Data.Start_Time;
   end Get_Start_Time;

   --  ------------------------------
   --  Get the snapshot end time.
   --  ------------------------------
   function Get_End_Time (Data : in Snapshot_Type) return Ada.Real_Time.Time is
   begin
      return Data.End_Time;
   end Get_End_Time;

   --  ------------------------------
   --  Finish updating the current values of the snapshot.
   --  ------------------------------
   procedure Finish (Data : in out Snapshot_Type) is
   begin
      Data.Offset := Data.Offset + Data.Count;
   end Finish;

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
      Count      : constant Helios.Datas.Value_Array_Index := Data.Count;
      Pos        : Helios.Datas.Value_Array_Index := Node.Index;
   begin
      Log.Debug ("Iterate {0} from {1} to {2} step {3}", Node.Name,
                 Value_Array_Index'Image (Count),
                 Value_Array_Index'Image (Pos));
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
                      Process_Snapshot : not null access
                        procedure (D : in Snapshot_Type;
                                   N : in Definition_Type_Access);
                      Process_Values  : not null access
                        procedure (D : in Snapshot_Type;
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
   --  Iterate over the values of the reports.
   --  ------------------------------
   procedure Iterate (Report  : in Report_Queue_Type;
                      Process : not null access procedure (Data : in Snapshot_Type;
                                                           Node : in Definition_Type_Access)) is
   begin
      if not Report.Snapshot.Is_Null then
         declare
            List      : constant Snapshot_Accessor := Report.Snapshot.Value;
            Snapshot  : Snapshot_Type_Access := List.First;
         begin
            while Snapshot /= null loop
               Process (Snapshot.all, Snapshot.Schema);
               Snapshot := Snapshot.Next;
            end loop;
         end;
      end if;
   end Iterate;

   --  ------------------------------
   --  Prepare the snapshot queue to collect new values.
   --  ------------------------------
   procedure Prepare (Queue    : in out Snapshot_Queue_Type;
                      Snapshot : out Snapshot_Type_Access) is
   begin
      Snapshot := Queue.Current;
      --  Snapshot.Offset := Snapshot.Offset + Snapshot.Count;
      if Snapshot.Offset >= Snapshot.Values'Last then
         Flush (Queue);
         Snapshot := Queue.Current;
      end if;
   end Prepare;

   --  ------------------------------
   --  Flush the snapshot to start a fresh one for the queue.
   --  ------------------------------
   procedure Flush (Queue : in out Snapshot_Queue_Type) is
      Snapshot : constant Snapshot_Type_Access := Queue.Current;
   begin
      if Reports.Snapshot.Is_Null then
         Reports.Snapshot := Snapshot_Refs.Create;
      end if;
      Snapshot.Next := Reports.Snapshot.Value.First;
      Reports.Snapshot.Value.First := Queue.Current;
      Queue.Current := Allocate (Queue);
      Snapshot.End_Time := Queue.Current.Start_Time;
   end Flush;

   --  ------------------------------
   --  Release the snapshots when the reference counter reaches 0.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Snapshot_List) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Snapshot_Type,
                                        Name   => Snapshot_Type_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Value_Array,
                                        Name   => Value_Array_Access);

      Snapshot : Snapshot_Type_Access := Object.First;
      Next     : Snapshot_Type_Access;
   begin
      while Snapshot /= null loop
         Next := Snapshot.Next;
         Free (Snapshot.Values);
         Free (Snapshot);
         Snapshot := Next;
      end loop;
   end Finalize;

   Empty_Snapshot : Snapshot_Refs.Ref;

   function Get_Report return Report_Queue_Type is
      Result : constant Report_Queue_Type := Reports;
   begin
      Reports.Snapshot := Empty_Snapshot;
      return Result;
   end Get_Report;

end Helios.Datas;
