-----------------------------------------------------------------------
--  helios-reports -- Produce reports for the agent
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
with Ada.Real_Time;
package body Helios.Reports is

   use type Uint64;
   use type Helios.Schemas.Value_Index;
   use type Helios.Schemas.Definition_Type_Access;
   use type Ada.Real_Time.Time;

   procedure Write_Timestamp (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                              Name   : in String;
                              Time   : in Ada.Real_Time.Time);

   procedure Write_Timestamp (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                              Name   : in String;
                              Time   : in Ada.Real_Time.Time) is
      Seconds : Ada.Real_Time.Seconds_Count;
      Remain  : Ada.Real_Time.Time_Span;
   begin
      Ada.Real_Time.Split (Time, Seconds, Remain);
      Stream.Write_Long_Entity (Name, Long_Long_Integer (Seconds));
   end Write_Timestamp;

   --  ------------------------------
   --  Write the collected snapshot in the IO stream.  The output stream can be an XML
   --  or a JSON stream.  The node definition is used for the structure of the output content.
   --  ------------------------------
   procedure Write_Snapshot (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                             Data   : in Helios.Datas.Snapshot_Type;
                             Node   : in Helios.Schemas.Definition_Type_Access) is
      Child      : Helios.Schemas.Definition_Type_Access;
      Value      : Uint64;
      Prev_Value : Uint64;
      Offset     : Long_Long_Integer;
      Pos        : Helios.Datas.Value_Array_Index;
      Count      : constant Helios.Datas.Value_Array_Index := Data.Schema.Index;
   begin
      Stream.Start_Entity (Node.Name);
      Stream.Write_Entity ("period", 10);
      Write_Timestamp (Stream, "timestamp", Data.Start_Time);
      Stream.Start_Entity ("snapshot");
      Child := Node.Child;
      while Child /= null loop
         if Child.Child /= null then
            Write_Snapshot (Stream, Data, Child);
         elsif Child.Index > 0 then
            Stream.Start_Array (Child.Name);
            Prev_Value := 0;
            Pos := Child.Index;
            while Pos < Data.Offset loop
               Value := Data.Values (Pos);
               Pos := Pos + Count;
               if Value > Prev_Value then
                  Offset := Long_Long_Integer (Value - Prev_Value);
               else
                  Offset := -Long_Long_Integer (Prev_Value - Value);
               end if;
               Prev_Value := Value;
               if Offset < Long_Long_Integer (Integer'Last)
                 and Offset > Long_Long_Integer (Integer'First)
               then
                  Stream.Write_Entity (Child.Name, Integer (Offset));
               else
                  Stream.Write_Long_Entity (Child.Name, Offset);
               end if;
            end loop;
            Stream.End_Array (Child.Name);
         end if;
         Child := Child.Next;
      end loop;
      Stream.End_Entity ("snapshot");
      Stream.End_Entity (Node.Name);
   end Write_Snapshot;

end Helios.Reports;
