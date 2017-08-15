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

      procedure Write_Values (Data   : in Helios.Datas.Snapshot_Type;
                              Node   : in Helios.Schemas.Definition_Type_Access) is
         Prev_Value : Uint64;

         procedure Write_Value (Value : in Uint64) is
            Offset : Long_Long_Integer;
         begin
            if Value > Prev_Value then
               Offset := Long_Long_Integer (Value - Prev_Value);
            else
               Offset := -Long_Long_Integer (Prev_Value - Value);
            end if;
            Prev_Value := Value;
            if Offset < Long_Long_Integer (Integer'Last)
              and Offset > Long_Long_Integer (Integer'First)
            then
               Stream.Write_Entity (Node.Name, Integer (Offset));
            else
               Stream.Write_Long_Entity (Node.Name, Offset);
            end if;
         end Write_Value;
      begin
         Prev_Value := 0;
         Stream.Start_Array (Node.Name);
         Helios.Datas.Iterate (Data, Node, Write_Value'Access);
         Stream.End_Array (Node.Name);
      end Write_Values;

      procedure Write_Snapshot (Data   : in Helios.Datas.Snapshot_Type;
                                Node   : in Helios.Schemas.Definition_Type_Access) is
      begin
         Stream.Start_Entity (Node.Name);
         Stream.Write_Entity ("period", 10);
         --  Write_Timestamp (Stream, "timestamp", Data.Start_Time);
         Stream.Start_Entity ("snapshot");
         Helios.Datas.Iterate (Data, Node, Write_Snapshot'Access, Write_Values'Access);
         Stream.End_Entity ("snapshot");
         Stream.End_Entity (Node.Name);
      end Write_Snapshot;

   begin
      Write_Snapshot (Data, Node);
   end Write_Snapshot;

end Helios.Reports;
