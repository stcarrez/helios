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

package body Helios.Reports is

   use type Uint64;
   use type Helios.Schemas.Value_Index;
   use type Helios.Schemas.Definition_Type_Access;

   --  ------------------------------
   --  Write the collected snapshot in the IO stream.  The output stream can be an XML
   --  or a JSON stream.  The node definition is used for the structure of the output content.
   --  ------------------------------
   procedure Write_Snapshot (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                             Data   : in Helios.Datas.Snapshot_Type;
                             Node   : in Helios.Schemas.Definition_Type_Access) is
      Child : Helios.Schemas.Definition_Type_Access;
      Value : Uint64;
   begin
      Stream.Start_Entity (Node.Name);
      Child := Node.Child;
      while Child /= null loop
         if Child.Child /= null then
            Write_Snapshot (Stream, Data, Child);
         elsif Child.Index > 0 then
            Value := Data.Values (Child.Index);
            if Value < Uint64 (Integer'Last) then
               Stream.Write_Entity (Child.Name, Integer (Value));
            elsif Value < Uint64 (Long_Long_Integer'Last) then
               Stream.Write_Long_Entity (Child.Name, Long_Long_Integer (Value));
            else
               Stream.Write_Entity (Child.Name, Uint64'Image (Value));
            end if;
         end if;
         Child := Child.Next;
      end loop;
      Stream.End_Entity (Node.Name);
   end Write_Snapshot;

end Helios.Reports;
