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

package body Helios.Monitor is

   Root          : aliased Definition_Type (Len => 0);
   Current_Index : Value_Index := 0;

   --  ------------------------------
   --  Get the root definition tree.
   --  ------------------------------
   function Get_Root return Definition_Type_Access is
   begin
      return Root'Access;
   end Get_Root;

   --  ------------------------------
   --  Initialize the snapshot.
   --  ------------------------------
   procedure Initialize (Data : in out Snapshot_Type) is
   begin
      Data.Values := new Value_Array (1 .. Current_Index);
   end Initialize;

   --  ------------------------------
   --  Set the value in the snapshot.
   --  ------------------------------
   procedure Set_Value (Into  : in out Snapshot_Type;
                        Def   : in Definition_Type_Access;
                        Value : in Uint64) is
   begin
      if Def /= null and then Def.Index > 0 then
         Into.Values (Def.Index) := Value;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create a new definition with the given name.
   --  ------------------------------
   function Create_Definition (Agent : in Agent_Type;
                               Name  : in String) return Definition_Type_Access is
      Result : constant Definition_Type_Access := new Definition_Type (Len => Name'Length);
   begin
      Current_Index := Current_Index + 1;
      Result.Name   := Name;
      Result.Parent := Agent.Node;
      Result.Next   := Agent.Node.Child;
      Agent.Node.Child := Result;
      Result.Index  := Current_Index;
      return Result;
   end Create_Definition;

   --  ------------------------------
   --  Add a new definition node to the definition.
   --  ------------------------------
   procedure Add_Definition (Into : in Definition_Type_Access;
                             Name : in String) is
      Result : constant Definition_Type_Access := new Definition_Type (Len => Name'Length);
   begin
      Current_Index := Current_Index + 1;
      Result.Name   := Name;
      Result.Parent := Into;
      Result.Next   := Into.Child;
      Into.Child    := Result;
      Result.Index  := Current_Index;
   end Add_Definition;

   --  ------------------------------
   --  Add a definition to the agent.
   --  ------------------------------
   procedure Add_Definition (Agent : in out Agent_Type;
                             Def   : in Definition_Type_Access) is
   begin
      Current_Index := Current_Index + 1;
      Def.Parent := Agent.Node;
      Def.Next := Agent.Node.Child;
      Def.Index := Current_Index;
      Agent.Node.Child := Def;
   end Add_Definition;

   --  ------------------------------
   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   --  ------------------------------
   function Find_Definition (Agent : in Agent_Type;
                             Name  : in String) return Definition_Type_Access is
      Node : Definition_Type_Access := Agent.Node;
   begin
      if Node = null then
         return null;
      end if;
      Node := Node.Child;
      while Node /= null loop
         if Node.Name = Name then
            return Node;
         end if;
         Node := Node.Next;
      end loop;
      return null;
   end Find_Definition;

   --  ------------------------------
   --  Register the agent.
   --  ------------------------------
   procedure Register (Agent : in out Agent_Type'Class;
                       Name  : in String) is
      Node : constant Definition_Type_Access := new Definition_Type (Len => Name'Length);
   begin
      Node.Name   := Name;
      Node.Parent := Root'Access;
      Node.Next   := Root.Child;
      Root.Child  := Node;
      Agent.Node  := Node;
      Agent.Start;
   end Register;

end Helios.Monitor;
