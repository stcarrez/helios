-----------------------------------------------------------------------
--  helios-schemas -- Helios schemas
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

package body Helios.Schemas is

   Root          : aliased Definition_Type (Len => 0);
   Current_Index : Monitor_Index := 0;

   --  ------------------------------
   --  Get the root definition tree.
   --  ------------------------------
   function Get_Root return Definition_Type_Access is
   begin
      return Root'Access;
   end Get_Root;

   --  ------------------------------
   --  Get the number of definition values.
   --  ------------------------------
   function Get_Count return Value_Index is
   begin
      return 0; --  Current_Index;
   end Get_Count;

   function Allocate_Index (From : in Definition_Type_Access) return Value_Index is
   begin
      if From.Kind = V_NONE then
         From.Index := From.Index + 1;
         return From.Index;
      else
         return Allocate_Index (From.Parent);
      end if;
   end Allocate_Index;

   --  ------------------------------
   --  Add a new definition node to the definition.
   --  ------------------------------
   function Create_Definition (Into  : in Definition_Type_Access;
                               Name  : in String;
                               Kind  : in Value_Type := V_INTEGER) return Definition_Type_Access is
      Result : constant Definition_Type_Access := new Definition_Type (Len => Name'Length);
   begin
      Result.Kind := Kind;
      if Kind = V_NONE then
         Current_Index := Current_Index + 1;
         Result.Monitor := Current_Index;
      else
         Result.Monitor := Into.Monitor;
         Result.Index := Allocate_Index (Into);
      end if;
      Result.Name   := Name;
      Result.Parent := Into;
      if Into /= null then
         Result.Next   := Into.Child;
         Into.Child    := Result;
      else
         Result.Parent := Root'Access;
         Result.Next   := Root.Child;
         Root.Child    := Result;
      end if;
      return Result;
   end Create_Definition;

   --  ------------------------------
   --  Add a definition to the agent.
   --  ------------------------------
   procedure Add_Definition (Into : in Definition_Type_Access;
                             Def  : in Definition_Type_Access) is
   begin
      Def.Index := Allocate_Index (Into);
      Def.Monitor := Into.Monitor;
      Def.Parent := Into;
      Def.Next := Into.Child;
      Into.Child := Def;
   end Add_Definition;

   --  ------------------------------
   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   --  ------------------------------
   function Find_Definition (From : in Definition_Type_Access;
                             Name : in String) return Definition_Type_Access is
      Node : Definition_Type_Access := From;
   begin
      while Node /= null loop
         if Node.Name = Name then
            return Node;
         end if;
         Node := Node.Next;
      end loop;
      return null;
   end Find_Definition;

end Helios.Schemas;
