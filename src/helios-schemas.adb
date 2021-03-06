-----------------------------------------------------------------------
--  helios-schemas -- Helios schemas
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
with Ada.Strings.Fixed;
package body Helios.Schemas is

   function Allocate_Index (From : in Definition_Type_Access) return Value_Index;

   Root          : aliased Definition_Type (Len => 0);
   Current_Index : Monitor_Index := 0;

   function Allocate_Index (From : in Definition_Type_Access) return Value_Index is
   begin
      if From.Kind = V_NONE and From.Parent = Root'Access then
         From.Index := From.Index + 1;
         return From.Index;
      else
         return Allocate_Index (From.Parent);
      end if;
   end Allocate_Index;

   --  ------------------------------
   --  Returns true if the node has some definition children.
   --  ------------------------------
   function Has_Children (Node : in Definition_Type_Access) return Boolean is
   begin
      return Node.Child /= null;
   end Has_Children;

   --  ------------------------------
   --  Returns true if the node has other nodes that contain values.
   --  ------------------------------
   function Has_Snapshots (Node : in Definition_Type_Access) return Boolean is
   begin
      return Node.Child /= null and then Node.Child.Child = null;
   end Has_Snapshots;

   --  ------------------------------
   --  Returns true if the name is allowed by the filter configuration.
   --  The filter string is a comma separated list of allowed names.
   --  The special value "*" allows any name.
   --  ------------------------------
   function Is_Filter_Enable (Name   : in String;
                              Filter : in String) return Boolean is
      Pos : Natural;
   begin
      if Filter = "*" then
         return True;
      end if;
      Pos := Ada.Strings.Fixed.Index (Filter, Name);
      if Pos = 0 then
         return False;
      end if;
      return True;
   end Is_Filter_Enable;

   --  ------------------------------
   --  Add a new definition node to the definition.
   --  ------------------------------
   function Create_Definition (Into   : in Definition_Type_Access;
                               Name   : in String;
                               Filter : in String := "*";
                               Kind   : in Value_Type := V_INTEGER)
                               return Definition_Type_Access is
      Result : Definition_Type_Access;
   begin
      if not Is_Filter_Enable (Name, Filter) then
         return null;
      end if;
      Result := new Definition_Type (Len => Name'Length);
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
