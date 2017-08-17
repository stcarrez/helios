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

--  == Schema Definition ==
--  The <tt>Helios.Schemas</tt> package describes the data schema that describes the data
--  collected by the monitoring agent.
package Helios.Schemas is

   type Value_Type is (V_NONE, V_INTEGER);

   type Value_Index is new Natural;
   subtype Value_Array_Index is Value_Index range 1 .. Value_Index'Last;

   type Monitor_Index is new Natural;

   type Definition_Type;
   type Definition_Type_Access is access all Definition_Type'Class;

   --  The schema definition.
   type Definition_Type (Len : Natural) is tagged record
      Kind    : Value_Type := V_NONE;
      Index   : Value_Index := 0;
      Monitor : Monitor_Index := 0;
      Parent  : Definition_Type_Access;
      Next    : Definition_Type_Access;
      Child   : Definition_Type_Access;
      Sibling : Definition_Type_Access;
      Name    : String (1 .. Len);
   end record;

   --  Returns true if the node has some definition children.
   function Has_Children (Node : in Definition_Type_Access) return Boolean;

   --  Returns true if the node has other nodes that contain values.
   function Has_Snapshots (Node : in Definition_Type_Access) return Boolean;

   --  Add a new definition node to the definition.
   function Create_Definition (Into   : in Definition_Type_Access;
                               Name   : in String;
                               Filter : in String := "*";
                               Kind   : in Value_Type := V_INTEGER)
                               return Definition_Type_Access with
     Pre => Kind = V_NONE or else Into /= null;

   --  Add a definition to the list.
   procedure Add_Definition (Into : in Definition_Type_Access;
                             Def  : in Definition_Type_Access);

   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   function Find_Definition (From : in Definition_Type_Access;
                             Name : in String) return Definition_Type_Access with
     Pre => From /= null;

   --  Returns true if the name is allowed by the filter configuration.
   --  The filter string is a comma separated list of allowed names.
   --  The special value "*" allows any name.
   function Is_Filter_Enable (Name   : in String;
                              Filter : in String) return Boolean;

end Helios.Schemas;
