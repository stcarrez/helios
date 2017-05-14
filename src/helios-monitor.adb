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

   List : Agent_Type_Access;

   --  ------------------------------
   --  Create a new definition with the given name.
   --  ------------------------------
   function Create_Definition (Agent : in Agent_Type;
                               Name  : in String) return Schemas.Definition_Type_Access is
   begin
      return Schemas.Create_Definition (Agent.Node, Name);
   end Create_Definition;

   --  ------------------------------
   --  Add a definition to the agent.
   --  ------------------------------
   procedure Add_Definition (Agent : in out Agent_Type;
                             Def   : in Schemas.Definition_Type_Access) is
   begin
      Schemas.Add_Definition (Agent.Node, Def);
   end Add_Definition;

   --  ------------------------------
   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   --  ------------------------------
   function Find_Definition (Agent : in Agent_Type;
                             Name  : in String) return Schemas.Definition_Type_Access is
      use type Schemas.Definition_Type_Access;
   begin
      if Agent.Node = null then
         return null;
      else
         return Schemas.Find_Definition (Agent.Node, Name);
      end if;
   end Find_Definition;

   --  ------------------------------
   --  Register the agent.
   --  ------------------------------
   procedure Register (Agent : in out Agent_Type'Class;
                       Name  : in String) is
   begin
      Agent.Next := List;
      List := Agent'Unchecked_Access;
      Agent.Node := Schemas.Create_Definition (null, Name, Schemas.V_NONE);
      Agent.Start;
   end Register;

   --  ------------------------------
   --  Collect the values for each registered plugin.
   --  ------------------------------
   procedure Collect_All (Values : in out Datas.Snapshot_Type) is
      Agent : Agent_Type_Access := List;
   begin
      while Agent /= null loop
         Agent.Collect (Values);
         Agent := Agent.Next;
      end loop;
   end Collect_All;

end Helios.Monitor;
