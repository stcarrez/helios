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
with Ada.Calendar;
with Ada.Real_Time;
with Ada.Finalization;
package Helios.Monitor is

   type Value_Type is (V_INTEGER);

   type Value_Index is new Natural;
   subtype Value_Array_Index is Value_Index range 1 .. Value_Index'Last;

   type Definition_Type;
   type Definition_Type_Access is access all Definition_Type'Class;

   type Definition_Type (Len : Natural) is tagged record
      Kind    : Value_Type;
      Index   : Value_Index := 0;
      Parent  : Definition_Type_Access;
      Next    : Definition_Type_Access;
      Child   : Definition_Type_Access;
      Sibling : Definition_Type_Access;
      Name    : String (1 .. Len);
   end record;

   type Value_Array is array (Value_Array_Index range <>) of Uint64;
   type Value_Array_Access is access all Value_Array;

   type Snapshot_Type is limited record
      Time          : Ada.Calendar.Time;
      Start_Time    : Ada.Real_Time.Time;
      End_Time      : Ada.Real_Time.Time;
      Values        : Value_Array_Access;
   end record;

   --  Get the root definition tree.
   function Get_Root return Definition_Type_Access;

   --  Initialize the snapshot.
   procedure Initialize (Data : in out Snapshot_Type);

   --  Set the value in the snapshot.
   procedure Set_Value (Into  : in out Snapshot_Type;
                        Def   : in Definition_Type_Access;
                        Value : in Uint64);

   type Agent_Type is new Ada.Finalization.Limited_Controlled with record
      Node : Definition_Type_Access;
   end record;

   --  Create a new definition with the given name.
   function Create_Definition (Agent : in Agent_Type;
                               Name  : in String) return Definition_Type_Access;

   --  Add a definition to the agent.
   procedure Add_Definition (Agent : in out Agent_Type;
                             Def   : in Definition_Type_Access);

   --  Add a new definition node to the definition.
   procedure Add_Definition (Into : in Definition_Type_Access;
                             Name : in String);

   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   function Find_Definition (Agent : in Agent_Type;
                             Name  : in String) return Definition_Type_Access;

   --  Register the agent.
   procedure Register (Agent : in out Agent_Type'Class;
                       Name  : in String);

   --  Start the agent and build the definition tree.
   procedure Start (Agent : in out Agent_Type) is null;

   --  Collect the values in the snapshot.
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Snapshot_Type) is null;

end Helios.Monitor;
