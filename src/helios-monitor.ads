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

with Ada.Finalization;
with Ada.Real_Time;
with Util.Properties;
with Util.Events.Timers;
with Helios.Schemas;
with Helios.Datas;
package Helios.Monitor is

   type Agent_Type;
   type Agent_Type_Access is access all Agent_Type'Class;
   type Agent_Type is new Ada.Finalization.Limited_Controlled
     and Util.Events.Timers.Timer with record
      Next   : Agent_Type_Access;
      Index  : Helios.Schemas.Monitor_Index;
      Node   : Schemas.Definition_Type_Access;
      Period : Ada.Real_Time.Time_Span;
      Data   : Helios.Datas.Snapshot_Queue_Access;
   end record;

   --  The timer handler executed when the timer deadline has passed.
   overriding
   procedure Time_Handler (Agent : in out Agent_Type;
                           Event : in out Util.Events.Timers.Timer_Ref'Class);

   --  Create a new definition with the given name.
   function Create_Definition (Agent : in Agent_Type;
                               Name  : in String) return Schemas.Definition_Type_Access;

   --  Add a definition to the agent.
   procedure Add_Definition (Agent : in out Agent_Type;
                             Def   : in Schemas.Definition_Type_Access);

   --  Find a child definition with the given name.
   --  Returns null if there is no such definition.
   function Find_Definition (Agent : in Agent_Type;
                             Name  : in String) return Schemas.Definition_Type_Access;

   --  Register the agent.
   procedure Register (Agent  : in Agent_Type_Access;
                       Name   : in String;
                       Config : in Util.Properties.Manager);

   --  Start the agent and build the definition tree.
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager) is null;

   --  Collect the values in the snapshot.
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type) is null;

   --  Collect the values for each registered plugin.
   procedure Collect_All (Values : in out Datas.Snapshot_Type);

   --  Get a period configuration parameter.
   function Get_Period (Config  : in Util.Properties.Manager;
                        Name    : in String;
                        Default : in Natural) return Ada.Real_Time.Time_Span;

end Helios.Monitor;
