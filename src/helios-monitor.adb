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
with Util.Properties.Basic;
package body Helios.Monitor is

   List : Agent_Type_Access;

   --  ------------------------------
   --  The timer handler executed when the timer deadline has passed.
   --  ------------------------------
   overriding
   procedure Time_Handler (Agent : in out Agent_Type;
                           Event : in out Util.Events.Timers.Timer_Ref'Class) is
   begin
      Event.Repeat (Agent.Period);
   end Time_Handler;

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
      if Agent.Node = null or else Agent.Node.Child = null then
         return null;
      else
         return Schemas.Find_Definition (Agent.Node.Child, Name);
      end if;
   end Find_Definition;

   --  ------------------------------
   --  Register the agent.
   --  ------------------------------
   procedure Register (Agent  : in Agent_Type_Access;
                       Name   : in String;
                       Config : in Util.Properties.Manager) is
   begin
      Agent.Period := Get_Period (Config, "period", 1);
      Agent.Next := List;
      List := Agent;
      Agent.Node := Schemas.Create_Definition (null, Name, Schemas.V_NONE);
      Agent.Start (Config);
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

   --  ------------------------------
   --  Get a period configuration parameter.
   --  ------------------------------
   function Get_Period (Config  : in Util.Properties.Manager;
                        Name    : in String;
                        Default : in Natural) return Ada.Real_Time.Time_Span is
      Period : Integer
        := Util.Properties.Basic.Integer_Property.Get (Config, Name, Default);
   begin
      if Period <= 0 then
         Period := Default;
      end if;
      return Ada.Real_Time.Seconds (Period);
   end Get_Period;

end Helios.Monitor;
