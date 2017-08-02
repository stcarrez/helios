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
with Ada.Strings.Fixed;
with Util.Properties.Basic;
with Util.Log.Loggers;
package body Helios.Monitor is

   use type Helios.Schemas.Value_Index;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Monitor");

   List : Agent_Type_Access;

   function Is_Filter_Enable (Name   : in String;
                              Filter : in String) return Boolean;

   --  ------------------------------
   --  The timer handler executed when the timer deadline has passed.
   --  ------------------------------
   overriding
   procedure Time_Handler (Agent : in out Agent_Type;
                           Event : in out Util.Events.Timers.Timer_Ref'Class) is
      Data : Helios.Datas.Snapshot_Type_Access;
   begin
      Log.Debug ("Running agent {0}", Agent.Node.Name);

      Helios.Datas.Prepare (Agent.Data, Data);
      Agent_Type'Class (Agent).Collect (Data.all);
      Event.Repeat (Agent.Period);
   end Time_Handler;

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
   --  Create a new definition with the given name.  The filter parameter allows to control
   --  which definition values are really needed.  The "*" indicates that all values are required.
   --  Otherwise, it is a comma separated list of strings.  A null definition is returned if
   --  the filter does not contain the definition name.
   --  ------------------------------
   function Create_Definition (Agent  : in Agent_Type;
                               Name   : in String;
                               Filter : in String := "*") return Schemas.Definition_Type_Access is
   begin
      if Is_Filter_Enable (Name, Filter) then
         return Schemas.Create_Definition (Agent.Node, Name);
      else
         return null;
      end if;
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
      Log.Info ("Register agent {0}", Name);

      Agent.Period := Get_Period (Config, "period", 1);
      Agent.Node := Schemas.Create_Definition (null, Name, Schemas.V_NONE);
      Agent.Start (Config);
      if Agent.Node.Index > 0 then
         Agent.Next := List;
         List := Agent;
      end if;
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
   --  Iterate over the plugin agents that are registered and execute
   --  the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (Process : not null access procedure (Agent : in out Agent_Type'Class)) is
      Agent : Agent_Type_Access := List;
   begin
      while Agent /= null loop
         Process (Agent.all);
         Agent := Agent.Next;
      end loop;
   end Iterate;

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
