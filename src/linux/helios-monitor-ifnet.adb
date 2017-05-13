-----------------------------------------------------------------------
--  helios-monitor-ifnet -- Linux network interface monitor
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
with Helios.Tools.Files;
with Util.Strings.Transforms;
package body Helios.Monitor.Ifnet is

   use Util.Strings.Transforms;

   --  ------------------------------
   --  Make a new interface definition for the given interface name.
   --  ------------------------------
   procedure Make_Interface (Agent : in out Agent_Type;
                             Name  : in String) is
      Itf : constant Interface_Definition_Type_Access
        := new Interface_Definition_Type (Len => Name'Length);
   begin
      Itf.Name := Name;
      Agent.Add_Definition (Itf.all'Access);
      for I in Itf.Stats'Range loop
         Itf.Stats (I) := Create_Definition (Itf.all'Access, To_Lower_Case (Stat_Type'Image (I)));
      end loop;
   end Make_Interface;

   --  ------------------------------
   --  Start the agent and build the definition tree.
   --  ------------------------------
   overriding
   procedure Start (Agent : in out Agent_Type) is
   begin
      Make_Interface (Agent, "eth1");
      Make_Interface (Agent, "eth3");
      Make_Interface (Agent, "tap0");
   end Start;

   --  ------------------------------
   --  Collect the values in the snapshot.
   --  ------------------------------
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Snapshot_Type) is
      Line : Helios.Tools.Files.File_Extractor;
      Node : Definition_Type_Access;
      Itf  : Interface_Definition_Type_Access;
   begin
      Line.Open ("/proc/net/dev");
      Line.Read;
      Line.Read;
      loop
         Line.Read;
         exit when Line.Is_Eof;
         Node := Agent.Find_Definition (Line.Get_Value (1));
         if Node /= null then
            Itf := Interface_Definition_Type'Class (Node.all)'Access;
            for I in Itf.Stats'Range loop
               Set_Value (Values, Itf.Stats (I), Line.Get_Value (2 + Stat_Type'Pos (I)));
            end loop;
         end if;
      end loop;
   end Collect;

end Helios.Monitor.Ifnet;
