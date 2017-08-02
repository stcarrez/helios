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
   procedure Make_Interface (Agent      : in out Agent_Type;
                             Name       : in String;
                             Interfaces : in String;
                             Filter     : in String) is
      Itf : Interface_Definition_Type_Access;
   begin
      if not Helios.Schemas.Is_Filter_Enable (Name, Interfaces) then
         return;
      end if;
      Itf := new Interface_Definition_Type (Len => Name'Length);
      Itf.Name := Name;
      Agent.Add_Definition (Itf.all'Access);
      for I in Itf.Stats'Range loop
         Itf.Stats (I) := Schemas.Create_Definition (Itf.all'Access,
                                                     To_Lower_Case (Stat_Type'Image (I)),
                                                     Filter);
      end loop;
   end Make_Interface;

   --  ------------------------------
   --  Start the agent and build the definition tree.
   --  ------------------------------
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager) is
      Values     : constant String := Config.Get ("values", "*");
      Interfaces : constant String := Config.Get ("interfaces", "*");
      Line       : Helios.Tools.Files.File_Extractor;
   begin
      Line.Open ("/proc/net/dev");
      Line.Read;
      Line.Read;
      loop
         Line.Read;
         exit when Line.Is_Eof;
         Make_Interface (Agent, Line.Get_Value (1), Interfaces, Values);
      end loop;
   end Start;

   --  ------------------------------
   --  Collect the values in the snapshot.
   --  ------------------------------
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type) is
      use type Schemas.Definition_Type_Access;

      Line : Helios.Tools.Files.File_Extractor;
      Node : Schemas.Definition_Type_Access;
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
               Values.Set_Value (Itf.Stats (I), Line.Get_Value (2 + Stat_Type'Pos (I)));
            end loop;
         end if;
      end loop;
   end Collect;

end Helios.Monitor.Ifnet;
