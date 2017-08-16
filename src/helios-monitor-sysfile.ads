-----------------------------------------------------------------------
--  helios-monitor-sysfile -- Generic system file monitor
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
with Ada.Strings.Unbounded;
package Helios.Monitor.Sysfile is

   type Agent_Type is new Helios.Monitor.Agent_Type with record
      Path            : Ada.Strings.Unbounded.Unbounded_String;
      Value           : Schemas.Definition_Type_Access;
   end record;

   --  Start the agent and build the definition tree.
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager);

   --  Collect the values in the snapshot.
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type);

end Helios.Monitor.Sysfile;
