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
package Helios.Monitor.Ifnet is

   type Stat_Type is (RX_BYTES, RX_PACKETS, RX_ERRS, RX_DROP, RX_FIFO, RX_FRAME,
                      RX_COMPRESS, RX_MULTICAST,
                      TX_BYTES, TX_PACKETS, TX_ERRS, TX_DROP, TX_FIFO, TX_COLLS,
                      TX_CARRIER, TX_COMPRESSED);

   type Stat_Definition_Array is array (Stat_Type) of Schemas.Definition_Type_Access;

   type Interface_Definition_Type is new Schemas.Definition_Type with record
      Stats : Stat_Definition_Array;
   end record;
   type Interface_Definition_Type_Access is access all Interface_Definition_Type'Class;

   type Agent_Type is new Helios.Monitor.Agent_Type with null record;

   --  Start the agent and build the definition tree.
   overriding
   procedure Start (Agent  : in out Agent_Type;
                    Config : in Util.Properties.Manager);

   --  Collect the values in the snapshot.
   overriding
   procedure Collect (Agent  : in out Agent_Type;
                      Values : in out Datas.Snapshot_Type);

   --  Make a new interface definition for the given interface name.
   procedure Make_Interface (Agent  : in out Agent_Type;
                             Name   : in String;
                             Filter : in String);

end Helios.Monitor.Ifnet;
