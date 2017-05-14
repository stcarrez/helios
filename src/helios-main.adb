-----------------------------------------------------------------------
--  helios-main -- Helios agent main procedure
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
with Ada.Exceptions;
with Ada.Text_IO;

with Util.Log.loggers;
with Util.Streams.Texts;
with Util.Streams.Buffered;
with Util.Serialize.IO.JSON;
with Helios.Monitor.CPU;
with Helios.Monitor.Ifnet;
with Helios.Monitor.Disks;
with Helios.Reports;
with Helios.Schemas;
with Helios.Datas;
procedure Helios.Main is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Main");

   Mon       : Helios.Monitor.CPU.Agent_Type;
   Ifnet_Mon : Helios.Monitor.Ifnet.Agent_Type;
   Disk_Mon  : Helios.Monitor.Disks.Agent_Type;
   Data      : Helios.Datas.Snapshot_Type;
   Output    : aliased Util.Streams.Texts.Print_Stream;
   Stream    : Util.Serialize.IO.JSON.Output_Stream;
begin
   Util.Log.Loggers.Initialize ("helios.properties");

   Helios.Monitor.Register (Mon, "cpu");
   Helios.Monitor.Register (Ifnet_Mon, "ifnet");
   Helios.Monitor.Register (Disk_Mon, "disks");
   Output.Initialize (Size => 1_000_000);
   Stream.Initialize (Output'Unchecked_Access);
   Stream.Start_Document;
   Stream.Start_Array ("raw");
   Helios.Datas.Initialize (Data);
   for I in 1 .. 1 loop
      Mon.Collect (Data);
      Ifnet_Mon.Collect (Data);
      Disk_Mon.Collect (Data);
      Helios.Reports.Write_Snapshot (Stream, Data, Helios.Schemas.Get_Root);
      delay 1.0;
   end loop;
   Stream.End_Array ("raw");
   Stream.End_Document;
   Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Output));
end Helios.Main;
