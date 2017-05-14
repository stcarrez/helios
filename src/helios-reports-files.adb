-----------------------------------------------------------------------
--  helios-reports-files -- Write reports in files
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
with Ada.Streams.Stream_IO;
with Util.Serialize.IO.JSON;
with Util.Streams.Texts;
with Util.Streams.Files;
package body Helios.Reports.Files is

   --  ------------------------------
   --  Write the collected snapshot in the file in JSON format.
   --  ------------------------------
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Snapshot_Type;
                            Node : in Helios.Schemas.Definition_Type_Access) is
      File      : aliased Util.Streams.Files.File_Stream;
      Output    : aliased Util.Streams.Texts.Print_Stream;
      Stream    : Util.Serialize.IO.JSON.Output_Stream;
   begin
      File.Create (Ada.Streams.Stream_IO.Out_File, Path);
      Output.Initialize (File'Unchecked_Access);
      Stream.Initialize (Output'Unchecked_Access);
      Stream.Start_Document;
      Write_Snapshot (Stream, Data, Helios.Schemas.Get_Root);
      Stream.End_Document;
      Stream.Close;
   end Save_Snapshot;

   --  ------------------------------
   --  Write the collected snapshot in the file in JSON format.
   --  ------------------------------
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Snapshot_Queue_Type;
                            Node : in Helios.Schemas.Definition_Type_Access) is
      File      : aliased Util.Streams.Files.File_Stream;
      Output    : aliased Util.Streams.Texts.Print_Stream;
      Stream    : Util.Serialize.IO.JSON.Output_Stream;
   begin
      File.Create (Ada.Streams.Stream_IO.Out_File, Path);
      Output.Initialize (File'Unchecked_Access);
      Stream.Initialize (Output'Unchecked_Access);
      Stream.Start_Document;
      Write_Snapshot (Stream, Data, Helios.Schemas.Get_Root);
      Stream.End_Document;
      Stream.Close;
   end Save_Snapshot;

end Helios.Reports.Files;
