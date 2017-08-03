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
with Ada.Calendar;
with Util.Serialize.IO.JSON;
with Util.Streams.Texts;
with Util.Streams.Files;
with Helios.Tools.Formats;
package body Helios.Reports.Files is

   use type Ada.Real_Time.Time_Span;
   use type Helios.Datas.Snapshot_Type_Access;

   --  ------------------------------
   --  The timer handler executed when the timer deadline has passed.
   --  ------------------------------
   overriding
   procedure Time_Handler (Report : in out File_Report_Type;
                           Event  : in out Util.Events.Timers.Timer_Ref'Class) is
      Pattern : constant String := Ada.Strings.Unbounded.To_String (Report.Path);
      Path    : constant String := Helios.Tools.Formats.Format (Pattern, Ada.Calendar.Clock);
   begin
      Save_Snapshot (Path, Helios.Datas.Get_Report);
      if Report.Period /= Ada.Real_Time.Time_Span_Zero then
         Event.Repeat (Report.Period);
      end if;
   end Time_Handler;

   --  ------------------------------
   --  Write the collected snapshot in the file in JSON format.
   --  ------------------------------
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Snapshot_Type) is
      File      : aliased Util.Streams.Files.File_Stream;
      Output    : aliased Util.Streams.Texts.Print_Stream;
      Stream    : Util.Serialize.IO.JSON.Output_Stream;
   begin
      File.Create (Ada.Streams.Stream_IO.Out_File, Path);
      Output.Initialize (File'Unchecked_Access);
      Stream.Initialize (Output'Unchecked_Access);
      Stream.Start_Document;
      Write_Snapshot (Stream, Data, Data.Schema);
      Stream.End_Document;
      Stream.Close;
   end Save_Snapshot;

   --  ------------------------------
   --  Write the collected snapshot in the file in JSON format.
   --  ------------------------------
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Report_Queue_Type) is
      File      : aliased Util.Streams.Files.File_Stream;
      Output    : aliased Util.Streams.Texts.Print_Stream;
      Stream    : Util.Serialize.IO.JSON.Output_Stream;
      Snapshot  : Helios.Datas.Snapshot_Type_Access;
   begin
      File.Create (Ada.Streams.Stream_IO.Out_File, Path);
      Output.Initialize (File'Unchecked_Access);
      Stream.Initialize (Output'Unchecked_Access);
      Stream.Start_Document;
      Snapshot := Data.Snapshot;
      while Snapshot /= null loop
         Write_Snapshot (Stream, Snapshot.all, Snapshot.Schema);
         Snapshot := Snapshot.Next;
      end loop;
      Stream.End_Document;
      Stream.Close;
   end Save_Snapshot;

end Helios.Reports.Files;
