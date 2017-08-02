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
with Ada.Strings.Unbounded;
with Util.Events.Timers;
package Helios.Reports.Files is

   type File_Report_Type is limited new Util.Events.Timers.Timer with record
      Path   : Ada.Strings.Unbounded.Unbounded_String;
      Period : Natural := 0;
   end record;

   --  The timer handler executed when the timer deadline has passed.
   overriding
   procedure Time_Handler (Report : in out File_Report_Type;
                           Event  : in out Util.Events.Timers.Timer_Ref'Class);

   --  Write the collected snapshot in the file in JSON format.
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Snapshot_Type;
                            Node : in Helios.Schemas.Definition_Type_Access);

   --  Write the collected snapshot in the file in JSON format.
   procedure Save_Snapshot (Path : in String;
                            Data : in Helios.Datas.Report_Queue_Type);

end Helios.Reports.Files;
