-----------------------------------------------------------------------
--  helios-reports-remote -- Send reports to a remote server using REST API
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
with Ada.Real_Time;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Util.Events.Timers;
with Util.Properties;
with Swagger.Credentials.OAuth;
package Helios.Reports.Remote is

   package Queue_Interface is
     new Ada.Containers.Synchronized_Queue_Interfaces (Helios.Datas.Report_Queue_Type);

   package Snapshot_Queue is
      new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);

   type Remote_Report_Type;
   type Remote_Report_Access is access all Remote_Report_Type;

   task type Report_Task_Type is
      entry Start (Report : in Remote_Report_Access);
   end Report_Task_Type;

   type Remote_Report_Type is limited new Util.Events.Timers.Timer with record
      URI      : Ada.Strings.Unbounded.Unbounded_String;
      Bearer   : Ada.Strings.Unbounded.Unbounded_String;
      Period   : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Reporter : access Report_Task_Type;
      Queue    : Snapshot_Queue.Queue;
      Host_Id  : Integer := 0;
      Host_Key : Ada.Strings.Unbounded.Unbounded_String;
      Username : Ada.Strings.Unbounded.Unbounded_String;
      Password : Ada.Strings.Unbounded.Unbounded_String;
      Scope    : Ada.Strings.Unbounded.Unbounded_String;
      Cred     : aliased Swagger.Credentials.OAuth.OAuth2_Credential_Type;
   end record;

   procedure Start (Report : in Remote_Report_Access);

   --  The timer handler executed when the timer deadline has passed.
   overriding
   procedure Time_Handler (Report : in out Remote_Report_Type;
                           Event  : in out Util.Events.Timers.Timer_Ref'Class);

   --  Send a snapshot report to the server.
   procedure Send (Report : in out Remote_Report_Type;
                   Data   : in Helios.Datas.Report_Queue_Type);

   --  Set the remote report configuration to connect to the server.
   procedure Set_Server_Config (Report : in out Remote_Report_Type;
                                Config : in Util.Properties.Manager);

end Helios.Reports.Remote;
