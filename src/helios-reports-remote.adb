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
with Util.Serialize.IO.JSON;
with Util.Streams.Texts;
with Util.Http.Clients;
with Util.Log.Loggers;
with Util.Strings;
with Util.Beans.Objects;
with Helios.Monitor;
package body Helios.Reports.Remote is

   use Ada.Strings.Unbounded;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Reports.Remote");

   task body Report_Task_Type is
      R      : Remote_Report_Access;
      Data   : Helios.Datas.Report_Queue_Type;
   begin
      select
         accept Start (Report : in Remote_Report_Access) do
            R := Report;
         end Start;
      end select;
      loop
         R.Queue.Dequeue (Data);
         R.Send (Data);
      end loop;
   end Report_Task_Type;

   procedure Start (Report : in Remote_Report_Access) is
   begin
      Log.Info ("Starting remote report task");
      Report.Reporter := new Report_Task_Type;
      Report.Reporter.Start (Report);
   end Start;

   --  ------------------------------
   --  The timer handler executed when the timer deadline has passed.
   --  ------------------------------
   overriding
   procedure Time_Handler (Report : in out Remote_Report_Type;
                           Event  : in out Util.Events.Timers.Timer_Ref'Class) is
      use type Ada.Real_Time.Time_Span;
   begin
      Report.Queue.Enqueue (Helios.Monitor.Get_Report);
      if Report.Period /= Ada.Real_Time.Time_Span_Zero then
         Event.Repeat (Report.Period);
      end if;
   end Time_Handler;

   --  Set the remote report configuration to connect to the server.
   procedure Set_Server_Config (Report : in out Remote_Report_Type;
                                Config : in Util.Properties.Manager) is
      Value : Util.Beans.Objects.Object;
      URI   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Value := Config.Get_Value ("host_id");
      if Util.Beans.Objects.Is_Null (Value) then
         return;
      end if;
      Report.Host_Id := Util.Beans.Objects.To_Integer (Value);
      Report.Host_Key := Config.Get ("host_key");
      Report.Username := Config.Get ("username");
      Report.Password := Config.Get ("password");
      Report.Scope := To_Unbounded_String ("write:snapshot");

      URI := Config.Get ("server_url");
      Report.URI := URI;
      Append (Report.URI, "/hosts/");
      Append (Report.URI, Util.Strings.Image (Report.Host_Id));
      Append (Report.URI, "/snapshot");

      --  Step 1: get an OAuth access token.
      Report.Cred.Set_Application_Identifier (Config.Get ("client_id"));
      Report.Cred.Set_Application_Secret (Config.get ("client_secret"));
      Report.Cred.Set_Provider_URI (Config.Get ("server_oauth_url"));
      Report.Cred.Request_Token (To_String (Report.Username),
                                 To_String (Report.Password),
                                 To_String (Report.Scope));
   end Set_Server_Config;

   --  ------------------------------
   --  Send a snapshot report to the server.
   --  ------------------------------
   procedure Send (Report : in out Remote_Report_Type;
                   Data   : in Helios.Datas.Report_Queue_Type) is
      Output    : aliased Util.Streams.Texts.Print_Stream;
      Stream    : Util.Serialize.IO.JSON.Output_Stream;
      Response  : Util.Http.Clients.Response;
      Http      : Util.Http.Clients.Client;
      URI       : constant String := To_String (Report.URI);

      procedure Write (Data : in Helios.Datas.Snapshot_Type;
                       Node : in Helios.Schemas.Definition_Type_Access) is
      begin
         Write_Snapshot (Stream, Data, Node);
      end Write;

   begin
      Output.Initialize (Size => 1_000_000);
      Stream.Initialize (Output'Unchecked_Access);
      Stream.Start_Document;
      Stream.Write_Entity ("host_key", Report.Host_Key);
      Stream.Start_Entity ("snapshot");
      Helios.Datas.Iterate (Data, Write'Access);
      Stream.End_Entity ("snapshot");
      Stream.End_Document;
      Stream.Close;
      Http.Add_Header ("X-Requested-By", "helios");
      Http.Add_Header ("Content-Type", "application/json");
      Http.Add_Header ("Accept", "application/json");

      for Retry in 1 .. 5 loop
         Report.Cred.Set_Credentials (Http);

         Log.Info ("Sending report to {0}", URI);
         Http.Post (URI, Util.Streams.Texts.To_String (Output), Response);
         exit when Response.Get_Status = 200;

         if Response.Get_Status = 401 then
            Report.Cred.Request_Token (To_String (Report.Username),
                                       To_String (Report.Password),
                                       To_String (Report.Scope));
         end if;
         if Response.Get_Status /= 200 then
            Log.Error ("Sending snapshot failed");
         end if;
      end loop;

   exception
      when E : others =>
         Log.Error ("Error when sending report", E);

   end Send;

end Helios.Reports.Remote;
