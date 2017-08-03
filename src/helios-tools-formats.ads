-----------------------------------------------------------------------
--  helios-tools-formats -- Formatting utilities for the agent
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
with Ada.Calendar;
package Helios.Tools.Formats is

   --  Format the date by using the given pattern.
   --  @see Utils.Dates.Formats
   function Format (Pattern : in String;
                    Date    : in Ada.Calendar.Time) return String;

   --  Initialize the resource bundle directory.
   procedure Initialize (Path : in String);

end Helios.Tools.Formats;
