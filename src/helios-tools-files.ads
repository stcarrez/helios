-----------------------------------------------------------------------
--  helios-tools-files -- File parsing utilities for Helios
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
with Ada.Text_IO;
with Ada.Finalization;
package Helios.Tools.Files is

   subtype Position_Type is Natural range 0 .. 65535;

   type Uint64_Array is array (Positive range <>) of Uint64;

   type Position_Array is array (Positive range <>) of Position_Type;

   type File_Extractor (Length : Position_Type := 1024;
                        Count  : Natural := 64) is limited
   new Ada.Finalization.Limited_Controlled with record
      File        : Ada.Text_IO.File_Type;
      Is_Opened   : Boolean := False;
      Current_Len : Natural := 0;
      Field_Count : Natural := 0;
      Name_Pos    : Natural := 1;
      Line        : String (1 .. Length);
      Field_Start : Position_Array (1 .. Count);
      Field_End   : Position_Array (1 .. Count);
      Values      : Uint64_Array (1 .. Count);
   end record;

   --  Open the file and prepare for the extraction.
   procedure Open (File : in out File_Extractor;
                   Path : in String);

   --  Returns True if we are at end of the file.
   function Is_Eof (File : in File_Extractor) return Boolean;

   --  Returns true if the line named field matches the given name.
   function Is_Field (File : in File_Extractor;
                      Name : in String) return Boolean;

   --  Get the value of the field at the given position.
   function Get_Value (File : in File_Extractor;
                       Pos  : in Positive) return Uint64;

   --  Get the value of the field at the given position.
   function Get_Value (File : in File_Extractor;
                       Pos  : in Positive) return String;

   --  Read one line and prepare for extraction.
   procedure Read (File : in out File_Extractor);

   --  Close the file.
   overriding
   procedure Finalize (File : in out File_Extractor);

end Helios.Tools.Files;
