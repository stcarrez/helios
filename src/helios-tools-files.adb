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
with Util.Log.Loggers;
package body Helios.Tools.Files is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Helios.Tools.Files");

   --  ------------------------------
   --  Open the file and prepare for the extraction.
   --  ------------------------------
   procedure Open (File : in out File_Extractor;
                   Path : in String) is
   begin
      Log.Info ("Open file {0}", Path);
      Ada.Text_IO.Open (File => File.File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Path);
      File.Is_Opened := True;
   end Open;

   --  ------------------------------
   --  Returns True if we are at end of the file.
   --  ------------------------------
   function Is_Eof (File : in File_Extractor) return Boolean is
   begin
      return Ada.Text_IO.End_Of_File (File.File);
   end Is_Eof;

   --  ------------------------------
   --  Returns true if the line named field matches the given name.
   --  ------------------------------
   function Is_Field (File : in File_Extractor;
                      Name : in String) return Boolean is
      Pos : constant Natural := File.Name_Pos;
   begin
      if File.Field_Count < Pos then
         return False;
      else
         return File.Line (File.Field_Start (Pos) .. File.Field_End (Pos)) = Name;
      end if;
   end Is_Field;

   --  ------------------------------
   --  Get the value of the field at the given position.
   --  ------------------------------
   function Get_Value (File : in File_Extractor;
                       Pos  : in Positive) return Uint64 is
   begin
      return Uint64'Value (File.Line (File.Field_Start (Pos) .. File.Field_End (Pos)));
   end Get_Value;

   --  ------------------------------
   --  Get the value of the field at the given position.
   --  ------------------------------
   function Get_Value (File : in File_Extractor;
                       Pos  : in Positive) return String is
   begin
      return File.Line (File.Field_Start (Pos) .. File.Field_End (Pos));
   end Get_Value;


   --  ------------------------------
   --  Read one line and prepare for extraction.
   --  ------------------------------
   procedure Read (File : in out File_Extractor) is
      Pos       : Positive := 1;
      Last      : Natural := 0;
      Field_Pos : Natural := 0;
      Line      : constant String := Ada.Text_IO.Get_Line (File.File);
   begin
      Last := (if Line'Last > File.Line'Last then File.Line'Last else Line'Last);
      File.Line (1 .. Last) := Line (1 .. Last);
      while Pos <= Last and Field_Pos < File.Count loop
         while Pos <= Last and then Line (Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
         exit when Pos > Last;
         Field_Pos := Field_Pos + 1;
         File.Field_Start (Field_Pos) := Pos;
         while Pos <= Last and then Line (Pos) /= ' ' loop
            Pos := Pos + 1;
         end loop;
         File.Field_End (Field_Pos) := Pos - 1;
      end loop;
      File.Field_Count := Field_Pos;
   end Read;

   --  ------------------------------
   --  Close the file.
   --  ------------------------------
   overriding
   procedure Finalize (File : in out File_Extractor) is
   begin
      if File.Is_Opened then
         Ada.Text_IO.Close (File.File);
      end if;
   end Finalize;

end Helios.Tools.Files;
