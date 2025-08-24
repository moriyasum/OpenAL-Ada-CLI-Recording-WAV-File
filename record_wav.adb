with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;
with Interfaces;               use Interfaces;  --  for unsigned_16,32
with Ada.Characters;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;  use Ada.Strings.Unbounded.Text_IO;
with OpenAL;                   use OpenAL;
with OpenAL.Context;           use OpenAL.Context;
with OpenAL.Context.Capture;
with OpenAL.Types;             use OpenAL.Types;
with OpenAL.Buffer;            use OpenAL.Buffer;
with OpenAL_Info;           --  NOTE:Underline (not Dash)

procedure Record_WAV is

   --  WAV header structure
   type WAV_Header is record
      ChunkID       : String (1 .. 4);  --  RIFF
      ChunkSize     : Unsigned_32;
      Format        : String (1 .. 4);  --  WAVE
      Subchunk1ID   : String (1 .. 4);  --  fmt(space)
      Subchunk1Size : Unsigned_32;
      AudioFormat   : Unsigned_16;
      NumChannels   : Unsigned_16;
      SampleRate    : Unsigned_32;
      ByteRate      : Unsigned_32;
      BlockAlign    : Unsigned_16;
      BitsPerSample : Unsigned_16;
      Subchunk2ID   : String (1 .. 4);  --  data
      Subchunk2Size : Unsigned_32;
   end record;
--
--
--  Constants for recording
--  *****************************************************
   SAMPLE_RATE : constant := 48000;
   CHANNELS : constant := 1;
   BITS_PER_SAMPLE : constant := 16;   --  16 bit
--  *****************************************************
   BUFFER_SIZE : constant := 4096;     --  4K sample (8kByte)
   MAX_SAMPLES : constant := 4096;     --  Capture Samples (4kByte)
   MAX_DURATION : constant Time_Span := To_Time_Span (60.0); --  Limited Time
   LOOP_PERIOD : constant Time_Span := To_Time_Span (0.01);  --  10ms Cycle

   --  Variables
   Playback_Device_t : OpenAL.Context.Device_t;
   Capture_Device_t : OpenAL.Context.Device_t;
   PLB_Context_t : OpenAL.Context.Context_t;

   Available_Samples : Natural;
   Total_Samples : Unsigned_32 := 0;

   Header : WAV_Header;
   File_Handle : Ada.Streams.Stream_IO.File_Type;
   Stream : Ada.Streams.Stream_IO.Stream_Access;

   Start_Time : Ada.Real_Time.Time;
   Next_Loop_Time : Ada.Real_Time.Time;
   Last_Progress_Second : Integer := -1;  --  Track last progress display
--
--
--
------------------------------------------------
--  Function to check if Enter key was pressed (non-blocking)
------------------------------------------------
   function Key_Pressed return Boolean is
      Available : Boolean;
      C : Character;
   begin
      --  Check if input is available without blocking
      Get_Immediate (C, Available);
      if Available and then (C = CR or else C = LF) then
         return True;
      end if;
      return False;
   exception  --  Insurance against any errors
      when others =>
         return False;
   end Key_Pressed;
--
--
-----------------------------------------------------
--  All Capture Devices List
-----------------------------------------------------
   procedure List_Capture_Device_List is
   begin
--
      OpenAL_Info.List_Playback_Devices;
      Put_Line ("");
      OpenAL_Info.List_Capture_Devices;
      Put_Line ("");
      OpenAL_Info.Defaults;  --  WORKS
      Put_Line ("");

--  Contents of "Run"
--    Init;
--    List_Playback_Devices;
--    List_Capture_Devices;
--    Defaults;
--    Open_Device;
--    Versions;
--    Finish;
--
   end List_Capture_Device_List;
--
--
-----------------------------------------------------
--  All Capture Devices Test
-----------------------------------------------------
   procedure Test_All_Formats is
      type Unbounded_String_Array is
        array (Positive range <>) of Unbounded_String;
      type Format_Test is record
         Format : OpenAL.Context.Format_t;
         Name : String (1 .. 10);
      end record;
      Format_List : constant array (1 .. 2) of Format_Test := (
--         (Mono_8, "Mono_8    "),
         (Mono_16, "Mono_16   "),
--         (Stereo_8, "Stereo_8  "),
         (Stereo_16, "Stereo_16 ")
      );

      Sample_Rates :
         constant array (1 .. 4) of Frequency_t := (8000, 22050, 44100, 48000);

      Device_Name_List : constant Unbounded_String_Array (1 .. 7) :=
        (To_Unbounded_String (""),
         To_Unbounded_String ("default"),
         To_Unbounded_String ("sof-hda-dsp Digital Microphone"),
         To_Unbounded_String ("PCM2906C Audio CODEC Analog Stereo"),
         To_Unbounded_String ("sof-hda-dsp Headphones Stereo Microphone"),
         To_Unbounded_String ("Monitor of sof-hda-dsp Speaker + Headphones"),
         To_Unbounded_String ("Monitor of HDA NVidia Digital Stereo (HDMI)")
        );
--
   begin
      Put_Line ("                        8000Hz   22050Hz  44100Hz  48000Hz");
      for Device_Name of Device_Name_List loop
         Put_Line ("Testing Capture device: '" & Device_Name & "'");
         for Format_Info of Format_List loop
            Put ("  Testing " & Format_Info.Name & "    ");
            for Rate of Sample_Rates loop

               begin
                  Capture_Device_t := OpenAL.Context.Capture.Open_Device
                    (Name => To_String (Device_Name),   --  String
                     Frequency => Rate,                 --  Types.Frequency_t
                     Format => Format_Info.Format,      --  Request_Format_t
                     Buffer_Size => BUFFER_SIZE);       --  Buffer_Size_t

                  if Capture_Device_t /= OpenAL.Context.Invalid_Device then
                     Put ("SUCCESS  ");
               --  Simple capture test
                     OpenAL.Context.Capture.Start (Capture_Device_t);
                     delay 0.01;  --  wait 10ms
                     Available_Samples :=
                        OpenAL.Context.Get_Capture_Samples (Capture_Device_t);
--                     Put_Line ("    Available samples: " &
--                               Natural'Image (Available_Samples));
                  else
                     Put ("FAILED   ");
                  end if;
                  OpenAL.Context.Capture.Stop (Capture_Device_t);
                  OpenAL.Context.Capture.Close_Device (Capture_Device_t);
                  delay 0.01;  --  wait 10ms for dummy

               exception
                  when Constraint_Error =>
                     Put_Line ("Constraint_Error-likely index range problem");
                     Put_Line ("   Available samples: " &
                                 Natural'Image (Available_Samples));
                     OpenAL.Context.Capture.Stop (Capture_Device_t);
                     OpenAL.Context.Capture.Close_Device (Capture_Device_t);
                     raise;
                  when E : others =>
                     Put_Line ("Other exception:");
                     Put_Line (Exception_Name (E) & ": " &
                               Exception_Message (E));
                     OpenAL.Context.Capture.Stop (Capture_Device_t);
                     OpenAL.Context.Capture.Close_Device (Capture_Device_t);
                     raise;
               end;
            end loop;  --  Rate
            Put_Line ("");
         end loop;   --  Format_Info
      end loop;   --  for Device_Name of
      Put_Line ("");
   end Test_All_Formats;
--
--
--
-------------------------------------------------------
-------------------------------------------------------
--  MAIN
-------------------------------------------------------
-------------------------------------------------------
begin
--
   List_Capture_Device_List;
--
   Test_All_Formats;
--
--
   Put_Line ("Starting WAV recording...");
   Put_Line ("Press Enter to stop recording (max 60 seconds)");

   --  First open playback device (required by some OpenAL implementations)
   begin
      --  alcOpenDevice
      Playback_Device_t := OpenAL.Context.Open_Device ("");
      Put_Line ("Playback device opened just for dummy");
      --  alcCreateContext
      PLB_Context_t := Create_Context (Playback_Device_t);
      Put_Line ("Playback Context opened just for dummy");
   exception
      when others =>
         Put_Line ("Dummy Open Playback Device ERROR");
         OpenAL.Context.Destroy_Context (PLB_Context_t);
         OpenAL.Context.Close_Device (Playback_Device_t);
   end;
--
-------------------------
--  Open Capture Device
-------------------------
--  alcCaptureOpenDevice
   Capture_Device_t := OpenAL.Context.Capture.Open_Device
     (Name => "",   -- "sof-hda-dsp Digital Microphone"
      Frequency => Types.Frequency_t (SAMPLE_RATE),
      Format => Mono_16,
      Buffer_Size => BUFFER_SIZE);
--
   if OpenAL.Context.Is_Valid_Device (Capture_Device_t) then
      Put_Line ("Open Valid_Device GOOD");
   else
      Put_Line ("Open ERROR Capture_Device_t BAD");
   end if;
--
------------------------
--  Create WAV output file
------------------------
   Ada.Streams.Stream_IO.Create (File_Handle,
                                Ada.Streams.Stream_IO.Out_File,
                                "rec-1ch-48000-16b.wav");
   Stream := Ada.Streams.Stream_IO.Stream (File_Handle);
--
--  Initialize WAV header
   Header.ChunkID := "RIFF";  --  (+00)
   Header.ChunkSize := 0;     --  (+04) FileSize - 8 Will be updated later
   Header.Format := "WAVE";
   Header.Subchunk1ID := "fmt ";
   Header.Subchunk1Size := 16; --  (+10H) Linear PCM = 16
   Header.AudioFormat := 1;    --  (+14H) Linear PCM=1
   Header.NumChannels := CHANNELS;    --  (+16H) Mono=1, Stereo:2
   Header.SampleRate := SAMPLE_RATE;  --  (+18H)
   Header.ByteRate := SAMPLE_RATE * CHANNELS * (BITS_PER_SAMPLE / 8); --  (+1C)
   Header.BlockAlign := CHANNELS * (BITS_PER_SAMPLE / 8);  --  (+20H)
   Header.BitsPerSample := BITS_PER_SAMPLE;                --  (+22H)
   Header.Subchunk2ID := "data";          --  (+24)
   Header.Subchunk2Size := 0; --  (+28H) Number of data. Will be updated later

--  Write header
   WAV_Header'Write (Stream, Header);

---------------------------
--  Start recording
---------------------------
   OpenAL.Context.Capture.Start (Capture_Device_t);

   Next_Loop_Time := Clock + LOOP_PERIOD;
   delay until Next_Loop_Time;
   Start_Time := Clock;
   Put_Line ("Recording started...");

   --  LOOP Main recording ---------------------------------------
   Recording_Loop : loop
      Next_Loop_Time := Next_Loop_Time + LOOP_PERIOD;
      delay until Next_Loop_Time;

      --  Check for Enter key press
      if Key_Pressed then
         Put_Line ("Recording stopped by user");
         exit Recording_Loop;
      end if;

      --  Check for timeout (60 seconds)
      if Clock - Start_Time >= MAX_DURATION then
         Put_Line ("Recording stopped - reached maximum duration " &
                   "(60 seconds)");
         exit Recording_Loop;
      end if;
--
----------------------------
--  Check Sample
----------------------------
      --  alcGetIntegerv
      --   (Capture_Device, ALC_CAPTURE_SAMPLES, 1, Available_Samples'Address);
      Available_Samples :=
        OpenAL.Context.Get_Capture_Samples (Capture_Device_t);

      if Available_Samples > 0 then
         --  Valid Samples are available
         declare
            Samples_To_Read : constant Natural :=
               Natural'Min (Available_Samples, MAX_SAMPLES);  --  Lesser value
            Buffer_t : OpenAL.Buffer.Sample_Array_16_t
              (1 .. Sample_Size_t (Samples_To_Read));
            Capture_Flag : Boolean := True;  -- Initialize Before Capture
         begin
----------------------------
--  Get Sample and copy to Buffer
----------------------------
         --  alcCaptureSamples
         --    (Capture_Device, Buffer'Address, Samples_To_Read);
            OpenAL.Context.Capture.Samples_Mono_16
              (Device => Capture_Device_t,
               Samples => Buffer_t);  --  OpenAL.Buffer.Sample_Array_16_t

            Capture_Flag := False;  --  Set After Capture
--
----------------------------
--  Write Data to File
----------------------------
            Total_Samples :=
              Total_Samples + Buffer_t'Length;

            Sample_Array_16_t'Write (Stream, Buffer_t);

         exception
            when Constraint_Error =>
               Put_Line ("Constraint_Error - likely index range problem");
               if Capture_Flag then
                  Put_Line ("Capture Process Error");
               else
                  Put_Line ("Writing File Process Error");
               end if;
               return;
            when E : others =>
               Ada.Text_IO.Put_Line ("ERROR in Samples_Mono_16: " &
                                       Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line ("Exception Name: " &
                                       Ada.Exceptions.Exception_Name (E));
               Put_Line ("Samples_To_Read=" & Natural'Image (Samples_To_Read));
               OpenAL.Context.Capture.Stop (Capture_Device_t);
               OpenAL.Context.Capture.Close_Device (Capture_Device_t);
               if Capture_Flag then
                  Put_Line ("Capture Process Error");
               else
                  Put_Line ("Writing File Process Error");
               end if;
               return;
         end;

      end if;  --  end of Available_Samples

      --  Show progress every 5 seconds
      declare
         Current_Second : constant Integer :=
           Integer (To_Duration (Clock - Start_Time));
      begin
         if Current_Second > 0 and then
            Current_Second mod 5 = 0 and then
            Current_Second /= Last_Progress_Second and then
            Total_Samples > 0
         then
            Put_Line ("Recording..." & Integer'Image (Current_Second) &
                      " sec, Total:" & Unsigned_32'Image (Total_Samples) &
                      " Samples " & Unsigned_32'Image (Total_Samples * 2) &
                      " byte");
            Last_Progress_Second := Current_Second;
         end if;
      end;

      --  Small delay to prevent busy waiting
   end loop Recording_Loop;  --  --------------------------------
--
--
---------------------------
--  Update the WAV file info and CLOSE
--------------------------
   --  Update WAV headers with the corrected sizes
   Header.Subchunk2Size := Total_Samples * (BITS_PER_SAMPLE / 8); -- Pos41
   Header.ChunkSize := 36 + Header.Subchunk2Size;                 --  Pos5
--   Rewrite header with correct sizes
   Ada.Streams.Stream_IO.Set_Index (File_Handle, 1);
   WAV_Header'Write (Stream, Header);
   --  Close file
   Ada.Streams.Stream_IO.Close (File_Handle);
--
   Put_Line ("Total Samples=" &
             Unsigned_32'Image (Total_Samples) &
             " (" & Unsigned_32'Image (Total_Samples * 2) &
             " Byte  +44:FileSize=" &
             Unsigned_32'Image (Total_Samples * 2 + 44) &
             " Byte");
   Put_Line ("File Closed");
--
------------------------------
--  Clean up devices
------------------------------
   OpenAL.Context.Capture.Close_Device (Capture_Device_t);
   Destroy_Context (PLB_Context_t);
   OpenAL.Context.Capture.Close_Device (Playback_Device_t);
--
end Record_WAV;
