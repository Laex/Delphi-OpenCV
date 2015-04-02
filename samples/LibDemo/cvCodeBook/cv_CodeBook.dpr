(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)

program cv_CodeBook;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.legacy;

const
  NCHANNELS: Integer = 3;

var
  ch: array [0 .. 2] of boolean = (
    true,
    true,
    true
  );
  // VARIABLES for CODEBOOK METHOD:
  model: pCvBGCodeBookModel;
  rawImage, yuvImage: pIplImage;
  ImaskCodeBook, ImaskCodeBookCC: pIplImage;
  capture: pCvCapture;
  c, n, v, nframes, nframesToLearnBG: Integer;
  pause: boolean;
  size: TCvSize;
  scalar: TCvScalar;
  ptr, ptr1: pByte;
  hdl: THandle;
  tmp: AnsiString;
  singlestep: boolean = False;

procedure help;
begin
  Writeln('h - Help');
  Writeln('p - Pause (', singlestep, ')');
  Writeln('s - Single step (', singlestep, ')');
  Writeln('r - resume (set Pause=false, SingleStep=False');
  Writeln('SPACE - clear');
  Writeln('y0u1v2a3b - set CODEBOOK params');
  Writeln('iokl - modify max classification bounds (max bound goes higher)');
  Writeln('q or ESC - quit');
  Writeln('---------------------------------------');
end;

begin
  try
    nframes := 0;
    nframesToLearnBG := 300;
    pause := False;
    model := cvCreateBGCodeBookModel;
    model^.modMin[0] := 3;
    model^.modMin[1] := 3;
    model^.modMin[2] := 3;
    model^.modMax[0] := 10;
    model^.modMax[1] := 10;
    model^.modMax[2] := 10;
    model^.cbBounds[0] := 10;
    model^.cbBounds[1] := 10;
    model^.cbBounds[2] := 10;

    pause := False;
    singlestep := False;
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    Assert(Assigned(capture));

    help;

    // MAIN PROCESSING LOOP:
    while (true) do
    begin
      if (Not pause) then
      begin
        rawImage := cvQueryFrame(capture);
        Inc(nframes);
        if (rawImage = NIL) then
          break;
      end;
      if (singlestep) then
        pause := true; // ????

      // First time:
      if (nframes = 1) and (rawImage <> NIL) then
      begin
        // CODEBOOK METHOD ALLOCATION
        yuvImage := cvCloneImage(rawImage);
        size := cvGetSize(rawImage);
        ImaskCodeBook := cvCreateImage(size, IPL_DEPTH_8U, 1);
        // cvSaveImage('book.png', ImaskCodeBook);
        ImaskCodeBookCC := cvCreateImage(size, IPL_DEPTH_8U, 1);
        // cvSaveImage('cc.png', ImaskCodeBookCC);
        scalar := cvScalar(255);
        cvSet(ImaskCodeBook, scalar);

        cvNamedWindow('Raw', 1);
        cvNamedWindow('ForegroundCodeBook', 1);
        cvNamedWindow('CodeBook_ConnectComp', 1);
        cvMoveWindow('ForegroundCodeBook', 450, 50);
      end;
      // If we've got an rawImage and are good to go:
      if (rawImage <> NIL) then
      begin
        // YUV For codebook method
        cvCvtColor(rawImage, yuvImage, CV_BGR2YCrCb);
        // This is where we build our background model
        // tmp := format('yuv%d.png', [nframes]);
        if ((Not pause) and ((nframes - 1) < nframesToLearnBG)) then
          cvBGCodeBookUpdate(model, yuvImage, cvZeroRect, nil);
        // cvSaveImage( @tmp[1], yuvImage, 0 );
        if ((nframes - 1) = nframesToLearnBG) then
          cvBGCodeBookClearStale(model, model^.t div 2, cvZeroRect, nil);

        // Find the foreground if any
        if (nframes - 1 >= nframesToLearnBG) then
        begin
          // Find foreground by codebook method
          cvBGCodeBookDiff(model, yuvImage, ImaskCodeBook, cvZeroRect);
          // This part just to visualize bounding boxes and centers if desired
          cvCopy(ImaskCodeBook, ImaskCodeBookCC, NIL);
          cvSegmentFGMask(ImaskCodeBookCC, 0, 0, NIL, cvZeroPoint);
        end;
        // Display
        cvShowImage('Raw', rawImage);
        cvShowImage('ForegroundCodeBook', ImaskCodeBook);
        cvShowImage('CodeBook_ConnectComp', ImaskCodeBookCC);
        // tmp := '1.png';
        // if nframes = 1 then
        // cvSaveImage( @tmp[1], ImaskCodeBookCC, 0 );
      end;

      // User input:
      c := cvWaitKey(10);
      // End processing on ESC, q or Q
      if ((c = 27) or (c = Ord('q'))) then
        break;
      // Else check for user input
      case chr(c) of
        'h':
          help;
        'p':
          pause := Not pause;
        's':
          begin
            singlestep := Not singlestep;
            pause := False;
          end;
        'r':
          begin
            pause := False;
            singlestep := False;
          end;
        ' ':
          begin
            cvBGCodeBookClearStale(model, 0, cvZeroRect);
            nframes := 0;
          end;
        // CODEBOOK PARAMS
        'y', '0', 'u', '1', 'v', '2', 'a', '3', 'b':
          begin
            ch[0] := AnsiChar(c) in ['y', '0', 'a', '3'];
            ch[1] := AnsiChar(c) in ['u', '1', 'a', '3', 'b'];
            ch[2] := AnsiChar(c) in ['v', '2', 'a', '3', 'b'];
            Writeln(Format('CodeBook YUV Channels active: %s, %s, %s', [BoolToStr(ch[0], true), BoolToStr(ch[1], true),
              BoolToStr(ch[2], true)]));
          end;
        'i', 'o', 'k', 'l': // modify max classification bounds (max bound goes higher)
          begin
            if (AnsiChar(c) in ['i', 'o']) then
              ptr := @model^.modMax[0]
            else
              ptr := @model^.modMin[0];
            for n := 0 to NCHANNELS - 1 do
            begin
              if (ch[n]) then
              begin
                ptr1 := ptr;
                Inc(ptr1, n);
                if (AnsiChar(c) = 'i') or (AnsiChar(c) = 'l') then
                begin
                  v := ptr1^ + 1
                end
                else
                  v := ptr1^ - 1;
                ptr1^ := CV_CAST_8U(v);
              end;
              Write(Format('%d,', [ptr[n]]));
            end;
            Writeln(Format('CodeBook %s Side', [iif((c = Ord('i')) or (c = Ord('o')), 'High', 'Low')]));
          end;
      end
    end;
    cvReleaseCapture(capture);
    cvDestroyAllWindows;
  except
    on e: Exception do
      Writeln(e.ClassName, ': ', e.Message);
  end;

end.
