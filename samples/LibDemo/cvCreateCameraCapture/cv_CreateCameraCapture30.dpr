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

program cv_CreateCameraCapture30;

{$APPTYPE CONSOLE}
{$R *.res}
{$I OpenCV.inc}

uses
  System.SysUtils,
  System.Math,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  {$IFDEF DelphiOCVVersion_30}
  ocv.classes,
  {$ENDIF }
  uResourcePaths,
  ocv.editor;


var
{$IFDEF DelphiOCVVersion_30}
  M: IMat;
  V: IVideoCapture;
  i: TIplImage;
{$ELSE}
  capture: PCvCapture = nil;
{$ENDIF}
  width: double;
  height: double;
  frame, capframe: pIplImage;
  counter: Integer;
  filename: pCVChar;
  op: Integer = 8;
  opvalue: Integer = -10;

begin
  try
{$IFDEF DelphiOCVVersion_30}
    // получаем любую подключённую камеру
    V := TVideoCapture.Create;
    if not V.Open(CAP_ANY) then
      Halt;
    // узнаем ширину и выcоту кадра
    width := V.Prop[CAP_PROP_FRAME_WIDTH];
    height := V.Prop[CAP_PROP_FRAME_HEIGHT];
{$ELSE}
    // получаем любую подключённую камеру
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Assigned(capture) then
      Halt;
    // узнаем ширину и выcоту кадра
    width := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH);
    height := cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT);
{$ENDIF}
    WriteLn(Format('[i] %.0f x %.0f', [width, height]));
    cvNamedWindow('capture', CV_WINDOW_AUTOSIZE);
    WriteLn('[i] press Enter for capture image');
    WriteLn('[i] press Space for captured image info');
    WriteLn('[i] press Esc for quit!');
    counter := 0;

    while true do
    begin
      // получаем кадр
{$IFDEF DelphiOCVVersion_30}
      if not V.read(M) then
        Continue;
      i.InitFromMat(M);
      frame := @i;
{$ELSE}
      frame := cvQueryFrame(capture);
{$ENDIF}
      if Assigned(frame) then
      begin
        if not Assigned(capframe) then
          capframe := cvCreateImage(cvGetSize(frame), frame^.depth, frame^.nChannels);
        case op of
          0:
            cvCopy(frame, capframe);
          1:
            Sature(opvalue, frame, capframe);
          2:
            Expo(opvalue, frame, capframe);
          3:
            Hue(opvalue, frame, capframe);
          4:
            Temperature(opvalue, frame, capframe);
          5:
            White(opvalue, frame, capframe);
          6:
            Shadow(opvalue, frame, capframe);
          7:
            Contrast(opvalue, frame, capframe);
          8:
            Clarity(opvalue, frame, capframe);
        else
          cvCopy(frame, capframe);
        end;
      end;
      // показываем
      if Assigned(capframe) then
        cvShowImage('capture', capframe);
      case cvWaitKey(33) of
        13:
          if Assigned(capframe) then
          begin
            // cохраняем кадр в файл
            filename := pCVChar(AnsiString(cResourceResult + Format('Image %d.jpg'#0, [counter])));
            WriteLn('[i] capture - ', filename);
            cvSaveImage(filename, capframe);
            Inc(counter);
          end;
        $20:
          begin
            WriteLn('-------- Camera info --------');
{$IFDEF DelphiOCVVersion_30}
            WriteLn('FRAME_WIDTH:               ', Trunc(V.Prop[CAP_PROP_FRAME_WIDTH]));
            WriteLn('FRAME_HEIGHT:              ', Trunc(V.Prop[CAP_PROP_FRAME_HEIGHT]));
            WriteLn('FPS:                       ', Trunc(V.Prop[CAP_PROP_FPS]));
            WriteLn('FRAME_COUNT:               ', Trunc(V.Prop[CAP_PROP_FRAME_COUNT]));
            WriteLn('PROP_FORMAT:               ', Trunc(V.Prop[CAP_PROP_FORMAT]));
            WriteLn('PROP_MODE:                 ', Trunc(V.Prop[CAP_PROP_MODE]));
            WriteLn('PROP_BRIGHTNESS:           ', Trunc(V.Prop[CAP_PROP_BRIGHTNESS]));
            WriteLn('PROP_CONTRAST:             ', Trunc(V.Prop[CAP_PROP_CONTRAST]));
            WriteLn('PROP_SATURATION:           ', Trunc(V.Prop[CAP_PROP_SATURATION]));
            WriteLn('PROP_HUE:                  ', Trunc(V.Prop[CAP_PROP_HUE]));
            WriteLn('PROP_GAIN:                 ', Trunc(V.Prop[CAP_PROP_GAIN]));
            WriteLn('PROP_EXPOSURE:             ', Trunc(V.Prop[CAP_PROP_EXPOSURE]));
            WriteLn('PROP_CONVERT_RGB:          ', Trunc(V.Prop[CAP_PROP_CONVERT_RGB]));
            WriteLn('PROP_WHITE_BALANCE_BLUE_U: ', Trunc(V.Prop[CAP_PROP_WHITE_BALANCE_BLUE_U]));
            WriteLn('PROP_RECTIFICATION:        ', Trunc(V.Prop[CAP_PROP_RECTIFICATION]));
            WriteLn('PROP_SHARPNESS:            ', Trunc(V.Prop[CAP_PROP_SHARPNESS]));
            WriteLn('PROP_AUTO_EXPOSURE:        ', Trunc(V.Prop[CAP_PROP_AUTO_EXPOSURE]));
            WriteLn('PROP_GAMMA:                ', Trunc(V.Prop[CAP_PROP_GAMMA]));
            WriteLn('PROP_TEMPERATURE:          ', Trunc(V.Prop[CAP_PROP_TEMPERATURE]));
            WriteLn('PROP_TRIGGER:              ', Trunc(V.Prop[CAP_PROP_TRIGGER]));
            WriteLn('PROP_TRIGGER_DELAY:        ', Trunc(V.Prop[CAP_PROP_TRIGGER_DELAY]));
            WriteLn('PROP_WHITE_BALANCE_RED_V:  ', Trunc(V.Prop[CAP_PROP_WHITE_BALANCE_RED_V]));
            WriteLn('PROP_ZOOM:                 ', Trunc(V.Prop[CAP_PROP_ZOOM]));
            WriteLn('PROP_FOCUS:                ', Trunc(V.Prop[CAP_PROP_FOCUS]));
            WriteLn('PROP_GUID:                 ', Trunc(V.Prop[CAP_PROP_GUID]));
            WriteLn('PROP_ISO_SPEED:            ', Trunc(V.Prop[CAP_PROP_ISO_SPEED]));
            WriteLn('PROP_BACKLIGHT:            ', Trunc(V.Prop[CAP_PROP_BACKLIGHT]));
            WriteLn('PROP_PAN:                  ', Trunc(V.Prop[CAP_PROP_PAN]));
            WriteLn('PROP_TILT:                 ', Trunc(V.Prop[CAP_PROP_TILT]));
            WriteLn('PROP_ROLL:                 ', Trunc(V.Prop[CAP_PROP_ROLL]));
            WriteLn('PROP_IRIS:                 ', Trunc(V.Prop[CAP_PROP_IRIS]));
            WriteLn('PROP_SETTINGS:             ', Trunc(V.Prop[CAP_PROP_SETTINGS]));
{$ELSE}
            WriteLn('FRAME_WIDTH:               ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH)));
            WriteLn('FRAME_HEIGHT:              ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT)));
            WriteLn('FPS:                       ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FPS)));
            WriteLn('FRAME_COUNT:               ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_COUNT)));
            WriteLn('PROP_FORMAT:               ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FORMAT)));
            WriteLn('PROP_MODE:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_MODE)));
            WriteLn('PROP_BRIGHTNESS:           ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_BRIGHTNESS)));
            WriteLn('PROP_CONTRAST:             ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_CONTRAST)));
            WriteLn('PROP_SATURATION:           ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_SATURATION)));
            WriteLn('PROP_HUE:                  ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_HUE)));
            WriteLn('PROP_GAIN:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_GAIN)));
            WriteLn('PROP_EXPOSURE:             ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_EXPOSURE)));
            WriteLn('PROP_CONVERT_RGB:          ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_CONVERT_RGB)));
            WriteLn('PROP_WHITE_BALANCE_BLUE_U: ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_WHITE_BALANCE_BLUE_U)));
            WriteLn('PROP_RECTIFICATION:        ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_RECTIFICATION)));
            WriteLn('PROP_MONOCROME:            ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_MONOCROME)));
            WriteLn('PROP_SHARPNESS:            ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_SHARPNESS)));
            WriteLn('PROP_AUTO_EXPOSURE:        ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_AUTO_EXPOSURE)));
            WriteLn('PROP_GAMMA:                ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_GAMMA)));
            WriteLn('PROP_TEMPERATURE:          ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_TEMPERATURE)));
            WriteLn('PROP_TRIGGER:              ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_TRIGGER)));
            WriteLn('PROP_TRIGGER_DELAY:        ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_TRIGGER_DELAY)));
            WriteLn('PROP_WHITE_BALANCE_RED_V:  ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_WHITE_BALANCE_RED_V)));
            WriteLn('PROP_ZOOM:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_ZOOM)));
            WriteLn('PROP_FOCUS:                ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_FOCUS)));
            WriteLn('PROP_GUID:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_GUID)));
            WriteLn('PROP_ISO_SPEED:            ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_ISO_SPEED)));
            WriteLn('PROP_MAX_DC1394:           ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_MAX_DC1394)));
            WriteLn('PROP_BACKLIGHT:            ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_BACKLIGHT)));
            WriteLn('PROP_PAN:                  ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_PAN)));
            WriteLn('PROP_TILT:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_TILT)));
            WriteLn('PROP_ROLL:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_ROLL)));
            WriteLn('PROP_IRIS:                 ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_IRIS)));
            WriteLn('PROP_SETTINGS:             ', Trunc(cvGetCaptureProperty(capture, CV_CAP_PROP_SETTINGS)));
{$ENDIF}
          end;
        27:
          Break;
      end;
    end;
    // оcвобождаем реcурcы
    if Assigned(capframe) then
      cvReleaseImage(capframe);
{$IFDEF DelphiOCVVersion_30}
{$ELSE}
    cvReleaseCapture(capture);
{$ENDIF}
    cvDestroyWindow('capture');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
