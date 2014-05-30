// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************
// Original file:
// opencv\samples\c\adaptiveskindetector.cpp
// ***************************************************************

program cv_AdaptiveSkinDetector;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  WinApi.Windows,
  ocv.lib,
  ocv.highgui_c,
  ocv.core_c,
  ocv.Core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.contrib;

const
  CLOCKS_PER_SEC = 1000;

procedure help;
begin
  Writeln('This program demonstrates the contributed flesh detector CvAdaptiveSkinDetector which can be found in contrib.cpp');
  Writeln('Usage: ');
  Writeln(ExtractFileName(ParamStr(0)), ' fileMask firstFrame lastFrame');
  Writeln('Example:');
  Writeln(ExtractFileName(ParamStr(0)), ' C:\VideoSequences\sample1\right_view\temp_%05d.jpg  0  1000');
  Writeln('   iterates through temp_00000.jpg  to  temp_01000.jpg');
  Writeln('If no parameter specified, this application will try to capture from the default Webcam.');
  Writeln('Please note: Background should not contain large surfaces with skin tone.');
  Writeln(' ESC will stop');
  Writeln('Using OpenCV version ', CV_VERSION);
end;

Type
  TASDFrameHolder = class
  private
    image: pIplImage;
    timeStamp: double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure assignFrame(sourceImage: pIplImage; frameTime: double); virtual;
    function getImage: pIplImage;
    function getTimeStamp: double;
    procedure setImage(sourceImage: pIplImage); virtual;
  end;

  TASDFrameSequencer = class
  public
    destructor Destroy; override;
    function getNextImage: pIplImage; virtual;
    procedure close; virtual;
    function isOpen: bool; virtual;
    procedure getFrameCaption(var caption: AnsiString); virtual;
  end;

  TASDCVFrameSequencer = class(TASDFrameSequencer)
  protected
    capture: pCvCapture;
  public
    function getNextImage: pIplImage; override;
    procedure close; override;
    function isOpen: bool; override;
  end;

  TASDFrameSequencerWebCam = class(TASDCVFrameSequencer)
  public
    function open(cameraIndex: Integer): boolean; virtual;
    procedure getFrameCaption(var caption: AnsiString); override;
  end;

  TASDFrameSequencerVideoFile = class(TASDCVFrameSequencer)
  public
    function open(const fileName: pCvChar): bool; virtual;
  end;

  TASDFrameSequencerImageFile = class(TASDFrameSequencer)
  private
    sFileNameMask: AnsiString;
    nCurrentIndex, nStartIndex, nEndIndex: Integer;
  public
    procedure open(const fileNameMask: AnsiString; startIndex, endIndex: Integer); virtual;
    procedure getFrameCaption(var caption: AnsiString); override;
    function getNextImage: pIplImage; override;
    procedure close; override;
    function isOpen: bool; override;
  end;

  {TASDFrameHolder}

procedure TASDFrameHolder.assignFrame(sourceImage: pIplImage; frameTime: double);
begin
  if Assigned(image) then
  begin
    cvReleaseImage(image);
    image := nil;
  end;
  image := cvCloneImage(sourceImage);
  timeStamp := frameTime;
end;

constructor TASDFrameHolder.Create;
begin
  image := nil;
  timeStamp := 0;
end;

destructor TASDFrameHolder.Destroy;
begin
  cvReleaseImage(image);
  inherited;
end;

function TASDFrameHolder.getImage: pIplImage;
begin
  Result := image;
end;

function TASDFrameHolder.getTimeStamp: double;
begin
  Result := timeStamp;
end;

procedure TASDFrameHolder.setImage(sourceImage: pIplImage);
begin
  image := sourceImage;
end;

{TASDFrameSequencer}

procedure TASDFrameSequencer.close;
begin

end;

destructor TASDFrameSequencer.Destroy;
begin
  close;
  inherited;
end;

procedure TASDFrameSequencer.getFrameCaption(var caption: AnsiString);
begin

end;

function TASDFrameSequencer.getNextImage: pIplImage;
begin
  Result := nil;
end;

function TASDFrameSequencer.isOpen: bool;
begin
  Result := False;
end;

{TASDFrameSequencerImageFile}

procedure TASDFrameSequencerImageFile.close;
begin
  nCurrentIndex := nEndIndex + 1;
end;

procedure TASDFrameSequencerImageFile.getFrameCaption(var caption: AnsiString);
begin
  caption := Format(sFileNameMask, [nCurrentIndex]);
end;

function TASDFrameSequencerImageFile.getNextImage: pIplImage;
Var
  fileName: AnsiString;
begin
  Inc(nCurrentIndex);
  if (nCurrentIndex > nEndIndex) then
    Exit(nil);
  fileName := Format(sFileNameMask, [nCurrentIndex]);
  Result := cvLoadImage(pCvChar(@fileName[1]));
end;

function TASDFrameSequencerImageFile.isOpen: bool;
begin
  Result := (nCurrentIndex <= nEndIndex);
end;

procedure TASDFrameSequencerImageFile.open(const fileNameMask: AnsiString; startIndex, endIndex: Integer);
begin
  nCurrentIndex := startIndex - 1;
  nStartIndex := startIndex;
  nEndIndex := endIndex;
  sFileNameMask := Format('%s', [fileNameMask]);
end;

{TASDFrameSequencerVideoFile}

function TASDFrameSequencerVideoFile.open(const fileName: pCvChar): bool;
begin
  close();
  capture := cvCreateFileCapture(fileName);
  Result := Assigned(capture);
end;

{TASDFrameSequencerWebCam}

procedure TASDFrameSequencerWebCam.getFrameCaption(var caption: AnsiString);
begin
  caption := 'Web cam';
end;

function TASDFrameSequencerWebCam.open(cameraIndex: Integer): boolean;
begin
  close();
  capture := cvCreateCameraCapture(cameraIndex);
  Result := Assigned(capture);
end;

{TASDCVFrameSequencer}

procedure TASDCVFrameSequencer.close;
begin
  inherited;
  if (capture <> nil) then
    cvReleaseCapture(capture);
end;

function TASDCVFrameSequencer.getNextImage: pIplImage;
Var
  image: pIplImage;
begin
  image := cvQueryFrame(capture);
  if (image <> nil) then
    Exit(cvCloneImage(image))
  else
    Exit(nil);
end;

function TASDCVFrameSequencer.isOpen: bool;
begin
  Result := (capture <> nil);
end;

// ---------------------------------------------------------------------
procedure putTextWithShadow(img: pIplImage; const str: pCvChar; point: TCvPoint; font: pCvFont;
  color: TCvScalar {= CV_RGB(255, 255, 128)} );
begin
  cvPutText(img, str, cvPoint(point.x - 1, point.y - 1), font, CV_RGB(0, 0, 0));
  cvPutText(img, str, point, font, color);
end;

procedure ASD_RGB_SET_PIXEL(ptr: pByte; r, g, b: uchar); inline;
begin
  // (*pointer) = (unsigned char)b;
  ptr[0] := b;
  // (*(pointer+1)) = (unsigned char)g;
  ptr[1] := g;
  // (*(pointer+2)) = (unsigned char)r;
  ptr[2] := r;
end;

procedure ASD_RGB_GET_PIXEL(ptr: pByte; Var r, g, b: uchar); inline;
begin
  // b = (unsigned char)(*(pointer));
  b := ptr[0];
  // g = (unsigned char)(*(pointer+1));
  g := ptr[1];
  // r = (unsigned char)(*(pointer+2));
  r := ptr[2];
end;

procedure displayBuffer(rgbDestImage: pIplImage; buffer: pIplImage; rValue, gValue, bValue: Integer);
Var
  x, y, nWidth, nHeight: Integer;
  destX, destY, dx, dy: double;
  c: uchar;
  pSrc: pByte;
begin
  nWidth := buffer^.width;
  nHeight := buffer^.height;

  dx := rgbDestImage^.width / nWidth;
  dy := rgbDestImage^.height / nHeight;

  destX := 0;
  for x := 0 to nWidth - 1 do
  begin
    destY := 0;
    for y := 0 to nHeight - 1 do
    begin
      c := (buffer^.imageData + buffer^.widthStep * y)[x];

      if (c <> 0) then
      begin
        pSrc := rgbDestImage^.imageData + rgbDestImage^.widthStep * Trunc(destY) + (Trunc(destX) * rgbDestImage^.nChannels);
        ASD_RGB_SET_PIXEL(pSrc, rValue, gValue, bValue);
      end;
      destY := destY + dy;
    end;
    destY := 0;
    destX := destX + dx;
  end;
end;

Var
  caption, s, windowName: AnsiString;
  img: pIplImage;
  filterMask: pIplImage = nil;
  filter: TCvAdaptiveSkinDetector;
  sequencer: TASDFrameSequencer;
  base_font: TCvFont;
  clockTotal, numFrames: LongInt;
  clock: Cardinal;

begin
  try
    help;
    filter := TCvAdaptiveSkinDetector.Create(1, TCvAdaptiveSkinDetector.MORPHING_METHOD_ERODE_DILATE);
    clockTotal := 0;
    numFrames := 0;

    if (ParamCount < 3) then
    begin
      help;
      sequencer := TASDFrameSequencerWebCam.Create;
      (sequencer as TASDFrameSequencerWebCam).open(-1);
      if (not sequencer.isOpen()) then
      begin
        Writeln('Error: Cannot initialize the default Webcam');
      end;
    end
    else
    begin
      sequencer := TASDFrameSequencerImageFile.Create;
      // A sequence of images captured from video source, is stored here
      (sequencer as TASDFrameSequencerImageFile).open(ParamStr(1), StrToInt(ParamStr(2)), StrToInt(ParamStr(3)));
    end;

    windowName := 'Adaptive Skin Detection Algorithm for Video Sequences';

    cvNamedWindow(pCvChar(@windowName[1]), CV_WINDOW_AUTOSIZE);
    cvInitFont(@base_font, CV_FONT_VECTOR0, 0.5, 0.5);

    // Usage:
    // c:\>CvASDSample 'C:\VideoSequences\sample1\right_view\temp_%05d.jpg' 0 1000

    Writeln('Press ESC to stop.');
    img := sequencer.getNextImage();
    while Assigned(img) do
    begin
      Inc(numFrames);

      if not Assigned(filterMask) then
        filterMask := cvCreateImage(cvSize(img^.width, img^.height), IPL_DEPTH_8U, 1);
      clock := GetTickCount;
      filter.process(img, filterMask); // DETECT SKIN
      clockTotal := clockTotal + (GetTickCount - clock);

      displayBuffer(img, filterMask, 0, 255, 0);

      sequencer.getFrameCaption(caption);
      s := Format('%s - %d x %d', [caption, img^.width, img^.height]);
      putTextWithShadow(img, pCvChar(@s[1]), cvPoint(10, img^.height - 35), @base_font, CV_RGB(0, 255, 128));

      s := Format('Average processing time per frame: %5.2fms', [((clockTotal * 1000) / CLOCKS_PER_SEC) / numFrames]);
      putTextWithShadow(img, pCvChar(@s[1]), cvPoint(10, img^.height - 15), @base_font, CV_RGB(255, 255, 128));

      cvShowImage(pCvChar(@windowName[1]), img);
      cvReleaseImage(img);

      if (cvWaitKey(1) = 27) then
        break;
      img := sequencer.getNextImage();
    end;

    sequencer.close();
    sequencer.Free;
    filter.Free;

    cvReleaseImage(filterMask);

    cvDestroyAllWindows;

    Writeln('Finished, ', numFrames, ' frames processed.');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
