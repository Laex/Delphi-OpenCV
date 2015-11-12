program art;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Math,
  ocv.utils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  uResourcePaths;

const
  SELECTED       = 255;
  ABORT          = 100;
  UNCHECKED      = 0;
  CLOCKS_PER_SEC = 1000;

  filename = cResourceMedia + 'cat2.jpg';

Var
  threshhold: Integer = 10;
  patch_radius: Integer;

  input: PIplImage = nil;
  mask: PIplImage = nil;
  res: PIplImage = nil;

  width: Integer;
  height: Integer;
  widthstep: Integer;
  gwidthstep: Integer;
  nch: Integer;

  center_pos: TCvPoint;

  _begin, _end: Int64;
  time_spent: Double;

Type
  pColor = ^TColor;

  TColor = record
    r: Integer;
    g: Integer;
    b: Integer;
  end;

Var
  p: TArray<TCvPoint>;

function colordiff(cur: pColor; center: pColor): Integer;
Var
  diff: Double;
begin
  diff := (cur^.b - center^.b) * (cur^.b - center^.b) + (cur^.g - center^.g) * (cur^.g - center^.g) + (cur^.r - center^.r) *
    (cur^.r - center^.r);

  diff := sqrt(diff);

  if (diff <= threshhold) then
    Result := 1
  else
    Result := 0;
end;

procedure point_grow(i: Integer; j: Integer; center: TColor; radius: Integer);
Var
  cur: TColor;
  st_row, ed_row: Integer;
  st_col, ed_col: Integer;
  diff_flag: Integer;
  cur_pos: TCvPoint;
  m, n: Integer;
  mask_value: Integer;
begin
  st_row := i - radius;
  ed_row := i + radius;
  st_col := j - radius;
  ed_col := j + radius;

  if st_row <= (center_pos.y - patch_radius) then
    st_row := center_pos.y - patch_radius;
  if ed_row > (center_pos.y + patch_radius) then
    ed_row := center_pos.y + patch_radius;
  if st_col <= (center_pos.x - patch_radius) then
    st_col := center_pos.x - patch_radius;
  if ed_col > (center_pos.x + patch_radius) then
    ed_col := center_pos.x + patch_radius;

  if (st_row <= 0) then
    st_row := 1;
  if (ed_row > height) then
    ed_row := height;
  if (st_col <= 0) then
    st_col := 1;
  if (ed_col > width) then
    ed_col := width;

  for m := st_row to ed_row do
  begin
    for n := st_col to ed_col do
    begin
      if not((m = i) and (n = j)) then
      begin
        mask_value := mask^.imageData[(m - 1) * gwidthstep + (n - 1)];
        if ((mask_value <> ABORT) and (mask_value <> SELECTED)) then
        begin
          cur.b := input^.imageData[(m - 1) * widthstep + (n - 1) * nch];
          cur.g := input^.imageData[(m - 1) * widthstep + (n - 1) * nch + 1];
          cur.r := input^.imageData[(m - 1) * widthstep + (n - 1) * nch + 2];

          diff_flag := colordiff(@cur, @center);

          if (diff_flag = 1) then
          begin
            mask^.imageData[(m - 1) * gwidthstep + (n - 1)] := SELECTED;
            cur_pos.x := j;
            cur_pos.y := i;
            SetLength(p, Length(p) + 1);
            p[High(p)] := cur_pos;
          end
          else
          begin
            mask^.imageData[(m - 1) * gwidthstep + (n - 1)] := ABORT;
          end; // End if diff_flag
        end;
      end;
    end;
  end; // End for loop m, n
end;

procedure rgrow(res: PIplImage; input: PIplImage; mask: PIplImage; threshhold: Integer; patch_radius: Integer);
Var
  radius: Integer;
  center: TColor;
  i, j: Integer;
  cur_pos: TCvPoint;
begin
  radius := 1;
  for i := 1 to height do
  begin
    for j := 1 to width do
    begin
      center.b := input^.imageData[(i - 1) * widthstep + (j - 1) * nch];
      center.g := input^.imageData[(i - 1) * widthstep + (j - 1) * nch + 1];
      center.r := input^.imageData[(i - 1) * widthstep + (j - 1) * nch + 2];

      // CvPoint center_pos;
      center_pos.x := j;
      center_pos.y := i;

      SetLength(p, Length(p) + 1);
      p[High(p)] := center_pos;

      while (Length(p) > 0) do
      begin
        cur_pos.x := p[0].x;
        cur_pos.y := p[0].y;
        Delete(p, 0, 1);
        point_grow(cur_pos.y, cur_pos.x, center, radius);
      end;
    end;
  end; // End for loop i, j
end;

begin
  try

    if ParamCount = 0 then
      input := cvLoadImage(filename)
    else
      input := cvLoadImage(c_str(ParamStr(1)));

    width := input^.width;
    height := input^.height;
    widthstep := input^.widthstep;
    nch := input^.nChannels;

    cvNamedWindow('Input');
    cvShowImage('Input', input);
    cvWaitKey();

    cvNamedWindow('Mask');
    cvResizeWindow('Mask', width, height);

    mask := cvCreateImage(cvSize(width, height), IPL_DEPTH_8U, 1);
    cvZero(mask);
    gwidthstep := mask^.widthstep;

    res := cvCreateImage(cvSize(width, height), IPL_DEPTH_8U, 3);

    patch_radius := Trunc(MIN(width, height) * 0.02);

    _begin := cvGetTickCount();
    rgrow(res, input, mask, threshhold, patch_radius);
    _end := cvGetTickCount();
    time_spent := (_end - _begin) / CLOCKS_PER_SEC;
    WriteLn('Region Grow ', time_spent:3:5);

    cvNamedWindow('Mask');
    cvShowImage('Mask', mask);

    cvWaitKey();
    cvDestroyAllWindows();
    cvReleaseImage(input);
    cvReleaseImage(mask);
    cvReleaseImage(res);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
