unit frmCamshiftdemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls, ComCtrls, ExtCtrls, Math,
  IPL, OpenCV, frmHistogram, jpeg;

type
  TfCamshiftdemo = class(TForm)
    Timer1: TTimer;
    Panel2: TPanel;
    Label1: TLabel;
    tbVmin: TTrackBar;
    Label2: TLabel;
    tbVmax: TTrackBar;
    Label3: TLabel;
    tbSmin: TTrackBar;
    Panel1: TPanel;
    formImage: TImage;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure formImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure formImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure formImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fCamshiftdemo: TfCamshiftdemo;

{-----------------------}
image: pIplImage = 0;
 hsv: pIplImage = 0;
 hue: pIplImage = 0;
 mask: pIplImage = 0;
 backproject: pIplImage = 0;
 histimg: pIplImage = 0;
hist: PCvHistogram = 0;

backproject_mode: longint = 0;
select_object: longint = 0;
track_object: longint = 0;
show_hist: longint = 0;

 origin: CvPoint;
 selection: CvRect;
 track_window: CvRect;
 track_box: CvBox2D;
 track_comp: CvConnectedComp;

 hdims: longint = 16;


hranges_arr: array[0..1] of float = (0, 180);
hranges: Pfloat = @hranges_arr;

capture: PCvCapture;
frame: PIplImage;
color: CvScalar;
bmp: TBitmap;

{-----------------------------}

sector_data : array[0..5] of array[0..2] of longint =
((0,2,1), (1,2,0), (1,0,2), (2,0,1), (2,1,0), (0,1,2));

{*************************************************************************}
implementation

{$R *.dfm}






function hsv2rgb(hue: float ): CvScalar ;
var
    rgb : array[0..2] of longint;
    p, sector: longint;
//    sector_data : array[0..5] of array[0..2] of longint;
begin
    hue := hue * 0.033333333333333333333333333333333;
    sector := cvFloor(hue);
    p := cvRound(255*(hue - sector));
    if (sector and 1) <> 0 then
        p := p xor 255
    else
        p := p xor 0;

    rgb[sector_data[sector][0]] := 255;
    rgb[sector_data[sector][1]] := 0;
    rgb[sector_data[sector][2]] := p;

    result := cvScalar_(rgb[2], rgb[1], rgb[0], 0);
end;



procedure main_cycle();
var
        i, bin_w: integer;
        _vmin, _vmax: integer;
        max_val: float;
        val: integer ;
        cs: CvSize;
        rec: TRect;
begin
    begin
        frame := cvQueryFrame( capture );
        if not(assigned(frame) ) then
            exit;

        if not(assigned(image) ) then
        begin
            //* allocate all the buffers */
            cs.width := frame.Width;
            cs.height := frame.Height;
            image := cvCreateImage( cs, 8, 3 );
            image.Origin := frame.Origin;
            hsv := cvCreateImage( cs, 8, 3 );
            hue := cvCreateImage( cs, 8, 1 );
            mask := cvCreateImage( cs, 8, 1 );
            backproject := cvCreateImage( cs, 8, 1 );
            hist := cvCreateHist( 1, @hdims, CV_HIST_ARRAY, @hranges, 1 );
            histimg := cvCreateImage( cvSize_(320,200), 8, 3 );
            cvZero( histimg );
        end;

        cvCopy( frame, image, 0 );
        cvCvtColor( image, hsv, CV_BGR2HSV );

        if( track_object <> 0 ) then
        begin
            _vmin := fCamshiftdemo.tbVmin.Position;
            _vmax := fCamshiftdemo.tbVmax.Position;

            cvInRangeS( hsv, cvScalar_(0, fCamshiftdemo.tbSmin.Position,
                        MIN(_vmin,_vmax),0),
                        cvScalar_(180,256,MAX(_vmin,_vmax),0), mask );
            cvSplit( hsv, hue, 0, 0, 0 );

            if( track_object < 0 ) then
            begin
                max_val := 0.0;
                cvSetImageROI( hue, selection );
                cvSetImageROI( mask, selection );
                cvCalcHist( longint(@hue), hist, 0, mask );
                cvGetMinMaxHistValue( hist, 0, @max_val, 0, 0 );
                if (max_val <> 0) then
                   cvConvertScale( hist^.bins, hist^.bins, (255.0 / max_val), 0 )
                else
                        cvConvertScale( hist^.bins, hist^.bins, 0.0, 0 );
                // cvConvertScale( hist^.bins, hist^.bins, max_val ? 255. / max_val : 0., 0 );
                cvResetImageROI( hue );
                cvResetImageROI( mask );
                track_window := selection;
                track_object := 1;

                cvZero( histimg );
                bin_w := round(histimg^.Width / hdims);
                for i := 0 to hdims-1 do
                begin
                    val := cvRound( cvGetReal1D(hist^.bins,i)*histimg^.Height/255 );
                    color := hsv2rgb(i*180.0/hdims);
                    cvRectangle( histimg, cvPoint_(i * bin_w, histimg^.height),
                                 cvPoint_( (i+1) * bin_w, histimg^.height - val),
                                 color, -1, 8, 0 );
                end;
            end;

            cvCalcBackProject( @hue, backproject, hist );
            cvAnd( backproject, mask, backproject, 0 );
            cvCamShift( backproject, track_window,
                        cvTermCriteria_( (CV_TERMCRIT_EPS or CV_TERMCRIT_ITER), 10, 1 ),
                        @track_comp, @track_box );
            track_window := track_comp.rect;

            if( backproject_mode <> 0 ) then
                cvCvtColor( backproject, image, CV_GRAY2BGR );
            if( image.Origin <> IPL_ORIGIN_TL ) then
                track_box.angle := -track_box.angle;
            {draw an ellipse around the tracked object}
            cvEllipseBox( image, track_box, CV_RGB(255,0,0), 3, CV_AA, 0 );
        end;

        {draw a rectangle on the area selected with mouse}
        if( select_object >0) and ( selection.width > 0) and ( selection.height > 0 ) then
        begin
            cvSetImageROI( image, selection );
            cvXorS( image, cvScalarAll(255), image, 0 );
            cvResetImageROI( image );
        end;

        {visualize the camera image in the window}
        IplImage2Bitmap(image, bmp);
        rec := fCamshiftdemo.formImage.canvas.ClipRect;
        fCamshiftdemo.formImage.canvas.StretchDraw(rec , bmp);

       if (show_hist <> 0) then
       begin
        IplImage2Bitmap(histimg, bmp);
        fHistogram.histimage.canvas.StretchDraw(fHistogram.histimage.canvas.ClipRect , bmp);
       end;
    end;
end;

procedure TfCamshiftdemo.FormCreate(Sender: TObject);
begin
        capture := cvCaptureFromCAM( 0);
    //    capture = cvCaptureFromAVI( argv[1] );

    if not(assigned(capture ))  then
    begin
        MessageDlg('Could not initialize capturing from camera!!', mtError, [mbOK], 0);
        halt;
    end;

//    printf( "Hot keys: \n"
//        "\tESC - quit the program\n"
//        "\tc - stop the tracking\n"
//        "\tb - switch to/from backprojection view\n"
//        "\th - show/hide object histogram\n"
//        "To initialize tracking, select the object with mouse\n" );

        tbVmin.Position := 10;
        tbVmax.Position := 256;
        tbSmin.Position := 30;
        bmp := TBitmap.Create;
        bmp.PixelFormat :=  pf24bit;
       timer1.enabled := true;
end;

procedure TfCamshiftdemo.FormKeyPress(Sender: TObject; var Key: Char);
begin
        if( key = char(27) ) then
        begin
            self.Destroy;
            halt;
        end;
        case key of
        'b':
            backproject_mode := backproject_mode xor 1;
        'c': begin
                track_object := 0;
                cvZero( histimg );
            end;
        'h':
            begin
            show_hist :=  show_hist xor 1;
            if (show_hist=0) then
                fHistogram.Free
            else
            begin
                fHistogram := TfHistogram.Create(self);
                fHistogram.Show;
            end;
            end;
        else
            ;
        end;

end;

procedure TfCamshiftdemo.FormDestroy(Sender: TObject);
begin
    cvReleaseCapture( @capture );
    if assigned(fHistogram) then
        fHistogram.Destroy;
end;

procedure TfCamshiftdemo.Timer1Timer(Sender: TObject);
begin
        main_cycle;
        application.HandleMessage;
end;


procedure TfCamshiftdemo.formImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
        xConv, yConv: integer;
begin
    {convert x and y mouse coords to OpenCV image coords}
    xConv := round(x *(image.Width / formImage.Width));
    if( image.Origin <> IPL_ORIGIN_TL ) then
        y :=  formImage.Height - y;
    yConv := round(y *(image.Height / formImage.Height));
    origin := cvPoint_(xConv ,yConv );
    selection := cvRect_(xConv,yConv,0,0);
    select_object := 1;
end;

procedure TfCamshiftdemo.formImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
        xConv, yConv: integer;
begin

    if not(assigned(image )) or (select_object =0 ) then
        // nop
    else
    begin

        {convert x and y mouse coords to OpenCV image coords}
        xConv := round(x *(image.Width / formImage.Width));
        if( image.Origin <> IPL_ORIGIN_TL ) then
            y :=  formImage.Height - y;
        yConv := round(y *(image.Height / formImage.Height));

        begin
            selection.x := MIN(xConv, origin.x);
            selection.y := MIN(yConv, origin.y);
            selection.width := ABS(xConv - origin.x);
            selection.height := ABS(yConv - origin.y);
            selection.x := MAX( selection.x, 0 );
            selection.y := MAX( selection.y, 0 );
            selection.width := MIN( selection.width, image.Width );
            selection.height := MIN( selection.height, image.Height );

        end;
     end;
end;

procedure TfCamshiftdemo.formImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
        select_object := 0;
        if( selection.width > 0) and (selection.height > 0 ) then
            track_object := -1;

end;

{**********************************************************************}

end.
