unit uMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  core_c,
  core.types_c,
  imgproc_c,
  highgui_c,
  dglOpenGL;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    // ---- OpenGL ---------
    DC       : HDC; // контекст устройства
    RC       : HGLRC;
    angle    : GLfloat;
    listIndex: GLuint;
    texture  : GLuint;
    // ---- OpenCV ---------
    capture: pCvCapture;
    procedure SetupGL;
    procedure IdleHandler(Sender: TObject; var Done: boolean);
    procedure Render;
    procedure ErrorHandler;
    function ConvertIplToTexture(image: pIplImage): GLuint;
    procedure DrawCube;
    procedure SetupData;
    procedure StartupDraw; // контекст рендеринга
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  NearClipping = 1;
  FarClipping  = 1000;

function TMainForm.ConvertIplToTexture(image: pIplImage): GLuint;
begin
  glGenTextures(
    1,
    @Result);
  glBindTexture(
    GL_TEXTURE_2D,
    Result);
  // ------------------------
  glTexEnvf(
    GL_TEXTURE_ENV,
    GL_TEXTURE_ENV_MODE,
    GL_DECAL);
  glTexParameterf(
    GL_TEXTURE_2D,
    GL_TEXTURE_MIN_FILTER,
    GL_LINEAR);
  glTexParameterf(
    GL_TEXTURE_2D,
    GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTexParameterf(
    GL_TEXTURE_2D,
    GL_TEXTURE_WRAP_S,
    GL_REPEAT);
  glTexParameterf(
    GL_TEXTURE_2D,
    GL_TEXTURE_WRAP_T,
    GL_REPEAT);
  // ------------------------
  gluBuild2DMipmaps(
    GL_TEXTURE_2D,
    3,
    image^.width,
    image^.height,
    GL_BGR,
    GL_UNSIGNED_BYTE,
    image^.imageData);
end;

procedure TMainForm.DrawCube;
var
  frame: pIplImage;
begin
  frame := cvQueryFrame(capture);
  if Assigned(frame) then
  begin
    if texture <> 0 then
      glDeleteTextures(
        1,
        @texture);
    texture := ConvertIplToTexture(frame);

    glEnable(GL_TEXTURE_2D);
    glBindTexture(
      GL_TEXTURE_2D,
      texture);
    glCallList(listIndex);
    glDisable(GL_TEXTURE_2D);
  end;
end;

procedure TMainForm.SetupGL;
begin
  glClearColor(
    0.0,
    0.0,
    0.0,
    0.0);                  // цвет фона
  glEnable(GL_DEPTH_TEST); // ¬ключить тест глубины
  glEnable(GL_CULL_FACE);  // показывать только передние грани
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  tmpBool: boolean;
begin
  glViewport(
    0,
    0,
    ClientWidth,
    ClientHeight);

  StartupDraw;

  IdleHandler(
    Sender,
    tmpBool);
end;

procedure TMainForm.Render;
begin
  gluLookAt(
    0.0,
    2.0,
    1.5,
    0.0,
    0.0,
    -0.5,
    0.0,
    -1.0,
    0.0);
  glRotatef(
    angle,
    0.0,
    1.0,
    0.0);
  DrawCube;
  angle := angle + 1.0;
end;

procedure TMainForm.ErrorHandler;
begin
  Caption := gluErrorString(glGetError);
end;

procedure TMainForm.StartupDraw;
begin
 glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(
    60.0,
    ClientWidth / ClientHeight,
    NearClipping,
    FarClipping);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TMainForm.IdleHandler(Sender: TObject; var Done: boolean);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // очищаем буфер цвета и буфер глубины
  StartupDraw;
  glPushMatrix;
  try
    Render;
  except
    ErrorHandler;
  end;
  glPopMatrix;
  SwapBuffers(DC); // выводим содержание буфера на экран
  Done := FALSE;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if not InitOpenGL then
    FatalAppExit(
      0,
      'Ќе удалось проинициализировать OpenGL');
  DC := GetDC(Handle); // определ€ем, что контекстом устройства будет наше окно
  RC := CreateRenderingContext(
    DC,
    [opDoubleBuffered],
    32,
    24,
    0,
    0,
    0,
    0); // «десь
  // создаем контекст рендеринга, с необходимыми параметрами
  ActivateRenderingContext(
    DC,
    RC); // активируем и св€зываем контект рендеринга с
  // с контекстом устройства
  SetupGL;
  SetupData;
  Application.OnIdle := IdleHandler;
end;

procedure TMainForm.SetupData;
const
  vert: array [0 .. 47] of GLfloat = (-0.5, 0.0, 0.5, 0.5, 0.0, 0.5, 0.5, 1.0, 0.5, -0.5, 1.0, 0.5, -0.5, 1.0, -0.5,
    0.5, 1.0, -0.5, 0.5, 0.0, -0.5, -0.5, 0.0, -0.5, 0.5, 0.0, 0.5, 0.5, 0.0, -0.5, 0.5, 1.0, -0.5, 0.5, 1.0, 0.5, -0.5,
    0.0, -0.5, -0.5, 0.0, 0.5, -0.5, 1.0, 0.5, -0.5, 1.0, -0.5);

  texcoords: array [0 .. 31] of GLfloat = (0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0,
    1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0);

  cubeIndices: array [0 .. 23] of GLubyte = (0, 1, 2, 3, 4, 5, 6, 7, 3, 2, 5, 4, 7, 6, 1, 0, 8, 9, 10, 11, 12,
    13, 14, 15);
begin
  capture := cvCreateCameraCapture(CV_CAP_ANY);
  if not Assigned(capture) then
    FatalAppExit(
      0,
      'Ќе удалось проинициализировать захват с камеры');
  listIndex := glGenLists(1);
  glNewList(
    listIndex,
    GL_COMPILE);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);
  glTexCoordPointer(
    2,
    GL_FLOAT,
    0,
    @texcoords);
  glVertexPointer(
    3,
    GL_FLOAT,
    0,
    @vert);

  glDrawElements(
    GL_QUADS,
    24,
    GL_UNSIGNED_BYTE,
    @cubeIndices);
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glEndList();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(capture) then
    cvReleaseCapture(capture);
  DeactivateRenderingContext;  // деактивируем контекст рендеринга
  DestroyRenderingContext(RC); // разрушаем контекст рендеринга
  ReleaseDC(
    Handle,
    DC); // разрушаем контекст устройства
end;

end.
