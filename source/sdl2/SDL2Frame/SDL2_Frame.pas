{
  ================================================================================
  ==                                                                            ==
  ==  ATENCIÓN: NO CARGAR LOS GRÁFICOS EN EL ONCREATE NI ONSHOW PORQUE EL       ==
  ==            COMPONENTE SDL2FRAME AUN NO ESTA COMPLETADO Y NO FUNCIONA       ==
  ==                                                                            ==
  ================================================================================
}
unit SDL2_Frame;

interface

uses
{$IFDEF FPC}
  Windows,
  Dialogs,
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
{$ELSE}
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.Dialogs,
{$ENDIF}
  SDL2;

type
  TSDL2Frame = class(TPanel)
  private
    FSDLPantalla: PSDLPantalla;
    FFlags: UInt32;
    FRenderInfo: PSDL_RendererInfo;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    procedure Finalize;
    property SDLPantalla: PSDLPantalla read FSDLPantalla;
  published
    property Flags: UInt32 read FFlags write FFlags;
  end;

procedure Register;

implementation

uses
  forms;

// ******************************************************************************

procedure Register;
begin
  RegisterComponents('SDL2', [TSDL2Frame]);
end;

// ******************************************************************************

procedure TSDL2Frame.CreateWnd;
begin
  inherited;
  if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
    SDL_InitSubSystem(SDL_INIT_VIDEO);
  New(FSDLPantalla);
  FSDLPantalla.Window := SDL_CreateWindowFrom(Pointer(WindowHandle));
  if FSDLPantalla.Window <> nil then
  begin
    FSDLPantalla.Renderer := SDL_CreateRenderer(FSDLPantalla.Window, -1, 0);
    // no forzamos ningún tipo de render (0) para que el sistema coja el que pueda Hard-Soft
    if FSDLPantalla.Renderer <> nil then
    begin
      New(FRenderInfo);
      if SDL_GetRendererInfo(FSDLPantalla.Renderer, FRenderInfo) = 0 then
      begin
        FSDLPantalla.max_texture_width := FRenderInfo.max_texture_width;
        FSDLPantalla.max_texture_height := FRenderInfo.max_texture_height;
        FSDLPantalla.hardware := ((FRenderInfo.Flags and SDL_RENDERER_ACCELERATED) > 0);
        FSDLPantalla.render_name := FRenderInfo.name; // PAnsiChar(FRenderInfo.name);
        SDL_ShowWindow(FSDLPantalla.Window);
        if SDL_SetRenderDrawColor(FSDLPantalla.Renderer, 0, 0, 0, SDL_ALPHA_OPAQUE) = 0 then
        begin
          if SDL_RenderFillRect(FSDLPantalla.Renderer, nil) = 0 then
            FFlags := SDL_GetWindowFlags(FSDLPantalla.Window)
          else
            ShowMessage('Error clearing render context');
        end
        else
          ShowMessage('Error setting render draw color');
      end
      else
        ShowMessage('Error getting information about rendering context');

    end
    else
      ShowMessage('Error crearting SDL2 Render');

  end
  else
    ShowMessage('Error creating SDL2 Window.')
end;

// ******************************************************************************

procedure TSDL2Frame.Finalize;
begin
  if FSDLPantalla.Renderer <> nil then
  begin
    SDL_DestroyRenderer(FSDLPantalla.Renderer);
    FSDLPantalla.Renderer := nil;
  end;
  if FSDLPantalla.Window <> nil then
  begin
    SDL_DestroyWindow(FSDLPantalla.Window);
    FSDLPantalla.Window := nil;
  end;
  Dispose(FSDLPantalla);
end;

// ******************************************************************************

procedure TSDL2Frame.DestroyWnd;
begin
  Finalize;
  inherited;
end;

end.
