unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

Const
  WM_PLAYVIDEO = WM_USER + 100;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure WMPlayVideo(var Message: TMessage); message WM_PLAYVIDEO;
    procedure Decode;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

Uses
  SDL2,
  ffm.avformat,
  ffm.avutil,
  ffm.frame,
  ffm.swscale,
  ffm.libavcodec.avcodec,
  uResourcePaths;

const
  std_filename = cResourceMedia + 'trailer.avi';
  pixel_format = SDL_PIXELFORMAT_YV12;

Var
  sdlWnd: PSDL_Window;
  renderer: PSDL_Renderer;
  MooseTexture: pSDL_Texture;

  format_context: pAVFormatContext = nil;
  video_stream: Integer;
  codec_context: pAVCodecContext;
  img_convert_context: pSwsContext;
  codec: pAVCodec;
  pFrame, frame: pAVFrame;
  packet: TAVPacket;
  frame_finished: Integer;
  ImgBufferSize: Integer;
  ImgBuffer: PByte;
  pict: TAVPicture;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  PostMessage(Handle, WM_PLAYVIDEO, 0, 0);
end;

procedure TMainForm.FormCreate(Sender: TObject);
Var
  err: Integer;
begin
  if (SDL_Init(SDL_INIT_VIDEO or SDL_INIT_NOPARACHUTE) < 0) then
    FatalAppExit(0, PChar('Couldn''t initialize SDL: ' + SDL_GetError()));
  // SDL_InitSubSystem(SDL_INIT_VIDEO);
  sdlWnd := SDL_CreateWindowFrom(Pointer(Handle));
  renderer := SDL_CreateRenderer(sdlWnd, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if not Assigned(renderer) then
    FatalAppExit(0, PChar('SDL_CreateRenderer Error: ' + SDL_GetError()));

  // Register all available file formats and codecs
  av_register_all();
  avformat_network_init();
  // Open video file
  Assert(avformat_open_input(format_context, std_filename, nil, nil) >= 0);
  // Retrieve stream information
  Assert(avformat_find_stream_info(format_context, nil) >= 0);
  // Dump information about file onto standard error
  av_dump_format(format_context, 0, std_filename, 0);
  // Find the first video stream
  for video_stream := 0 to format_context^.nb_streams - 1 do
    if (format_context^.streams[video_stream]^.codec^.codec_type = AVMEDIA_TYPE_VIDEO) then
      break;
  Assert(video_stream < format_context^.nb_streams);
  codec_context := format_context^.streams[video_stream]^.codec;
  codec := avcodec_find_decoder(codec_context^.codec_id);
  Assert(avcodec_open2(codec_context, codec, nil) >= 0);

  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Set default colour to black.
  Assert(SDL_RenderClear(renderer) >= 0);

  MooseTexture := SDL_CreateTexture(renderer, pixel_format, Integer(SDL_TEXTUREACCESS_STREAMING), codec_context^.width,
    codec_context^.height);
  Assert(Assigned(MooseTexture));

  img_convert_context := sws_getCachedContext(nil, codec_context^.width, codec_context^.height, codec_context^.pix_fmt,
    codec_context^.width, codec_context^.height, codec_context^.pix_fmt, SWS_BICUBIC, nil, nil, nil);
  Assert(Assigned(img_convert_context));

  pFrame := avcodec_alloc_frame();

  frame := avcodec_alloc_frame();
  ImgBufferSize := avpicture_get_size(codec_context^.pix_fmt, codec_context^.width, codec_context^.height);
  ImgBuffer := AllocMem(ImgBufferSize);
  avpicture_fill(pAVPicture(frame), ImgBuffer, codec_context^.pix_fmt, codec_context^.width, codec_context^.height);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { TODO: destroy ffmpeg objects }
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(sdlWnd);
  SDL_Quit();
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SDL_RenderSetViewport(renderer, nil);
end;

procedure TMainForm.WMPlayVideo(var Message: TMessage);
begin
  Decode;
end;

procedure TMainForm.Decode;
begin
  frame_finished := 1;
  repeat
    if av_read_frame(format_context, packet) >= 0 then
    begin
      if (packet.stream_index = video_stream) then
      begin
        // Video stream packet
        avcodec_decode_video2(codec_context, pFrame, frame_finished, @packet);

        if (frame_finished <> 0) then
        begin
          pict.data[0] := frame^.data[0];
          pict.data[1] := frame^.data[2];
          pict.data[2] := frame^.data[1];

          pict.linesize[0] := frame^.linesize[0];
          pict.linesize[1] := frame^.linesize[2];
          pict.linesize[2] := frame^.linesize[1];

          sws_scale(img_convert_context, @pFrame^.data, @pFrame^.linesize, 0, codec_context^.height, @pict.data,
            @pict.linesize);

          SDL_UpdateTexture(MooseTexture, nil, ImgBuffer, codec_context^.width * SDL_BYTESPERPIXEL(pixel_format));

          SDL_RenderClear(renderer);

          SDL_RenderCopy(renderer, MooseTexture, nil, nil);
          SDL_RenderPresent(renderer);

          av_frame_unref(pFrame);
          avcodec_get_frame_defaults(pFrame);
        end;
      end;
      av_free_packet(packet);
    end;
    Application.ProcessMessages;
  until frame_finished <> 0;
  PostMessage(Handle, WM_PLAYVIDEO, 0, 0);
end;

end.
