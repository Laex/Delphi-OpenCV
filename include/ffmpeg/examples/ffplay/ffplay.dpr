(*
  * Copyright (c) 2003 Fabrice Bellard
  *
  * This file is part of FFmpeg.
  *
  * FFmpeg is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Lesser General Public
  * License as published by the Free Software Foundation; either
  * version 2.1 of the License, or (at your option) any later version.
  *
  * FFmpeg is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Lesser General Public License for more details.
  *
  * You should have received a copy of the GNU Lesser General Public
  * License along with FFmpeg; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

program ffplay;

{$APPTYPE CONSOLE}
{$R *.res}
{$include ffmpeg.inc}

uses
  System.SysUtils,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  SDL,
  ffmpeglib in '..\..\ffmpeglib.pas',
  ctypes in '..\..\..\ctypes\ctypes.pas',
  avformat in '..\..\libavformat\avformat.pas',
  avio in '..\..\libavformat\avio.pas',
  avutil in '..\..\libavutil\avutil.pas',
  buffer in '..\..\libavutil\buffer.pas',
  dict in '..\..\libavutil\dict.pas',
  frame in '..\..\libavutil\frame.pas',
  log in '..\..\libavutil\log.pas',
  opt in '..\..\libavutil\opt.pas',
  pixfmt in '..\..\libavutil\pixfmt.pas',
  rational in '..\..\libavutil\rational.pas',
  samplefmt in '..\..\libavutil\samplefmt.pas',
  parseutils in '..\..\libavutil\parseutils.pas',
  swscale in '..\..\libswscale\swscale.pas',
  pixdesc in '..\..\libavutil\pixdesc.pas',
  imgutils in '..\..\libavutil\imgutils.pas',
  mem in '..\..\libavutil\mem.pas',
  error in '..\..\libavutil\error.pas',
  avfilter in '..\..\libavfilter\avfilter.pas',
  buffersink in '..\..\libavfilter\buffersink.pas',
  mathematics in '..\..\libavutil\mathematics.pas',
  libavcodec.avcodec in '..\..\libavcodec\libavcodec.avcodec.pas',
  buffersrc in '..\..\libavfilter\buffersrc.pas',
  errno in '..\..\libavutil\errno.pas',
  cmdutils in 'cmdutils.pas';

const
  dummy_videodriver = 'SDL_VIDEODRIVER=dummy';

Var
  flags: Integer;
  is_: TVideoState;
  vi: pSDL_VideoInfo;

begin
  try
    av_log_set_flags(AV_LOG_SKIP_REPEATED);
    parse_loglevel(options);

    (* register all codecs, demux and protocols *)
{$IF CONFIG_AVDEVICE}
    avdevice_register_all();
{$ENDIF}
{$IF CONFIG_AVFILTER}
    avfilter_register_all();
{$ENDIF}
    av_register_all();
    avformat_network_init();

    init_opts();

    signal(SIGINT, sigterm_handler); (* Interrupt (ANSI). *)
    signal(SIGTERM, sigterm_handler); (* Termination (ANSI). *)

    show_banner(argc, argv, options);

    parse_options(nil, argc, argv, options, opt_input_file);

    if Length(input_filename) = 0 then
    begin
      show_usage();
      av_log(nil, AV_LOG_FATAL, 'An input file must be specified'#13#10);
      av_log(nil, AV_LOG_FATAL, Format('Use -h to get full help or, even better, run ''man %s'''#13#10, [program_name]));
      Halt(1);
    end;

    if not display_disable then
      video_disable := true;

    flags := SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_TIMER;
    if audio_disable then
      flags := flags and (not SDL_INIT_AUDIO);
    if display_disable then
      SDL_putenv(dummy_videodriver); (* For the event queue, we always need a video driver. *)

    // #if !defined(__MINGW32__) && !defined(__APPLE__)
    // flags |= SDL_INIT_EVENTTHREAD; (* Not supported on Windows or Mac OS X *)
    // #endif
    if (SDL_Init(flags) <> 0) then
    begin
      av_log(nil, AV_LOG_FATAL, 'Could not initialize SDL - %s'#13#10, SDL_GetError());
      av_log(nil, AV_LOG_FATAL, '(Did you set the DISPLAY variable?)'#13#10);
      Halt(1);
    end;

    if (not display_disable) then
    begin
      vi := SDL_GetVideoInfo();
      fs_screen_width := vi^.current_w;
      fs_screen_height := vi^.current_h;
    end;

    SDL_EventState(SDL_ACTIVEEVENT, SDL_IGNORE);
    SDL_EventState(SDL_SYSWMEVENT, SDL_IGNORE);
    SDL_EventState(SDL_USEREVENT, SDL_IGNORE);

    if (av_lockmgr_register(lockmgr) <> 0) then
    begin
      av_log(nil, AV_LOG_FATAL, 'Could not initialize lock manager!'#13#10);
      do_exit(nil);
    end;

    av_init_packet(flush_pkt);
    flush_pkt.data := @flush_pkt;

    is_ := stream_open(input_filename, file_iformat);
    if not Assigned(is_) then
    begin
      av_log(nil, AV_LOG_FATAL, 'Failed to initialize VideoState!'#13#10);
      do_exit(nil);
    end;
    event_loop(is_);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
