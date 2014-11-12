unit ffm.lib;

{$i ffmpeg.inc}

interface

const
  avformat_dll = 'avformat-56.dll';
  avcodec_dll = 'avcodec-56.dll';
  avutil_dll = 'avutil-54.dll';
  swscale_dll = 'swscale-3.dll';
  avfilter_dll = 'avfilter-5.dll';
  avio_dll = avformat_dll;
  samplefmt_dll = avutil_dll;
  channel_layout_dll=avutil_dll;

implementation

end.
