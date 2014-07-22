unit ffm.lib;

{$i ffmpeg.inc}

interface

const
  avformat_dll = 'avformat-55.dll';
  avcodec_dll = 'avcodec-55.dll';
  avutil_dll = 'avutil-52.dll';
  swscale_dll = 'swscale-2.dll';
  avfilter_dll = 'avfilter-4.dll';
  avio_dll = avformat_dll;
  samplefmt_dll = avutil_dll;
  channel_layout_dll=avutil_dll;

implementation

end.
