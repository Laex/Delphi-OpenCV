unit ffm.lib;

{$i ffmpeg.inc}

interface

const
  avformat_dll = 'avformat-57.dll';
  avcodec_dll = 'avcodec-57.dll';
  avutil_dll = 'avutil-55.dll';
  swscale_dll = 'swscale-4.dll';
  avfilter_dll = 'avfilter-6.dll';
  avio_dll = avformat_dll;
  samplefmt_dll = avutil_dll;
  channel_layout_dll=avutil_dll;

implementation

end.
