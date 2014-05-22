(*
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

unit ffmpeg.error;

{$include ffmpeg.inc}

interface

uses ffmpeg.errno;

(* error handling *)
{$IF EDOM > 0}
function AVERROR(e: integer): integer; inline;
/// < Returns a negative error code from a POSIX error code, to return from library functions.
function AVUNERROR(e: integer): integer; inline;
/// < Returns a POSIX error code from a library function error return value.
{$ELSE}
(* Some platforms have E* and errno already negated. *)
AVERROR(e)(e)AVUNERROR(e)(e)
{$ENDIF}
// #define FFERRTAG(a, b, c, d) (-(int)MKTAG(a, b, c, d))

Const
  AVERROR_BSF_NOT_FOUND = -(ord($F8) or (ord('B') shl 8) or (ord('S') shl 16) or (ord('F') shl 24));
  /// < Bitstream filter not found
  AVERROR_BUG = -(ord('B') or (ord('U') shl 8) or (ord('G') shl 16) or (ord('!') shl 24));
  /// < Internal bug, also see AVERROR_BUG2
  AVERROR_BUFFER_TOO_SMALL = -(ord('B') or (ord('U') shl 8) or (ord('F') shl 16) or (ord('S') shl 24));
  /// < Buffer too small
  AVERROR_DECODER_NOT_FOUND = -(ord($F8) or (ord('D') shl 8) or (ord('E') shl 16) or (ord('C') shl 24));
  /// < Decoder not found
  AVERROR_DEMUXER_NOT_FOUND = -(ord($F8) or (ord('D') shl 8) or (ord('E') shl 16) or (ord('M') shl 24));
  /// < Demuxer not found
  AVERROR_ENCODER_NOT_FOUND = -(ord($F8) or (ord('E') shl 8) or (ord('N') shl 16) or (ord('C') shl 24));
  /// < Encoder not found
  AVERROR_EOF = -(ord('E') or (ord('O') shl 8) or (ord('F') shl 16) or (ord(' ') shl 24));
  /// < End of file
  AVERROR_EXIT = -(ord('E') or (ord('X') shl 8) or (ord('I') shl 16) or (ord('T') shl 24));
  /// < Immediate exit was requested; the called function should not be restarted
  AVERROR_EXTERNAL = -(ord('E') or (ord('X') shl 8) or (ord('T') shl 16) or (ord(' ') shl 24));
  /// < Generic error in an external library
  AVERROR_FILTER_NOT_FOUND = -(ord($F8) or (ord('F') shl 8) or (ord('I') shl 16) or (ord('L') shl 24));
  /// < Filter not found
  AVERROR_INVALIDDATA = -(ord('I') or (ord('N') shl 8) or (ord('D') shl 16) or (ord('A') shl 24));
  /// < Invalid data found when processing input
  AVERROR_MUXER_NOT_FOUND = -(ord($F8) or (ord('M') shl 8) or (ord('U') shl 16) or (ord('X') shl 24));
  /// < Muxer not found
  AVERROR_OPTION_NOT_FOUND = -(ord($F8) or (ord('O') shl 8) or (ord('P') shl 16) or (ord('T') shl 24));
  /// < Option not found
  AVERROR_PATCHWELCOME = -(ord('P') or (ord('A') shl 8) or (ord('W') shl 16) or (ord('E') shl 24));
  /// < Not yet implemented in FFmpeg, patches welcome
  AVERROR_PROTOCOL_NOT_FOUND = -(ord($F8) or (ord('P') shl 8) or (ord('R') shl 16) or (ord('O') shl 24));
  /// < Protocol not found
  AVERROR_STREAM_NOT_FOUND = -(ord($F8) or (ord('S') shl 8) or (ord('T') shl 16) or (ord('R') shl 24));
  /// < Stream not found
  (*
    * This is semantically identical to AVERROR_BUG
    * it has been introduced in Libav after our AVERROR_BUG and with a modified value.
  *)
  AVERROR_BUG2 = -(ord('B') or (ord('U') shl 8) or (ord('G') shl 16) or (ord(' ') shl 24));
  AVERROR_UNKNOWN = -(ord('U') or (ord('N') shl 8) or (ord('K') shl 16) or (ord('N') shl 24));
  /// < Unknown error, typically from an external library
  AVERROR_EXPERIMENTAL = -($2BB2AFA8);
  /// < Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.

  AV_ERROR_MAX_STRING_SIZE = 64;

  (*
    * Put a description of the AVERROR code errnum in errbuf.
    * In case of failure the global variable errno is set to indicate the
    * error. Even in case of failure av_strerror() will print a generic
    * error message indicating the errnum provided to errbuf.
    *
    * @param errnum      error code to describe
    * @param errbuf      buffer to which description is written
    * @param errbuf_size the size in bytes of errbuf
    * @return 0 on success, a negative value if a description for errnum
    * cannot be found
  *)
  // int av_strerror(int errnum, char *errbuf, size_t errbuf_size);
function av_strerror(errnum: integer; errbuf: pAnsiChar; errbuf_size: integer): integer; cdecl;

(*
  * Fill the provided buffer with a string containing an error string
  * corresponding to the AVERROR code errnum.
  *
  * @param errbuf         a buffer
  * @param errbuf_size    size in bytes of errbuf
  * @param errnum         error code to describe
  * @return the buffer in input, filled with the error description
  * @see av_strerror()
*)
// static inline char *av_make_error_string(char *errbuf, size_t errbuf_size, int errnum)
{
  av_strerror(errnum, errbuf, errbuf_size);
  return errbuf;
}

function av_make_error_string(errbuf: pAnsiChar; errbuf_size: integer; errnum: integer): pAnsiChar; inline;

(*
  * Convenience macro, the return value should be used only directly in
  * function arguments but never stand-alone.
*)
// #define av_err2str(errnum) \
// av_make_error_string((char[AV_ERROR_MAX_STRING_SIZE]){0}, AV_ERROR_MAX_STRING_SIZE, errnum)
function av_err2str(errnum: integer): pAnsiChar; inline;

implementation

uses ffmpeglib;

function AVERROR(e: integer): integer; inline;
begin
  Result := -e;
end;

function AVUNERROR(e: integer): integer; inline;
begin
  Result := -e;
end;

function av_make_error_string(errbuf: pAnsiChar; errbuf_size: integer; errnum: integer): pAnsiChar; inline;
begin
  av_strerror(errnum, errbuf, errbuf_size);
  Result := errbuf;
end;

function av_err2str(errnum: integer): pAnsiChar; inline;
Var
  buf: array [0 .. AV_ERROR_MAX_STRING_SIZE - 1] of AnsiChar;
begin
  FillChar(buf, SizeOf(buf), 0);
  av_make_error_string(buf, AV_ERROR_MAX_STRING_SIZE, errnum);
  Result := buf;
end;

function av_strerror; external avutil_dll;

end.
