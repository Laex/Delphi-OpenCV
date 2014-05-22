(*
 * Copyright (c) 2000-2003 Fabrice Bellard
 *
 * This file is part of ffm.
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
unit ffm.time;

interface

(**
 * Get the current time in microseconds.
 *)
function av_gettime():int64;cdecl;

(**
 * Sleep for a period of time.  Although the duration is expressed in
 * microseconds, the actual delay may be rounded to the precision of the
 * system timer.
 *
 * @param  usec Number of microseconds to sleep.
 * @return zero on success or (negative) error code.
 *)
function av_usleep(usec:cardinal):Integer;cdecl;

implementation

uses ffm.lib;

function av_gettime;external avutil_dll;
function av_usleep;external avutil_dll;

end.
