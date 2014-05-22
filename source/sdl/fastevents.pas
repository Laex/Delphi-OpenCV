unit fastevents;
{
    FastEvents is a high-performance event queue manager for SDL.
    Copyright (C) 2002 Bob Pendleton

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License
    as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA

    If you do not wish to comply with the terms of the LGPL please
    contact the author as other terms are available for a fee.

    Bob Pendleton
    Bob@Pendleton.com
}
{
    Translated to Object Pascal by Mason Wheeler.
    masonwheeler@yahoo.com

    The original C library can be found at
    http://www.gameprogrammer.com/fastevents/fastevents.zip
}

interface

uses
   sdl;

function FE_Init: integer;  // Initialize FE
procedure FE_Quit;          // shutdown FE
procedure FE_PumpEvents;    // replacement for SDL_PumpEvents
function FE_PollEvent(event: PSDL_Event): integer; // replacement for SDL_PollEvent
procedure FE_WaitEvent(event: PSDL_Event); // replacement for SDL_WaitEvent
procedure FE_PushEvent(event: PSDL_Event); // replacement for SDL_PushEvent
function FE_GetError: string; // returns the last FastEvents error

implementation
//----------------------------------------
//
// error handling code
//

var
   errorString: string = '';

procedure setError(err: string); inline;
begin
   errorString := err;
end;

function FE_GetError: string;
begin
   result := errorString;
end;

//----------------------------------------
//
// Threads, mutexs, thread utils, and
// thread safe wrappers
//
var
   eventLock: PSDL_Mutex = nil;
   eventWait: PSDL_Cond = nil;
   eventTimer: PSDL_TimerID = nil;

//----------------------------------------
//
// Timer callback
//

function timerCallback(interval: Uint32; param: pointer): Uint32; {$IFNDEF __GPC__} cdecl; {$ENDIF}
begin
   SDL_CondBroadcast(eventWait);
   result := interval;
end;

//----------------------------------------
//  Initialization and
//  cleanup routines
//

function FE_Init: integer;
begin
   result := -1;
   if (SDL_INIT_TIMER and SDL_WasInit(SDL_INIT_TIMER)) = 0 then
      SDL_InitSubSystem(SDL_INIT_TIMER);

   eventLock := SDL_CreateMutex();
   if eventLock = nil then
   begin
      setError('FE: can''t create a mutex');
      Exit;
   end;

   eventWait := SDL_CreateCond();
   if eventWait = nil then
   begin
      setError('FE: can''t create a condition variable');
      Exit;
   end;

   eventTimer := SDL_AddTimer(10, timerCallback, nil);
   if eventTimer = nil then
   begin
      setError('FE: can''t add a timer');
      Exit;
   end;

   result := 0;
end;

procedure FE_Quit;
begin
   SDL_DestroyMutex(eventLock);
   eventLock := nil;

   SDL_DestroyCond(eventWait);
   eventWait := nil;

   SDL_RemoveTimer(eventTimer);
   eventTimer := nil;
end;

//----------------------------------------
//
// replacement for SDL_PushEvent();
//
// This was originally an int function; I changed it to a
// procedure because it only had one possible return value: 1.
// This seemed a bit pointless. -- Mason Wheeler
//

procedure FE_PushEvent(event: PSDL_Event);
begin
   SDL_LockMutex(eventLock);
   while SDL_PushEvent(event) = -1 do
     SDL_CondWait(eventWait, eventLock);
   SDL_UnlockMutex(eventLock);
   SDL_CondSignal(eventWait);
end;

//----------------------------------------
//
// replacement for SDL_PumpEvents();
//

procedure FE_PumpEvents;
begin
   SDL_LockMutex(eventLock);
   SDL_PumpEvents();
   SDL_UnlockMutex(eventLock);
end;

//----------------------------------------
//
// replacement for SDL_PollEvent();
//

function FE_PollEvent(event: PSDL_Event): integer;
var
   val: integer;
begin
   SDL_LockMutex(eventLock);
   val := SDL_PollEvent(event);
   SDL_UnlockMutex(eventLock);

   if val > 0 then
      SDL_CondSignal(eventWait);
   result := val;
end;

//----------------------------------------
//
// Replacement for SDL_WaitEvent();
//
// This was originally an int function; I changed it to a
// procedure because it only had one possible return value: 1.
// This seemed a bit pointless. -- Mason Wheeler
//

procedure FE_WaitEvent(event: PSDL_Event);
begin
   SDL_LockMutex(eventLock);
   while SDL_PollEvent(event) <= 0 do
      SDL_CondWait(eventWait, eventLock);
   SDL_UnlockMutex(eventLock);
   SDL_CondSignal(eventWait);
end;

end.
