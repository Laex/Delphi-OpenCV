unit xplatformutils;

{$I jedi-sdl.inc}

interface

uses
  {$IFDEF WIN32}
  Windows;
  {$ELSE}
  {$IFDEF UNIX}
    {$IFDEF FPC}
    libc;
    {$ELSE}
    Libc,
    Xlib;
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}

const
{$IFDEF MACOS}
  DIR_SEP = ':';
  DIR_CUR = ':';
{$ELSE}
{$IFDEF DARWIN}
  DIR_SEP = ':';
  DIR_CUR = ':';
{$ELSE}
{$IFDEF WIN32}
  DIR_SEP = '\';
  DIR_CUR = '';
{$ELSE}
  DIR_SEP = '/';
  DIR_CUR = '';
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure ExecAndWait( aProcess : string; aArguments : array of string );


implementation

procedure ExecAndWait( aProcess : string; aArguments : array of string );
var
{$IFDEF WIN32}
  CommandLine : string;
  ProcessInfo: TProcessInformation;
  Startupinfo: TStartupInfo;
  ExitCode: longword;
  x : integer;
{$ELSE}
{$IFDEF UNIX}
  pid: PID_T;
  Max: Integer;
  i: Integer;
  parg: PPCharArray;
  argnum: Integer;
  returnvalue : Integer;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF WIN32}
  // Initialize the structures
  FillChar(ProcessInfo, sizeof(TProcessInformation), 0);
  FillChar(Startupinfo, sizeof(TStartupInfo), 0);
  Startupinfo.cb := sizeof(TStartupInfo);

  // Attempts to create the process
  Startupinfo.dwFlags := STARTF_USESHOWWINDOW;
  Startupinfo.wShowWindow := 1;
  CommandLine := aProcess;
  for x := Low(aArguments ) to High(aArguments ) do
    CommandLine := CommandLine + ' ' + aArguments[ x ];
  if CreateProcess( nil, PChar( CommandLine ), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, Startupinfo, ProcessInfo ) then
  begin
    // The process has been successfully created
    // No let's wait till it ends...
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

    // Process has finished. Now we should close it.
    GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);  // Optional
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
  {$ELSE}
  {$IFDEF UNIX}
  pid := fork;

  if pid = 0 then
  begin
    Max := sysconf(_SC_OPEN_MAX);
    for i := (STDERR_FILENO+1) to Max do
    begin
      fcntl(i, F_SETFD, FD_CLOEXEC);
    end;

    argnum := High(aArguments) + 1;

    GetMem(parg,(2 + argnum) * sizeof(PChar));
    parg[0] := PChar(aProcess);

    i := 0;

    while i <= high(aArguments) do
    begin
      inc(i);
      parg[i] := PChar(aArguments[i-1]);
    end;

    parg[i+1] := nil;
    execvp(PChar(aProcess),PPChar(@parg[0]));
    halt;
  end;

  if pid > 0 then
  begin
    waitpid(pid,@returnvalue,0);
  end;
  {$ENDIF}
  {$ENDIF}
end;

end.
 