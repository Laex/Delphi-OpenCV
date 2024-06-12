program TestErrorHandler;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.core.types_c,
  ocv.core_c;

function cvDelphiErrorHandler( //
  status: Integer;             //
  const func_name: PCvChar;    //
  const err_msg: PCvChar;      //
  const file_name: PCvChar;    //
  line: Integer;               //
  user_data: pointer           //
  ): Integer; cdecl;
begin
  cvSetErrStatus(0);
  // Ignore
  // reset error status avoiding a cascade of subsequent errors
  Result := 0;
  // Or
  // Raise Exception
  Raise Exception.CreateFmt('OpenCV error: in file <<%s>> function <<%s>> line <<%d>> - %s', //
    [string(file_name), string(func_name), line, String(err_msg)]);
end;

begin
  try
    cvRedirectError(cvDelphiErrorHandler);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
