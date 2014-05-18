program uCompressHaar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.ZLib;

Const
  Path = '.\facedetectxml\';

Var
  S: TSearchRec;
  ZC: TZCompressionStream;
  Fs, Fd: TFileStream;

begin
  try
    if FindFirst(Path+'*.xml', faAnyFile, S) = 0 then
      repeat
        Fs := TFileStream.Create(Path + S.Name, fmOpenRead);
        Fd := TFileStream.Create(Path + S.Name + '.z', fmCreate);
        ZC := TZCompressionStream.Create(clMax,Fd);
        try
          WriteLn(S.Size:8,' ',S.Name);
          ZC.CopyFrom(Fs, S.Size);
        finally
          ZC.Free;
          Fs.Free;
          Fd.Free;
        end;
      until FindNext(S) <> 0;
    FindClose(S);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
