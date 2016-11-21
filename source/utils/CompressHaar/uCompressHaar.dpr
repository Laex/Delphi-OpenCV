program uCompressHaar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.ZLib;

function CreateResourceName(const FileName: String): string;
begin
  Result := StringReplace(FileName, 'haarcascade', 'H', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, 'frontalface', 'FF', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, 'mcs', 'M', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, 'right', 'R', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, 'left', 'L', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '.xml', '', [rfIgnoreCase, rfReplaceAll]);
  Result := ToUpper(StringReplace(Result, '_', '', [rfIgnoreCase, rfReplaceAll]));
end;

Var
  S: TSearchRec;
  ZC: TZCompressionStream;
  Fs, Fd: TFileStream;
  iFileName, oFileName: string;
  rName: String;

/// Автоматическое формирование файлов haarcascade.rc и
/// haarcascade.inc убрано так как порядок файлов на диске
/// не соответвует порядку TocvHaarCascadeType
///
///  Надо корректировать вручную

begin
  try
    if FindFirst('*.xml', faAnyFile, S) = 0 then
      repeat
        iFileName := S.Name;
        oFileName := S.Name + '.z';
        Fs := TFileStream.Create(iFileName, fmOpenRead);
        Fd := TFileStream.Create(oFileName, fmCreate);
        ZC := TZCompressionStream.Create(clMax, Fd);
        try
          WriteLn(S.Size:8, ' ', S.Name);
          ZC.CopyFrom(Fs, S.Size);
          rName := CreateResourceName(S.Name);
        finally
          ZC.Free;
          Fs.Free;
          Fd.Free;
        end;
      until FindNext(S) <> 0;
    FindClose(S);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
