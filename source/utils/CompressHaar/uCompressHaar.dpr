program uCompressHaar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Winapi.Windows,
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

function CreateTocvHaarCascadeTypeName(const FileName: String): string;
Var
  i: Integer;
begin
  Result := StringReplace(FileName, 'haarcascade', 'hc', [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '.xml', '', [rfIgnoreCase, rfReplaceAll]);
  While Pos('_', Result) > 0 do
  begin
    i := Pos('_', Result);
    Delete(Result, i, 1);
    Result[i] := UpCase(Result[i]);
  end;
end;

function FileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  Result := -1;

  if NOT GetFileAttributesEx(PWideChar(aFilename), GetFileExInfoStandard, @info) then
    EXIT;

  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

Var
  S: TSearchRec;
  ZC: TZCompressionStream;
  Fs, Fd: TFileStream;
  iFileName, oFileName: string;
  rName: String;

  haarcascade_rc: TStringList;
  haarcascade_inc: TStringList;
  ocvHaarCascadeType: TStringList;

  /// Автоматическое формирование файлов haarcascade.rc и
  /// haarcascade.inc убрано так как порядок файлов на диске
  /// не соответвует порядку TocvHaarCascadeType
  ///
  /// Надо корректировать вручную

begin
  try
    if FindFirst('*.xml', faAnyFile, S) = 0 then
    begin
      haarcascade_rc := TStringList.Create;
      haarcascade_rc.Add('// Created uCompressHaar.exe');

      haarcascade_inc := TStringList.Create;
      haarcascade_inc.Add('// Created uCompressHaar.exe');
      haarcascade_inc.Add('');
      haarcascade_inc.Add('Type');
      haarcascade_inc.Add('  TocvHaarCascadeRecord = record');
      haarcascade_inc.Add('    Name: String;');
      haarcascade_inc.Add('    FileName: String;');
      haarcascade_inc.Add('  end;');
      haarcascade_inc.Add('');
      haarcascade_inc.Add('const');
      haarcascade_inc.Add('CascadeRecourse: array [TocvHaarCascadeType] of TocvHaarCascadeRecord =');
      haarcascade_inc.Add('(');

      ocvHaarCascadeType := TStringList.Create;
      ocvHaarCascadeType.Add('// Created uCompressHaar.exe');
      ocvHaarCascadeType.Add('// Haar cascade types');
      ocvHaarCascadeType.Add('  TocvHaarCascadeType =');
      ocvHaarCascadeType.Add('  (');

      repeat
        iFileName := S.Name;
        oFileName := S.Name + '.z';
        Fs := TFileStream.Create(iFileName, fmOpenRead);
        Fd := TFileStream.Create(oFileName, fmCreate);
        ZC := TZCompressionStream.Create(clMax, Fd);
        try

          ZC.CopyFrom(Fs, S.Size);

          rName := CreateResourceName(S.Name);
          haarcascade_rc.Add(rName + ' RCDATA "' + oFileName + '"');

          if haarcascade_inc.Count > 11 then
            haarcascade_inc[haarcascade_inc.Count - 1] := haarcascade_inc[haarcascade_inc.Count - 1] + ',';
          haarcascade_inc.Add('(Name: ' + QuotedStr(rName) + '; FileName: ' + QuotedStr(S.Name) + ')');

          rName := CreateTocvHaarCascadeTypeName(S.Name);
          if ocvHaarCascadeType.Count > 4 then
            ocvHaarCascadeType[ocvHaarCascadeType.Count - 1] := ocvHaarCascadeType[ocvHaarCascadeType.Count - 1] + ',';
          ocvHaarCascadeType.Add(rName);

        finally
          ZC.Free;
          Fs.Free;
          Fd.Free;
          WriteLn(S.Size:8, FileSize(oFileName):8, ' ', S.Name);
        end;
      until FindNext(S) <> 0;
      FindClose(S);

      haarcascade_inc.Add(');');
      ocvHaarCascadeType.Add(');');

      haarcascade_rc.SaveToFile('haarcascade.rc');
      haarcascade_inc.SaveToFile('haarcascade.inc');
      ocvHaarCascadeType.SaveToFile('ocvHaarCascadeType.inc');

      haarcascade_rc.Free;
      haarcascade_inc.Free;
      ocvHaarCascadeType.Free;

      if FileExists('haarcascade.RES') then
        DeleteFile('haarcascade.RES');

    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
