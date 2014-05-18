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
  S_rc: TStringList;
  S_inc: TStringList;
  iFileName, oFileName: string;
  rName: String;

begin
  try
    S_rc := TStringList.Create;
    S_rc.Add('// Created uCompressHaar.exe');
    S_inc := TStringList.Create;
    S_inc.Add('// Created uCompressHaar.exe');
    S_inc.Add('const');
    S_inc.Add('FrontalFaceXML: array [TocvHaarCascadeType] of TocvHaarCascadeRecord =');
    S_inc.Add('(');
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
          S_rc.Add(rName + ' RCDATA "' + S.Name + '.z"');
          S_inc.Add(Format('(Name: ''%s''; FileName: ''%s''),', [rName, S.Name]));
        finally
          ZC.Free;
          Fs.Free;
          Fd.Free;
        end;
      until FindNext(S) <> 0;
    FindClose(S);
    S_rc.SaveToFile('haarcascade.rc');
    rName := S_inc[S_inc.Count - 1];
    Delete(rName, Length(rName), 1);
    S_inc[S_inc.Count - 1] := rName;
    S_inc.Add(');');
    S_inc.SaveToFile('haarcascade.inc');
    S_rc.Free;
    S_inc.Free;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
