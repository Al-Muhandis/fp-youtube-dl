program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  opensslsockets,
  eventlog,
  Process;

const
  Target: string = 'src';
  Dependencies: array of string = ();

type
  Output = record
    Success: boolean;
    Output: string;
  end;

  procedure OutLog(const Knd: TEventType; const Msg: string);
  begin
    case Knd of
      etError: Writeln(stderr, #27'[31m', Msg, #27'[0m');
      etInfo: Writeln(stderr, #27'[32m', Msg, #27'[0m');
      etDebug: Writeln(stderr, #27'[33m', Msg, #27'[0m');
    end;
  end;

  function CheckModules: string;
  begin
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result) then
        OutLog(etInfo, Result)
      else
        OutLog(etError, Result);
  end;

  function AddPackage(const Path: string): string;
  begin
    if RunCommand('lazbuild', ['--add-package-link', Path], Result) then
       OutLog(etDebug, 'Add package:'#9 + Path);
  end;

  function SelectString(const Input, Reg: string): string;
  var
    Line: string;
  begin
    Result := EmptyStr;
    for Line in Input.Split(LineEnding) do
      with TRegExpr.Create do
      begin
        Expression := Reg;
        if Exec(Line) then
          Result += Line + LineEnding;
        Free;
      end;
  end;

  function RunTest(const Path: String): string;
  begin
    OutLog(etDebug, #9'run:'#9 + Path);
    if RunCommand(Path, ['--all', '--format=plain'], Result) then
      OutLog(etInfo, #9'success!')
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function BuildProject(const Path: string): Output;
  begin
    OutLog(etDebug, 'Build from:'#9 + Path);
    Result.Success := RunCommand('lazbuild',
      ['--build-all', '--recursive', '--no-write-project', Path], Result.Output);
    Result.Output := SelectString(Result.Output, '(Fatal:|Error:|Linking)');
    if Result.Success then
    begin
      Result.Output := Result.Output.Split(' ')[2].Replace(LineEnding, EmptyStr);
      OutLog(etInfo, #9'to:'#9 + Result.Output);
      if ContainsStr(ReadFileToString(Path.Replace('.lpi', '.lpr')), 'consoletestrunner') then
        RunTest(Result.Output);
    end
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result.Output);
    end;
  end;

  function DownloadFile(const Uri: string): string;
  var
    OutFile: TStream;
  begin
    InitSSLInterface;
    Result := GetTempFileName;
    OutFile := TFileStream.Create(Result, fmCreate or fmOpenWrite);
    with TFPHttpClient.Create(nil) do
    begin
      try
        AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        AllowRedirect := True;
        Get(Uri, OutFile);
        OutLog(etDebug, 'Download from ' + Uri + ' to ' + Result);
      finally
        Free;
        OutFile.Free;
      end;
    end;
  end;

  procedure UnZip(const ZipFile, ZipPath: string);
  begin
    with TUnZipper.Create do
    begin
      try
        FileName := ZipFile;
        OutputPath := ZipPath;
        Examine;
        UnZipAllFiles;
        OutLog(etDebug, 'Unzip from'#9 + ZipFile + #9'to'#9 + ZipPath);
        DeleteFile(ZipFile);
      finally
        Free;
      end;
    end;
  end;

  function InstallOPM(const Path: string): string;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Path;
    if not DirectoryExists(Result) then
    begin
      CreateDir(Result);
      UnZip(DownloadFile('https://packages.lazarus-ide.org/' + Path + '.zip'), Result);
    end;
  end;

  function BuildAll: string;
  var
    List: TStringList;
  begin
    CheckModules;
    List := FindAllFiles(GetCurrentDir, '*.lpk', True);
    try
      for Result in Dependencies do
        List.AddStrings(FindAllFiles(InstallOPM(Result), '*.lpk', True));
      for Result in List do
        AddPackage(Result);
      List := FindAllFiles(Target, '*.lpi', True);
      for Result in List do
        BuildProject(Result);
    finally
      List.Free;
    end;
    case ExitCode of
      0: OutLog(etInfo, 'Errors:'#9 + IntToStr(ExitCode));
      else
        OutLog(etError, 'Errors:'#9 + IntToStr(ExitCode));
    end;
  end;

begin
  try
    BuildAll
  except
    on E: Exception do
      Writeln(E.ClassName, #9, E.Message);
  end;
end.
