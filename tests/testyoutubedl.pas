unit testyoutubedl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, IniFiles, youtubedl, eventlog
  ;

type

  { TTestBase }

  TTestBase=class(TTestCase)
  private
    FIni: TMemIniFile;
    FLogger: TEventLog;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TTestYoutubeDL }

  TTestYoutubeDL=class(TTestBase)
  private
    FYoutubeDl: TYoutubeDL;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure YoutubeDownloadCommon;
  published
    procedure YoutubeDownload;
    procedure YoutubeFormats;
  end;

var
  AppDir: String;

implementation

{ TTestBase }

procedure TTestBase.SetUp;
begin
  inherited SetUp;
  FIni:=TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  FLogger:=TEventLog.Create(nil);
  FLogger.LogType:=ltFile;
  FLogger.FileName:=ChangeFileExt(ParamStr(0), '.log');
end;

procedure TTestBase.TearDown;
begin
  FLogger.Free;
  FIni.Free;
  inherited TearDown;
end;

{ TTestYoutube }

procedure TTestYoutubeDL.SetUp;
begin
  inherited SetUp;
  FYoutubeDl:=TYoutubeDL.Create;
  FYoutubeDl.Logger:=FLogger;
end;

procedure TTestYoutubeDL.TearDown;
begin
  FYoutubeDl.Free;
  inherited TearDown;
end;

procedure TTestYoutubeDL.YoutubeDownloadCommon;
begin
  //FYoutubeDl.Options.Add('--verbose');
  FYoutubeDl.LogDebug:=True;
  FYoutubeDl.Url:=FIni.ReadString('YoutubeDL', 'Url', 'https://www.youtube.com/watch?v=7PuvsyLapgw');
  FYoutubeDl.HTTPProxyHost:=FIni.ReadString('Proxy', 'Host', EmptyStr);
  FYoutubeDl.HTTPProxyPort:=FIni.ReadInteger('Proxy', 'Port', 0);
  FYoutubeDl.HTTPProxyUsername:=FIni.ReadString('Proxy', 'Username', EmptyStr);
  FYoutubeDl.HTTPProxyPassword:=FIni.ReadString('Proxy', 'Password', EmptyStr); 
  AssertTrue('Failed to parse!', FYoutubeDl.Execute);
end;

procedure TTestYoutubeDL.YoutubeDownload;
begin
  YoutubeDownloadCommon;
  AssertNotNull('The path to the output file could not be determined!', FYoutubeDl.DestFile);
end;

procedure TTestYoutubeDL.YoutubeFormats;
begin
  FYoutubeDl.OnlyFormats:=True;
  YoutubeDownloadCommon;  
  AssertTrue('Failed to parse the formats', FYoutubeDl.Formats.Count>0);
end;

initialization

  RegisterTest(TTestYoutubeDL);
  AppDir:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

end.

