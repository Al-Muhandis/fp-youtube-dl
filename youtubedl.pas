unit youtubedl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, eventlog, fgl
  ;

type

  { TFormatObject }
  { Output format object }
  TFormatObject = class
  private
    FExtension: String;
    FSelector: Integer;
  public
    { selector number }
    property Selector: Integer read FSelector write FSelector;
    { filename extension}
    property Extension: String read FExtension write FExtension;
  end;

  TFormatList =  class(specialize TFPGObjectList<TFormatObject>)

  end;

  { TYoutubeDL }

  TYoutubeDL = class
  private
    FDestFile: String;
    FHTTPProxyHost: String;
    FHTTPProxyPassword: String;
    FHTTPProxyPort: Word;
    FHTTPProxyUsername: String;
    FLibPath: String;
    FLogDebug: Boolean;
    FLogger: TEventLog;
    FOnlyFormats: Boolean;
    FOptions: TStrings;
    FOutputTemplate: String;
    FUrl: String;
    procedure DoLog(aEventType: TEventType; const aMessage: String);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOptions(AValue: TStrings);
    procedure SetUrl(AValue: String);
    function InternalExecute: Boolean;
    function ExtractDestFileName(const aOutput: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    function Download(const aUrl: String = ''): Boolean; deprecated;
    function Execute(const aUrl: String = ''): Boolean;
    property DestFile: String read FDestFile write FDestFile;
    property LibPath: String read FLibPath write FLibPath;
    property LogDebug: Boolean read FLogDebug write FLogDebug;
    property Logger: TEventLog read FLogger write SetLogger;
    property Url: String read FUrl write SetUrl;
    property OnlyFormats: Boolean read FOnlyFormats write FOnlyFormats;
    property Options: TStrings read FOptions write SetOptions;
    property OutputTemplate: String read FOutputTemplate write FOutputTemplate;
    property HTTPProxyHost: String read FHTTPProxyHost write FHTTPProxyHost;
    property HTTPProxyPort: Word read FHTTPProxyPort write FHTTPProxyPort; 
    property HTTPProxyUsername: String read FHTTPProxyUsername write FHTTPProxyUsername;
    property HTTPProxyPassword: String read FHTTPProxyPassword write FHTTPProxyPassword;
  end;

implementation

uses
  strutils
  ;

function ExtractBetweenKeys(const ASource, Key1, Key2: String; out ADest: String): Boolean;
var
  AStart, AnEnd: Integer;
begin
  Result := False;
  AStart := Pos(Key1, ASource);
  if AStart <> 0 then
  begin
    Inc(AStart, Length(Key1));
    AnEnd := PosEx(Key2, ASource, AStart);
    Result := AnEnd <> 0;
    if Result then
      ADest := copy(ASource, AStart, AnEnd - AStart);
  end
end;

{ TYoutubeDL }

procedure TYoutubeDL.SetUrl(AValue: String);
begin
  if FUrl=AValue then Exit;
  FUrl:=AValue;
end;

procedure TYoutubeDL.DoLog(aEventType: TEventType; const aMessage: String);
begin
  if Assigned(FLogger) and (FLogDebug or (aEventType<>etDebug)) then
    FLogger.Log(aEventType, aMessage);
end;

procedure TYoutubeDL.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

procedure TYoutubeDL.SetOptions(AValue: TStrings);
begin
  FOptions.Assign(AValue);
end;

function TYoutubeDL.Download(const aUrl: String): Boolean;
begin
  Result:=Execute(aUrl);
end;

function TYoutubeDL.Execute(const aUrl: String): Boolean;
begin
  if aUrl<> EmptyStr then
    FUrl:=aUrl;
  Result:=InternalExecute;
end;

function TYoutubeDL.InternalExecute: Boolean;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  aProcess: TProcess;
  aOutputStream: TMemoryStream;
  aStringStream: TStringStream;
  aBuffer       : array[1..BUF_SIZE] of byte;
  aBytesRead: LongInt;
  aProxyString, aOutput: String;
begin 
  Result:=False;
  try
    aProcess:=TProcess.Create(nil);
    try
      aProcess.Executable:={$IFDEF MSWINDOWS}'youtube-dl.exe'{$ENDIF}{$IFDEF UNIX}FLibPath+'youtube-dl'{$ENDIF};
      aProcess.Parameters.AddStrings(FOptions);
      if FHTTPProxyHost<>EmptyStr then
      begin
        aProxyString:=FHTTPProxyHost+':'+IntToStr(FHTTPProxyPort);
        if FHTTPProxyUsername<>EmptyStr then
          aProxyString:=FHTTPProxyUsername+':'+FHTTPProxyPassword+'@'+aProxyString;
        aProxyString:='https://'+aProxyString;
        aProcess.Parameters.Add('--proxy');
        aProcess.Parameters.Add(aProxyString);
      end;
      if FOutputTemplate<>EmptyStr then
      begin
        aProcess.Parameters.Add('-o'); 
        aProcess.Parameters.Add(FOutputTemplate);
      end;
      if FOnlyFormats then
        aProcess.Parameters.Add('-F');
      aProcess.Parameters.Add(FUrl);
      aProcess.Options:=[poUsePipes, poStderrToOutPut, poNoConsole];
      DoLog(etDebug, 'Start of downloading url '+FUrl);
      aProcess.Execute;
      aOutputStream := TMemoryStream.Create;

      repeat
        aBytesRead := aProcess.Output.Read(aBuffer{%H-}, BUF_SIZE);
        aOutputStream.Write(aBuffer, aBytesRead)
      until aBytesRead = 0;

      aStringStream:=TStringStream.Create(EmptyStr);
      try
        aOutputStream.SaveToStream(aStringStream);
        FDestFile:=ExtractDestFileName(aStringStream.DataString);
        if FDestFile=EmptyStr then
          DoLog(etError, 'Destination file is not determined in the output buffer');
      finally
        aStringStream.Free;
      end;
      aOutput:='~youtube-dl.txt';
      with TFileStream.Create(aOutput, fmCreate) do
      begin
        aOutputStream.Position := 0;
        CopyFrom(aOutputStream, aOutputStream.Size);
        Free
      end;
      Result:=aProcess.ExitStatus=0;
      DoLog(etDebug, 'End of downloading');
      DoLog(etInfo, 'ExitStatus: '+aProcess.ExitStatus.ToString); 
      DoLog(etInfo, 'ExitCode: '+aProcess.ExitCode.ToString);
      if not Result then
        DoLog(etError, 'Youtube-dl error is occured while downloading! See '+aOutput);
      aOutputStream.Free;
    finally
      aProcess.Free;
    end;
  except
    on E: Exception do
      DoLog(etError, E.ClassName+': '+E.Message);
  end;
end;

function TYoutubeDL.ExtractDestFileName(const aOutput: String): String;
const
  STARTKEY1='[download] Destination:';
  STARTKEY2='[download] ';
  ENDKEY1=#10;
  ENDKEY2=' has already been downloaded';
begin
  if not ExtractBetweenKeys(aOutput, STARTKEY1, ENDKEY1, Result) then
    if not ExtractBetweenKeys(aOutput, STARTKEY2, ENDKEY2, Result) then
      Exit(EmptyStr);
  Result:=Trim(Result);
end;

constructor TYoutubeDL.Create;
begin
  FOptions:=TStringList.Create;
  {$IFDEF UNIX}
  FLibPath:='/usr/local/bin/'{$ENDIF};
end;

destructor TYoutubeDL.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

end.

