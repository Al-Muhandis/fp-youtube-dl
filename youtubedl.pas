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
    FHeight: Word;
    FResolution: String;
    FCode: Integer;
    FWidth: Word;
  public
    { Format code }
    property Code: Integer read FCode write FCode;
    { filename extension}
    property Extension: String read FExtension write FExtension;
    property Resolution: String read FResolution write FResolution;  
    property Width: Word read FWidth write FWidth;
    property Height: Word read FHeight write FHeight;
  end;

  TFormatList =  class(specialize TFPGObjectList<TFormatObject>)

  end;

  { TYoutubeDL }

  TYoutubeDL = class
  private
    FDestFile: String;
    FFormat: String;
    FFormats: TFormatList;
    FHTTPProxyHost: String;
    FHTTPProxyPassword: String;
    FHTTPProxyPort: Word;
    FHTTPProxyUsername: String;
    FLibPath: String;
    FLogDebug: Boolean;
    FLogger: TEventLog;
    FMediaID: String;
    FOnlyFormats: Boolean;
    FOptions: TStrings;
    FOutputTemplate: String;
    FUrl: String;
    procedure DoLog(aEventType: TEventType; const aMessage: String);
    function GetFormats: TFormatList;
    function InternalExecute: Boolean;
    function ExtractDestFileName(const aOutput: String): String;
    function ParseFormats(const aOutput: String): Boolean;
    procedure SetOptions(AValue: TStrings);
    procedure SetUrl(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    function Download(const aUrl: String = ''): Boolean; deprecated;
    function Execute(const aUrl: String = ''): Boolean;
    property DestFile: String read FDestFile write FDestFile;
    property Format: String read FFormat write FFormat;
    { Nil by default. It is filled in the case of OnlyFormats }
    property Formats: TFormatList read GetFormats;
    property LibPath: String read FLibPath write FLibPath;
    property LogDebug: Boolean read FLogDebug write FLogDebug;
    property Logger: TEventLog read FLogger write FLogger;
    property MediaID: String read FMediaID write FMediaID;
    property Url: String read FUrl write SetUrl;
    property OnlyFormats: Boolean read FOnlyFormats write FOnlyFormats;
    property Options: TStrings read FOptions write SetOptions;
    property OutputTemplate: String read FOutputTemplate write FOutputTemplate;
    property HTTPProxyHost: String read FHTTPProxyHost write FHTTPProxyHost;
    property HTTPProxyPort: Word read FHTTPProxyPort write FHTTPProxyPort; 
    property HTTPProxyUsername: String read FHTTPProxyUsername write FHTTPProxyUsername;
    property HTTPProxyPassword: String read FHTTPProxyPassword write FHTTPProxyPassword;
  end;

var
  _YoutubeDLExec: String;

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

function ParseFormat(aLine: String; out aCode: Integer; out aExtension: String; out aResolution: String): Boolean;
const
  FORMATCODE_WIDTH=13;
  EXTENSION_WIDTH=11;
  COLs2_WIDTH=FORMATCODE_WIDTH+EXTENSION_WIDTH;
begin
  if Length(aLine)<COLs2_WIDTH then
    Exit(False);
  if not TryStrToInt(Trim(Copy(aLine, 0, FORMATCODE_WIDTH)), aCode) then
    Exit(False);
  aExtension:=Trim(Copy(aLine, FORMATCODE_WIDTH+1, EXTENSION_WIDTH));
  aResolution:=Trim(Copy(aLine, COLs2_WIDTH, Length(aLine)-COLs2_WIDTH+1));
  Result:=True;
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

function TYoutubeDL.GetFormats: TFormatList;
begin
  if not Assigned(FFormats) then
    FFormats:=TFormatList.Create;
  Result:=FFormats;
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
      aProcess.Executable:={$IFDEF UNIX}FLibPath+{$ENDIF}_YoutubeDLExec;
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
      if FFormat<>EmptyStr then
      begin
        aProcess.Parameters.Add('-f');
        aProcess.Parameters.Add(FFormat);
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
        if not FOnlyFormats then
        begin
          FDestFile:=ExtractDestFileName(aStringStream.DataString);
          if FDestFile=EmptyStr then
            DoLog(etError, 'Destination file is not determined in the output buffer');
        end
        else
          if not ParseFormats(aStringStream.DataString) then
            DoLog(etError, 'Can''t parse formats');
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
      Result:=aProcess.ExitCode=0;
      DoLog(etInfo, 'Exit. Status: '+aProcess.ExitStatus.ToString+'. Code: '+aProcess.ExitCode.ToString);
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

function TYoutubeDL.ParseFormats(const aOutput: String): Boolean;
const
  KEY1='[info] Available formats for';
  KEY2='format code  extension  resolution note';

  KEY_AUDIO='audio only';
var
  i, j: SizeInt;
  aStrings: TStringList;
  aFormat: TFormatObject;
  aResolution, aExtension, aRes: String;
  aCode: Integer;
begin
  Result:=False;
  Formats.Clear;
  aStrings:=TStringList.Create;
  try
    aStrings.Text:=aOutput;
    j:=-1;
    for i:=0 to aStrings.Count-1 do
      if AnsiStartsStr(KEY1, aStrings[i]) then
      begin
        FMediaID:=EmptyStr;
        ExtractBetweenKeys(aStrings[i], KEY1, ':', FMediaID);
        FMediaID:=Trim(FMediaID);
        if FMediaID=EmptyStr then
          DoLog(etError, 'Can''t retrieve media ID');
        j:=i;
        Break;
      end;
    if j=-1 then
      Exit;
    Inc(j);
    if not AnsiStartsStr(KEY2, aStrings[j]) then
      Exit;
    Result:=True;
    Inc(j);
    for i:=j to aStrings.Count-1 do
    begin
      if not ParseFormat(aStrings[i], aCode, aExtension, aResolution) then
        Break;
      aFormat:=TFormatObject.Create;
      aFormat.Code:=aCode;
      aFormat.Extension:=aExtension;
      aFormat.Resolution:=aResolution;
      if not AnsiStartsStr(KEY_AUDIO, aFormat.Resolution) then
      begin
        aRes:=ExtractDelimited(1, aFormat.Resolution, [' ']);
        if aRes<>EmptyStr then
        begin
          aFormat.Width:=StrToIntDef(ExtractDelimited(1, aRes, ['x']), 0);
          aFormat.Height:=StrToIntDef(ExtractDelimited(2, aRes, ['x']), 0);
        end;
      end;
      FFormats.Add(aFormat);
    end;
  finally
    aStrings.Free;
  end;
end;

constructor TYoutubeDL.Create;
begin
  FOptions:=TStringList.Create;
  {$IFDEF UNIX}
  FLibPath:='/usr/local/bin/'{$ENDIF};
end;

destructor TYoutubeDL.Destroy;
begin
  FFormats.Free;
  FOptions.Free;
  inherited Destroy;
end;

initialization
  _YoutubeDLExec:={$IFDEF MSWINDOWS}'youtube-dl.exe'{$ENDIF}{$IFDEF UNIX}'youtube-dl'{$ENDIF}

end.

