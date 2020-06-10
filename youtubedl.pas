unit youtubedl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, eventlog
  ;

type

  { TYoutubeDL }

  TYoutubeDL = class
  private
    FHTTPProxyHost: String;
    FHTTPProxyPassword: String;
    FHTTPProxyPort: Word;
    FHTTPProxyUsername: String;
    FLogDebug: Boolean;
    FLogger: TEventLog;
    FOptions: TStrings;
    FOutputTemplate: String;
    FUrl: String;
    procedure DoLog(aEventType: TEventType; const aMessage: String);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOptions(AValue: TStrings);
    procedure SetUrl(AValue: String);
    function InternalDownload: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Download(const aUrl: String = ''): Boolean;
    property LogDebug: Boolean read FLogDebug write FLogDebug;
    property Logger: TEventLog read FLogger write SetLogger;
    property Url: String read FUrl write SetUrl;   
    property Options: TStrings read FOptions write SetOptions;
    property OutputTemplate: String read FOutputTemplate write FOutputTemplate;
    property HTTPProxyHost: String read FHTTPProxyHost write FHTTPProxyHost;
    property HTTPProxyPort: Word read FHTTPProxyPort write FHTTPProxyPort; 
    property HTTPProxyUsername: String read FHTTPProxyUsername write FHTTPProxyUsername;
    property HTTPProxyPassword: String read FHTTPProxyPassword write FHTTPProxyPassword;

  end;

implementation

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
  if aUrl<> EmptyStr then
    FUrl:=aUrl;
  Result:=InternalDownload;
end;

function TYoutubeDL.InternalDownload: Boolean;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  aProcess: TProcess;
  aOutputStream: TMemoryStream;
  aBuffer       : array[1..BUF_SIZE] of byte;
  aBytesRead: LongInt;
  aProxyString: String;
begin 
  Result:=False;
  try
    aProcess:=TProcess.Create(nil);
    try
      aProcess.Executable:={$IFDEF MSWINDOWS}'youtube-dl.exe'{$ENDIF}{$IFDEF UNIX}'youtube-dl'{$ENDIF};
      aProcess.Parameters.AddStrings(FOptions);
      aProcess.Parameters.Add(FUrl);
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
      aProcess.Options:=[poUsePipes, poStderrToOutPut, poNoConsole];
      DoLog(etDebug, 'Start of downloading url '+FUrl);
      aProcess.Execute;
      aOutputStream := TMemoryStream.Create;

      repeat
        aBytesRead := aProcess.Output.Read(aBuffer{%H-}, BUF_SIZE);
        aOutputStream.Write(aBuffer, aBytesRead)
      until aBytesRead = 0;

      with TFileStream.Create('~youtube-output.txt', fmCreate) do
      begin
        aOutputStream.Position := 0; // Required to make sure all data is copied from the start
        CopyFrom(aOutputStream, aOutputStream.Size);
        Free
      end;
      Result:=aProcess.ExitStatus=0;
      DoLog(etDebug, 'End of downloading');
      DoLog(etInfo, 'ExitStatus: '+aProcess.ExitStatus.ToString);
      DoLog(etError, 'Youtube-dl error is occured while downloading! See '+'~youtube-output.txt');
      aOutputStream.Free;
    finally
      aProcess.Free;
    end;
  except
    on E: Exception do
      DoLog(etError, E.ClassName+': '+E.Message);
  end;
end;

constructor TYoutubeDL.Create;
begin
  FOptions:=TStringList.Create;
end;

destructor TYoutubeDL.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

end.

