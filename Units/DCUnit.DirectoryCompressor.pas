unit DCUnit.DirectoryCompressor;

interface

uses
  System.Classes, System.Diagnostics, System.IOUtils, System.SyncObjs, DCUnit.CommandLine,
  OtlParallel, OtlCommon, OtlCollections, OtlTask;

type
  TDirectoryCompressor = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
    FRunningTasks: Boolean;
    FCommandLineOptions: TDirectoryCompressLineOptions;
    FTaksStarted: Int64;
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure LockingWriteLn(const ALine: string);
    procedure CompressFile(const ACurrentDirectoryName, ADestinationRoot: string);
    procedure FilterByDirectories(const ADirectories: TStringList; const ADestinationRootDirectory: string);
  public
    constructor Create(const ACommandLineOptions: TDirectoryCompressLineOptions);
    destructor Destroy; override;

    procedure Execute;
  end;

implementation

uses
  Winapi.Windows, System.Math, System.SysUtils, DCUnit.Utils;

function TDirectoryCompressor.Lock: Boolean;
begin
  Result := False;

  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;

  Result := True;
end;

procedure TDirectoryCompressor.Unlock;
begin
  FCriticalSection.Release;
end;

procedure TDirectoryCompressor.LockingWriteLn(const ALine: string);
begin
  if Lock then
  try
    WriteLn(ALine);
  finally
    Unlock;
  end;
end;

procedure TDirectoryCompressor.Execute;
var
  LDirectories: TStringList;
begin
  LDirectories := TStringList.Create;
  try
    LDirectories.AddStrings(TDirectory.GetDirectories(FCommandLineOptions.SourceRoot, '*.*', TSearchOption.soTopDirectoryOnly));

    FilterByDirectories(LDirectories, FCommandLineOptions.DestinationRoot);

    if LDirectories.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LDirectories)
        .NumTasks(GetMaxThreadCount)
        .OnStop(
          procedure
          begin
            if Lock then
            try
              FRunningTasks := False;
            finally
              Unlock;
            end;
          end)
        .NoWait
        .Execute(
          procedure(const ADirectoryName: TOmniValue)
          var
            LCurrentDirectoryName: string;
          begin
            LCurrentDirectoryName := ADirectoryName;

            CompressFile(LCurrentDirectoryName, FCommandLineOptions.DestinationRoot);
          end
      );

      while True do
      begin
        Sleep(200);
        ProcessMessages;

        if Lock then
        try
          if not FRunningTasks then
            Break;
        finally
          Unlock;
        end;
      end;

      LStopWatch.Stop;
      LockingWriteLn(' Elapsed time: ' + LStopWatch.Elapsed.ToString);
    end
    else
    begin
      LockingWriteLn('No files found from directory "' + FCommandLineOptions.SourceRoot);

      Exit;
    end;
  finally
    LDirectories.Free;
  end;
end;

constructor TDirectoryCompressor.Create(const ACommandLineOptions: TDirectoryCompressLineOptions);
begin
  inherited Create;

  FCriticalSection := TCriticalSection.Create;
  FCommandLineOptions := ACommandLineOptions;

  { Need one or more calls to stabilize, it seems....
    So we warm CPU usage code up }
  for var LIndex := 1 to 5 do
  begin
    TotalCpuUsagePercentage;
    Sleep(Random(100));
  end;
end;

destructor TDirectoryCompressor.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

procedure TDirectoryCompressor.CompressFile(const ACurrentDirectoryName, ADestinationRoot: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LDestinationDirName: string;
  LDestinationRoot: string;
  LCommandLine: string;
begin
  LDestinationDirName := GetFileNameOnly(GetFileNameWithFilter(IncludeTrailingPathDelimiter(ACurrentDirectoryName),
    FCommandLineOptions.DestinationFileNameFilter));

  if LDestinationDirName.IsEmpty then
    Exit;

  LDestinationRoot := IncludeTrailingPathDelimiter(ADestinationRoot + LDestinationDirName);

  if Lock then
  try
    LCommandLine := EXE_7Z.QuotedString('"') + ' ' + 'a -r'
      + GetCompressionCommandlineOptions(FCommandLineOptions.CompressionLevel) + '-v1000m '
      + '"' + LDestinationRoot + LDestinationDirName + '.7z" "'
      + IncludeTrailingPathDelimiter(ACurrentDirectoryName) + '*.*' +  '"';

    WriteLn('Executing: ' + LCommandLine + '...');

    Inc(FTaksStarted);
  finally
    Unlock
  end;

  WaitForSystemStatus(IfThen(FTaksStarted <= 1, 333, 10 * 666), 76.66, 76.66);

  if not DirectoryExists(LDestinationRoot) then
    ForceDirectories(LDestinationRoot);

  ExecuteAndWait(LCommandLine, fcpcIdle);
end;

procedure TDirectoryCompressor.FilterByDirectories(const ADirectories: TStringList; const ADestinationRootDirectory: string);
var
  LIndex: Integer;
  LCurrentDirectory: string;
  LLDestinationDirName: string;
  LDestinationDir: string;
begin
  for LIndex := ADirectories.Count - 1 downto 0 do
  begin
    LCurrentDirectory := ADirectories[LIndex];
    LLDestinationDirName := GetFileNameOnly(GetFileNameWithFilter(LCurrentDirectory, FCommandLineOptions.DestinationFileNameFilter));

    if LLDestinationDirName.IsEmpty then
    begin
      ADirectories.Delete(LIndex);

      Continue;
    end;

    LDestinationDir := ADestinationRootDirectory + LLDestinationDirName;

    if not DirEmpty(LDestinationDir) then
    begin
      WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));

      ADirectories.Delete(LIndex);
    end;
  end;
end;


end.
