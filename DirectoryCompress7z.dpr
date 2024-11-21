program DirectoryCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.IOUtils,
  System.Math,
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.Diagnostics,
  OtlParallel,
  OtlCommon,
  OtlCollections,
  OtlTask,
  DCUnit.CommandLine in 'Units\DCUnit.CommandLine.pas',
  DirectoryCompress7z.Utils in 'Units\DirectoryCompress7z.Utils.pas',
  DirectoryCompress7z.Consts in 'Units\DirectoryCompress7z.Consts.pas';

type
  TDirectoryCompress7z = class(TObject)
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

function TDirectoryCompress7z.Lock: Boolean;
begin
  Result := False;

  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;

  Result := True;
end;

procedure TDirectoryCompress7z.Unlock;
begin
  FCriticalSection.Release;
end;

procedure TDirectoryCompress7z.LockingWriteLn(const ALine: string);
begin

  if Lock then
  try
    WriteLn(ALine);
  finally
    Unlock;
  end;
end;

procedure TDirectoryCompress7z.Execute;
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

constructor TDirectoryCompress7z.Create(const ACommandLineOptions: TDirectoryCompressLineOptions);
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

destructor TDirectoryCompress7z.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

procedure TDirectoryCompress7z.CompressFile(const ACurrentDirectoryName, ADestinationRoot: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LTargetArchiveName: string;
  LDestinationDir: string;
  LCommandLine: string;
begin
  LTargetArchiveName := GetFileNameOnly(ACurrentDirectoryName);
  LDestinationDir := ADestinationRoot + LTargetArchiveName;

  if Lock then
  try
    LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md1024m -mfb256 -mmt=off -v1000m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LTargetArchiveName + '.7z" "'
      + ADestinationRoot + ACurrentDirectoryName + '"';

    WriteLn('Executing: ' + LCommandLine + '...');
  finally
    Unlock
  end;

  WaitForSystemStatus(IfThen(TInterlocked.Read(FTaksStarted) = 0, 100, 4000), 76.66, 76.66);
  TInterlocked.Add(FTaksStarted, 1);

  ExecuteAndWait(LCommandLine, fcpcIdle);
end;

procedure TDirectoryCompress7z.FilterByDirectories(const ADirectories: TStringList; const ADestinationRootDirectory: string);
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
      Continue;

    LDestinationDir := ADestinationRootDirectory + LLDestinationDirName;

    if not DirectoryExists(LDestinationDir) then
      ForceDirectories(LDestinationDir)
    else if not DirEmpty(LDestinationDir) then
    begin
      WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));

      ADirectories.Delete(LIndex);
    end;
  end;
end;

// main program body
var
  LDirectoryCompress: TDirectoryCompress7z;
begin
  var LCommandLineOptions := TDirectoryCompressLineOptions.Create;
  try
    if not ParseCommandLine(LCommandLineOptions) then
    begin
      ExitCode := EXIT_CODE_ERROR_IN_COMMANDLINE_PARAMS;

      Exit;
    end;

    if DirectoryExists(LCommandLineOptions.SourceRoot) then
    begin
      LDirectoryCompress := TDirectoryCompress7z.Create(LCommandLineOptions);
      try
        LDirectoryCompress.Execute;
      finally
        LDirectoryCompress.Free;
      end;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
