program DirectoryCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Types,
  DCUnit.CommandLine in 'Units\DCUnit.CommandLine.pas',
  DCUnit.Utils in 'Units\DCUnit.Utils.pas',
  DCUnit.Consts in 'Units\DCUnit.Consts.pas',
  DCUnit.DirectoryCompressor in 'Units\DCUnit.DirectoryCompressor.pas';

// main program body
var
  LDirectoryCompress: TDirectoryCompressor;
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
      LDirectoryCompress := TDirectoryCompressor.Create(LCommandLineOptions);
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
