unit DCUnit.CommandLine;

interface

uses
  Delphi.CommandLineParser;

type
  TCompressionLevel = (Store, Fastest, Fast, Normal, Maximum, Ultra);

  TDirectoryCompressLineOptions = class(TObject)
  strict private
    FSourceRoot: string;
    FDestinationRoot: string;
    FCompressionLevel: TCompressionLevel;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('DestinationRoot'), CLPDescription('Destination Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property DestinationRoot: string read FDestinationRoot write FDestinationRoot;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra>'), CLPDefault('Ultra')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel; 
  end;

  function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;

implementation

function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;
var
  LParser: ICommandLineParser;
begin
  LParser := CreateCommandLineParser;

  Result := LParser.Parse(ACommandlineOptions);

  if not Result then
  begin
    DefaultUsageConsoleOutput(LParser);

{$IFDEF DEBUG}
    WaitAnyKey;
{$ENDIF}
  end;
end;


end.
