unit DCUnit.CommandLine;

interface

uses
  Delphi.CommandLineParser, DCUnit.Utils;

type
  TDirectoryCompressLineOptions = class(TObject)
  strict private
    FSourceRoot: string;
    FDestinationRoot: string;
    FDestinationFileNameFilter: string;
    FCompressionLevel: TCompressionLevel;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('DestinationFileNameFilter'), CLPDescription('Filter (like *..vbox) to take name, from source root, for destination dir and filename ', '<directory>'), CLPDefault(''), CLPRequired]
    property DestinationFileNameFilter: string read FDestinationFileNameFilter write FDestinationFileNameFilter;

    [CLPLongName('DestinationRoot'), CLPDescription('Destination Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property DestinationRoot: string read FDestinationRoot write FDestinationRoot;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra>'), CLPDefault('Ultra')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;
  end;

  function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;

implementation

uses
  DCUnit.DirectoryCompressor;

function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;
var
  LParser: ICommandLineParser;
begin
  LParser := CreateCommandLineParser;

  Result := LParser.Parse(ACommandlineOptions);

  if not Result then
    DefaultUsageConsoleOutput(LParser);
end;


end.
