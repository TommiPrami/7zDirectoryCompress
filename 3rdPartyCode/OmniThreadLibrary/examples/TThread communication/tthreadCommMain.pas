unit tthreadCommMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlContainerObserver,
  OtlComm;

type
  TfrmTThreadComm = class(TForm)
    btn: TButton;
    lbLog: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure btnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCommandQueue : TOmniMessageQueue;
    FResponseQueue: TOmniMessageQueue;
    FWorker       : TThread;
    procedure HandleThreadMessage(Sender: TObject; const msg: TOmniMessage);
    procedure Query(value: integer);
    procedure StartWorker;
    procedure StopWorker;
  public
  end;

var
  frmTThreadComm: TfrmTThreadComm;

implementation

uses
  SyncObjs;

{$R *.dfm}

type
  TWorker = class(TThread)
  strict private
    FCommandQueue : TOmniMessageQueue;
    FResponseQueue: TOmniMessageQueue;
    FStopEvent    : TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(commandQueue, responseQueue: TOmniMessageQueue);
    destructor Destroy; override;
    procedure Stop;
  end;

{ TfrmTThreadComm }

procedure TfrmTThreadComm.btnClick(Sender: TObject);
begin
  Query(42);
end;

procedure TfrmTThreadComm.Button1Click(Sender: TObject);
begin
  Query(Random(1000));
  Query(Random(1000));
end;

procedure TfrmTThreadComm.Button2Click(Sender: TObject);
var
  th1: TThread;
  th2: TThread;
begin
  th1 := TThread.CreateAnonymousThread(
    procedure
    begin
      Query(Random(1000));
    end);
  th2 := TThread.CreateAnonymousThread(
    procedure
    begin
      Query(Random(1000));
    end);
  th1.Start;
  th2.Start;
end;

procedure TfrmTThreadComm.FormDestroy(Sender: TObject);
begin
  StopWorker;
end;

procedure TfrmTThreadComm.StartWorker;
begin
  FCommandQueue := TOmniMessageQueue.Create(1000);

  FResponseQueue := TOmniMessageQueue.Create(1000, false);
  FResponseQueue.OnMessage := HandleThreadMessage;

  FWorker := TWorker.Create(FCommandQueue, FResponseQueue);
end;

procedure TfrmTThreadComm.StopWorker;
begin
  if assigned(FWorker) then begin
    TWorker(FWorker).Stop;
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;
  FreeAndNil(FResponseQueue);
  FreeAndNil(FCommandQueue);
end;

procedure TfrmTThreadComm.FormCreate(Sender: TObject);
begin
  StartWorker;
end;

procedure TfrmTThreadComm.HandleThreadMessage(Sender: TObject; const msg: TOmniMessage);
begin
  //msg.MsgID is ignored in this demo
  //msg.MsgData contains a string, generated by the worker
  lbLog.ItemIndex := lbLog.Items.Add(msg.MsgData);
end;

procedure TfrmTThreadComm.Query(value: integer);
begin
  if GetCurrentThreadID = MainThreadID then
    lbLog.ItemIndex := lbLog.Items.Add(Format('%d * 2 ?', [value]))
  else
    TThread.Synchronize(nil,
      procedure
      begin
        lbLog.ItemIndex := lbLog.Items.Add(Format('%d * 2 ?', [value]));
      end);

  if not FCommandQueue.Enqueue(TOmniMessage.Create(0 {ignored}, value)) then
    raise Exception.Create('Command queue is full!');
end;

{ TWorker }

constructor TWorker.Create(commandQueue, responseQueue: TOmniMessageQueue);
begin
  inherited Create;
  FCommandQueue := commandQueue;
  FResponseQueue := responseQueue;
  FStopEvent := TEvent.Create;
end;

destructor TWorker.Destroy;
begin
  FreeAndNil(FStopEvent);
  inherited;
end;

procedure TWorker.Execute;
var
  handles: array [0..1] of THandle;
  msg    : TOmniMessage;
begin
  handles[0] := FStopEvent.Handle;
  handles[1] := FCommandQueue.GetNewMessageEvent;
  while WaitForMultipleObjects(2, @handles, false, INFINITE) = (WAIT_OBJECT_0 + 1) do begin
    while FCommandQueue.TryDequeue(msg) do begin
      //msg.MsgID is ignored in this demo
      //msg.MsgData contains a number which will be multiplied by 2,
      //converted into a string, prepended by '=' and returned to the owner form
      if not FResponseQueue.Enqueue(TOmniMessage.Create(0 {ignored}, Format('= %d', [msg.MsgData.AsInteger * 2]))) then
        raise Exception.Create('Response queue is full!');
    end;
  end;
end;

procedure TWorker.Stop;
begin
  FStopEvent.SetEvent;
end;

end.