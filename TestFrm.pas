unit TestFrm;

interface

{ The purpose of this test code is to demonstrate the comparison between using
  TMonitor and TCriticalSection for multi-thread synchronization to a resource.
  Author - Sean Solberg
  Freely available code with no restrictions.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.SyncObjs, System.Diagnostics, Vcl.Samples.Spin;

type
  TMyResource = class
  public
    FCriticalSection: TCriticalSection;
    FValue: Int64;
    constructor Create;
    destructor Destroy; override;
  end;

  TMyThread = class(TThread)
  private
    fResource: TMyResource;
    fTest: Integer;
    fIterationCount: integer;
    procedure Test0;
    procedure Test1;
    procedure Test2;
    procedure DoMath;
  public
    constructor Create(const AResource: TMyResource; const ATest, AIterationCount: Integer);
    procedure Execute; override;
  end;

  TForm54 = class(TForm)
    ButtonNoSync: TButton;
    MemoLog: TMemo;
    ButtonTCriticalSection: TButton;
    ButtonTMonitor: TButton;
    EditTestIterationCount: TEdit;
    SpinEditThreadCount: TSpinEdit;
    procedure ButtonNoSyncClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure FormDestroy(ASender: TObject);
  private
    { Private declarations }
    FResource: TMyResource;
    function GetIterationCount: Integer;
    function GetThreadCount: Integer;
    procedure CreateThreads(var AThreadsArray: TArray<TMyThread>; const AThreadCount, ATestType, AIterationCount: Integer);
    function BeforeRun: TStopwatch;
    procedure AfterRun(const AStopwatch: TStopwatch; const ATestButton: TButton);
  public
    { Public declarations }
  end;

var
  Form54: TForm54;

implementation

{$R *.dfm}

function TForm54.BeforeRun: TStopwatch;
begin
  Screen.Cursor := crHourGlass;
  Enabled := False;
  ButtonNoSync.Enabled := Enabled;
  ButtonTCriticalSection.Enabled := Enabled;
  ButtonTMonitor.Enabled := Enabled;
  EditTestIterationCount.Enabled := Enabled;
  SpinEditThreadCount.Enabled := Enabled;
  MemoLog.Enabled := Enabled;

  // Give all time to breath and update GUI
  Application.ProcessMessages;

  Result := TStopwatch.StartNew;
end;

procedure TForm54.AfterRun(const AStopwatch: TStopwatch; const ATestButton: TButton);

  function  GetTestName(const ATestButton: TButton): string;
  var
    LCaptionParts: TArray<string>;
    LHigh: Integer;
  begin
    Result := '';
    LCaptionParts := string(ATestButton.Caption).Split([' ']);

    LHigh := High(LCaptionParts);
    if LHigh >= 0 then
      Result := LCaptionParts[LHigh];

    Result := Result + StringOfChar(' ', 18 - Result.Length);
  end;

var
  LTestName: string;
begin
  AStopwatch.Stop;

  Enabled := True;
  ButtonNoSync.Enabled := Enabled;
  ButtonTCriticalSection.Enabled := Enabled;
  ButtonTMonitor.Enabled := Enabled;
  EditTestIterationCount.Enabled := Enabled;
  SpinEditThreadCount.Enabled := Enabled;
  MemoLog.Enabled := Enabled;

  LTestName := GetTestName(ATestButton);

  MemoLog.Lines.Add(LTestName +  ' => Value: ' + FormatFloat('#,##0', FResource.FValue)
    + ', Time: ' + FormatFloat('#,##0.000', AStopwatch.Elapsed.TotalSeconds) + ' sec');

  Screen.Cursor := crDefault;
end;

procedure TForm54.ButtonNoSyncClick(ASender: TObject);
var
  LTestType: Integer;
  LCurrentThread: TMyThread;
  LStopWatch: TStopWatch;
  LThreads: TArray<TMythread>;
begin
  FResource.FValue := 0;

  LStopWatch := BeforeRun;
  CreateThreads(LThreads, GetThreadCount, TButton(ASender).Tag, GetIterationCount);
  try
    for LCurrentThread in LThreads do
      LCurrentThread.WaitFor;

    AfterRun(LStopWatch, TButton(ASender));
  finally
    for LCurrentThread in LThreads do
      LCurrentThread.Free;
  end;
end;

procedure TForm54.CreateThreads(var AThreadsArray: TArray<TMyThread>; const AThreadCount, ATestType,
  AIterationCount: Integer);
var
  LIndex: Integer;
begin
  SetLength(AThreadsArray, AThreadCount);

  for LIndex := Low(AThreadsArray) to High(AThreadsArray) do
    AThreadsArray[LIndex] := TMyThread.Create(FResource, ATestType, AIterationCount);
end;

procedure TForm54.FormCreate(ASender: TObject);
begin
  FResource := TMyResource.Create;
end;

procedure TForm54.FormDestroy(ASender: TObject);
begin
  FResource.Free;
end;

function TForm54.GetIterationCount: Integer;
var
  LHandledValue: string;
begin
  LHandledValue := EditTestIterationCount.Text;
  LHandledValue := LHandledValue.Replace(' ', '').Replace(FormatSettings.DecimalSeparator, '');

  Result := StrToIntDef(LHandledValue, 0);
end;

function TForm54.GetThreadCount: Integer;
begin
  Result := SpinEditThreadCount.Value;
end;

{ TMyResource }

constructor TMyResource.Create;
begin
  inherited Create;

  FCriticalSection := TCriticalSection.create;
end;

destructor TMyResource.Destroy;
begin
  FCriticalSection.Free;

  inherited Destroy;
end;

{ TMyThread }

constructor TMyThread.Create(const AResource: TMyResource; const ATest, AIterationCount: Integer);
begin
  inherited Create(True);

  FResource := AResource;
  FTest := ATest;
  FIterationCount := AIterationCount;

  Resume;
end;

procedure TMyThread.DoMath;
begin
  fResource.fValue := fResource.fValue + 3;
  fResource.fValue := fResource.fValue - 3;
  fResource.fValue := fResource.fValue + 1;
end;

procedure TMyThread.Execute;
begin
  case FTest of
    0: Test0;
    1: Test1;
    2: Test2;
  end;
end;

procedure TMyThread.Test0;
var
  LIndex: integer;
begin
  // This test will execute the math problem without any thread synching
  // Note that this test will NOT correctly calculate the final resource value because it is not thread synchronized.
  for LIndex := 1 to FIterationcount do
  try
    DoMath;
  finally
    // DoNothing
  end;
end;

procedure TMyThread.Test1;
var
  LIndex: integer;
begin
  // this test will execute the math problem using the CriticalSection
  for LIndex := 1 to FIterationcount do
  begin
    FResource.FCriticalSection.Enter;
    try
      DoMath;
    finally
      FResource.FCriticalSection.Leave;
    end;
  end;
end;

procedure TMyThread.Test2;
var
  LIndex: integer;
begin
  // this test will execute the math problem using TMonitor
  for LIndex := 1 to fIterationcount do
  begin
    System.TMonitor.Enter(FResource);
    try
      DoMath;
    finally
      System.TMonitor.Exit(FResource);
    end;
  end;
end;

end.
