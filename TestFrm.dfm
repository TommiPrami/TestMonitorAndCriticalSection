object FormTest: TFormTest
  Left = 0
  Top = 0
  Caption = 'Comparison of TCriticalSection and TMonitor'
  ClientHeight = 433
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    706
    433)
  TextHeight = 15
  object ButtonNoSync: TButton
    Left = 8
    Top = 16
    Width = 150
    Height = 25
    Caption = 'Run Test NoSync'
    TabOrder = 0
    OnClick = ButtonNoSyncClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 47
    Width = 690
    Height = 378
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object ButtonTCriticalSection: TButton
    Tag = 1
    Left = 164
    Top = 16
    Width = 150
    Height = 25
    Caption = 'Run Test TCriticalSection'
    TabOrder = 2
    OnClick = ButtonNoSyncClick
  end
  object ButtonTMonitor: TButton
    Tag = 2
    Left = 320
    Top = 16
    Width = 150
    Height = 25
    Caption = 'Run Test TMonitor'
    TabOrder = 3
    OnClick = ButtonNoSyncClick
  end
  object EditTestIterationCount: TEdit
    Left = 577
    Top = 18
    Width = 121
    Height = 23
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '10 000 000'
    OnEnter = EditTestIterationCountEnter
    OnExit = EditTestIterationCountExit
  end
  object SpinEditThreadCount: TSpinEdit
    Left = 496
    Top = 17
    Width = 65
    Height = 24
    MaxValue = 256
    MinValue = 1
    TabOrder = 5
    Value = 4
  end
end
