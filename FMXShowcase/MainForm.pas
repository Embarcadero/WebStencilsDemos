unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, System.Actions, FMX.ActnList, Web.HTTPApp, Web.Stencils,
  System.Math, FMX.WebBrowser, FMX.TabControl, FMX.Layouts,

  PersonClass;

type
  TWebStencilsLabs = class(TForm)
    WebStencilsProcessor: TWebStencilsProcessor;
    ActionList: TActionList;
    ActGenerateWSResult: TAction;
    ActUpdateFields: TAction;
    StyleBook1: TStyleBook;
    LblTitle: TLabel;
    GridPanelLayout: TGridPanelLayout;
    LayoutPerson: TLayout;
    GroupPerson: TGroupBox;
    LblName: TLabel;
    LblAge: TLabel;
    BtnSave: TButton;
    EdtName: TEdit;
    EdtAge: TEdit;
    LayoutTemplates: TLayout;
    LblTitleTemplates: TLabel;
    TabControlTemplates: TTabControl;
    TabPlainTextTemplate: TTabItem;
    TabHTMLTemplate: TTabItem;
    MemoHTMLTemplate: TMemo;
    BtnGenerate: TButton;
    LayoutContent: TLayout;
    LblTitleContent: TLabel;
    TabControlContent: TTabControl;
    TabPlainTextPreview: TTabItem;
    MemoWSGenerated: TMemo;
    TabHTMLPreview: TTabItem;
    BrowserWSGenerated: TWebBrowser;
    MemoPlainTemplate: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActGenerateWSResultExecute(Sender: TObject);
    procedure ActUpdateFieldsExecute(Sender: TObject);
    procedure ActUpdateFieldsUpdate(Sender: TObject);
  private
    LPerson: TPerson;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebStencilsLabs: TWebStencilsLabs;

implementation

uses MyTemplates;

{$R *.fmx}

procedure TWebStencilsLabs.FormCreate(Sender: TObject);
begin
  MemoPlainTemplate.Text := PlainTemplate1;
  MemoHTMLTemplate.Text := HTMLTemplate1;
  LPerson := TPerson.Create('Alba', 21);
  EdtName.Text := LPerson.Name;
  EdtAge.Text := LPerson.Age.ToString;
  WebStencilsProcessor.AddVar('person', LPerson, false);
end;

procedure TWebStencilsLabs.FormDestroy(Sender: TObject);
begin
  LPerson.Free;
end;

procedure TWebStencilsLabs.ActGenerateWSResultExecute(Sender: TObject);
begin
  var GeneratedPlainContent := WebStencilsProcessor.ContentFromString(MemoPlainTemplate.Text);
  var GeneratedHTMLContent := WebStencilsProcessor.ContentFromString(MemoHTMLTemplate.Text);
  MemoWSGenerated.Text := GeneratedPlainContent;
  BrowserWSGenerated.LoadFromStrings(GeneratedHTMLContent, '');
end;

procedure TWebStencilsLabs.ActUpdateFieldsExecute(Sender: TObject);
begin
  LPerson.Name := EdtName.Text;
  LPerson.Age := ifThen(EdtAge.Text = '', 0, EdtAge.Text.ToInteger);
end;

procedure TWebStencilsLabs.ActUpdateFieldsUpdate(Sender: TObject);
begin
  BtnSave.Enabled := (LPerson.Age.ToString <> EdtAge.Text) or (LPerson.Name <> EdtName.Text);
end;

end.
