{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2025 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$WARN IMPLICIT_STRING_CAST OFF}

unit Web.Stencils;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  System.Bindings.EvalProtocol, Data.DB, Web.HTTPApp;

type
  TWebStencilsProcessor = class;
  TWebStencilsEngine = class;
  TWebStencilsDataVar = class;

  EWebStencilsException = class(EWebBrokerException);

  TWhitelistAwareScope = class(TInterfacedObject, IScope)
  private
    FInnerScope: IScope;
    FObject: TObject;
    FWhitelist: IWhitelist;
  public
    constructor Create(AInnerScope: IScope; AObject: TObject; AWhitelist: IWhitelist);
    function Lookup(const Name: string): IInterface;
  end;


  /// <summary> Attribute class is used by AddModule methods to filter module
  /// members. Only members with [WebStencilsVar] attribute will be added to data
  /// variables. When AName argument is specified, then it will be used as
  /// a variable name, otherwise the member name is used. When AWebStencilsOwned
  /// argument is specified, then it will be used as a variable object ownership,
  /// otherwise the module is the object owner.
  ///
  /// Note: [WebStencilsVar] attribute cannot be used in C++ code. Instead use
  /// TWebStencilsDataVars.AddVar, TWebStencilsDataVars.AddInitFunc methods. </summary>
  WebStencilsVarAttribute = class(TCustomAttribute)
  private
    FName: string;
    FWebStencilsOwned: Boolean;
  public
    constructor Create(const AName: string; AWebStencilsOwned: Boolean); overload;
    constructor Create(const AName: string); overload;
    constructor Create(AWebStencilsOwned: Boolean); overload;
    constructor Create; overload;
    property Name: string read FName;
    property WebStencilsOwned: Boolean read FWebStencilsOwned;
  end;

  /// <summary> Internal interface unifying TWebStencilsProcessor and TWebStencilsEngine
  /// for the needs of the other WebStencils library classes. </summary>
  IWebStencilsComponent = interface
    function GetRttiContext: TRttiContext;
    procedure AddError(const AWarning: string); overload;
    procedure AddError(const AWarning: string; const AArgs: array of const); overload;
    property RttiContext: TRttiContext read GetRttiContext;
  end;

  TWebStencilsValueEvent = procedure(Sender: TObject; const AObjectName, APropName: string;
    var AValue: string; var AHandled: Boolean) of object;
  TWebStencilsLanguageEvent = procedure(Sender: TObject; const APropName: string;
    var AValue: string) of object;
  TWebStencilsScaffoldingEvent = procedure(Sender: TObject; const AQualifClassName: string;
    var AValue: string) of object;
  TWebStencilsErrorEvent = procedure(Sender: TObject; const AMessage: string) of object;
  TWebStencilsFileEvent = procedure(Sender: TObject; const AFilename: string;
    var AText: string; var AHandled: Boolean) of object;
  TWebStencilsPathInitEvent = procedure(Sender: TObject; const ARequest: TWebPostProcessorRequest) of object;
  TWebStencilsBeforeRequestEvent = procedure(Sender: TObject; const ARequest: TWebPostProcessorRequest;
    var AAccept: Boolean) of object;
  TWebStencilsAfterRequestEvent = procedure(Sender: TObject; const ARequest: TWebPostProcessorRequest;
    var AOutStream: TStream) of object;
  TWebStencilsFileNotFoundEvent = procedure(Sender: TObject; const ARequest: TWebPostProcessorRequest;
    var ANotFoundPagePath: string) of object;

  TWebStencilsInitFunc = reference to function(): TObject;
  TWebStencilsLookupFunc = reference to function(AVar: TWebStencilsDataVar;
    const APropName: string; var AValue: string): Boolean;
  TWebStencilsDataVarKind = (rdikObject, rdikStrings, rdikDataset, rdikLookup);
  TWebStencilsDataVarDuplicates = (ddIgnore, ddReplace, ddError);

  TWebStencilsParser = class(TObject)
  public const
    ptEOL     = UTF8Char(5);
    ptEOF     = UTF8Char(0);
    ptSymbol  = UTF8Char(1);
    ptString  = UTF8Char(2);
    ParseBufSize = 4096;
  private
    FStream: TStream;
    FOutStream: TStream;
    FOrigin: Int64;
    FBuffer: PUTF8Char;
    FBufPtr: PUTF8Char;
    FBufEnd: PUTF8Char;
    FSourcePtr: PUTF8Char;
    FSourceEnd: PUTF8Char;
    FTokenPtr: PUTF8Char;
    FStringPtr: PUTF8Char;
    FSourceLine: Integer;
    FSaveChar: UTF8Char;
    FToken: UTF8Char;
    FABuff: array [0 .. ParseBufSize - 1] of UTF8Char;
    procedure Error(const Ident: string);
    function GetNextChar: UTF8Char; inline;
    procedure UpdateOutStream(StartPos: PUTF8Char);
    procedure ReadBuffer;
    procedure SkipBlanks(DoCopy: Boolean);
    class function IsValidUTF8Char(var ABuffer: PByte; AFirstChar: Boolean): Boolean; static;
    function SkipToNextToken(CopyBlanks, DoCopy: Boolean): UTF8Char;
    function CopySkipToToken(AToken: UTF8Char; DoCopy: Boolean): UTF8String;
  public
    constructor Create(Stream, OutStream: TStream);
    destructor Destroy; override;
    function SkipToken(CopyBlanks: Boolean): UTF8Char;
    function SkipNumber: Boolean;
    function SkipAloneLineBreak: Boolean;
    function SkipToToken(AToken: UTF8Char): UTF8String;
    procedure CopyTokenToOutput;
    function TokenString: UTF8String;
    property Token: UTF8Char read FToken;
    property NextChar: UTF8Char read GetNextChar;
  end;

  TWebStencilsDataVar = class(TObject)
  private
    FName: string;
    FTheObject: TObject;
    FKind: TWebStencilsDataVarKind;
    FOwned: Boolean;
    FInitFunc: TWebStencilsInitFunc;
    FLookupFunc: TWebStencilsLookupFunc;
  protected
    function CheckObject(const AComponent: IWebStencilsComponent; const APropName: string;
      var AValue: string): Boolean;
  public
    destructor Destroy; override;
    property Name: string read FName;
    property TheObject: TObject read FTheObject;
    property Kind: TWebStencilsDataVarKind read FKind;
    property Owned: Boolean read FOwned;
    property InitFunc: TWebStencilsInitFunc read FInitFunc;
    property LookupFunc: TWebStencilsLookupFunc read FLookupFunc;
  end;

  TWebStencilsDataVars = class(TObjectDictionary<string, TWebStencilsDataVar>)
  private
    FComponent: IWebStencilsComponent;
    FEvalScope: IScope;
    FDuplicates: TWebStencilsDataVarDuplicates;
    function GetEvalScope: IScope;
    procedure InternalAdd(const AName: string; AObject: TObject; AOwned: Boolean;
      const AInitFunc: TWebStencilsInitFunc; const ALookupFunc: TWebStencilsLookupFunc);
  protected
    procedure ValueNotify(const Value: TWebStencilsDataVar; Action: TCollectionNotification); override;
  public
    constructor Create(const AComponent: IWebStencilsComponent); overload;
    procedure Add(const AName: string; AObject: TObject; AOwned: Boolean); overload;
    procedure Add(const AName: string; AObject: TObject; AOwned: Boolean;
      const ALookupFunc: TWebStencilsLookupFunc); overload;
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean); overload;
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean;
      const ALookupFunc: TWebStencilsLookupFunc); overload;
    procedure AddModule(AModule: TObject);
    procedure Delete(AObject: TObject);
    property EvalScope: IScope read GetEvalScope;
    property Duplicates: TWebStencilsDataVarDuplicates read FDuplicates
      write FDuplicates default ddIgnore;
  end;

  TWebStencilsLoopVar = class(TObject)
  private
    FProcessor: TWebStencilsProcessor;
    FLoopObject: TObject;
    FLoopIterObject: TObject;
    FLoopIterNext: TRttiMethod;
    FLoopIterCur: TRttiProperty;
    FCurrentObj: TObject;
    FObjName: string;
    FPropName: string;
    FVarName: string;
    FCurrPos: NativeInt;
  public
    constructor Create(AProc: TWebStencilsProcessor; const AObjName, APropName, AVarName: string);
    function InitLoop: Boolean;
    function NextLoop: Boolean;
    procedure DoneLoop;
    property ObjName: string read FObjName;
    property PropName: string read FPropName;
    property VarName: string read FVarName;
    property LoopObject: TObject read FLoopObject;
    property CurrentObj: TObject read FCurrentObj;
  end;

  TWebStencilsLoopVars = class(TObjectDictionary<string, TWebStencilsLoopVar>)
  private
    FEvalScope: IScope;
    function GetEvalScope: IScope;
  protected
    procedure ValueNotify(const Value: TWebStencilsLoopVar; Action: TCollectionNotification); override;
  public
    constructor Create;
    property EvalScope: IScope read GetEvalScope;
  end;

  TWebStencilsAliasVars = class(TStringList)
  private
    FProcessor: TWebStencilsProcessor;
    FEvalScope: IScope;
    function GetEvalScope: IScope;
  public
    constructor Create(AProcessor: TWebStencilsProcessor);
    function ResolveAlias(const AObjName: string): string;
    procedure Cleanup(ANum: Integer);
    property EvalScope: IScope read GetEvalScope;
  end;

  TWebStencilsPathTemplate = class
  public const
    CFileName = 'filename';
  private
    FItems: TStringList;
    FTemplate: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(const ATemplate: string);
    function Consume(const APath: string; AVars: TStrings): Boolean;
    function GetFullPathName(AVars: TStrings; const ADefaultFileExt: string): string;
    property Template: string read FTemplate;
  end;

  TWebStencilsPathTemplateValidateFunc = reference to function (const APath: string): Boolean;
  TWebStencilsPathTemplateItem = class(TCollectionItem)
  public const
    CAnyPath = '*';
  private
    FTemplate: string;
    FRedirect: string;
    FOnPathInit: TWebStencilsPathInitEvent;
  protected
    function GetDisplayName: string; override;
    procedure DoPathInit(const ARequest: TWebPostProcessorRequest); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    function Match(const APath, ADefaultFileExt: string; var ANewPath: string): Boolean; overload;
    function Match(const APath, ADefaultFileExt: string;
      const AValidateFunc: TWebStencilsPathTemplateValidateFunc;
      var ANewPath: string; AVars: TStrings): Boolean; overload;
  published
    /// <summary> Path template used to match a request path and extract variables.
    /// A template is a request path starting with '/' and may include '{name}' as
    /// a variable marker. For example '/mysite/{filename}/{arg1}'. The variables
    /// are accessible using '@page.variable'. When {filename} variable is
    /// not specified, then last path segment is the value of 'filename' variable.
    /// Note, '*' is used as special marker, when matching template is not found
    /// or when template file is not found. </summary>
    property Template: string read FTemplate write FTemplate;
    /// <summary> Path template used to produce a request path, when matched to
    /// Template. It can contain variable markers from Template. </summary>
    property Redirect: string read FRedirect write FRedirect;
    /// <summary> The event is fired, when a post processor request is matched to
    /// Template, and before post processing will start. The event allows to perform
    /// a path specific setup, eg update DataVars. </summary>
    property OnPathInit: TWebStencilsPathInitEvent read FOnPathInit write FOnPathInit;
  end;

  TWebStencilsPathTemplateCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TWebStencilsPathTemplateItem;
    procedure SetItem(Index: Integer; const AValue: TWebStencilsPathTemplateItem);
  public
    constructor Create(AOwner: TComponent);
    function Add: TWebStencilsPathTemplateItem; overload;
    function Add(const ATemplate: string; const ARedirect: string = '';
      const AInitMethod: TWebStencilsPathInitEvent = nil): TWebStencilsPathTemplateItem; overload;
    function Match(const APath, ADefaultFileExt: string;
      var ANewPath: string; var APathItem: TWebStencilsPathTemplateItem): Boolean; overload;
    function Match(const APath, ADefaultFileExt: string;
      const AValidateFunc: TWebStencilsPathTemplateValidateFunc;
      var ANewPath: string; AVars: TStrings; var APathItem: TWebStencilsPathTemplateItem): Boolean; overload;
    property Items[Index: Integer]: TWebStencilsPathTemplateItem read GetItem write SetItem; default;
  end;

  /// <summary>
  /// The WebStencils Processor class is used to process an individual file
  /// (generally with HTML extension) and its associated template if any.
  /// Processor may be used standalone, or assigned to TWebActionItem.Producer,
  /// or created and called by TWebStencilsEngine as a post processor of text files
  /// returned by a file dispatcher. </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TWebStencilsProcessor = class(TCustomContentProducer, IWebStencilsComponent)
  public const
    CDefFileExt = '.html';
  private type
    TRetType = (rtString, rtBool, rtNotBool);
    TSwitchVar = record
      FValue: Variant;
      FProcessed: Boolean;
    end;
    TRenderPart = (rpBody, rpHeader, rpPages);
    TRenderParts = set of TRenderPart;
  private class var
    FBasicEnums: IScope;
    FWhitelist: IWhitelist;
  private
    FInputFileName: string;
    FInputFileNameTemp: Boolean;
    FInputLines: TStrings;
    FPathTemplate: TWebStencilsPathTemplate;
    FWebRequest: TWebRequest;
    FProcessorRequest: TWebPostProcessorRequest;
    FEngine: TWebStencilsEngine;
    FDataVars: TWebStencilsDataVars;
    FLoopVars: TWebStencilsLoopVars;
    FAliasVars: TWebStencilsAliasVars;
    FSwitchVars: TArray<TSwitchVar>;
    FAddedPages: TStringList;
    FAddedHeaders: TStringList;
    FUserLoggedIn: Boolean;
    FLanguageId: Integer;
    FDefaultFileExt: string;
    FUserRoles: string;
    FUserRoleList: TArray<string>;
    FRTTIContext: TRttiContext;
    FOwnerAdded: Boolean;
    FDisableProcessing: Integer;
    FFakeSession: TObject;
    FNestingLevel: Integer;
    FRenderParts: TRenderParts;
    FOnValue: TWebStencilsValueEvent;
    FOnError: TWebStencilsErrorEvent;
    FOnLanguage: TWebStencilsLanguageEvent;
    FOnScaffolding: TWebStencilsScaffoldingEvent;
    FOnFile: TWebStencilsFileEvent;
    FBeforeProduce: TNotifyEvent;
    FAfterProduce: TNotifyEvent;

    class constructor Create;
    procedure SetEngine(const AValue: TWebStencilsEngine);
    function GetDataVarDuplicates: TWebStencilsDataVarDuplicates;
    procedure SetDataVarDuplicates(const AValue: TWebStencilsDataVarDuplicates);
    procedure SetInputLines(const AValue: TStrings);
    function GetWebRequest: TWebRequest;
    function GetPathTemplate: string;
    procedure SetPathTemplate(const AValue: string);
    procedure SetUserRoles(const AValue: string);
    function IsExtStored: Boolean;
    procedure ResolveObjectProperty(const APropName: string; var AObj: TObject;
      var AProperty: TRttiProperty);
    function GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
      var AValue: string): Boolean; overload;
    function GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
      var AValue: Boolean): Boolean; overload;
    function GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
      var AValue: TObject): Boolean; overload;
    function GetDictionaryValue(AItem: TWebStencilsDataVar; const APropName: string;
      var AValue: string): Boolean;
    function GetVariableName(AParser: TWebStencilsParser;
      AExcludeFirstDot: Boolean = False): string;
    class function StringToBoolean(const AStr: string): Boolean; static;
    function EvalVariableValue(const AObjName, APropName: string; ARetType: TRetType): Variant;
    function BasicEnums: IScope;
    function MakeBasicEnums: IScope;
    function EvalExpressionValue(const AExpression: string; ARetType: TRetType): Variant;
    function GetLoopOrDictionaryObject(const AObjName: string): TObject;
    function GetEntireDottedValue(AParser: TWebStencilsParser): string;
    function QualifyFileName(const AFileName: string): string;
    function GetFormatSettings: PFormatSettings;
    function DoLoop(const ABlock: string; ALoopVar: TWebStencilsLoopVar): string;
    function DoString(const ABlock: string): string;
    function DoFile(const AFileName: string): string;
    procedure InitContent;
    procedure DoneContent;
    function ValidateMemberAccess(AObj: TObject; const AName: string): Boolean;
  protected
    function GetLang(const APropName: string): string; virtual;
    function GetOtherValue(const AObjectName, APropName: string; var AValue: string): Boolean; virtual;
    function GetScaffolding(const ADottedClass: string): string; virtual;
    function GetFileContent(const AFileName: string; var AValue: string): Boolean; virtual;
    function DoStream(AStream: TStream): string; virtual;
    procedure DoBeforeProduce; virtual;
    procedure DoAfterProduce; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // IWebStencilsComponent
    function GetRttiContext: TRttiContext;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    /// <summary> Adds a new data variable with AName name. An object is specified
    /// by AObject argument. Properties of the object are available in template
    /// using '@name.property' syntax. Only public and published properties are
    /// available. When AOwned is True, then object is owned by processor. </summary>
    procedure AddVar(const AName: string; AObject: TObject; AOwned: Boolean = True); overload;
    procedure AddVar(const AName: string; AObject: TObject; AOwned: Boolean;
      const ALookupFunc: TWebStencilsLookupFunc); overload;
    /// <summary> Adds a new data variable with AName name. An object is provided
    /// by AInitFunc argument. Properties of the object are available in template
    /// using '@name.property' syntax. Only public and published properties are
    /// available. When AOwned is True, then object is owned by processor. </summary>
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
      AOwned: Boolean = True); overload;
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
      AOwned: Boolean; const ALookupFunc: TWebStencilsLookupFunc); overload;
    /// <summary> Scans all public and published members (fields, properties, methods)
    /// of AModule, which are marked by [WebStencilsVar] attributes. And adds them to
    /// data variables. This method is called automatically for processor Owner,
    /// when Engine is nil.
    ///
    /// Note: [WebStencilsVar] attribute and AddModule cannot be used in C++ code.
    /// Instead use TWebStencilsDataVars.AddVar, TWebStencilsDataVars.AddInitFunc methods. </summary>
    procedure AddModule(AModule: TObject);
    function HasVar(const AName: string): Boolean;

    /// <summary> Outputs next error AMessage found while processing a template.
    /// This method calls OnError event. Note, a processing error is not fatal,
    /// and processing is not aborted. The method is used mostly by processor itself. </summary>
    procedure AddError(const AMessage: string); overload; virtual;
    procedure AddError(const AMessage: string; const AArgs: array of const); overload;

    /// <summary> Produces content from template specified by InputFileName. When
    /// InputFileName is empty, then InputLines is used. </summary>
    function Content: string; override;
    /// <summary> Produces content from template contained in AStream. </summary>
    function ContentFromStream(AStream: TStream): string; override;
    /// <summary> Produces content from template contained in AString. </summary>
    function ContentFromString(const AString: string): string; override;
    /// <summary> Produces content from template specified by AFileName. </summary>
    function ContentFromFile(const AFileName: string): string;

    /// <summary> Specifies a web request for which processor is called.
    /// It is set automatically when processor is assigned to TWebActionItem.Producer,
    /// or is called by engine for post processing. </summary>
    property WebRequest: TWebRequest read GetWebRequest write FWebRequest;
    /// <summary> Specifies a post processing request for which processor is called.
    /// It is set automatically when processor is called by engine for post processing. </summary>
    property ProcessorRequest: TWebPostProcessorRequest read FProcessorRequest write FProcessorRequest;
    /// <summary> Returns reference to data variables dictionary. </summary>
    property DataVars: TWebStencilsDataVars read FDataVars;

    /// <summary> Returns reference to RTTI members whitelist. </summary>
    class property Whitelist: IWhitelist read FWhitelist write FWhitelist;
  published
    /// <summary> Specifies the engine to inherit the data variables, event handlers,
    /// file system path processing, etc. </summary>
    property Engine: TWebStencilsEngine read FEngine write SetEngine;
    /// <summary> Specifies a file name to process using Content method. Use InputFileName,
    /// when processor is used standalone or assigned to TWebActionItem.Producer. Other
    /// option is to assign file content directly to InputLines. </summary>
    property InputFileName: string read FInputFileName write FInputFileName;
    /// <summary> Specifies a file content to process using Content method. Use InputLines,
    /// when processor is used standalone or assigned to TWebActionItem.Producer. Other
    /// option is to assign external file name to InputFileName. </summary>
    property InputLines: TStrings read FInputLines write SetInputLines;
    /// <summary> Path template used to parse request path and extract variables.
    /// A template is a request path starting with '/' and may include '{name}' as
    /// a variable marker. For example '/mysite/{filename}/{arg1}'. The variables
    /// are accessible using '@page.&lt;variable name&gt;'. When {filename} variable is
    /// not specified, then last path segment is the value of 'filename' variable. </summary>
    property PathTemplate: string read GetPathTemplate write SetPathTemplate;
    /// <summary> Specifies when a session has logged in user. This property is
    /// checked by @LoginRequired instruction. It is automatically assigned when
    /// WebRequest is not nil. </summary>
    property UserLoggedIn: Boolean read FUserLoggedIn write FUserLoggedIn default False;
    /// <summary> Specifies the active roles of current logged in user. This
    /// property is checked by @LoginRequired instruction. It is automatically
    /// assigned when WebRequest is not nil. </summary>
    property UserRoles: string read FUserRoles write SetUserRoles;
    property LanguageId: Integer read FLanguageId write FLanguageId default 0;
    /// <summary> Specifies alternative behavior in case of a new added data variable
    /// has a duplicated name. Default is ddIgnore. </summary>
    property DataVarDuplicates: TWebStencilsDataVarDuplicates read GetDataVarDuplicates
      write SetDataVarDuplicates default ddIgnore;
    /// <summary> Specifies default file extention to use when a file extention is
    /// not specified. It is used for all files processed by processor. Default
    /// value is '.html' </summary>
    property DefaultFileExt: string read FDefaultFileExt write FDefaultFileExt
      stored IsExtStored;

    /// <summary> The event is fired when processor founds an unknown variable name.
    /// An unknown is a variable, which is not a registered data variable, loop
    /// variable, or a reserved variable. When event handler is not specified, then
    /// Engine.OnValue is called. </summary>
    property OnValue: TWebStencilsValueEvent read FOnValue write FOnValue;
    /// <summary> The event is fired when processor founds an error in template.
    /// When event handler is not specified, then Engine.OnError is called. </summary>
    property OnError: TWebStencilsErrorEvent read FOnError write FOnError;
    property OnLanguage: TWebStencilsLanguageEvent read FOnLanguage write FOnLanguage;
    property OnScaffolding: TWebStencilsScaffoldingEvent read FOnScaffolding write FOnScaffolding;
    /// <summary> The event is fired when processor needs to load a file. The
    /// event handler may provide a custom content of the requested file.
    /// When event handler is not specified, then Engine.OnFile is called. </summary>
    property OnFile: TWebStencilsFileEvent read FOnFile write FOnFile;
    /// <summary> The event is fired before processor starting processing template.
    /// Use WebRequest and ProcessorRequest properties to check the context. </summary>
    property BeforeProduce: TNotifyEvent read FBeforeProduce write FBeforeProduce;
    /// <summary> The event is fired after processor finished processing template.
    /// Use WebRequest and ProcessorRequest properties to check the context. </summary>
    property AfterProduce: TNotifyEvent read FAfterProduce write FAfterProduce;
  end;

  /// <summary>
  /// The WebStencils Engine class exposes customization parameters and event handlers
  /// shared by all instances of WebStencilsProcessor connected to it. Engine may be used
  /// as a post processor of text files returned by a file dispatcher.
  /// </summary>
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TWebStencilsEngine = class(TComponent, IWebStencilsComponent, IWebPostProcessor)
  private
    FDispatcher: IInterface;
    FPathTemplates: TWebStencilsPathTemplateCollection;
    FRootDirectory: string;
    FDataVars: TWebStencilsDataVars;
    FLocales: TDictionary<Integer, PFormatSettings>;
    FRttiContext: TRttiContext;
    FDefaultFileExt: string;
    FOwnerAdded: Boolean;
    FOnValue: TWebStencilsValueEvent;
    FOnLanguage: TWebStencilsLanguageEvent;
    FOnError: TWebStencilsErrorEvent;
    FOnPathInit: TWebStencilsPathInitEvent;
    FOnFile: TWebStencilsFileEvent;
    FOnScaffolding: TWebStencilsScaffoldingEvent;
    FBeforeRequest: TWebStencilsBeforeRequestEvent;
    FAfterRequest: TWebStencilsAfterRequestEvent;
    FOnFileNotFound: TWebStencilsFileNotFoundEvent;
    procedure SetDispatcher(const AValue: IInterface);
    procedure SetPathTemplates(const AValue: TWebStencilsPathTemplateCollection);
    function GetDataVarDuplicates: TWebStencilsDataVarDuplicates;
    procedure SetDataVarDuplicates(const AValue: TWebStencilsDataVarDuplicates);
    function IsExtStored: Boolean;
    class function IsMimeTypeValid(const APath: string;
      const AGetMimeTypeFunc: TWebGetMimeTypeFunc;
      var AMimeType: string): Boolean; static;
    procedure CheckAddModule;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetFileContent(const AFileName: string; var AValue: string): Boolean; virtual;
    procedure DoPathInit(const ARequest: TWebPostProcessorRequest); virtual;
    procedure DoBeforeRequest(const ARequest: TWebPostProcessorRequest;
      var AAccept: Boolean); virtual;
    procedure DoAfterRequest(const ARequest: TWebPostProcessorRequest;
      var AOutStream: TStream); virtual;
    procedure DoFileNotFound(const ARequest: TWebPostProcessorRequest;
      var ANotFoundPagePath: string); virtual;
    // IWebStencilsComponent
    function GetRttiContext: TRttiContext;
    // IWebPostProcessor
    function CanPostProcess(var APath: string; const AVars: TStrings;
      const AGetMimeTypeFunc: TWebGetMimeTypeFunc;
      const AGetFileNameFromPathFunc: TWebGetFileNameFromPathFunc;
      const AGetLoggedUserFunc: TWebGetLoggedUserFunc;
      out AProcessorRequest: IInterface): Boolean;
    function CanCachePostProcessResponse(const AProcessorRequest: IInterface): Boolean;
    function PostProcess(const AProcessorRequest: IInterface; AInStream: TStream;
      out AOutStream: TStream): Boolean;
    function PostProcessFileNotFound(const AProcessorRequest: IInterface;
      out ANotFoundPagePath: string): Boolean;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure AddLangFormat(ALanguageId: Integer; const ASettings: TFormatSettings);
    procedure RemoveLangFormat(ALanguageId: Integer);
    function GetLangFormats(ALanguageId: Integer): PFormatSettings;
    function GetLang(const APropName: string): string; virtual;

    procedure AddVar(const AName: string; AObject: TObject; AOwned: Boolean = True); overload;
    procedure AddVar(const AName: string; AObject: TObject; AOwned: Boolean;
      const ALookupFunc: TWebStencilsLookupFunc); overload;
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
      AOwned: Boolean = True); overload;
    procedure AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
      AOwned: Boolean; const ALookupFunc: TWebStencilsLookupFunc); overload;
    procedure AddModule(AModule: TObject);
    function HasVar(const AName: string): Boolean;

    function GetOtherValue(const AObjectName, APropName: string; var AValue: string): Boolean; virtual;
    function GetScaffolding(const ADottedClass: string): string; virtual;

    procedure AddError(const AMessage: string); overload; virtual;
    procedure AddError(const AMessage: string; const AArgs: array of const); overload;

    /// <summary> Returns reference to data variables dictionary. They are
    /// available for all linked processors. </summary>
    property DataVars: TWebStencilsDataVars read FDataVars;
  published
    /// <summary> When Dispatcher is specified, then engine is registered as a
    /// post processor of text files returned by a file dispatcher. The supported
    /// file dispatchers are TWebFileDispatcher and TEMSFileResource. </summary>
    property Dispatcher: IInterface read FDispatcher write SetDispatcher;
    /// <summary> Collection of request path templates used when engine is
    /// registered as post processor of a file dispatcher. When a template is
    /// matched to a request path, then the engine is used as a post processor.
    /// When collection is empty, then any request path is matched. A requested
    /// file must be textual.
    ///
    /// Notes:
    /// * A path template is relative to a dispatcher base path, although template
    /// starts with '/'. In case of TWebFileDispatcher, it is relative to VirtualPath.
    /// * When a path template contains '{filename}' variable, then only part before
    /// and {filename} are used to build a file path. The full file system path is
    /// produced using dispatcher functionality. In case of TWebFileDispatcher,
    /// it is concatenated with RootDirectory (roughly speaking).
    ///
    /// Examples:
    /// * Template = '/', Redirect = '/index.html' - this will redirect, eg
    ///   http://www.host.com/mysite/, to /index.html
    /// * Template = '*', Redirect = '/notfound.html' - this will redirect a request
    ///   to a path, which does not match to any other TWebStencilsPathTemplateItem, to /notfound.html
    /// * Template = '/customers/{filename}/{custid} - this will match, for example,
    ///   /customers/show/123. A file path is /customers/show.html, and two
    ///   variables: filename=show, custid=123
    /// * Template = '/orders/list' - this will match to /orders/list. A file path
    ///   is /orders/list.html, and one variable: filename=list
    /// </summary>
    property PathTemplates: TWebStencilsPathTemplateCollection read FPathTemplates
      write SetPathTemplates;
    /// <summary> Specifies a file system root path for the relative file paths
    /// used when engine is not used as a post processor. It is used for all files
    /// processed by standalone engine and linked processors. When the engine is
    /// used as a post processor, then corresponding dispatcher logic is used
    /// to get a full file name. </summary>
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    /// <summary> Specifies alternative behavior in case of a new added data variable
    /// has a duplicated name. Default is ddIgnore. </summary>
    property DataVarDuplicates: TWebStencilsDataVarDuplicates read GetDataVarDuplicates
      write SetDataVarDuplicates default ddIgnore;
    /// <summary> Specifies default file extention to use when a file extention is
    /// not specified. It is used for all files processed by engine and linked
    /// processors. Default value is '.html' </summary>
    property DefaultFileExt: string read FDefaultFileExt write FDefaultFileExt
      stored IsExtStored;

    /// <summary> The event is fired when a linked processor founds an unknown
    /// variable name. An unknown is a variable, which is not a registered data
    /// variable, loop variable, or a reserved variable. </summary>
    property OnValue: TWebStencilsValueEvent read FOnValue write FOnValue;
    /// <summary> The event is fired when a linked processor founds an error
    /// in template. </summary>
    property OnError: TWebStencilsErrorEvent read FOnError write FOnError;
    property OnLanguage: TWebStencilsLanguageEvent read FOnLanguage write FOnLanguage;
    property OnScaffolding: TWebStencilsScaffoldingEvent read FOnScaffolding write FOnScaffolding;
    /// <summary> The event is fired when a linked processor needs to load a file. The
    /// event handler may provide a custom content of the requested file. </summary>
    property OnFile: TWebStencilsFileEvent read FOnFile write FOnFile;

    /// <summary> The event is fired, when a post processor request is matched to
    /// a PathTemplates item, and before post processing will start. The event
    /// allows to perform a path specific setup, eg update DataVars. </summary>
    property OnPathInit: TWebStencilsPathInitEvent read FOnPathInit write FOnPathInit;
    property BeforeRequest: TWebStencilsBeforeRequestEvent read FBeforeRequest write FBeforeRequest;
    property AfterRequest: TWebStencilsAfterRequestEvent read FAfterRequest write FAfterRequest;
    property OnFileNotFound: TWebStencilsFileNotFoundEvent read FOnFileNotFound write FOnFileNotFound;
  end;

// Doubles any @ to avoid processing as a WebStencils instruction
function DoubleAtSymbol(const AValue: string): string;

implementation

uses
  System.TypInfo, System.Generics.Defaults, System.Contnrs, System.Bindings.EvalSys,
  System.Bindings.Helper, System.Bindings.Expression, System.Bindings.ObjEval,
  System.Bindings.Methods, System.Character, System.NetEncoding, System.RTLConsts,
  Web.StencilsConst, System.IOUtils, System.Net.Mime, System.SysConst, System.Variants,
  System.SyncObjs;

const
  CLoopVarEof = 'Eof';
  CLoopVarBof = 'Bof';
  CLoopStrsName = 'name';
  CLoopStrsValue = 'value';

function DoubleAtSymbol(const AValue: string): string;
begin
  Result := AValue;
  if Pos('@', Result) > 0 then
    Result := StringReplace(Result, '@', '@@', [rfReplaceAll]);
end;

function UpdateFileNameExt(const APath, ADefaultFileExt: string): string;
var
  LExt: string;
begin
  LExt := ADefaultFileExt.Trim;
  if not APath.IsEmpty and not LExt.IsEmpty and
     not APath.EndsWith('/') and ExtractFileExt(APath).IsEmpty then
    Result := ChangeFileExt(APath, LExt)
  else
    Result := APath;
end;

function ExtractPosixFileName(const APath: string): string;
var
  I: Integer;
begin
  I := APath.LastDelimiter(['/']);
  if I >= 0 then
    Result := APath.Substring(I + 1)
  else
    Result := APath;
end;

{ WebStencilsVarAttribute }

constructor WebStencilsVarAttribute.Create(const AName: string; AWebStencilsOwned: Boolean);
begin
  inherited Create;
  FName := AName;
  FWebStencilsOwned := AWebStencilsOwned;
end;

constructor WebStencilsVarAttribute.Create(const AName: string);
begin
  Create(AName, False);
end;

constructor WebStencilsVarAttribute.Create(AWebStencilsOwned: Boolean);
begin
  Create('', AWebStencilsOwned);
end;

constructor WebStencilsVarAttribute.Create;
begin
  Create('', False);
end;

{ TWebStencilsParser }

constructor TWebStencilsParser.Create(Stream, OutStream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FOutStream := OutStream;
  FBuffer := @FABuff[0];
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + ParseBufSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;
  SkipToken(True);
end;

destructor TWebStencilsParser.Destroy;
begin
  FStream.Seek(IntPtr(FTokenPtr) - IntPtr(FBufPtr), soCurrent);
  inherited Destroy;
end;

procedure TWebStencilsParser.Error(const Ident: string);
begin
  raise EWebStencilsException.CreateResFmt(@SParseError, [Ident, FSourceLine]);
end;

function TWebStencilsParser.GetNextChar: UTF8Char;
begin
  Result := FSourcePtr^;
end;

procedure TWebStencilsParser.UpdateOutStream(StartPos: PUTF8Char);
begin
  if FOutStream <> nil then
    FOutStream.WriteBuffer(StartPos^, FSourcePtr - StartPos);
end;

procedure TWebStencilsParser.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], (FBufEnd - FBufPtr) * sizeof(UTF8Char)));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then Error(SLineTooLong);
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TWebStencilsParser.SkipBlanks(DoCopy: Boolean);
var
  Start: PUTF8Char;
begin
  Start := FSourcePtr;
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          if DoCopy then UpdateOutStream(Start);
          ReadBuffer;
          if FSourcePtr^ = #0 then Exit;
          Start := FSourcePtr;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Break;
    end;
    Inc(FSourcePtr);
  end;
  if DoCopy then UpdateOutStream(Start);
end;

class function TWebStencilsParser.IsValidUTF8Char(var ABuffer: PByte; AFirstChar: Boolean): Boolean;
var
  CharSize: Integer;
  C: UCS4Char;
  I: Integer;
  Categ: TUnicodeCategory;
begin
  case ABuffer^ of
    $00..$7F: CharSize := 1; //
    $C2..$DF: CharSize := 2; // 110x xxxx C0 - DF
    $E0..$EF: CharSize := 3; // 1110 xxxx E0 - EF
    $F0..$F7: CharSize := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
  else
    Exit(False); // Illegal leading character.
  end;
  for I := 1 to CharSize - 1 do
    if (ABuffer + I)^ = 0 then
    begin
      Inc(ABuffer, I);
      Exit(False);
    end;
  case CharSize of
    1: C := UCS4Char(ABuffer^);
    2: C := UCS4Char(((ABuffer^ and $1F) shl 6 ) or ((ABuffer + 1)^ and $3F));
    3: C := UCS4Char(((ABuffer^ and $0F) shl 12) or (((ABuffer + 1)^ and $3F) shl 6 ) or ((ABuffer + 2)^ and $3F));
    4: C := UCS4Char(((ABuffer^ and $07) shl 18) or (((ABuffer + 1)^ and $3F) shl 12) or (((ABuffer + 2)^ and $3F) shl 6) or ((ABuffer + 2)^ and $3F));
  else
    C := 0;
  end;
  Inc(ABuffer, CharSize - 1);
  if C > $0000FFFF then
    Categ := Char.GetUnicodeCategory(Char.ConvertFromUtf32(C), 0)
  else
    Categ := Char.GetUnicodeCategory(C);
  Result := Categ in [
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLetterNumber];
  if not Result and not AFirstChar then
    Result := Categ in [
      TUnicodeCategory.ucCombiningMark,
      TUnicodeCategory.ucNonSpacingMark,
      TUnicodeCategory.ucConnectPunctuation,
      TUnicodeCategory.ucFormat,
      TUnicodeCategory.ucDecimalNumber];
end;

function TWebStencilsParser.SkipToNextToken(CopyBlanks, DoCopy: Boolean): UTF8Char;
var
  P, StartPos: PUTF8Char;
begin
  SkipBlanks(CopyBlanks);
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']) or
              (Byte(P^) > 127) and IsValidUTF8Char(PByte(P), False) do Inc(P);
        Result := ptSymbol;
      end;
    #10:
      begin
        Inc(P);
        Inc(FSourceLine);
        Result := ptEOL;
      end;
  else
    if (Byte(P^) > 127) and IsValidUTF8Char(PByte(P), True) then
    begin
      Inc(P);
      while (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']) or
            (Byte(P^) > 127) and IsValidUTF8Char(PByte(P), False) do Inc(P);
      Result := ptSymbol;
    end
    else
    begin
      Result := P^;
      if Result <> ptEOF then Inc(P);
    end;
  end;
  StartPos := FSourcePtr;
  FSourcePtr := P;
  if DoCopy then UpdateOutStream(StartPos);
  FToken := Result;
end;

function TWebStencilsParser.SkipToken(CopyBlanks: Boolean): UTF8Char;
begin
  Result := SkipToNextToken(CopyBlanks, False);
end;

function TWebStencilsParser.SkipNumber: Boolean;
var
  P: PUTF8Char;
begin
  SkipBlanks(False);
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    '0'..'9', '+', '-':
      begin
        FToken := P^;
        Inc(P);
        while (P^ in ['0'..'9']) do Inc(P);
      end;
    else
      Exit(False);
  end;
  FSourcePtr := P;
  Result := True;
end;

function TWebStencilsParser.SkipAloneLineBreak: Boolean;
var
  Start: PUTF8Char;
begin
  Result := True;
  Start := FSourcePtr;
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          UpdateOutStream(Start);
          ReadBuffer;
          Result := FSourcePtr^ = #0;
          Break;
        end;
      #10:
        begin
          Inc(FSourceLine);
          Inc(FSourcePtr);
          Break;
        end;
      #33..#255:
        begin
          FSourcePtr := Start;
          Result := False;
          Break;
        end;
    end;
    Inc(FSourcePtr);
  end;
  FToken := FSourcePtr^;
  FTokenPtr := FSourcePtr;
end;

function TWebStencilsParser.TokenString: UTF8String;
var
  L: Int64;
begin
  if FToken = ptString then
    L := FStringPtr - FTokenPtr else
    L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

procedure TWebStencilsParser.CopyTokenToOutput;
begin
  UpdateOutStream(FTokenPtr);
end;

function TWebStencilsParser.CopySkipToToken(AToken: UTF8Char; DoCopy: Boolean): UTF8String;
var
  S: PUTF8Char;
  Temp: UTF8String;

  procedure InternalSkipBlanks;
  begin
    while True do
    begin
      case FSourcePtr^ of
        #0:
          begin
            SetString(Temp, S, FSourcePtr - S);
            Result := Result + Temp;
            if DoCopy then UpdateOutStream(S);
            ReadBuffer;
            if FSourcePtr^ = #0 then Exit;
            S := FSourcePtr;
            Continue;
          end;
        #10:
          Inc(FSourceLine);
        #33..#255:
          Break;
      end;
      Inc(FSourcePtr);
    end;
    if DoCopy then UpdateOutStream(S);
  end;

var
  Found: Boolean;
begin
  Found := False;
  Result := '';
  while (not Found) and (Token <> ptEOF) do
  begin
    S := FSourcePtr;
    InternalSkipBlanks;
    if S <> FSourcePtr then
    begin
      SetString(Temp, S, FSourcePtr - S);
      Result := Result + Temp;
    end;
    SkipToNextToken(DoCopy, DoCopy);
    Found := (Token = AToken);
    if not Found then
    begin
      SetString(Temp, FTokenPtr, FSourcePtr - FTokenPtr);
      Result := Result + Temp;
    end;
  end;
end;

function TWebStencilsParser.SkipToToken(AToken: UTF8Char): UTF8String;
begin
  Result := CopySkipToToken(AToken, False);
end;

{ TWebStencilsFakeSession }

type
  TWebStencilsFakeSession = class(TObject)
  private
    FProcessor: TWebStencilsProcessor;
    function GetAuthenticated: Boolean;
    function GetUserRoles: string;
  public
    constructor Create(AProcessor: TWebStencilsProcessor);
    function UserHasRole(const ARole: string): Boolean;
    property Authenticated: Boolean read GetAuthenticated;
    property UserRoles: string read GetUserRoles;
  end;

constructor TWebStencilsFakeSession.Create(AProcessor: TWebStencilsProcessor);
begin
  inherited Create;
  FProcessor := AProcessor;
end;

function TWebStencilsFakeSession.GetAuthenticated: Boolean;
begin
  Result := FProcessor.UserLoggedIn;
end;

function TWebStencilsFakeSession.GetUserRoles: string;
begin
  Result := FProcessor.UserRoles;
end;

function TWebStencilsFakeSession.UserHasRole(const ARole: string): Boolean;
begin
  Result := Web.HTTPApp.UserHasRole(FProcessor.UserRoles, ARole, FProcessor.FUserRoleList);
end;

{ TWebStencilsProcessor }

constructor TWebStencilsProcessor.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FPathTemplate := TWebStencilsPathTemplate.Create;
  FDataVars := TWebStencilsDataVars.Create(Self);
  FLoopVars := TWebStencilsLoopVars.Create;
  FAliasVars := TWebStencilsAliasVars.Create(Self);
  FLanguageID := 0; // use a global default value instead?
  FAddedPages := TStringList.Create;
  FAddedHeaders := TStringList.Create;
  FInputLines := TStringList.Create;
  FDefaultFileExt := CDefFileExt;
end;

destructor TWebStencilsProcessor.Destroy;
begin
  FInputLines.Free;
  FAddedHeaders.Free;
  FAddedPages.Free;
  FAliasVars.Free;
  FLoopVars.Free;
  FDataVars.Free;
  FPathTemplate.Free;
  FFakeSession.Free;
  inherited Destroy;
end;

class constructor TWebStencilsProcessor.Create;
begin
  FWhitelist := TMembersWhitelist.Create;
  FWhitelist.Configure(TDataSet,
    ['Active', 'FieldByName', 'First', 'Last', 'Next', 'Prior',
     'Bof', 'Eof', 'FieldCount', 'Fields', 'Found', 'RecordCount', 'RecNo'], nil, True);
  FWhitelist.Configure(TStrings,
    ['Contains', 'ContainsName', 'IndexOf', 'IndexOfName',
     'CommaText', 'Count', 'IsEmpty', 'DelimitedText', 'Names', 'KeyNames',
       'Values', 'ValueFromIndex', 'Strings', 'Text'], nil, True);
end;

function TWebStencilsProcessor.GetDataVarDuplicates: TWebStencilsDataVarDuplicates;
begin
  Result := FDataVars.Duplicates;
end;

procedure TWebStencilsProcessor.SetEngine(const AValue: TWebStencilsEngine);
begin
  if FEngine <> AValue then
  begin
    if Assigned(FEngine) then
      FEngine.RemoveFreeNotification(Self);
    FEngine := AValue;
    if Assigned(FEngine) then
      FEngine.FreeNotification(Self);
  end;
end;

procedure TWebStencilsProcessor.SetDataVarDuplicates(const AValue: TWebStencilsDataVarDuplicates);
begin
  FDataVars.Duplicates := AValue;
end;

procedure TWebStencilsProcessor.SetInputLines(const AValue: TStrings);
begin
  FInputLines.SetStrings(AValue);
end;

function TWebStencilsProcessor.GetWebRequest: TWebRequest;
begin
  if ProcessorRequest <> nil then
    Result := ProcessorRequest.WebRequest
  else if FWebRequest <> nil then
    Result := FWebRequest
  else if Dispatcher <> nil then
    Result := Dispatcher.Request
  else
    Result := nil;
end;

function TWebStencilsProcessor.GetRttiContext: TRttiContext;
begin
  Result := FRTTIContext;
end;

function TWebStencilsProcessor.GetPathTemplate: string;
begin
  Result := FPathTemplate.Template;
end;

procedure TWebStencilsProcessor.SetPathTemplate(const AValue: string);
begin
  FPathTemplate.Compile(AValue);
end;

procedure TWebStencilsProcessor.SetUserRoles(const AValue: string);
begin
  if FUserRoles <> AValue then
  begin
    FUserRoles := AValue;
    FUserRoleList := nil;
  end;
end;

function TWebStencilsProcessor.IsExtStored: Boolean;
begin
  Result := not SameStr(DefaultFileExt, CDefFileExt);
end;

procedure TWebStencilsProcessor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Engine) then
    Engine := nil;
end;

procedure TWebStencilsProcessor.AddVar(const AName: string; AObject: TObject; AOwned: Boolean);
begin
  FDataVars.Add(AName, AObject, AOwned);
end;

procedure TWebStencilsProcessor.AddVar(const AName: string; AObject: TObject; AOwned: Boolean;
  const ALookupFunc: TWebStencilsLookupFunc);
begin
  FDataVars.Add(AName, AObject, AOwned, ALookupFunc);
end;

procedure TWebStencilsProcessor.AddInitFunc(const AName: string;
  const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean);
begin
  FDataVars.AddInitFunc(AName, AInitFunc, AOwned);
end;

procedure TWebStencilsProcessor.AddInitFunc(const AName: string;
  const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean;
  const ALookupFunc: TWebStencilsLookupFunc);
begin
  FDataVars.AddInitFunc(AName, AInitFunc, AOwned, ALookupFunc);
end;

procedure TWebStencilsProcessor.AddModule(AModule: TObject);
begin
  FDataVars.AddModule(AModule);
end;

function TWebStencilsProcessor.HasVar(const AName: string): Boolean;
begin
  Result := FDataVars.ContainsKey(AName);
end;

procedure TWebStencilsProcessor.AddError(const AMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage)
  else if Assigned(FEngine) then
    FEngine.AddError(AMessage);
end;

procedure TWebStencilsProcessor.AddError(const AMessage: string; const AArgs: array of const);
begin
  AddError(Format(AMessage, AArgs));
end;

function TWebStencilsProcessor.ValidateMemberAccess(AObj: TObject; const AName: string): Boolean;
begin
  if Whitelist = nil then
    Exit(True);
  if not Whitelist.Validate(AObj, AName) then
    Exit(False);
  Result := Whitelist.Validate(AObj.ClassInfo, AName);
end;

procedure TWebStencilsProcessor.ResolveObjectProperty(const APropName: string;
  var AObj: TObject; var AProperty: TRttiProperty);
var
  P1, P2: PChar;
  LType: TRttiType;
  LProp: TRttiProperty;
  LName: string;
  LObj: TObject;
  LField: TField;
begin
  LObj := AObj;
  LProp := nil;
  P1 := PChar(APropName);
  P2 := nil;
  while LObj <> nil do
  begin
    P2 := StrScan(P1, '.');
    if P2 = nil then
      if P1 = Pointer(APropName) then
        LName := APropName
      else
        LName := P1
    else
      SetString(LName, P1, P2 - P1);
    LField := nil;
    if LObj is TObjectField then
    begin
      LProp := nil;
      LField := TObjectField(LObj).Fields.FindField(LName);
      if LField <> nil then
      begin
        if LField is TDataSetField then
          LObj := TDataSetField(LField).NestedDataSet
        else
          LObj := LField;
        if P2 = nil then
          Break;
      end;
    end
    else if LObj is TDataSet then
    begin
      LProp := nil;
      LField := TDataSet(LObj).FindField(LName);
      if LField <> nil then
      begin
        if LField is TDataSetField then
          LObj := TDataSetField(LField).NestedDataSet
        else
          LObj := LField;
        if P2 = nil then
          Break;
      end;
    end;
    if LField = nil then
    begin
      if not ValidateMemberAccess(LObj, LName) then
      begin
        LProp := nil;
        Break;
      end;
      LType := FRTTIContext.GetType(LObj.ClassType);
      LProp := LType.GetProperty(LName);
      if (LProp = nil) or (P2 = nil) or (LProp.PropertyType.TypeKind <> tkClass) then
        Break;
      LObj := LProp.GetValue(LObj).AsObject;
    end;
    P1 := P2 + 1;
  end;
  if P2 <> nil then
    LProp := nil;
  AObj := LObj;
  AProperty := LProp;
end;

function TWebStencilsProcessor.GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
  var AValue: string): Boolean;
var
  LProp: TRttiProperty;
  LObj: TObject;
begin
  Result := False;
  LObj := AObj;
  ResolveObjectProperty(APropName, LObj, LProp);
  if LObj = nil then
  begin
    AddError(SWebStencilsNullObject, [APropName, AObjName]);
    AValue := AObjName + '.' + APropName;
  end
  else if (LObj is TField) and (LProp = nil) then
  begin
    AValue := TField(LObj).AsString;
    Result := True;
  end
  else if (LProp <> nil) and (LProp.Visibility in [mvPublic, mvPublished]) then
  begin
    AValue := LProp.GetValue(LObj).ToString(GetFormatSettings^);
    Result := True;
  end
  else
  begin
    if AObj is TDataSet then
      AddError(SWebStencilsMissingFieldName, [APropName, AObjName])
    else
      AddError(SWebStencilsPropNotFound, [APropName, AObjName, AObj.ClassName]);
    AValue := AObjName + '.' + APropName;
  end;
end;

function TWebStencilsProcessor.GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
  var AValue: Boolean): Boolean;
var
  LProp: TRttiProperty;
  LObj: TObject;
begin
  Result := False;
  AValue := False;
  LObj := AObj;
  ResolveObjectProperty(APropName, LObj, LProp);
  if LObj = nil then
    AddError(SWebStencilsNullObject, [APropName, AObjName])
  else if (LObj is TField) and (LProp = nil) then
  begin
    Result := True;
    case TField(LObj).DataType of
    ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftAutoInc, ftLargeint,
    ftFMTBcd, ftLongWord, ftShortint, ftByte, TFieldType.ftExtended, TFieldType.ftSingle, ftLargeUint:
      AValue := TField(LObj).AsExtended <> 0.0;
    ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftOraClob, ftFixedWideChar, ftWideMemo:
      AValue := TField(LObj).AsString <> '';
    ftOraTimeStamp, ftDate, ftTime, ftDateTime, ftTimeStamp:
      AValue := TField(LObj).AsDateTime <> 0.0;
    ftBoolean:
      AValue := TField(LObj).AsBoolean;
    else
      AValue := not TField(LObj).IsNull;
    end;
  end
  else if (LProp <> nil) and (LProp.Visibility in [mvPublic, mvPublished]) then
  begin
    Result := True;
    if LProp.PropertyType.TypeKind = tkInteger then
      AValue := LProp.GetValue(LObj).AsInteger <> 0 // true if not zero, false if zero
    else if LProp.PropertyType.TypeKind in [tkChar, tkWChar, tkString, tkWString, tkLString,
                                            tkUnicodeString, tkWideString, tkAnsiString] then
      AValue := LProp.GetValue(LObj).AsString <> '' // true if not empty string, false if empty
    else if LProp.PropertyType.TypeKind in [tkChar, tkWideChar] then
      AValue := LProp.GetValue(LObj).AsString <> #0 // true if not zero char, false if zero char
    else if IsBoolType(LProp.PropertyType.Handle) then
      AValue := LProp.GetValue(LObj).AsBoolean // true if true
    else
    begin
      AddError(SWebStencilsPropNotSupported, [APropName, AObjName, AObj.ClassName]);
      Result := False;
    end;
  end
  else
  begin
    if AObj is TDataSet then
      AddError(SWebStencilsMissingFieldName, [APropName, AObjName])
    else
      AddError(SWebStencilsPropNotFound, [APropName, AObjName, AObj.ClassName]);
  end;
end;

function TWebStencilsProcessor.GetObjectProperty(AObj: TObject; const AObjName, APropName: string;
  var AValue: TObject): Boolean;
var
  LProp: TRttiProperty;
  LObj: TObject;
begin
  Result := False;
  AValue := nil;
  LObj := AObj;
  ResolveObjectProperty(APropName, LObj, LProp);
  if LObj = nil then
    AddError(SWebStencilsNullObject, [APropName, AObjName])
  else if (LObj is TDataSet) and (LProp = nil) then
  begin
    AValue := LObj;
    Result := True;
  end
  else if (LProp <> nil) and (LProp.Visibility in [mvPublic, mvPublished]) then
  begin
    if LProp.PropertyType.TypeKind = tkClass then
    begin
      AValue := LProp.GetValue(LObj).AsObject;
      Result := True;
    end
    else
      AddError(SWebStencilsPropNotSupported, [APropName, AObjName, AObj.ClassName]);
  end
  else
  begin
    if AObj is TDataSet then
      AddError(SWebStencilsMissingFieldName, [APropName, AObjName])
    else
      AddError(SWebStencilsPropNotFound, [APropName, AObjName, AObj.ClassName]);
  end;
end;

function TWebStencilsProcessor.GetDictionaryValue(AItem: TWebStencilsDataVar;
  const APropName: string; var AValue: string): Boolean;
begin
  Result := False;
  if not AItem.CheckObject(Self, APropName, AValue) then
    Exit;
  case AItem.Kind of
    rdikObject,
    rdikDataset:
      Result := GetObjectProperty(AItem.TheObject, AItem.Name, APropName, AValue);
    rdikStrings:
      begin
        AValue := TStrings(AItem.TheObject).Values[APropName];
        Result := True;
      end;
    rdikLookup:
      begin
        Result := AItem.LookupFunc(AItem, APropName, AValue);
        if not Result then
        begin
          AddError(SWebStencilsDotValNotFound, [APropName, AItem.Name]);
          AValue := AItem.Name + '.' + APropName;
        end;
      end;
  end;
end;

function TWebStencilsProcessor.GetVariableName(AParser: TWebStencilsParser;
  AExcludeFirstDot: Boolean = False): string;
var
  LBlock: string;
  LToken: string;
begin
  Result := '';
  while AExcludeFirstDot or (AParser.NextChar = '.') do
  begin
    if not AExcludeFirstDot then
      AParser.SkipToken(False);
    AParser.SkipToken(False);

    if AParser.Token = '@' then
    begin
      AParser.SkipToken(False);
      if AParser.Token = '(' then
      begin
        LBlock := AParser.SkipToToken(')');
        LToken := EvalExpressionValue(LBlock, rtString);
      end
      else
      begin
        LBlock := AParser.TokenString;
        LToken := EvalVariableValue(Result, LBlock, rtString);
      end;
    end
    else
      LToken := AParser.TokenString;

    if Result <> '' then
      Result := Result + '.';
    Result := Result + LToken;
    AExcludeFirstDot := False;
  end;
end;

class function TWebStencilsProcessor.StringToBoolean(const AStr: string): Boolean;
begin
  Result := (AStr <> '') and not SameText(AStr, 'False') and not SameText(AStr, 'Off') and (AStr <> '0');
end;

function TWebStencilsProcessor.EvalVariableValue(const AObjName, APropName: string;
  ARetType: TRetType): Variant;
var
  LObjName: string;
  LItem: TWebStencilsDataVar;
  LVar: TWebStencilsLoopVar;
  LStr: string;
  LBool: Boolean;
  LOk: Boolean;
  LBmk: TBookmark;
begin
  LStr := '';
  LOk := True;
  LObjName := FAliasVars.ResolveAlias(AObjName);

  // depending on the main symbol: special processing
  if LObjName = 'lang' then
    LStr := GetLang(APropName)

  // loop variable processing
  else if FLoopVars.TryGetValue(LObjName, LVar) then
  begin
    if APropName = '' then
    begin
      AddError(SWebStencilsMissingDotAfterLoop, [AObjName]);
      LOk := False;
    end
    else if LVar.LoopObject is TStrings then
    begin
      if SameText(APropName, CLoopVarEof) then
      begin
        LBool := LVar.FCurrPos >= TStrings(LVar.LoopObject).Count - 1;
        if ARetType in [rtBool, rtNotBool] then
          Exit(LBool);
        LStr := BoolToStr(LBool, True);
      end
      else if SameText(APropName, CLoopVarBof) then
      begin
        LBool := LVar.FCurrPos <= 0;
        if ARetType in [rtBool, rtNotBool] then
          Exit(LBool);
        LStr := BoolToStr(LBool, True);
      end
      else if SameText(APropName, 'name') then
        LStr := TStrings(LVar.LoopObject).Names[LVar.FCurrPos]
      else if SameText(APropName, 'value') then
        LStr := TStrings(LVar.LoopObject).ValueFromIndex[LVar.FCurrPos]
      else
      begin
        AddError(SWebStencilsLoopSubObjNotFound, [APropName, AObjName]);
        LOk := False;
      end;
    end
    else if LVar.LoopObject is TDataSet then
    begin
      if SameText(APropName, CLoopVarEof) then
      begin
        LBool := True;
        if not TDataSet(LVar.LoopObject).Eof then
        begin
          LBmk := TDataSet(LVar.LoopObject).Bookmark;
          TDataSet(LVar.LoopObject).Next;
          LBool := TDataSet(LVar.LoopObject).CompareBookmarks(LBmk, TDataSet(LVar.LoopObject).Bookmark) = 0;
          if not LBool then
            TDataSet(LVar.LoopObject).Prior;
        end;
        if ARetType in [rtBool, rtNotBool] then
          Exit(LBool);
        LStr := BoolToStr(LBool, True)
      end
      else if SameText(APropName, CLoopVarBof) then
      begin
        LBool := TDataSet(LVar.LoopObject).Bof;
        if ARetType in [rtBool, rtNotBool] then
          Exit(LBool);
        LStr := BoolToStr(LBool, True);
      end
      else
      begin
        if ARetType in [rtBool, rtNotBool] then
        begin
          GetObjectProperty(LVar.LoopObject, AObjName, APropName, LBool);
          Exit(LBool);
        end;
        LOk := GetObjectProperty(LVar.LoopObject, AObjName, APropName, LStr);
      end;
    end
    else
    begin
      if SameText(APropName, CLoopVarBof) then
      begin
        LBool := LVar.FCurrPos <= 0;
        if ARetType in [rtBool, rtNotBool] then
          Exit(LBool);
        LStr := BoolToStr(LBool, True);
      end
      else
      begin
        if ARetType in [rtBool, rtNotBool] then
        begin
          GetObjectProperty(LVar.CurrentObj, AObjName, APropName, LBool);
          Exit(LBool);
        end;
        LOk := GetObjectProperty(LVar.CurrentObj, AObjName, APropName, LStr);
      end;
    end;
  end

  // generic processing
  else
  begin
    if DataVars.TryGetValue(LObjName, LItem) then
      LOk := GetDictionaryValue(LItem, APropName, LStr)
    else if Assigned(Engine) and Engine.DataVars.TryGetValue(LObjName, LItem) then
      LOk := GetDictionaryValue(LItem, APropName, LStr)
    else
      LOk := GetOtherValue(AObjName, APropName, LStr);
  end;

  if ARetType in [rtBool, rtNotBool] then
    if not LOk then
      Result := ARetType = rtNotBool
    else
      Result := StringToBoolean(LStr)
  else
    if not LOk and (LStr = '') then
      Result := AObjName + '.' + APropName
    else
      Result := LStr;
end;

function TWebStencilsProcessor.MakeBasicEnums: IScope;
var
  LScope: TDictionaryScope;
  LType: TRttiEnumerationType;
  LName: string;
begin
  LScope := TDictionaryScope.Create;
  Result := LScope;

  LType := FRTTIContext.GetType(TypeInfo(TFieldType)) as TRttiEnumerationType;
  for LName in LType.GetNames do
    LScope.Map.Add(LName, TValueWrapper.Create(
      TValue.From<TFieldType>(LType.GetValue<TFieldType>(LName))
      ));
  LType := FRTTIContext.GetType(TypeInfo(TAlignment)) as TRttiEnumerationType;
  for LName in LType.GetNames do
    LScope.Map.Add(LName, TValueWrapper.Create(
      TValue.From<TAlignment>(LType.GetValue<TAlignment>(LName))
      ));
end;

function TWebStencilsProcessor.BasicEnums: IScope;
begin
  if FBasicEnums = nil then
  begin
    Result := MakeBasicEnums;
    if TInterlocked.CompareExchange(Pointer(FBasicEnums), Pointer(Result), nil) = nil then
      Pointer(Result) := nil;
  end;
  Result := FBasicEnums;
end;

function TWebStencilsProcessor.EvalExpressionValue(const AExpression: string;
  ARetType: TRetType): Variant;
var
  LExpression: TBindingExpression;
  LScopes: array[0..5] of IScope;
  i: Integer;
  LValue: TValue;
  LStr: string;
begin
  try
    i := 0;
    LScopes[i] := TBindingMethodsFactory.GetMethodScope;
    Inc(i);
    LScopes[i] := BasicEnums;
    Inc(i);
    if not FAliasVars.IsEmpty then
    begin
      LScopes[i] := FAliasVars.EvalScope;
      Inc(i);
    end;
    if not FLoopVars.IsEmpty then
    begin
      LScopes[i] := FLoopVars.EvalScope;
      Inc(i);
    end;
    if not DataVars.IsEmpty then
    begin
      LScopes[i] := DataVars.EvalScope;
      Inc(i);
    end;
    if Assigned(Engine) and not Engine.DataVars.IsEmpty then
    begin
      LScopes[i] := Engine.DataVars.EvalScope;
      Inc(i);
    end;
    LExpression := TBindings.CreateExpression(Slice(LScopes, i), AExpression, Whitelist);
    try
      LValue := LExpression.Evaluate.GetValue;
      if ARetType in [rtBool, rtNotBool] then
      begin
        if LValue.IsType<Boolean> then
          Result := LValue.AsBoolean
        else
        begin
          LStr := LValue.ToString(GetFormatSettings^);
          Result := StringToBoolean(LStr);
        end;
      end
      else
        Result := LValue.ToString(GetFormatSettings^);
    finally
      LExpression.Free;
    end;
  except
    on E: Exception do
    begin
      AddError(SWebStencilsErrorEvalExpression, [AExpression, E.Message]);
      if ARetType = rtString then
        Result := 'error'
      else if ARetType = rtBool then
        Result := False
      else if ARetType = rtNotBool then
        Result := True;
    end;
  end;
end;

function TWebStencilsProcessor.GetLoopOrDictionaryObject(const AObjName: string): TObject;
var
  LObjName: string;
  LLoopVar: TWebStencilsLoopVar;
  LDictVar: TWebStencilsDataVar;
begin
  Result := nil;
  LObjName := FAliasVars.ResolveAlias(AObjName);

  // search loop variables first
  if FLoopVars.TryGetValue(LObjName, LLoopVar) then
    if LLoopVar.CurrentObj = nil then
      Exit(LLoopVar.LoopObject)
    else
      Exit(LLoopVar.CurrentObj); // the current element of the loop

  // now search the local dictionary
  if DataVars.TryGetValue(LObjName, LDictVar) then
    Exit(LDictVar.TheObject);

  // search the engine
  if Assigned(Engine) then
    if Engine.DataVars.TryGetValue(LObjName, LDictVar) then
      Exit(LDictVar.TheObject);
end;

function TWebStencilsProcessor.GetEntireDottedValue(AParser: TWebStencilsParser): string;
var
  LTokenAfterDot: string;
begin
  Result := AParser.TokenString;
  LTokenAfterDot := GetVariableName(AParser);
  if LTokenAfterDot <> '' then
    Result := Result + '.' + LTokenAfterDot;
  AParser.SkipToken(False);
end;

function TWebStencilsProcessor.QualifyFileName(const AFileName: string): string;
var
  LRootDir: string;
begin
  if Assigned(FProcessorRequest) and Assigned(FProcessorRequest.GetFileNameFromPathFunc) then
    Result := FProcessorRequest.GetFileNameFromPathFunc(AFileName)
  else
  begin
    Result := AFileName;
    if PathDelim <> '/' then
      Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
    if TPath.IsRelativePath(Result) then
    begin
      if Assigned(FEngine) then
        LRootDir := FEngine.RootDirectory
      else if InputFileName <> '' then
        LRootDir := ExtractFilePath(InputFileName)
      else
        raise EWebStencilsException.CreateRes(@SWebStencilsUndefFileFolder);
      Result := TPath.Combine(LRootDir, Result);
    end;
  end;
  Result := UpdateFileNameExt(Result, DefaultFileExt);
  Result := ExpandFileName(Result);
end;

function TWebStencilsProcessor.GetFormatSettings: PFormatSettings;
begin
  if Assigned(FEngine) then
    Result := FEngine.GetLangFormats(LanguageId)
  else
    Result := @FormatSettings;
end;

function TWebStencilsProcessor.GetLang(const APropName: string): string;
begin
  Result := APropName;
  if Assigned(FOnLanguage) then
    FOnLanguage(Self, APropName, Result)
  else if Assigned(FEngine) then
    Result := FEngine.GetLang(APropName);
end;

function TWebStencilsProcessor.GetOtherValue(const AObjectName, APropName: string;
  var AValue: string): Boolean;
begin
  AValue := '';
  Result := False;
  if Assigned(FOnValue) then
    FOnValue(Self, AObjectName, APropName, AValue, Result)
  else if Assigned(FEngine) then
    Result := FEngine.GetOtherValue(AObjectName, APropName, AValue);
  if not Result and (AValue = '') then
  begin
    AddError(SWebStencilsDotValNotFound, [APropName, AObjectName]);
    AValue := AObjectName + '.' + APropName; // return the input
  end;
end;

function TWebStencilsProcessor.GetFileContent(const AFileName: string; var AValue: string): Boolean;
begin
  AValue := '';
  Result := False;
  if Assigned(FOnFile) then
    FOnFile(Self, AFileName, AValue, Result)
  else if Assigned(FEngine) then
    Result := FEngine.GetFileContent(AFileName, AValue);
end;

function TWebStencilsProcessor.GetScaffolding(const ADottedClass: string): string;
begin
  Result := ADottedClass;
  if Assigned(FOnScaffolding) then
    FOnScaffolding(Self, ADottedClass, Result)
  else if Assigned(FEngine) then
    Result := FEngine.GetScaffolding(ADottedClass);
end;

function TWebStencilsProcessor.DoLoop(const ABlock: string; ALoopVar: TWebStencilsLoopVar): string;
var
  LStream: TStringStream;
begin
  Result := '';
  LStream := TStringStream.Create(ABlock, TEncoding.UTF8);
  try
    if ALoopVar.InitLoop then
    try
      while ALoopVar.NextLoop do
      begin
        // reset input stream
        LStream.Position := 0;
        Result := Result + DoStream(LStream);
      end;
    finally
      ALoopVar.DoneLoop;
    end;
  finally
    LStream.Free;
  end;
end;

function TWebStencilsProcessor.DoStream(AStream: TStream): string;
label
  reuse_token;
const
  CBoolRetType: array[Boolean] of TRetType = (rtNotBool, rtBool);
var
  LParser: TWebStencilsParser;
  LParsedTemplate: string;
  LParsedExtraHeader: string;
  LAddedPageContent: string;
  LRole: string;
  LOutStream: TStringStream;
  LTokenStr: string;
  LFollowToken: string;
  LBlock: string;
  LIfValue: Boolean;
  LIfValueExpected: Boolean;
  LLoopVar: TWebStencilsLoopVar;
  LAddedText: string;
  LOk, LEncode: Boolean;
  I: Integer;
  LFileName: string;
  LAliasVarCount: Integer;
  LRecursion: Boolean;
  LPadding: Integer;
  LSwitchValue: Variant;

  procedure SkipToOpenCurlyBrace;
  begin
    while not (LParser.Token in ['{', TWebStencilsParser.ptEOF]) do
      LParser.SkipToken(False);
  end;

  // returns loop object token
  function ParseForEach: TWebStencilsLoopVar;
  var
    LVarToken: string;
    LLoopVarToken: string;
    LLoopCurrentObjToken: string;
    LLoopObjTokenAfterDot: string;
    LInToken: string;
    LParCloseToken: string;
  begin
    Result := nil;
    LParser.SkipToken(False);  // Skip (

    LVarToken := LParser.TokenString;
    if not SameText(LVarToken, 'var') then
    begin
      AddError(SWebStencilsMissingVar);
      SkipToOpenCurlyBrace;
      Exit;
    end;
    LParser.SkipToken(False);  // Skip var

    LLoopVarToken := LParser.TokenString;
    LParser.SkipToken(False);  // skip loop object

    LInToken := LParser.TokenString;
    if not SameText(LInToken, 'in') then
    begin
      AddError(SWebStencilsMissingIn);
      SkipToOpenCurlyBrace;
      Exit;
    end;
    LParser.SkipToken(False);  // Skip in

    LLoopCurrentObjToken := LParser.TokenString;
    LParser.SkipToken(False);  // Skip list object
    LParCloseToken := LParser.TokenString;

    if LParCloseToken = '.' then
    begin
      LLoopObjTokenAfterDot := GetVariableName(LParser, True);
      LParser.SkipToken(False);  // Skip object property
      LParCloseToken := LParser.TokenString;
    end;

    if LParCloseToken <> ')' then
    begin
      AddError(SWebStencilsMissingBrace);
      SkipToOpenCurlyBrace;
      Exit;
    end;
    LParser.SkipToken(False); // Skip )

    if FLoopVars.ContainsKey(LLoopVarToken) then
    begin
      AddError(SWebStencilsLoopVarRedefined, [LLoopVarToken]);
      Exit;
    end;

    // add loop variable to current list, and return it
    Result := TWebStencilsLoopVar.Create(Self, LLoopCurrentObjToken, LLoopObjTokenAfterDot, LLoopVarToken);
    FLoopVars.Add(LLoopVarToken, Result);
  end;

  // return block within braces
  function ExtractBlock(AOpen, AClose: UTF8Char; const AOper: string; var ABlock: string): Boolean;

    procedure TrimAloneFinalSpaces(var ABlock: string);
    var
      P, PStart: PChar;
    begin
      P := PChar(ABlock) + Length(ABlock) - 1;
      PStart := P;
      while True do
      begin
        case P^ of
          #0, #10:
            Break;
          #33..#255:
            Exit;
        end;
        Dec(P);
      end;
      if P < PStart then
        SetLength(ABlock, P - PChar(Pointer(ABlock)) + 1);
    end;

  var
    LParOpenCounter: Integer;
    LSavPos, LSavSize: Int64;
    LToken: UTF8Char;
    L: Integer;
    LMultiLine: Boolean;
  begin
    LParOpenCounter := 0;
    LSavPos := LOutStream.Position;
    LSavSize := LOutStream.Size;
    LMultiLine := False;

    repeat
      LToken := LParser.Token;
      if LToken = AOpen then
        Inc(LParOpenCounter)
      else if LToken = AClose then
        Dec(LParOpenCounter);

      if (LToken = AOpen) and (LParOpenCounter = 1) then
      begin
        if AOpen = '{' then
        begin
          LMultiLine := LParser.SkipAloneLineBreak;
          if not LMultiLine then
            LParser.SkipToken(True);
        end
        else
          LParser.SkipToken(True);
      end
      else if (LToken = AClose) and (LParOpenCounter = 0) then
        // none
      else
      begin
        LParser.CopyTokenToOutput;
        LParser.SkipToken(True);
      end;
    until (LParOpenCounter = 0) or (LParser.Token = TWebStencilsParser.ptEOF);

    // Reset outStream reposition
    L := LOutStream.Position - LSavPos;
    LOutStream.Position := LSavPos;
    // Grab source within braces from outStream
    ABlock := LOutStream.ReadString(L);
    LOutStream.Size := LSavSize;

    if LParOpenCounter <> 0 then
    begin
      if AOper <> '' then
        AddError(SWebStencilsMissingToken, [AClose, AOper]);
      ABlock := '';
      Result := False;
    end
    else
    begin
      if AClose = '}' then
      begin
        if LMultiLine and LParser.SkipAloneLineBreak then
          TrimAloneFinalSpaces(ABlock);
        LParser.SkipToken(False);
      end;
      Result := True;
    end;
  end;

  function ReplaceSymbol(const AString, ASymbol, AValue: string): string;
  var
    i: Integer;
  begin
    Result := AString;
    i := 1;
    while True do
    begin
      i := Pos('@', Result, i);
      if (i = 0) or (i + Length(ASymbol) - 1 > Length(Result)) then
        Break;
      if (AnsiStrLIComp(PChar(ASymbol), PChar(Result) + i - 1, Length(ASymbol)) = 0) and
         ((i + Length(ASymbol) - 1 = Length(Result)) or
          not (Result[i + Length(ASymbol)].IsLetterOrDigit or (Result[i + Length(ASymbol)] = '_'))) then
      begin
        Result := Copy(Result, 1, i - 1) + AValue + Copy(Result, i + Length(ASymbol));
        Inc(i, Length(AValue));
      end
      else
        Inc(i);
    end;
  end;

  function ParseValue(const AOper: string): string;
  begin
    LParser.SkipToken(False);
    if LParser.Token = '@' then
    begin
      LParser.SkipToken(False);
      // process expression
      if LParser.Token = '(' then
        if ExtractBlock('(', ')', AOper, LBlock) then
          Result := EvalExpressionValue(LBlock, rtString)
        else
          Result := 'error'
      // process variable
      else
      begin
        LTokenStr := LParser.TokenString;
        Result := EvalVariableValue(LTokenStr, GetVariableName(LParser), rtString);
      end;
      LParser.SkipToken(False);
    end
    else if LParser.Token = '"' then
    begin
      // process inline double quoted literal
      Result := LParser.SkipToToken('"');
      LParser.SkipToken(False);
    end
    else if LParser.Token = '''' then
    begin
      // process inline single quoted literal
      Result := LParser.SkipToToken('''');
      LParser.SkipToken(False);
    end
    else
    begin
      // process inline literal
      Result := '';
      while True do
      begin
        Result := Result + LParser.TokenString;
        if (LParser.NextChar <= #31) or
           (LParser.NextChar in [' ', ',', ';', '@', '"', '''', '<', '>', '|']) then
        begin
          LParser.SkipToken(False);
          Break;
        end;
        LParser.SkipToken(False);
      end;
    end;
  end;

  procedure ParseVarAliases(const AOper: string);
  var
    LBlock: string;
    LItems: TStrings;
    i: Integer;
  begin
    if not ExtractBlock('{', '}', AOper, LBlock) then
      Exit;
    LItems := TStringList.Create(#0, ',');
    try
      LItems.StrictDelimiter := True;
      LItems.DelimitedText := LBlock;
      for i := 0 to LItems.Count - 1 do
        FAliasVars.AddPair(
          LItems.Names[i].Trim.TrimLeft(['@']),
          LItems.ValueFromIndex[i].Trim.TrimLeft(['@']));
    finally
      LItems.Free;
    end;
  end;

begin
  LOutStream := TStringStream.Create('', TEncoding.UTF8);
  Inc(FNestingLevel);
  try
    LParser := TWebStencilsParser.Create(AStream, LOutStream);
    try
      while True do
      begin
        LRecursion := False;
        while not (LParser.Token in [TWebStencilsParser.ptEOF, '@']) do
        begin
          // just copy input into output and move on
          LParser.CopyTokenToOutput;
          LParser.SkipToken(True);
        end;
                                     
        if LParser.Token = TWebStencilsParser.ptEOF then
          Break;
        // WebStencils token found, read the actual symbol following @
        if LParser.Token = '@' then
        begin
          LParser.SkipToken(FDisableProcessing > 0);
          LTokenStr := LParser.TokenString;

reuse_token:
          // process WebStencils symbol
          if SameText(LTokenStr, 'Processing') then
          begin
            LParser.SkipToken(False);
            LTokenStr := LParser.TokenString;
            if not StringToBoolean(LTokenStr) then
              Inc(FDisableProcessing)
            else if FDisableProcessing > 0 then
              Dec(FDisableProcessing)
            else
              AddError(SWebStencilsUnbalancedProcessing);
            LParser.SkipToken(False);
          end
          else if FDisableProcessing > 0 then
            LOutStream.WriteString('@')
          else if LTokenStr = '@' then
          begin
            LParser.SkipToken(False);
            LTokenStr := LParser.TokenString;
            if LTokenStr = '@' then
            begin
              // triple '@@@' to recursively process
              LParser.SkipToken(False);
              LTokenStr := LParser.TokenString;
              if LTokenStr = '@' then
                LOutStream.WriteString('@')
              else
                LRecursion := True;
              goto reuse_token;
            end
            else
              // double '@@' to output '@'
              LOutStream.WriteString('@');
          end
          else if SameText(LTokenStr, 'Import') then
          begin
            LFileName := ParseValue(LTokenStr);
            LAliasVarCount := FAliasVars.Count;
            try
              if LParser.Token = '{' then
                ParseVarAliases('Import');
              if GetFileContent(LFileName, LBlock) then
                LOutStream.WriteString(DoString(LBlock))
              else
                LOutStream.WriteString(DoFile(QualifyFileName(LFileName)));
            finally
              FAliasVars.Cleanup(LAliasVarCount);
            end;
          end
          else if SameText(LTokenStr, 'LayoutPage') then
          begin
            LFileName := ParseValue(LTokenStr);
            if not LParsedTemplate.IsEmpty then
              AddError(SWebStencilsMultipleLayouts)
            else if GetFileContent(LFileName, LBlock) then
              LParsedTemplate := DoString(LBlock)
            else
              LParsedTemplate := DoFile(QualifyFileName(LFileName));
          end
          else if SameText(LTokenStr, 'AddPage') then
          begin
            LFileName := ParseValue(LTokenStr);
            if GetFileContent(LFileName, LBlock) then
              LAddedPageContent := DoString(LBlock)
            else
              LAddedPageContent := DoFile(QualifyFileName(LFileName));
            // add page to stringlist
            FAddedPages.Add(LAddedPageContent);
          end
          else if SameText(LTokenStr, 'ExtraHeader') then
          begin
            LParser.SkipToken(False);
            if LParser.Token <> '{' then
              AddError(SWebStencilsMissingToken, ['{', 'ExtraHeader'])
            else if ExtractBlock('{', '}', 'ExtraHeader', LBlock) then
            begin
              LParsedExtraHeader := DoString(LBlock);
              FAddedHeaders.Add(LParsedExtraHeader);
            end;
          end
          else if SameText(LTokenStr, 'RenderBody') then
          begin
            // leave it in
            LOutStream.WriteString('@RenderBody');
            LParser.SkipToken(True);
            Include(FRenderParts, rpBody);
          end
          else if SameText(LTokenStr, 'RenderHeader') then
          begin
            // leave it in
            LOutStream.WriteString('@RenderHeader');
            LParser.SkipToken(True);
            Include(FRenderParts, rpHeader);
          end
          else if SameText(LTokenStr, 'RenderPages') then
          begin
            // leave it in
            LOutStream.WriteString('@RenderPages');
            LParser.SkipToken(True);
            Include(FRenderParts, rpPages);
          end
          else if SameText(LTokenStr, 'Scaffolding') then
          begin
            LParser.SkipToken(False);
            LFollowToken := GetEntireDottedValue(LParser);
            LBlock := GetScaffolding(LFollowToken);
            LOutStream.WriteString(DoString(LBlock));
          end
          else if SameText(LTokenStr, 'LoginRequired') then
          begin
            LParser.SkipToken(False);
            if LParser.Token = '(' then
            begin
              LParser.SkipToken(False);
              LRole := LParser.TokenString;
              LParser.SkipToken(False);
              if LParser.Token <> ')' then
                AddError(SWebStencilsMissingBrace2)
              else
                LParser.SkipToken(True);
            end
            else
              LRole := '';
            if not UserLoggedIn then
              raise EWebNotAuthenticated.CreateRes(@SWebStencilsLoginIsRequired)
            else if (LRole <> '') and not UserHasRole(UserRoles, LRole, FUserRoleList) then
              raise EWebNotAuthorized.CreateRes(@SWebStencilsLoginRoleRequired);
          end
          else if SameText(LTokenStr, 'ForEach') then
          begin
            LParser.SkipToken(False);
            LFollowToken := LParser.TokenString;
            if LFollowToken = '(' then
              // new WebStencils syntax with local var
              LLoopVar := ParseForEach
            else
            begin
              // old WebStencils syntax with "loop" var
              if FLoopVars.ContainsKey('loop') then
              begin
                AddError(SWebStencilsLoopVarRedefined, ['loop']);
                LLoopVar := nil;
              end
              else
              begin
                LLoopVar := TWebStencilsLoopVar.Create(Self, LFollowToken, '', 'loop');
                FLoopVars.Add('loop', LLoopVar);
              end;
              LParser.SkipToken(False);
            end;
            try
              if LParser.Token = '{' then
              begin
                if ExtractBlock('{', '}', 'foreach', LBlock) and (LLoopVar <> nil) then
                  LOutStream.WriteString(DoLoop(LBlock, LLoopVar));
              end
              else
              begin
                if LLoopVar <> nil then
                  AddError(SWebStencilsMissingToken, ['{', 'foreach']);
                LParser.SkipToToken('}');
                LParser.SkipToken(True);
              end;
            finally
              if LLoopVar <> nil then
                FLoopVars.Remove(LLoopVar.VarName);
            end;
          end
          else if SameText(LTokenStr, 'if') then
          begin
            LParser.SkipToken(False);
            LFollowToken := LParser.TokenString;
            if SameText(LFollowToken, 'not') then
            begin
              LIfValueExpected := False;
              LParser.SkipToken(False);
              LFollowToken := LParser.TokenString;
            end
            else
              LIfValueExpected := True;

            if LParser.Token = '{' then
            begin
              AddError(SWebStencilsMissingExpression);
              ExtractBlock('{', '}', '', LBlock);
            end
            else
            begin
              if LParser.Token = '(' then
              begin
                if ExtractBlock('(', ')', 'if', LBlock) then
                begin
                  LIfValue := EvalExpressionValue(LBlock, CBoolRetType[LIfValueExpected]);
                  LParser.SkipToken(False);
                end
                else
                  LIfValue := not LIfValueExpected;
              end
              else
              begin
                LIfValue := EvalVariableValue(LFollowToken,
                  GetVariableName(LParser), CBoolRetType[LIfValueExpected]);
                LParser.SkipToken(False);
              end;

              if LParser.Token = '{' then
              begin
                if ExtractBlock('{', '}', 'if', LBlock) then
                begin
                  // process the "if" block
                  if LIfValue = LIfValueExpected then
                    LOutStream.WriteString(DoString(LBlock));
                  if LParser.Token = '@' then
                  begin
                    LParser.SkipToken(False);
                    LTokenStr := LParser.TokenString;
                    if not SameText(LTokenStr, 'else') then
                      goto reuse_token;
                    LParser.SkipToken(False);
                    if LParser.Token = '{' then
                    begin
                      if ExtractBlock('{', '}', 'else', LBlock) then
                        // process the "else" block
                        if LIfValue <> LIfValueExpected then
                          LOutStream.WriteString(DoString(LBlock));
                    end
                    else
                    begin
                      AddError(SWebStencilsMissingToken, ['{', 'else']);
                      LParser.SkipToToken('}');
                      LParser.SkipToken(True);
                    end;
                  end;
                end;
              end
              else
              begin
                AddError(SWebStencilsMissingToken, ['{', 'if']);
                LParser.SkipToToken('}');
                LParser.SkipToken(True);
              end;
            end;
          end
          else if SameText(LTokenStr, 'switch') then
          begin
            LParser.SkipToken(False);
            if LParser.Token = '(' then
            begin
              if ExtractBlock('(', ')', 'switch', LBlock) then
              begin
                LSwitchValue := EvalExpressionValue(LBlock, rtString);
                LParser.SkipToken(False);
              end;
            end;
            if LParser.Token = '{' then
            begin
              if ExtractBlock('{', '}', 'switch', LBlock) then
              begin
                I := Length(FSwitchVars);
                SetLength(FSwitchVars,  I + 1);
                FSwitchVars[I].FValue := LSwitchValue;
                FSwitchVars[I].FProcessed := False;
                try
                  LOutStream.WriteString(DoString(LBlock));
                finally
                  SetLength(FSwitchVars, I);
                end;
              end;
            end
            else
            begin
              AddError(SWebStencilsMissingToken, ['{', 'switch']);
              LParser.SkipToToken('}');
              LParser.SkipToken(True);
            end;
          end
          else if SameText(LTokenStr, 'case') then
          begin
            LOk := Length(FSwitchVars) > 0;
            if not LOk then
              AddError(SWebStencilsNoSwitch, ['case']);
            LTokenStr := ParseValue('case');
            if LParser.Token = '{' then
            begin
              if ExtractBlock('{', '}', 'case', LBlock) and LOk then
              begin
                I := Length(FSwitchVars) - 1;
                try
                  LOk := VarSameValue(FSwitchVars[I].FValue, LTokenStr);
                except
                  LOk := False;
                end;
                if LOk then
                begin
                  FSwitchVars[I].FProcessed := True;
                  LOutStream.WriteString(DoString(LBlock));
                end;
              end;
            end
            else
            begin
              AddError(SWebStencilsMissingToken, ['{', 'case']);
              LParser.SkipToToken('}');
              LParser.SkipToken(True);
            end;
          end
          else if SameText(LTokenStr, 'default') then
          begin
            LOk := Length(FSwitchVars) > 0;
            if not LOk then
              AddError(SWebStencilsNoSwitch, ['default']);
            LParser.SkipToken(False);
            if LParser.Token = '{' then
            begin
              if ExtractBlock('{', '}', 'default', LBlock) and LOk then
                if not FSwitchVars[Length(FSwitchVars) - 1].FProcessed then
                  LOutStream.WriteString(DoString(LBlock));
            end
            else
            begin
              AddError(SWebStencilsMissingToken, ['{', 'default']);
              LParser.SkipToToken('}');
              LParser.SkipToken(True);
            end;
          end
          // comment @* .... *@
          else if SameText(LTokenStr, '*') then
          begin
            repeat
              LParser.SkipToToken('*');
              if LParser.NextChar in [#0, '@'] then
                LParser.SkipToken(False);
            until LParser.Token in [TWebStencilsParser.ptEOF, '@'];
            LParser.SkipToken(True);
          end
          else
          begin
            LOk := True;
            LEncode := True;
            // suppress HTML encoding
            if LParser.Token = '\' then
            begin
              LEncode := False;
              LParser.SkipToken(False);
              LTokenStr := LParser.TokenString;
            end;
            // process expression
            if LParser.Token = '(' then
            begin
              if ExtractBlock('(', ')', 'expression', LBlock) then
                LBlock := EvalExpressionValue(LBlock, rtString)
              else
                LOk := False;
            end
            // not a valid identifier
            else if not IsValidIdent(LTokenStr) then
            begin
              if LRecursion then
                LBlock := '@@@' + LTokenStr
              else
                LBlock := '@' + LTokenStr;
              LOutStream.WriteString(LBlock);
              LParser.SkipToken(True);
              LOk := False;
            end
            // process variable
            else
              LBlock := EvalVariableValue(LTokenStr, GetVariableName(LParser), rtString);
            if LOk then
            begin
              LPadding := 0;
              if LParser.NextChar = ':' then
              begin
                LParser.SkipToken(False);
                if not LParser.SkipNumber or not TryStrToInt(LParser.TokenString, LPadding) then
                  AddError(SWebStencilsMissingToken, ['number', ':']);
              end;
              if LRecursion then
                LBlock := DoString(LBlock);
              if LPadding > 0 then
                LBlock := LBlock.PadLeft(LPadding)
              else if LPadding < 0 then
                LBlock := LBlock.PadRight(-LPadding);
              if LEncode then
                LBlock := TNetEncoding.HTML.Encode(LBLock);
              LOutStream.WriteString(LBlock);
              LParser.SkipToken(True);
            end;
          end;
        end;
      end;
    finally
      LParser.Free;
    end;

    Result := LOutStream.DataString;
    if (rpBody in FRenderParts) and (LParsedTemplate <> '') then
      Result := ReplaceSymbol(LParsedTemplate, '@RenderBody', Result);

    if FNestingLevel = 1 then
    begin
      // collect extra headers content and place it
      if rpHeader in FRenderParts then
      begin
        LAddedText := '';
        for I := 0 to FAddedHeaders.Count - 1 do
          LAddedText := LAddedText + FAddedHeaders.Strings[I];
        Result := ReplaceSymbol(Result, '@RenderHeader', LAddedText);
      end;

      // collect secondary pages content and place it
      if rpPages in FRenderParts then
      begin
        LAddedText := '';
        for I := 0 to FAddedPages.Count - 1 do
          LAddedText := LAddedText + FAddedPages.Strings[I];
        Result := ReplaceSymbol(Result, '@RenderPages', LAddedText);
      end;
    end;
  finally
    Dec(FNestingLevel);
    LOutStream.Free;
  end;
end;

function TWebStencilsProcessor.DoString(const ABlock: string): string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(ABlock, TEncoding.UTF8);
  try
    Result := DoStream(LStream);
  finally
    LStream.Free;
  end;
end;

function TWebStencilsProcessor.DoFile(const AFileName: string): string;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if TFile.GetEncoding(LStream) <> TEncoding.UTF8 then
      raise EWebStencilsException.CreateResFmt(@SWebStencilsUnsupportedEncoding, [AFileName]);
    Result := DoStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TWebStencilsProcessor.DoBeforeProduce;
begin
  if Assigned(BeforeProduce) then
    BeforeProduce(Self);
end;

procedure TWebStencilsProcessor.DoAfterProduce;
begin
  if Assigned(AfterProduce) then
    AfterProduce(Self);
end;

procedure TWebStencilsProcessor.InitContent;
var
  LPage: TStringList;
  LReqPath,
  LFileName,
  LOrigFileName: string;
  I: Integer;
  LDataVars: TStrings;

  function ExtractLastSegment(const FileName: string): string;
  var
    I1, I2: Integer;
  begin
    I2 := Length(FileName);
    if (I2 > 0) and (FileName[I2] = '/') then
      Dec(I2);
    I1 := FileName.LastIndexOf('/', I2 - 1, I2);
    Result := Copy(FileName, I1 + 2, I2 - I1 - 1);
  end;

begin
  if not FOwnerAdded then
  begin
    FOwnerAdded := True;
    if (Owner <> nil) and (Engine = nil) then
      AddModule(Owner)
    else if Engine <> nil then
      Engine.CheckAddModule;
  end;
  FRTTIContext := TRTTIContext.Create;
  FInputFileNameTemp := False;
  LPage := TStringList.Create;
  try
    LReqPath := '';
    LFileName := '';
    LOrigFileName := '';

    if ProcessorRequest <> nil then
    begin
      LReqPath := ProcessorRequest.ResultingPath;
      LPage.AddStrings(ProcessorRequest.Vars);

      LOrigFileName := UpdateFileNameExt(
        ExtractLastSegment(ProcessorRequest.OriginalPath), DefaultFileExt);
      LFileName := ExtractLastSegment(LReqPath);
    end
    else
    begin
      if WebRequest <> nil then
      begin
        LPage.AddStrings(WebRequest.QueryFields);
        LReqPath := WebRequest.InternalPathInfo.Trim;
        if FPathTemplate.Template <> '' then
          FPathTemplate.Consume(LReqPath, LPage);
      end;

      if InputFileName <> '' then
        LFileName := ExtractFileName(InputFileName);
      if LFileName = '' then
        LFileName := ExtractLastSegment(LReqPath);
      if (InputFileName = '') and InputLines.IsEmpty and (LFileName <> '') then
      begin
        FInputFileNameTemp := True;
        InputFileName := QualifyFileName(LFileName);
      end;
    end;
    if LOrigFileName = '' then
      LOrigFileName := LFileName;

    LPage.Values['orig_filename'] := LOrigFileName;
    LPage.Values['orig_pagename'] := ChangeFileExt(LOrigFileName, '');
    LPage.Values['filename'] := LFileName;
    LPage.Values['pagename'] := ChangeFileExt(LFileName, '');
    LPage.Values['request_path'] := LReqPath;
    LPage.Values['request_segment'] := ExtractLastSegment(LReqPath);
    if WebRequest <> nil then
    begin
      LPage.AddPair('referer', WebRequest.Referer);
                                                         
      LPage.AddPair('browser', WebRequest.UserAgent);
      LPage.AddPair('address', WebRequest.RemoteAddr);
    end;
    DataVars.Add('page', LPage, True);
    if ProcessorRequest <> nil then
      DataVars.Add('query', ProcessorRequest.Vars, False)
    else if WebRequest <> nil then
      DataVars.Add('query', WebRequest.QueryFields, False);
    if WebRequest <> nil then
    begin
      if (ProcessorRequest = nil) or not Assigned(ProcessorRequest.GetLoggedUserFunc) then
        if (WebRequest.Session <> nil) and (WebRequest.Session.User <> nil) then
        begin
          UserLoggedIn := True;
          UserRoles := WebRequest.Session.User.UserRoles;
        end
        else
          UserLoggedIn := not WebRequest.AuthUserName.IsEmpty;
      if WebRequest.Session <> nil then
      begin
        DataVars.Add('session', WebRequest.Session, False);
        LDataVars := WebRequest.Session.DataVars;
        for I := 0 to LDataVars.Count - 1 do
          if LDataVars.Objects[I] <> nil then
            DataVars.Add(LDataVars.KeyNames[I], LDataVars.Objects[I], False);
      end;
    end;
    if (WebRequest = nil) or (WebRequest.Session = nil) then
    begin
      if FFakeSession = nil then
        FFakeSession := TWebStencilsFakeSession.Create(Self);
      DataVars.Add('session', FFakeSession, False);
    end;
  except
    LPage.Free;
    raise;
  end;
  DoBeforeProduce;
end;

procedure TWebStencilsProcessor.DoneContent;
var
  I: Integer;
  LDataVars: TStrings;
begin
  DoAfterProduce;
  if FInputFileNameTemp then
  begin
    FInputFileNameTemp := False;
    InputFileName := '';
  end;
  if Assigned(Engine) then
    Engine.DataVars.FEvalScope := nil;
  DataVars.FEvalScope := nil;
  FLoopVars.FEvalScope := nil;
  FLoopVars.Clear;
  FAliasVars.Clear;
  FAddedPages.Clear;
  FAddedHeaders.Clear;
  DataVars.Remove('page');
  DataVars.Remove('query');
  if (WebRequest <> nil) and (WebRequest.Session <> nil) then
  begin
    LDataVars := WebRequest.Session.DataVars;
    for I := 0 to LDataVars.Count - 1 do
      if LDataVars.Objects[I] <> nil then
        DataVars.Remove(LDataVars.KeyNames[I]);
  end;
  DataVars.Remove('session');
  FNestingLevel := 0;
  FRenderParts := [];
end;

function TWebStencilsProcessor.ContentFromStream(AStream: TStream): string;
begin
  if AStream = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  InitContent;
  try
    Result := DoStream(AStream);
  finally
    DoneContent;
  end;
end;

function TWebStencilsProcessor.ContentFromString(const AString: string): string;
begin
  InitContent;
  try
    Result := DoString(AString);
  finally
    DoneContent;
  end;
end;

function TWebStencilsProcessor.ContentFromFile(const AFileName: string): string;
var
  LBlock: string;
begin
  InitContent;
  try
    if GetFileContent(AFileName, LBlock) then
      Result := DoString(LBlock)
    else
      Result := DoFile(AFileName);
  finally
    DoneContent;
  end;
end;

function TWebStencilsProcessor.Content: string;
var
  LBlock: string;
begin
  InitContent;
  try
    if not InputFileName.IsEmpty then
      if GetFileContent(InputFileName, LBlock) then
        Result := DoString(LBlock)
      else
        Result := DoFile(InputFileName)
    else
      Result := DoString(InputLines.Text);
  finally
    DoneContent;
  end;
end;

{ TWebStencilsLocaleDictionary }

type
  TWebStencilsLocaleDictionary = class(TDictionary<Integer, PFormatSettings>)
  protected
    procedure ValueNotify(const Value: PFormatSettings; Action: TCollectionNotification); override;
  end;

procedure TWebStencilsLocaleDictionary.ValueNotify(const Value: PFormatSettings;
  Action: TCollectionNotification);
begin
  inherited ValueNotify(Value, Action);
  if Action = cnRemoved then
    Dispose(Value);
end;

{ TWebStencilsEngine }

constructor TWebStencilsEngine.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FPathTemplates := TWebStencilsPathTemplateCollection.Create(Self);
  FDataVars := TWebStencilsDataVars.Create(Self);
  FLocales := TWebStencilsLocaleDictionary.Create;
  FRttiContext := TRttiContext.Create;
  FDefaultFileExt := TWebStencilsProcessor.CDefFileExt;
end;

destructor TWebStencilsEngine.Destroy;
begin
  Dispatcher := nil;
  if not (csDesigning in ComponentState) then
    RemoveProcessor(nil, Self);
  FPathTemplates.Free;
  FDataVars.Free;
  FLocales.Free;
  inherited Destroy;
end;

procedure TWebStencilsEngine.SetDispatcher(const AValue: IInterface);
begin
  if FDispatcher <> AValue then
  begin
    if FDispatcher <> nil then
    begin
      if FDispatcher is TComponent then
        TComponent(FDispatcher).RemoveFreeNotification(Self);
      if not (csDesigning in ComponentState) then
        RemoveProcessor(FDispatcher, Self);
    end;
    if (AValue <> nil) and not (
        Supports(AValue, StringToGUID('{3CECB155-07D2-42CB-918E-27695972F1E3}')) or    // IEMSEndpointPublisher
        Supports(AValue, StringToGUID('{E6D33BE4-9FAE-D511-8D38-0050568E0E44}'))) then // IWebDispatch
      raise EInvalidCast.CreateRes(@SInvalidCast);
    FDispatcher := AValue;
    if FDispatcher <> nil then
    begin
      if FDispatcher is TComponent then
        TComponent(FDispatcher).FreeNotification(Self);
      if not (csDesigning in ComponentState) then
        AddProcessor(FDispatcher, Self);
    end;
  end;
end;

procedure TWebStencilsEngine.SetPathTemplates(const AValue: TWebStencilsPathTemplateCollection);
begin
  FPathTemplates.Assign(AValue);
end;

function TWebStencilsEngine.GetDataVarDuplicates: TWebStencilsDataVarDuplicates;
begin
  Result := FDataVars.Duplicates;
end;

procedure TWebStencilsEngine.SetDataVarDuplicates(const AValue: TWebStencilsDataVarDuplicates);
begin
  FDataVars.Duplicates := AValue;
end;

function TWebStencilsEngine.IsExtStored: Boolean;
begin
  Result := not SameStr(DefaultFileExt, TWebStencilsProcessor.CDefFileExt);
end;

function TWebStencilsEngine.GetRttiContext: TRttiContext;
begin
  Result := FRttiContext;
end;

procedure TWebStencilsEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (Dispatcher <> nil) and (TComponent(Dispatcher) = AComponent) then
      Dispatcher := nil;
end;

procedure TWebStencilsEngine.AddVar(const AName: string; AObject: TObject; AOwned: Boolean);
begin
  FDataVars.Add(AName, AObject, AOwned);
end;

procedure TWebStencilsEngine.AddVar(const AName: string; AObject: TObject; AOwned: Boolean;
  const ALookupFunc: TWebStencilsLookupFunc);
begin
  FDataVars.Add(AName, AObject, AOwned, ALookupFunc);
end;

procedure TWebStencilsEngine.AddInitFunc(const AName: string;
  const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean);
begin
  FDataVars.AddInitFunc(AName, AInitFunc, AOwned);
end;

procedure TWebStencilsEngine.AddInitFunc(const AName: string;
  const AInitFunc: TWebStencilsInitFunc; AOwned: Boolean;
  const ALookupFunc: TWebStencilsLookupFunc);
begin
  FDataVars.AddInitFunc(AName, AInitFunc, AOwned, ALookupFunc);
end;

procedure TWebStencilsEngine.AddModule(AModule: TObject);
begin
  FDataVars.AddModule(AModule);
end;

procedure TWebStencilsEngine.AddError(const AMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage);
end;

procedure TWebStencilsEngine.AddError(const AMessage: string; const AArgs: array of const);
begin
  AddError(Format(AMessage, AArgs));
end;

procedure TWebStencilsEngine.CheckAddModule;
begin
  if not FOwnerAdded then
  begin
    FOwnerAdded := True;
    if Owner <> nil then
      AddModule(Owner);
  end;
end;

procedure TWebStencilsEngine.DoPathInit(const ARequest: TWebPostProcessorRequest);
var
  LNewPath: string;
  LPath: TWebStencilsPathTemplateItem;
begin
  CheckAddModule;
  if PathTemplates.Match(ARequest.OriginalPath, DefaultFileExt, LNewPath, LPath) then
    LPath.DoPathInit(ARequest);
  if Assigned(FOnPathInit) then
    FOnPathInit(Self, ARequest);
end;

procedure TWebStencilsEngine.DoBeforeRequest(const ARequest: TWebPostProcessorRequest;
  var AAccept: Boolean);
begin
  if Assigned(FBeforeRequest) then
    FBeforeRequest(Self, ARequest, AAccept);
end;

procedure TWebStencilsEngine.DoFileNotFound(const ARequest: TWebPostProcessorRequest;
  var ANotFoundPagePath: string);
begin
  if Assigned(FOnFileNotFound) then
    FOnFileNotFound(Self, ARequest, ANotFoundPagePath);
end;

procedure TWebStencilsEngine.DoAfterRequest(const ARequest: TWebPostProcessorRequest;
  var AOutStream: TStream);
begin
  if Assigned(FAfterRequest) then
    FAfterRequest(Self, ARequest, AOutStream);
end;

function TWebStencilsEngine.GetLang(const APropName: string): string;
begin
  Result := '';
  if Assigned(FOnLanguage) then
    FOnLanguage(Self, APropName, Result);
end;

procedure TWebStencilsEngine.AddLangFormat(ALanguageId: Integer;
  const ASettings: TFormatSettings);
var
  LSettings: PFormatSettings;
begin
  New(LSettings);
  LSettings^ := ASettings;
  FLocales.AddOrSetValue(ALanguageId, LSettings);
end;

procedure TWebStencilsEngine.RemoveLangFormat(ALanguageId: Integer);
begin
  FLocales.Remove(ALanguageId);
end;

function TWebStencilsEngine.GetLangFormats(ALanguageId: Integer): PFormatSettings;
begin
  if not FLocales.TryGetValue(ALanguageId, Result) then
    Result := @FormatSettings;
end;

function TWebStencilsEngine.GetOtherValue(const AObjectName, APropName: string;
  var AValue: string): Boolean;
begin
  AValue := '';
  Result := False;
  if Assigned(FOnValue) then
    FOnValue(Self, AObjectName, APropName, AValue, Result);
end;

function TWebStencilsEngine.GetFileContent(const AFileName: string; var AValue: string): Boolean;
begin
  AValue := '';
  Result := False;
  if Assigned(FOnFile) then
    FOnFile(Self, AFileName, AValue, Result);
end;

function TWebStencilsEngine.GetScaffolding(const ADottedClass: string): string;
begin
  Result := ADottedClass;
  if Assigned(FOnScaffolding) then
    FOnScaffolding(Self, ADottedClass, Result)
end;

function TWebStencilsEngine.HasVar(const AName: string): Boolean;
begin
  Result := FDataVars.ContainsKey(AName);
end;

class function TWebStencilsEngine.IsMimeTypeValid(const APath: string;
  const AGetMimeTypeFunc: TWebGetMimeTypeFunc; var AMimeType: string): Boolean;
var
  LExt: string;
  LKind: TMimeTypes.TKind;
begin
  if not Assigned(AGetMimeTypeFunc) then
  begin
    AMimeType := '';
    Result := True;
  end
  else
  begin
    AMimeType := AGetMimeTypeFunc(APath);
    Result := TMimeTypes.Default.GetTypeInfo(AMimeType, LExt, LKind) and
      (LKind = TMimeTypes.TKind.Text);
  end;
end;

function TWebStencilsEngine.CanPostProcess(var APath: string; const AVars: TStrings;
  const AGetMimeTypeFunc: TWebGetMimeTypeFunc;
  const AGetFileNameFromPathFunc: TWebGetFileNameFromPathFunc;
  const AGetLoggedUserFunc: TWebGetLoggedUserFunc;
  out AProcessorRequest: IInterface): Boolean;
var
  LVars: TStringList;
  LPath: TWebStencilsPathTemplateItem;
  LNewPath: string;
  LProcRequest: TWebPostProcessorRequest;
  LMimeType: string;
begin
  LVars := TStringList.Create;
  try
    LPath := nil;
    LNewPath := '';
    LMimeType := '';
    if AVars <> nil then
      LVars.SetStrings(AVars);
    if PathTemplates.IsEmpty then
    begin
      LNewPath := UpdateFileNameExt(APath, DefaultFileExt);
      Result := IsMimeTypeValid(LNewPath, AGetMimeTypeFunc, LMimeType);
      if Result then
        LVars.Values[TWebStencilsPathTemplate.CFileName] := ExtractPosixFileName(APath);
    end
    else
                                                                                   
                                     
      Result := PathTemplates.Match(APath, DefaultFileExt,
        function (const APath: string): Boolean
        begin
          Result := IsMimeTypeValid(APath, AGetMimeTypeFunc, LMimeType);
        end, LNewPath, LVars, LPath);
    if Result then
    begin
      AProcessorRequest := TWebPostProcessorRequest.Create;
      LProcRequest := TWebPostProcessorRequest(AProcessorRequest);
      LProcRequest.OriginalPath := APath;
      if LPath <> nil then
        LProcRequest.PathTemplate := LPath.Template;
      LProcRequest.Vars := LVars;
      LProcRequest.ResultingPath := LNewPath;
      if (LMimeType <> '') and not LMimeType.Contains('charset') then
        LMimeType := LMimeType + '; charset=utf-8';
      LProcRequest.MimeType := LMimeType;
      LProcRequest.Processor := Self;
      LProcRequest.GetMimeTypeFunc := AGetMimeTypeFunc;
      LProcRequest.GetFileNameFromPathFunc := AGetFileNameFromPathFunc;
      LProcRequest.GetLoggedUserFunc := AGetLoggedUserFunc;
      APath := LNewPath;
      DoBeforeRequest(LProcRequest, Result);
      if not Result then
        AProcessorRequest := nil;
    end;
  finally
    LVars.Free;
  end;
end;

function TWebStencilsEngine.CanCachePostProcessResponse(const AProcessorRequest: IInterface): Boolean;
begin
  Result := False;
end;

function TWebStencilsEngine.PostProcess(const AProcessorRequest: IInterface;
  AInStream: TStream; out AOutStream: TStream): Boolean;
var
  LProc: TWebStencilsProcessor;
  LProcRequest: TWebPostProcessorRequest;
  LRoles: string;
  LOutput: string;
begin
  LProcRequest := TWebPostProcessorRequest(AProcessorRequest);
  if (AProcessorRequest = nil) or (AInStream = nil) or (LProcRequest = nil) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  LProc := TWebStencilsProcessor.Create(nil);
  try
    DoPathInit(LProcRequest);
    LProc.WebRequest := LProcRequest.WebRequest;
    LProc.ProcessorRequest := LProcRequest;
    LProc.PathTemplate := LProcRequest.PathTemplate;
    LProc.Engine := Self;
    LProc.DataVarDuplicates := DataVarDuplicates;
    LProc.DefaultFileExt := DefaultFileExt;
    if Assigned(LProcRequest.GetLoggedUserFunc) then
    begin
      LProc.UserLoggedIn := LProcRequest.GetLoggedUserFunc(LRoles);
      LProc.UserRoles := LRoles;
    end;
                                                                             
    LOutput := LProc.ContentFromStream(AInStream);
    AOutStream := TStringStream.Create(LOutput, TEncoding.UTF8);
    DoAfterRequest(LProcRequest, AOutStream);
  finally
    LProc.Free;
  end;
  Result := True;
end;

function TWebStencilsEngine.PostProcessFileNotFound(const AProcessorRequest: IInterface;
  out ANotFoundPagePath: string): Boolean;
var
  LItem: TWebStencilsPathTemplateItem;
  LProcRequest: TWebPostProcessorRequest;
begin
  LProcRequest := TWebPostProcessorRequest(AProcessorRequest);
  DoFileNotFound(LProcRequest, ANotFoundPagePath);
  if not ANotFoundPagePath.IsEmpty then
    Result := True
  else
    Result := PathTemplates.Match(TWebStencilsPathTemplateItem.CAnyPath, DefaultFileExt,
      nil, ANotFoundPagePath, nil, LItem);
end;

{ TWebStencilsDataVar }

function TWebStencilsDataVar.CheckObject(const AComponent: IWebStencilsComponent;
  const APropName: string; var AValue: string): Boolean;
begin
  AValue := '';
  Result := True;
  if not Assigned(TheObject) then
  begin
    if Assigned(InitFunc) then
    begin
      FTheObject := InitFunc();
      if FKind <> rdikLookup then
        if TheObject is TDataSet then
          FKind := rdikDataset
        else if TheObject is TStrings then
          FKind := rdikStrings
        else
          FKind := rdikObject;
    end;
    if not Assigned(TheObject) and not Assigned(LookupFunc) then
    begin
      AComponent.AddError(SWebStencilsNullObject, [APropName, Name]);
      AValue := Name + '.' + APropName;
      Result := False;
    end;
  end;
end;

destructor TWebStencilsDataVar.Destroy;
begin
  if Owned then
    FreeAndNil(FTheObject);
  inherited Destroy;
end;

{ TWebStencilsWrapperBinding }

type
  TWebStencilsWrapperBinding = class(TInterfacedObject, IWrapperBinding)
  private
    FBinding: Pointer; // weak reference to ICompiledBinding
    procedure UpdateBinding(const Scope: IScope);
  protected
    { IWrapperBinding }
    function GetBinding: ICompiledBinding; inline;
    procedure SetBinding(const Binding: ICompiledBinding); inline;
  public
    property Binding: ICompiledBinding read GetBinding write SetBinding;
  end;

function TWebStencilsWrapperBinding.GetBinding: ICompiledBinding;
begin
  Result := ICompiledBinding(FBinding);
end;

procedure TWebStencilsWrapperBinding.SetBinding(const Binding: ICompiledBinding);
begin
  FBinding := Pointer(Binding);
end;

procedure TWebStencilsWrapperBinding.UpdateBinding(const Scope: IScope);
var
  LBinding: IWrapperBinding;
begin
  if Supports(Scope, IWrapperBinding, LBinding) then
    LBinding.SetBinding(Binding);
end;

{ TWebStencilsDataSetScope }

type
  TWebStencilsDataSetScope = class(TWebStencilsWrapperBinding, IScope)
  private type
    TFieldValue = class(TWebStencilsWrapperBinding, IValue, IScope)
    private
      FField: TField;
      FObjectScope: IScope;
    protected
      function GetType: PTypeInfo;
      function GetValue: TValue;
      function Lookup(const Name: string): IInterface;
    public
      constructor Create(AField: TField);
    end;
    TBofEofValue = class(TInterfacedObject, IValue)
    private
      FDataSet: TDataSet;
      FBof: Boolean;
    protected
      function GetType: PTypeInfo;
      function GetValue: TValue;
    public
      constructor Create(ADataSet: TDataSet; ABof: Boolean);
    end;
  private
    FDataSet: TDataSet;
    FObjectScope: IScope;
    FParentName: string;
    FFields: TDictionary<string, IInterface>;
  protected
    function Lookup(const Name: string): IInterface;
  public
    constructor Create(ADataSet: TDataSet; const AParentName: string);
    destructor Destroy; override;
  end;

constructor TWebStencilsDataSetScope.TFieldValue.Create(AField: TField);
begin
  inherited Create;
  FField := AField;
end;

function TWebStencilsDataSetScope.TFieldValue.GetType: PTypeInfo;
begin
  case FField.DataType of
    ftString,
    ftFixedChar,
    ftWideString,
    ftFixedWideChar,
    ftMemo,
    ftWideMemo,
    ftOraClob:
      Result := TypeInfo(string);
    ftSmallint,
    ftInteger,
    ftAutoInc,
    ftShortint:
      Result := TypeInfo(Integer);
    ftWord,
    ftLongWord:
      Result := TypeInfo(Cardinal);
    ftBoolean:
      Result := TypeInfo(Boolean);
    TFieldType.ftFloat,
    TFieldType.ftExtended,
    TFieldType.ftSingle,
    ftFMTBcd:
      Result := TypeInfo(Extended);
    ftCurrency,
    ftBCD:
      Result := TypeInfo(Currency);
    ftLargeint:
      Result := TypeInfo(Int64);
    ftLargeUint:
      Result := TypeInfo(UInt64);
    ftDate:
      Result := TypeInfo(TDate);
    ftTime:
      Result := TypeInfo(TTime);
    ftDateTime,
    ftTimeStamp,
    ftOraTimeStamp,
    ftTimeStampOffset:
      Result := TypeInfo(TDateTime);
    ftGuid:
      Result := TypeInfo(TGUID);
  else
    Result := nil;
  end;
end;

function TWebStencilsDataSetScope.TFieldValue.GetValue: TValue;
begin
  Result := TValue.FromVariant(FField.Value);
end;

function TWebStencilsDataSetScope.TFieldValue.Lookup(const Name: string): IInterface;
begin
  if FObjectScope = nil then
  begin
//    FObjectScope := WrapObject(FField);
    FObjectScope := TWhitelistAwareScope.Create(
      WrapObject(FField),
      FField,
      TWebStencilsProcessor.Whitelist
    );
    UpdateBinding(FObjectScope);
  end;
  Result := FObjectScope.Lookup(Name);
end;

constructor TWebStencilsDataSetScope.TBofEofValue.Create(ADataSet: TDataSet; ABof: Boolean);
begin
  inherited Create;
  FDataSet := ADataSet;
  FBof := ABof;
end;

function TWebStencilsDataSetScope.TBofEofValue.GetType: PTypeInfo;
begin
  Result := TypeInfo(Boolean);
end;

function TWebStencilsDataSetScope.TBofEofValue.GetValue: TValue;
var
  LBmk: TBookmark;
  LVal: Boolean;
begin
  if FBof then
    Result := FDataSet.Bof
  else
  begin
    LVal := True;
    if not FDataSet.Eof then
    begin
      LBmk := FDataSet.Bookmark;
      FDataSet.Next;
      LVal := FDataSet.CompareBookmarks(LBmk, FDataSet.Bookmark) = 0;
      if not LVal then
        FDataSet.Prior;
    end;
    Result := LVal;
  end;
end;

constructor TWebStencilsDataSetScope.Create(ADataSet: TDataSet; const AParentName: string);
begin
  inherited Create;
  FDataSet := ADataSet;
  FParentName := AParentName;
  FFields := TDictionary<string, IInterface>.Create(TIStringComparer.Ordinal);
end;

destructor TWebStencilsDataSetScope.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TWebStencilsDataSetScope.Lookup(const Name: string): IInterface;
var
  LName: string;
  LField: TField;
begin
  LName := Name;
  if FParentName <> '' then
    LName := FParentName + '.' + LName;
  if not FFields.TryGetValue(LName, Result) then
  begin
    if SameText(LName, CLoopVarEof) then
      Result := TBofEofValue.Create(FDataSet, False)
    else if SameText(LName, CLoopVarBof) then
      Result := TBofEofValue.Create(FDataSet, True)
    else
    begin
      LField := FDataSet.FindField(LName);
      if LField = nil then
      begin
        if FObjectScope = nil then
        begin
          FObjectScope := WrapObject(FDataSet);
          UpdateBinding(FObjectScope);
        end;
        Result := FObjectScope.Lookup(Name);
      end
      else if LField is TObjectField then
        Result := TWebStencilsDataSetScope.Create(FDataSet, LName)
      else
        Result := TFieldValue.Create(LField);
    end;
    FFields.Add(LName, Result);
  end;
end;

{ TWebStencilsStringsScope }

type
  TWebStencilsStringsScope = class(TWebStencilsWrapperBinding, IScope)
  private
    FStrings: TStrings;
    FIndex: Integer;
    FObjectScope: IScope;
  protected
    function Lookup(const Name: string): IInterface;
  public
    constructor Create(AStrings: TStrings; AIndex: Integer);
  end;

constructor TWebStencilsStringsScope.Create(AStrings: TStrings; AIndex: Integer);
begin
  inherited Create;
  FStrings := AStrings;
  FIndex := AIndex;
end;

function TWebStencilsStringsScope.Lookup(const Name: string): IInterface;
var
  i: Integer;
begin
  Result := nil;
  if SameText(Name, CLoopVarEof) then
    Result := TValueWrapper.Create(FIndex >= FStrings.Count - 1)
  else if SameText(Name, CLoopVarBof) then
    Result := TValueWrapper.Create(FIndex <= 0)
  else if FIndex < 0 then
  begin
    i := FStrings.IndexOfName(Name);
    if (i = -1) then
    begin
      if FObjectScope = nil then
      begin
        FObjectScope := WrapObject(FStrings);
        UpdateBinding(FObjectScope);
      end;
      Result := FObjectScope.Lookup(Name);
    end
    else
      Result := TValueWrapper.Create(FStrings.ValueFromIndex[i]);
  end
  else if (FIndex >= 0) and (FIndex < FStrings.Count) then
    if SameText(Name, CLoopStrsName) then
      Result := TValueWrapper.Create(FStrings.Names[FIndex])
    else if SameText(Name, CLoopStrsValue) then
      Result := TValueWrapper.Create(FStrings.ValueFromIndex[FIndex]);
end;

{ TWebStencilsLookupScope }

type
  TWebStencilsLookupScope = class(TInterfacedObject, IScope)
  private
    FDictItem: TWebStencilsDataVar;
  protected
    function Lookup(const Name: string): IInterface;
  public
    constructor Create(ADictItem: TWebStencilsDataVar);
  end;

constructor TWebStencilsLookupScope.Create(ADictItem: TWebStencilsDataVar);
begin
  inherited Create;
  FDictItem := ADictItem;
end;

function TWebStencilsLookupScope.Lookup(const Name: string): IInterface;
var
  LText: string;
begin
  if FDictItem.LookupFunc(FDictItem, Name, LText) then
    Result := TValueWrapper.Create(LText)
  else
    Result := nil;
end;

{ TWebStencilsDeferredScope }

type
  TWebStencilsDeferredScope = class(TWebStencilsWrapperBinding, IScope)
  private
    FComponent: IWebStencilsComponent;
    FDictItem: TWebStencilsDataVar;
    FDictItemScope: IScope;
  protected
    function Lookup(const Name: string): IInterface;
  public
    constructor Create(const AComponent: IWebStencilsComponent; ADictItem: TWebStencilsDataVar);
  end;

constructor TWebStencilsDeferredScope.Create(const AComponent: IWebStencilsComponent;
  ADictItem: TWebStencilsDataVar);
begin
  inherited Create;
  FComponent := AComponent;
  FDictItem := ADictItem;
end;

function TWebStencilsDeferredScope.Lookup(const Name: string): IInterface;
var
  s: string;
begin
  if FDictItemScope = nil then
    if FDictItem.CheckObject(FComponent, Name, s) then
    begin
      case FDictItem.Kind of
        rdikObject:
          FDictItemScope := WrapObject(FDictItem.TheObject);
        rdikStrings:
          FDictItemScope := TWebStencilsStringsScope.Create(TStrings(FDictItem.TheObject), -1);
        rdikDataset:
          FDictItemScope := TWebStencilsDataSetScope.Create(TDataSet(FDictItem.TheObject), '');
        rdikLookup:
          FDictItemScope := TWebStencilsLookupScope.Create(FDictItem);
      end;
      UpdateBinding(FDictItemScope);
    end;
  Result := FDictItemScope;
end;

{ TWebStencilsDataVars }

constructor TWebStencilsDataVars.Create(const AComponent: IWebStencilsComponent);
begin
  inherited Create([doOwnsValues], TIStringComparer.Ordinal);
  FComponent := AComponent;
  FDuplicates := ddIgnore;
end;

procedure TWebStencilsDataVars.ValueNotify(const Value: TWebStencilsDataVar;
  Action: TCollectionNotification);
begin
  inherited ValueNotify(Value, Action);
  FEvalScope := nil;
end;

function TWebStencilsDataVars.GetEvalScope: IScope;
var
  LScope: TDictionaryScope;
  LPair: TPair<string, TWebStencilsDataVar>;
begin
  if FEvalScope <> nil then
    Exit(FEvalScope);

  LScope := TDictionaryScope.Create;
  Result := LScope;

  for LPair in Self do
    if (LPair.Value.TheObject <> nil) or Assigned(LPair.Value.LookupFunc) then
      case LPair.Value.Kind of
        rdikObject:
          LScope.Map.Add(LPair.Key, WrapObject(LPair.Value.TheObject));
        rdikStrings:
          LScope.Map.Add(LPair.Key, TWebStencilsStringsScope.Create(TStrings(LPair.Value.TheObject), -1));
        rdikDataset:
          LScope.Map.Add(LPair.Key, TWebStencilsDataSetScope.Create(TDataSet(LPair.Value.TheObject), ''));
        rdikLookup:
          LScope.Map.Add(LPair.Key, TWebStencilsLookupScope.Create(LPair.Value));
      end
    else if Assigned(LPair.Value.InitFunc) then
      LScope.Map.Add(LPair.Key, TWebStencilsDeferredScope.Create(FComponent, LPair.Value) as IScope);

  FEvalScope := Result;
end;

procedure TWebStencilsDataVars.InternalAdd(const AName: string; AObject: TObject; AOwned: Boolean;
  const AInitFunc: TWebStencilsInitFunc; const ALookupFunc: TWebStencilsLookupFunc);
var
  LItem: TWebStencilsDataVar;
begin
  if (AObject = nil) and not Assigned(AInitFunc) and not Assigned(ALookupFunc) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  if ContainsKey(AName) then
    case Duplicates of
      ddIgnore:
        begin
          if AOwned and (AObject <> nil) then
            AObject.Free;
          Exit;
        end;
      ddReplace: Remove(AName);
      ddError:   ; // let it go, will raise an error!
    end;

  LItem := TWebStencilsDataVar.Create;
  LItem.FName := AName;
  LItem.FTheObject := AObject;
  if Assigned(ALookupFunc) then
    LItem.FKind := rdikLookup
  else if AObject <> nil then
    if AObject is TDataSet then
      LItem.FKind := rdikDataset
    else if AObject is TStrings then
      LItem.FKind := rdikStrings
    else
      LItem.FKind := rdikObject;
  LItem.FOwned := AOwned;
  LItem.FInitFunc := AInitFunc;
  LItem.FLookupFunc := ALookupFunc;
  Add(AName, LItem);
end;

procedure TWebStencilsDataVars.Add(const AName: string; AObject: TObject; AOwned: Boolean);
begin
  InternalAdd(AName, AObject, AOwned, nil, nil);
end;

procedure TWebStencilsDataVars.Add(const AName: string; AObject: TObject; AOwned: Boolean;
  const ALookupFunc: TWebStencilsLookupFunc);
begin
  if not Assigned(ALookupFunc)then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  InternalAdd(AName, AObject, AOwned, nil, ALookupFunc);
end;

procedure TWebStencilsDataVars.AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
  AOwned: Boolean);
begin
  InternalAdd(AName, nil, AOwned, AInitFunc, nil);
end;

procedure TWebStencilsDataVars.AddInitFunc(const AName: string; const AInitFunc: TWebStencilsInitFunc;
  AOwned: Boolean; const ALookupFunc: TWebStencilsLookupFunc);
begin
  if not Assigned(ALookupFunc)then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  InternalAdd(AName, nil, AOwned, AInitFunc, ALookupFunc);
end;

procedure TWebStencilsDataVars.AddModule(AModule: TObject);

  function GetInitFunc(AModule: TObject; AMethod: TRttiMethod): TWebStencilsInitFunc;
  begin
    Result :=
      function: TObject
      begin
        Result := AMethod.Invoke(AModule, []).AsObject;
      end;
  end;

var
  LType: TRttiType;
  LAttr: WebStencilsVarAttribute;
  LField: TRttiField;
  LProp: TRttiProperty;
  LMeth: TRttiMethod;
  LName: string;
  LObj: TObject;
begin
  if AModule = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  LType := FComponent.RttiContext.GetType(AModule.ClassType);
  if LType = nil then
    Exit;
  for LField in LType.GetFields do
  begin
    LAttr := LField.GetAttribute<WebStencilsVarAttribute>;
    if LAttr <> nil then
      if (LField.DataType.TypeKind = tkClass) and
         (LField.Visibility in [mvPublic, mvPublished]) then
      begin
        LName := LAttr.Name;
        if LName = '' then
          LName := LField.Name;
        LObj := LField.GetValue(AModule).AsObject;
        if LObj <> nil then
          Add(LName, LObj, LAttr.WebStencilsOwned);
      end
      else
        raise EWebStencilsException.CreateResFmt(@SWebStencilsVarAttrNotSupported, [AModule.ClassName, LField.Name]);
  end;
  for LProp in LType.GetProperties do
  begin
    LAttr := LProp.GetAttribute<WebStencilsVarAttribute>;
    if LAttr <> nil then
      if (LProp.DataType.TypeKind = tkClass) and
         (LProp.Visibility in [mvPublic, mvPublished]) then
      begin
        LName := LAttr.Name;
        if LName = '' then
          LName := LProp.Name;
        LObj := LProp.GetValue(AModule).AsObject;
        if LObj <> nil then
          Add(LName, LObj, LAttr.WebStencilsOwned);
      end
      else
        raise EWebStencilsException.CreateResFmt(@SWebStencilsVarAttrNotSupported, [AModule.ClassName, LProp.Name]);
  end;
  for LMeth in LType.GetMethods do
  begin
    LAttr := LMeth.GetAttribute<WebStencilsVarAttribute>;
    if LAttr <> nil then
      if (Length(LMeth.GetParameters) = 0) and (LMeth.ReturnType <> nil) and
         (LMeth.ReturnType.TypeKind = tkClass) and
         (LMeth.Visibility in [mvPublic, mvPublished]) then
      begin
        LName := LAttr.Name;
        if LName = '' then
          LName := LMeth.Name;
        AddInitFunc(LName, GetInitFunc(AModule, LMeth), LAttr.WebStencilsOwned);
      end
      else
        raise EWebStencilsException.CreateResFmt(@SWebStencilsVarAttrNotSupported, [AModule.ClassName, LMeth.Name]);
  end;
end;

procedure TWebStencilsDataVars.Delete(AObject: TObject);
var
  LPair: TPair<string, TWebStencilsDataVar>;
begin
  for LPair in Self do
    if LPair.Value.TheObject = AObject then
    begin
      Remove(LPair.Key);
      Break;
    end;
end;

{ TWebStencilsLoopVar }

constructor TWebStencilsLoopVar.Create(AProc: TWebStencilsProcessor; const AObjName, APropName, AVarName: string);
begin
  inherited Create;
  FProcessor := AProc;
  FObjName := AObjName;
  FPropName := APropName;
  FVarName := AVarName;
end;

function TWebStencilsLoopVar.InitLoop: Boolean;
var
  LRefObj, LRefSubObj: TObject;
  LType: TRttiType;
  LMeth: TRttiMethod;
begin
  // used also for datasets
  FCurrPos := -1;
  Result := False;
  LRefSubObj := nil;
  FLoopIterObject := nil;
  FLoopIterNext := nil;
  FLoopIterCur := nil;

  // access the main object
  LRefObj := FProcessor.GetLoopOrDictionaryObject(FObjName);
  if not Assigned(LRefObj) then
    FProcessor.AddError(SWebStencilsLoopObjNotFound, [FObjName])
  // if we are accessing a subobject, let's grab it instead
  else if FPropName = '' then
  begin
    FLoopObject := LRefObj;
    Result := True;
  end
  else if FProcessor.GetObjectProperty(LRefObj, FObjName, FPropName, LRefSubObj) then
  begin
    FLoopObject := LRefSubObj;
    Result := True;
  end
  else
    FProcessor.AddError(SWebStencilsLoopSubObjNotFound, [FPropName, FObjName]);

  if LoopObject is TDataSet then
  begin
    TDataSet(LoopObject).Open;
    TDataSet(LoopObject).First;
  end
  else if not ((LoopObject is TObjectList) or (LoopObject is TStrings)) and (LoopObject <> nil) then
  begin
    LType := FProcessor.FRTTIContext.GetType(LoopObject.ClassType);
    if LType <> nil then
    begin
      LMeth := LType.GetMethod('GetEnumerator');
      if (LMeth <> nil) and (Length(LMeth.GetParameters) = 0) and
         (LMeth.ReturnType <> nil) and (LMeth.ReturnType.TypeKind = tkClass) then
      begin
        FLoopIterObject := LMeth.Invoke(LoopObject, []).AsObject;
        if FLoopIterObject <> nil then
        begin
          LType := FProcessor.FRTTIContext.GetType(FLoopIterObject.ClassType);
          if LType <> nil then
          begin
            FLoopIterNext := LType.GetMethod('MoveNext');
            if not ((FLoopIterNext <> nil) and (Length(FLoopIterNext.GetParameters) = 0) and
               (FLoopIterNext.ReturnType <> nil) and IsBoolType(FLoopIterNext.ReturnType.Handle)) then
              FLoopIterNext := nil;
            FLoopIterCur := LType.GetProperty('Current');
            if not ((FLoopIterCur <> nil) and (FLoopIterCur.PropertyType <> nil) and
               (FLoopIterCur.PropertyType.TypeKind = tkClass)) then
              FLoopIterCur := nil;
          end;
        end;
        if (FLoopIterObject = nil) or (FLoopIterNext = nil) or (FLoopIterCur = nil) then
        begin
          FProcessor.AddError(SWebStencilsLoopObjNotSupported, [FObjName]);
          FreeAndNil(FLoopIterObject);
          FLoopIterNext := nil;
          FLoopIterCur := nil;
        end;
      end;
    end;
  end;
end;

function TWebStencilsLoopVar.NextLoop: Boolean;
begin
  Result := True;
  FProcessor.FLoopVars.FEvalScope := nil;
  Inc(FCurrPos);

  if LoopObject is TDataSet then
  begin
    if FCurrPos > 0 then
      TDataSet(LoopObject).Next;
    if TDataSet(LoopObject).Eof then
      Result := False;
  end
  else if LoopObject is TStrings then
  begin
    if (FCurrPos >= TStrings(LoopObject).Count) then
      Result := False
    else
      FCurrentObj := LoopObject;
  end
  else if LoopObject is TObjectList then
  begin
    if (FCurrPos >= TObjectList(LoopObject).Count) then
      Result := False
    else
      FCurrentObj := TObjectList(LoopObject)[FCurrPos];
  end
  else if FLoopIterObject <> nil then
  begin
    Result := FLoopIterNext.Invoke(FLoopIterObject, []).AsBoolean;
    if Result then
      FCurrentObj := FLoopIterCur.GetValue(FLoopIterObject).AsObject;
  end
  else
  begin
    if FCurrPos = 0 then
      FCurrentObj := LoopObject
    else
      Result := False;
  end;
end;

procedure TWebStencilsLoopVar.DoneLoop;
begin
  FProcessor.FLoopVars.FEvalScope := nil;
  if FLoopIterObject <> nil then
    FreeAndNil(FLoopIterObject);
end;

{ TWebStencilsLoopVars }

constructor TWebStencilsLoopVars.Create;
begin
  inherited Create([doOwnsValues], TIStringComparer.Ordinal);
end;

procedure TWebStencilsLoopVars.ValueNotify(const Value: TWebStencilsLoopVar;
  Action: TCollectionNotification);
begin
  inherited ValueNotify(Value, Action);
  FEvalScope := nil;
end;

function TWebStencilsLoopVars.GetEvalScope: IScope;
var
  LScope: TDictionaryScope;
  LPair: TPair<string, TWebStencilsLoopVar>;
begin
  if FEvalScope <> nil then
    Exit(FEvalScope);

  LScope := TDictionaryScope.Create;
  Result := LScope;

  for LPair in Self do
    if LPair.Value.LoopObject is TDataSet then
      LScope.Map.Add(LPair.Key, TWebStencilsDataSetScope.Create(TDataSet(LPair.Value.LoopObject), ''))
    else if LPair.Value.LoopObject is TStrings then
      LScope.Map.Add(LPair.Key, TWebStencilsStringsScope.Create(TStrings(LPair.Value.LoopObject), LPair.Value.FCurrPos))
    else if (LPair.Value.LoopObject is TObjectList) or
            (LPair.Value.FLoopIterObject <> nil) then
      LScope.Map.Add(LPair.Key, WrapObject(LPair.Value.CurrentObj));

  FEvalScope := Result;
end;

{ TWebStencilsAliasesScope }

type
  TWebStencilsAliasesScope = class(TInterfacedObject, IScope)
  private
    FAliases: TWebStencilsAliasVars;
  protected
    function Lookup(const AName: string): IInterface;
  public
    constructor Create(const AAliases: TWebStencilsAliasVars);
  end;

constructor TWebStencilsAliasesScope.Create(const AAliases: TWebStencilsAliasVars);
begin
  inherited Create;
  FAliases := AAliases;
end;

function TWebStencilsAliasesScope.Lookup(const AName: string): IInterface;
var
  LName: string;
  LProc: TWebStencilsProcessor;
begin
  Result := nil;
  LProc := FAliases.FProcessor;
  LName := LProc.FAliasVars.ResolveAlias(AName);
  if not LProc.FLoopVars.IsEmpty then
    Result := LProc.FLoopVars.EvalScope.Lookup(LName);
  if (Result = nil) and not LProc.DataVars.IsEmpty then
    Result := LProc.DataVars.EvalScope.Lookup(LName);
  if (Result = nil) and Assigned(LProc.Engine) and not LProc.DataVars.IsEmpty then
    Result := LProc.Engine.DataVars.EvalScope.Lookup(AName);
end;

{ TWebStencilsAliasVars }

constructor TWebStencilsAliasVars.Create(AProcessor: TWebStencilsProcessor);
begin
  inherited Create;
  FProcessor := AProcessor;
end;

function TWebStencilsAliasVars.ResolveAlias(const AObjName: string): string;
var
  i: Integer;
begin
  Result := AObjName;
  for i := Count - 1 downto 0 do
    if AnsiSameText(Names[i], Result) then
      Result := ValueFromIndex[i];
end;

function TWebStencilsAliasVars.GetEvalScope: IScope;
begin
  if FEvalScope = nil then
    FEvalScope := TWebStencilsAliasesScope.Create(Self);
  Result := FEvalScope;
end;

procedure TWebStencilsAliasVars.Cleanup(ANum: Integer);
begin
  while ANum < Count do
    Delete(Count - 1);
end;

{ TWebStencilsPathTemplate }

constructor TWebStencilsPathTemplate.Create;
begin
  inherited Create;
  FItems := TStringList.Create(#0, '/', [soStrictDelimiter, soUseLocale]);
end;

destructor TWebStencilsPathTemplate.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TWebStencilsPathTemplate.Compile(const ATemplate: string);
var
  i: Integer;
  s: string;
begin
  if FTemplate = ATemplate then
    Exit;
  FTemplate := ATemplate;
  FItems.DelimitedText := ATemplate;
  if (ATemplate <> '') and (ATemplate[1] = '/') and (FItems[0] = '') then
    FItems.Delete(0);
  for i := 0 to FItems.Count - 1 do
  begin
    s := FItems[i];
    if (s <> '') and (s[1] = '{') and (s[Length(s)] = '}') then
    begin
      FItems[i] := Copy(s, 2, Length(s) - 2);
      FItems.Objects[i] := TObject(1);
    end;
  end;
end;

function TWebStencilsPathTemplate.Consume(const APath: string; AVars: TStrings): Boolean;
var
  i1, i2, L: Integer;
  j: Integer;
  s: string;
begin
  Result := True;
  i1 := 1;
  L := Length(APath);
  j := 0;
  while (i1 <= L) and (j < FItems.Count) do
  begin
    if APath[i1] = '/' then
      Inc(i1);
    i2 := Pos('/', APath, i1);
    if i2 = 0 then
      i2 := L + 1;
    s := Copy(APath, i1, i2 - i1);

    if FItems.Objects[j] = TObject(1) then
      if (AnsiSameText(CFileName, FItems[j])) and s.IsEmpty then
        Exit(False)
      else
        AVars.AddPair(FItems[j], s)
    else if not AnsiSameText(s, FItems[j]) then
      Exit(False);

    i1 := i2;
    Inc(j);
  end;

  while j < FItems.Count do
  begin
    if FItems.Objects[j] = TObject(0) then
    begin
      if not AnsiSameText(s, FItems[j]) then
        Exit(False);
    end
    else if FItems.Objects[j] = TObject(1) then
      if AnsiSameText(CFileName, FItems[j]) then
        Exit(False)
      else
        AVars.AddPair(FItems[j], '');
    Inc(j);
  end;

  if (i1 < L) or (i1 = L) and (j >= FItems.Count) then
    Exit(False);
end;

function TWebStencilsPathTemplate.GetFullPathName(AVars: TStrings; const ADefaultFileExt: string): string;
var
  i: Integer;
  s: string;
  LFileNameFound: Boolean;
begin
  Result := '';
  LFileNameFound := False;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Objects[i] = TObject(1) then
    begin
      if AVars <> nil then
        s := AVars.Values[FItems[i]]
      else
        s := '';
      if AnsiSameText(CFileName, FItems[i]) then
      begin
        s := UpdateFileNameExt(s, ADefaultFileExt);
        LFileNameFound := True;
      end;
      if not s.IsEmpty then
        Result := Result + '/' + s;
    end
    else
      Result := Result + '/' + FItems[i];
    if LFileNameFound then
      Break;
  end;
  if not LFileNameFound then
    Result := UpdateFileNameExt(Result, ADefaultFileExt);
end;

{ TWebStencilsPathTemplateItem }

function TWebStencilsPathTemplateItem.GetDisplayName: string;
begin
  if Template.IsEmpty then
    Result := inherited GetDisplayName
  else
  begin
    Result := Template;
    if not Redirect.IsEmpty then
      Result := Result + ' -> ' + Redirect;
  end;
end;

procedure TWebStencilsPathTemplateItem.Assign(Source: TPersistent);
begin
  if Source is TWebStencilsPathTemplateItem then
  begin
    Template := TWebStencilsPathTemplateItem(Source).Template;
    Redirect := TWebStencilsPathTemplateItem(Source).Redirect;
    OnPathInit := TWebStencilsPathTemplateItem(Source).OnPathInit;
  end
  else
    inherited Assign(Source);
end;

procedure TWebStencilsPathTemplateItem.DoPathInit(const ARequest: TWebPostProcessorRequest);
begin
  if Assigned(FOnPathInit) then
    FOnPathInit(Self, ARequest);
end;

function TWebStencilsPathTemplateItem.Match(const APath, ADefaultFileExt: string;
  var ANewPath: string): Boolean;
begin
  Result := Match(APath, ADefaultFileExt, nil, ANewPath, nil);
end;

function TWebStencilsPathTemplateItem.Match(const APath, ADefaultFileExt: string;
  const AValidateFunc: TWebStencilsPathTemplateValidateFunc; var ANewPath: string;
  AVars: TStrings): Boolean;
var
  LTempl: TWebStencilsPathTemplate;
  LRedir: TWebStencilsPathTemplate;
  LVars: TStringList;
begin
  LTempl := nil;
  LRedir := nil;
  LVars := nil;
  try
    if not Template.IsEmpty and not Template.Equals(CAnyPath) then
    begin
      LVars := TStringList.Create;
      LTempl := TWebStencilsPathTemplate.Create;
      LTempl.Compile(Template);
      if not LTempl.Consume(APath, LVars) then
      begin
        ANewPath := '';
        Exit(False);
      end;
    end;
    if not Redirect.IsEmpty then
    begin
      LRedir := TWebStencilsPathTemplate.Create;
      LRedir.Compile(Redirect);
    end;

    if LRedir <> nil then
      ANewPath := LRedir.GetFullPathName(LVars, ADefaultFileExt)
    else if LTempl <> nil then
      ANewPath := LTempl.GetFullPathName(LVars, ADefaultFileExt)
    else
    begin
      LVars := TStringList.Create;
      LVars.Values[TWebStencilsPathTemplate.CFileName] := ExtractPosixFileName(APath);
      ANewPath := UpdateFileNameExt(APath, ADefaultFileExt);
    end;
    if Assigned(AValidateFunc) and not AValidateFunc(ANewPath) then
    begin
      ANewPath := '';
      Exit(False);
    end;
    if (AVars <> nil) and (LVars <> nil) then
      AVars.AddStrings(LVars);
    Result := True;
  finally
    LVars.Free;
    LRedir.Free;
    LTempl.Free;
  end;
end;

{ TWebStencilsPathTemplateCollection }

constructor TWebStencilsPathTemplateCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TWebStencilsPathTemplateItem);
end;

function TWebStencilsPathTemplateCollection.GetItem(Index: Integer): TWebStencilsPathTemplateItem;
begin
  Result := TWebStencilsPathTemplateItem(inherited GetItem(Index));
end;

procedure TWebStencilsPathTemplateCollection.SetItem(Index: Integer; const AValue: TWebStencilsPathTemplateItem);
begin
  inherited SetItem(Index, AValue);
end;

function TWebStencilsPathTemplateCollection.Add: TWebStencilsPathTemplateItem;
begin
  Result := TWebStencilsPathTemplateItem(inherited Add);
end;

function TWebStencilsPathTemplateCollection.Add(const ATemplate, ARedirect: string;
  const AInitMethod: TWebStencilsPathInitEvent): TWebStencilsPathTemplateItem;
begin
  Result := Add;
  Result.Template := ATemplate;
  Result.Redirect := ARedirect;
  Result.OnPathInit := AInitMethod;
end;

function TWebStencilsPathTemplateCollection.Match(const APath, ADefaultFileExt: string;
  var ANewPath: string; var APathItem: TWebStencilsPathTemplateItem): Boolean;
begin
  Result := Match(APath, ADefaultFileExt, nil, ANewPath, nil, APathItem);
end;

function TWebStencilsPathTemplateCollection.Match(const APath, ADefaultFileExt: string;
  const AValidateFunc: TWebStencilsPathTemplateValidateFunc;
  var ANewPath: string; AVars: TStrings; var APathItem: TWebStencilsPathTemplateItem): Boolean;
var
  I: Integer;
  LAnyPath: TWebStencilsPathTemplateItem;
begin
  LAnyPath := nil;
  for I := 0 to Count - 1 do
    if Items[I].Template.Equals(TWebStencilsPathTemplateItem.CAnyPath) then
      LAnyPath := Items[I]
    else if Items[I].Match(APath, ADefaultFileExt, AValidateFunc, ANewPath, AVars) then
    begin
      APathItem := Items[I];
      Exit(True);
    end;
  if (LAnyPath <> nil) and
     LAnyPath.Match(APath, ADefaultFileExt, AValidateFunc, ANewPath, AVars) then
  begin
    APathItem := LAnyPath;
    Exit(True);
  end;
  ANewPath := '';
  APathItem := nil;
  Result := False;
end;

{ TWhitelistAwareScope }

constructor TWhitelistAwareScope.Create(AInnerScope: IScope; AObject: TObject; AWhitelist: IWhitelist);
begin
  inherited Create;
  FInnerScope := AInnerScope;
  FObject := AObject;
  FWhitelist := AWhitelist;
end;

function TWhitelistAwareScope.Lookup(const Name: string): IInterface;
begin
  // Apply whitelist validation first
  if (FWhitelist <> nil) then
  begin
    if not FWhitelist.Validate(FObject, Name) and
       not FWhitelist.Validate(FObject.ClassInfo, Name) then
      Exit(nil);
  end;

  // If validation passes, delegate to inner scope
  Result := FInnerScope.Lookup(Name);
end;

end.
