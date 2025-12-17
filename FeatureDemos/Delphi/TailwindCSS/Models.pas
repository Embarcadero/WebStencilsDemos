unit Models;

interface

type
  TAnalyticsMetric = class
  private
    FLabelText: string;
    FValue: Integer;
    FPercentage: Double;
    FColor: string;
  public
    property LabelText: string read FLabelText write FLabelText;
    property Value: Integer read FValue write FValue;
    property Percentage: Double read FPercentage write FPercentage;
    property Color: string read FColor write FColor;
  end;

  TProduct = class
  private
    FId: Integer;
    FName: string;
    FDescription: string;
    FPrice: Currency;
    FCategory: string;
    FImageUrl: string;
    FStock: Integer;
    FRating: Double;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Price: Currency read FPrice write FPrice;
    property Category: string read FCategory write FCategory;
    property ImageUrl: string read FImageUrl write FImageUrl;
    property Stock: Integer read FStock write FStock;
    property Rating: Double read FRating write FRating;
  end;

  TStat = class
  private
    FTitle: string;
    FValue: string;
    FChange: string; // "+12.5%" or "-5.2%"
    FChangeType: string; // 'increase' or 'decrease'
    FIcon: string; // Icon name or emoji
    FColor: string; // 'blue', 'green', 'red', etc.
  public
    property Title: string read FTitle write FTitle;
    property Value: string read FValue write FValue;
    property Change: string read FChange write FChange;
    property ChangeType: string read FChangeType write FChangeType;
    property Icon: string read FIcon write FIcon;
    property Color: string read FColor write FColor;
  end;

  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FRole: string;
    FStatus: string; // 'active', 'inactive', 'pending'
    FLastLogin: TDateTime;
    FAvatarUrl: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Role: string read FRole write FRole;
    property Status: string read FStatus write FStatus;
    property LastLogin: TDateTime read FLastLogin write FLastLogin;
    property AvatarUrl: string read FAvatarUrl write FAvatarUrl;
  end;

implementation

end.

