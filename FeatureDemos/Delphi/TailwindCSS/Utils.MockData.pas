unit Utils.MockData;

interface

uses
  Models,
  System.Generics.Collections,
  System.SysUtils,
  System.DateUtils;

function GenerateMockProducts: TObjectList<TProduct>;
function GenerateMockUsers: TObjectList<TUser>;
function GenerateDashboardStats: TObjectList<TStat>;
function GenerateAnalyticsMetrics: TObjectList<TAnalyticsMetric>;

implementation

function GenerateMockProducts: TObjectList<TProduct>;
var
  Product: TProduct;
begin
  Result := TObjectList<TProduct>.Create(True);

  Product := TProduct.Create;
  Product.Id := 1;
  Product.Name := 'Delphi Enterprise Suite';
  Product.Description := 'Complete development environment for building modern applications with powerful IDE and framework';
  Product.Price := 2999.99;
  Product.Category := 'Software';
  Product.ImageUrl := '💻';
  Product.Stock := 50;
  Product.Rating := 4.8;
  Result.Add(Product);

  Product := TProduct.Create;
  Product.Id := 2;
  Product.Name := 'WebStencils Framework';
  Product.Description := 'Modern template engine for Delphi web applications with powerful features';
  Product.Price := 499.99;
  Product.Category := 'Framework';
  Product.ImageUrl := '⚡';
  Product.Stock := 100;
  Product.Rating := 4.9;
  Result.Add(Product);

  Product := TProduct.Create;
  Product.Id := 3;
  Product.Name := 'Tailwind CSS Pro';
  Product.Description := 'Utility-first CSS framework for rapid UI development';
  Product.Price := 199.99;
  Product.Category := 'Design';
  Product.ImageUrl := '🎨';
  Product.Stock := 200;
  Product.Rating := 4.7;
  Result.Add(Product);

  Product := TProduct.Create;
  Product.Id := 4;
  Product.Name := 'Database Connector';
  Product.Description := 'High-performance database connectivity layer with support for multiple databases';
  Product.Price := 799.99;
  Product.Category := 'Database';
  Product.ImageUrl := '🗄️';
  Product.Stock := 75;
  Product.Rating := 4.6;
  Result.Add(Product);

  Product := TProduct.Create;
  Product.Id := 5;
  Product.Name := 'API Gateway';
  Product.Description := 'Enterprise-grade API gateway for microservices architecture';
  Product.Price := 1499.99;
  Product.Category := 'Infrastructure';
  Product.ImageUrl := '🚀';
  Product.Stock := 30;
  Product.Rating := 4.5;
  Result.Add(Product);

  Product := TProduct.Create;
  Product.Id := 6;
  Product.Name := 'Security Suite';
  Product.Description := 'Comprehensive security toolkit with authentication and authorization';
  Product.Price := 999.99;
  Product.Category := 'Security';
  Product.ImageUrl := '🔒';
  Product.Stock := 0;
  Product.Rating := 4.9;
  Result.Add(Product);
end;

function GenerateMockUsers: TObjectList<TUser>;
var
  User: TUser;
begin
  Result := TObjectList<TUser>.Create(True);

  User := TUser.Create;
  User.Id := 1;
  User.Name := 'John Doe';
  User.Email := 'john.doe@example.com';
  User.Role := 'Developer';
  User.Status := 'active';
  User.LastLogin := Now - 2;
  User.AvatarUrl := '👤';
  Result.Add(User);

  User := TUser.Create;
  User.Id := 2;
  User.Name := 'Jane Smith';
  User.Email := 'jane.smith@example.com';
  User.Role := 'Designer';
  User.Status := 'active';
  User.LastLogin := Now - 1;
  User.AvatarUrl := '👩';
  Result.Add(User);

  User := TUser.Create;
  User.Id := 3;
  User.Name := 'Bob Johnson';
  User.Email := 'bob.johnson@example.com';
  User.Role := 'Admin';
  User.Status := 'active';
  User.LastLogin := Now - 0.5;
  User.AvatarUrl := '👨';
  Result.Add(User);

  User := TUser.Create;
  User.Id := 4;
  User.Name := 'Alice Williams';
  User.Email := 'alice.williams@example.com';
  User.Role := 'Manager';
  User.Status := 'active';
  User.LastLogin := Now - 5;
  User.AvatarUrl := '👩‍💼';
  Result.Add(User);

  User := TUser.Create;
  User.Id := 5;
  User.Name := 'Charlie Brown';
  User.Email := 'charlie.brown@example.com';
  User.Role := 'Developer';
  User.Status := 'inactive';
  User.LastLogin := Now - 30;
  User.AvatarUrl := '👤';
  Result.Add(User);

  User := TUser.Create;
  User.Id := 6;
  User.Name := 'Diana Prince';
  User.Email := 'diana.prince@example.com';
  User.Role := 'Designer';
  User.Status := 'pending';
  User.LastLogin := Now;
  User.AvatarUrl := '👩';
  Result.Add(User);
end;

function GenerateDashboardStats: TObjectList<TStat>;
var
  Stat: TStat;
begin
  Result := TObjectList<TStat>.Create(True);

  Stat := TStat.Create;
  Stat.Title := 'Total Users';
  Stat.Value := '1,234';
  Stat.Change := '+12.5%';
  Stat.ChangeType := 'increase';
  Stat.Icon := '👥';
  Stat.Color := 'blue';
  Result.Add(Stat);

  Stat := TStat.Create;
  Stat.Title := 'Revenue';
  Stat.Value := '$45,678';
  Stat.Change := '+8.2%';
  Stat.ChangeType := 'increase';
  Stat.Icon := '💰';
  Stat.Color := 'green';
  Result.Add(Stat);

  Stat := TStat.Create;
  Stat.Title := 'Orders';
  Stat.Value := '567';
  Stat.Change := '-3.1%';
  Stat.ChangeType := 'decrease';
  Stat.Icon := '📦';
  Stat.Color := 'red';
  Result.Add(Stat);

  Stat := TStat.Create;
  Stat.Title := 'Active Sessions';
  Stat.Value := '89';
  Stat.Change := '+5.7%';
  Stat.ChangeType := 'increase';
  Stat.Icon := '🔌';
  Stat.Color := 'purple';
  Result.Add(Stat);
end;

function GenerateAnalyticsMetrics: TObjectList<TAnalyticsMetric>;
var
  Metric: TAnalyticsMetric;
begin
  Result := TObjectList<TAnalyticsMetric>.Create(True);

  Metric := TAnalyticsMetric.Create;
  Metric.LabelText := 'Page Views';
  Metric.Value := 12500;
  Metric.Percentage := 85.5;
  Metric.Color := 'blue';
  Result.Add(Metric);

  Metric := TAnalyticsMetric.Create;
  Metric.LabelText := 'Unique Visitors';
  Metric.Value := 3420;
  Metric.Percentage := 72.3;
  Metric.Color := 'green';
  Result.Add(Metric);

  Metric := TAnalyticsMetric.Create;
  Metric.LabelText := 'Bounce Rate';
  Metric.Value := 1850;
  Metric.Percentage := 45.2;
  Metric.Color := 'yellow';
  Result.Add(Metric);

  Metric := TAnalyticsMetric.Create;
  Metric.LabelText := 'Conversion Rate';
  Metric.Value := 890;
  Metric.Percentage := 23.8;
  Metric.Color := 'purple';
  Result.Add(Metric);
end;

end.

