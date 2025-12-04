# Feature Demos

This folder contains small, focused code snippets demonstrating specific WebStencils features. Each demo is a minimal WebBroker application that showcases a particular aspect of the WebStencils template engine.

## Available Demos

### AnonymousMethods
Demonstrates using anonymous methods as property accessors for WebStencils variables. Shows two approaches:
- **With object**: Using an anonymous method to access properties from an associated object (TDictionary)
- **Fully anonymous**: Using an anonymous method that provides values directly without an object reference

**Key Features:**
- `AddVar` with anonymous method property accessors
- Dynamic property resolution in templates

**Templates:**
- `BaseLayout.html` - Displays environment variables accessed via anonymous methods

---

### FireDACMetadata
Shows how to use FireDAC field metadata with WebStencils templates. Demonstrates exposing field properties (DisplayText, Value, DisplayLabel, FieldName, Required, Visible, DataType, Size...) and using the `@switch` operator to render different input types based on field data types.

**Key Features:**
- Whitelist configuration for FireDAC field properties
- `@switch` operator for conditional rendering based on field types
- `@forEach` and `@import` directives for template composition
- Dynamic form generation based on database schema

**Templates:**
- `BaseLayout.html` - Main layout with field iteration
- `Input.html` - Reusable input component with type-specific rendering

**Data:**
- Requires `database.sqlite3` with a `customers` table

---

### NestedExtraHeaders
Demonstrates nested `@ExtraHeader` directives in WebStencils templates. Shows how multiple layout levels can each contribute header content (CSS, scripts, meta tags) that gets properly nested and rendered.

**Key Features:**
- Multi-level layout nesting (`BaseLayout` → `MainLayout` → page content)
- Nested `@ExtraHeader` directives at each level
- `@RenderHeader` directive for header injection

**Templates:**
- `BaseLayout.html` - Base layout with `@RenderHeader`
- `MainLayout.html` - Intermediate layout adding CSS via `@ExtraHeader`
- `home.html`, `link1.html`, `link2.html` - Content pages with additional nested headers

---

### SessionManager
Shows WebStencils integration with WebBroker session management, authentication, and authorization. Demonstrates using `TWebStencilsEngine` for template routing alongside `TWebSessionManager`, `TWebFormsAuthenticator`, and `TWebAuthorizer`.

**Key Features:**
- `TWebStencilsEngine` for template routing and rendering
- Form-based authentication with role-based authorization
- Protected routes with different access levels (free, authenticated, admin)
- Session management integration

**Templates:**
- Multiple templates demonstrating different access levels
- Login, home, authenticated, admin, and forbidden pages

**Authentication:**
- Demo credentials: `demo/demo123` (user role), `admin/admin123` (admin role)

---

### SwitchOperator
Simple demonstration of the `@switch` operator in WebStencils templates. Shows conditional rendering based on a value with multiple cases and a default case.

**Key Features:**
- `@switch` operator syntax
- Multiple `@case` branches
- `@default` case handling

**Templates:**
- `BaseLayout.html` - Displays status badges using switch operator

---

## Running the Demos

Each demo is a standalone WebBroker console application:

1. Open the project file (`.dpr`) in RAD Studio
2. Compile and run the project
3. The console will display server commands (start, stop, status, etc.)
4. Type `start` to start the HTTP server (default port: 8080)
5. Open a web browser and navigate to `http://localhost:8080`

## Requirements

- RAD Studio 13.0 or higher
- WebStencils library
- WebBroker framework

## Notes

- These demos are minimal examples focused on specific features
- They use standard WebBroker server infrastructure (see `ServerConst1.pas` and main program file)
- Template paths are relative to the executable location
- Some demos may require additional resources (database files, etc.) as noted in their descriptions

