# WebStencils C++ Builder Demo Project

![WebStencils screenshot](../../.github/images/WebBroker.png)

## 🌟 Overview
This project demonstrates the usage of WebStencils using WebBroker in a C++ Builder environment. It mirrors the functionality of the Delphi version, showcasing WebStencils' capabilities with C++.

## 🚦 Getting Started
The project is designed to be run on Windows using C++ Builder and optimised for the platform `Windows 64-bit (Modern)`.

Simply open the project in the C++ Builder IDE, build it, and run it. The default behavior attempts to locate the shared HTML templates and SQLite database relative to the executable's path. For production, adjust these paths as needed.

## 📚 Examples
### Docs
Most menus explain the general use of WebStencils and suggest templating patterns.

### Big Table
Loads 1000 customer records from the shared SQLite database (`resources/data/database.sqlite3`) using FireDAC and displays them in a table.

### Pagination
Demonstrates server-side pagination using the same customer data from the SQLite database.

### To-Do app - HTMX Integration
Uses an in-memory `TList` (`ModelTasks.h/.cpp`) managed by a singleton to store task data. Uses HTMX with WebStencils for dynamic updates. See `partials/tasks/` and `ControllerTasks.h/.cpp`.

## 📁 Project Structure
1.  C++ source files (`.cpp`, `.h`)
2.  Datamodule files (`.dfm`)
3.  HTML templates (`.html`) from `resources/html`
4.  Static assets (CSS, JS, images) from `resources/static`

### 🔑 Key C++ Files
- `WebStencilsDemo.cbproj`: Main C++ Builder project file.
- `MainWebModule.h/.cpp`: Handles web requests, sets up the WebStencils engine, defines routes, and manages data modules.
- `ModelTasks.h/.cpp`, `ControllerTasks.h/.cpp`: Implement the in-memory Tasks demo.
- `ControllerCustomers.h/.cpp`: Implement the SQLite-based Customers demos (Big Table, Pagination).
- `ModelPaginationParams.h/.cpp`: Defines a reusable pagination parameter class.
- `ClassHelpers.h/.cpp`: Provides helper functions (routing, pagination).
- `CodeExamplesU.h/.cpp`: Contains code snippets for the documentation pages.

### 📄 HTML Templates
Located in the `resources/html` directory. This demonstrates WebStencils templating capabilities.
- `layouts/mainLayout.html`: Main layout.
- Various content pages (e.g., `home.html`, `basics.html`).
- Reusable partials in `partials/`.
- `partials/customers`: Templates for Big Table and Pagination demos.
- `partials/tasks`: Templates for the HTMX Tasks demo.

> **IMPORTANT**: The `codeBlock` partial includes a copy button that requires `localhost` or HTTPS due to browser security policies.

## ✨ Environment Variables & Data Sources
- `MainWebModule` demonstrates custom environment variables (e.g., `@env.APP_NAME`) and system variables (e.g., `@system.year`).
- `DEBUG_MODE` is set based on the `_DEBUG` preprocessor definition.
- **Data Sources:** Uses an in-memory list for Tasks and an SQLite database (`resources/data/database.sqlite3`) for Customers, demonstrating backend data source flexibility with WebStencils.

## 💻 Web Tech Used
- Bootstrap 5.3
- Bootstrap icons
- HTMX 2.0.2
- Minor custom CSS/JS (in `resources/static`)

Dependencies are loaded via CDNs.
