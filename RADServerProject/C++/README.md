# WebStencils Demo - RAD Server Project (C++Builder)

![WebStencils RAD Server screenshot](../../.github/images/WebStencils-RADServer.png)

## 🌟 Overview
This project demonstrates using WebStencils with RAD Server in C++Builder. It serves the same website content as the other demos but utilizes RAD Server's REST API architecture and an InterBase database, all implemented in C++.

## 🚦 Getting Started
RAD Server requires absolute paths for resources. Before running:
1.  **Open `WebResourceCpp.h/.cpp`**.
2.  Locate the constructor or an initialization method where paths are set (likely involving `FResourcesPath`).
3.  **Update the path** to the absolute path of the *main project directory* (`WebStencilsDemos`) on your machine (e.g., `C:\Path\To\WebStencilsDemos`). The code likely constructs the full path to `resources` from there. This ensures the server can find the shared templates and the InterBase database (`resources/data/tasks.ib`).
4.  Run the RAD Server development server (`EMSDevServer.exe`) and ensure the project's BPL is loaded.
5.  Open a browser and access `http://localhost:8080/web`

**IMPORTANT**: The `codeBlock` snippet in the docs includes a `copy` button. Due to browser security, this only works when accessing via `localhost` or HTTPS.

> [!IMPORTANT]
> _This project is only compatible with the platform `Windows 64-bit (Modern)`.

## 📚 Examples
### Docs
Most menus explain WebStencils usage and templating patterns.

### Customers (Big Table & Pagination)
Loads customer data from the `CUSTOMERS` table in the shared InterBase database (`resources/data/tasks.ib`) using FireDAC. Demonstrates both loading all customers (Big Table) and server-side pagination.

### To-Do app - HTMX Integration
Loads and manages tasks from the `TASKS` table in the shared InterBase database (`resources/data/tasks.ib`). Uses HTMX with WebStencils for dynamic updates. See `partials/tasks` and related C++ code (`ControllerTasks.h/.cpp`, `ModelTasks.h/.cpp`).

## 📁 Project Structure
1.  C++ source files (`.cpp`, `.h`)
2.  Delphi Form files (`.dfm`) for the RAD Server module
3.  InterBase database (`tasks.ib`) in `resources/data`
4.  HTML templates (`.html`) in `resources/html`
5.  Static assets (CSS, JS, images) in `resources/static`

### 🔑 Key C++ Files
- `WebStencilsRADServerCppDemo.cbproj`: Main C++ Builder project file.
- `WebResource.h/.cpp`: RAD Server resource unit handling endpoints, request processing, and WebStencils integration.
- `ModelTasks.h/.cpp`, `ControllerTasks.h/.cpp`: Implement Tasks demo functionality (InterBase backend).
- `ControllerCustomers.h/.cpp`: Implements Customers demo functionality (InterBase backend).
- `ModelPaginationParams.h/.cpp`: Defines a reusable pagination parameter class.
- `CodeExamplesU.h/.cpp`: Contains code snippets for documentation pages.

### 📄 HTML Templates
Located in the `resources/html` directory, demonstrating WebStencils templating capabilities.
- `layouts/mainLayout.html`: Main layout.
- Various content pages (e.g., `home.html`, `basics.html`).
- Reusable partials in `partials/`.
- `partials/customers`: Templates for Big Table and Pagination demos.
- `partials/tasks`: Templates for the HTMX Tasks demo.

## 💻 Web Tech Used
- Bootstrap 5.3
- Bootstrap icons
- HTMX 2.0.2 (with JSON-enc extension)
- Minor custom CSS/JS (in `resources/static`)

Dependencies are loaded via CDNs.

## ✨ Environment Variables & Data Sources
- `WebResourceCpp.cpp` shows how to pass environment-like data (`FEnv`) to the WebStencils engine.
- **Data Sources:** This project uses an InterBase database (`resources/data/tasks.ib`) for both Tasks and Customer data, showcasing WebStencils' integration with different database systems via FireDAC within RAD Server using C++Builder. 