# WebStencils Demo Project

![WebStencils screenshot](../../.github/images/WebBroker.png)

## 🌟 Overview 
This project demonstrates the usage of WebStencils using WebBroker. It includes part of the documentation built into the generated website, as well as a couple of demos. 

## 🚦 Getting Started
The project can be run on Windows and Linux. 

On Windows, nothing needs to be done. Run and off you go. The default behavior is trying to get the HTML templates and database from the `resources` folder (preconfigured). On a production environment, the path should be changed.

To deploy on Linux change the `Build configuration` to `Release`. The `resources` folder needs to be copied to the same location where the binary is deployed. It needs PAServer running on a Linux machine (it also works with PAServer docker image).

## 🐳 Docker Deployment
The project includes Docker support for easy deployment and containerization. The Docker setup includes automatic database initialization, logging, and health monitoring.

### Prerequisites
- WSL2 active in your system
- Docker installed on WSL
- Delphi IDE with Linux platform configured
- Valid Linux platform target configured in Delphi project settings

### Building and Running with Docker
1. Ensure all prerequisites are met
2. Open the project in Delphi
3. Select `Release` → `Docker` build configuration with `Linux` platform
4. Build the project (Ctrl+Shift+F9)
   - The post-build event will automatically execute the `build_docker_image.ps1` PowerShell script which:
     - Cleans up old Docker containers and images
     - Creates a temporary build context
     - Copies the Linux executable and resources to the build context
     - Sets proper file permissions
     - Builds the Docker image using the Dockerfile
     - Cleans up temporary files
5. Run the container: `docker run -d -p 8080:8080 --name=webstencils-demo webstencils-demo:latest`
6. Access the application at `http://localhost:8080`

**Advanced run options:**
With persistent storage:
```bash
docker run -d -p 8080:8080 \
  -v /host/path/to/logs:/app/logs \
  -v /host/path/to/data:/app/data \
  --name=webstencils-demo \
  webstencils-demo:latest
```

### Data Persistence
- The application uses SQLite for data storage
- On first run, it automatically initializes the database with demo data
- Subsequent runs will use the existing database from the volume
- Database location: `/app/data/database.sqlite3`

### Logging
- Logs are written to `/app/logs/app.log`
- Logs are persisted through Docker volumes

### Health Monitoring
The application includes a health check endpoint:
```bash
curl http://localhost:8080/health
```

### Troubleshooting
**Common issues:**
- **WSL not found**: Ensure WSL2 is properly installed and enabled
- **Docker not accessible**: Verify Docker is running in WSL with `wsl docker ps`
- **Build fails**: Check that the Linux platform is configured in Delphi project settings
- **Port already in use**: Change the port mapping in the docker run command (e.g., `-p 8081:8080`)
- **Permission denied**: Ensure the PowerShell script has execution permissions

> [!CAUTION]
> This demo has been developed using Docker CLI installed directly inside WSL2 and not Docker Desktop. If you have Docker Desktop installed or previously installed, you may experience issues. Several users have reported fixing these issues by following the potential solutions [listed here](https://github.com/docker/for-win/issues/7039).

## 📚 Examples
### Docs
Most of the menus explain the general use of WebStencils as well as some suggested ideas for templating patterns. 

### 🔐 Authentication System
The application includes a complete authentication system with:
- **User Login/Logout**: Secure session-based authentication
- **Role-Based Access**: User and admin roles with different permissions
- **Protected Features**: Admin-only access to customer management
- **Demo Credentials**: 
  - User: `demo` / `demo123` (user role)
  - Admin: `admin` / `admin123` (admin role)

### 📊 Enhanced Customer Management
Complete CRUD operations for customer data:
- **Create**: Add new customers with form validation
- **Read**: List customers with search and pagination
- **Update**: Edit customer information with validation
- **Delete**: Remove customers with confirmation
- **Search**: Multi-field search across name, email, company, phone, and city
- **Pagination**: Server-side pagination with configurable page sizes
- **Dynamic Forms**: Forms automatically generated from FireDAC query metadata

### Big Table
This demo loads 1000 customers loaded in a FireDAC query. Customers data is stored in a `sqlite` database in `resources/data/database.sqlite3`.

### Customers
Same `customers` table from the `sqlite` database, but this time using server-side pagination.

### To-Do app - HTMX Integration
This demo uses an in-memory `TList<TTaskItem>` managed by a singleton (`Model.Tasks.pas`) to store task data. It uses HTMX with WebStencils for dynamic content updates. See the `partials/tasks` templates and related Delphi code (`Controller.Tasks.pas`) for implementation details.

## 📁 Project Structure
The project consists of the following main components:
1. Delphi source files (`.pas` and `.dfm`)
2. **Shared** HTML templates (`.html`) located in `resources/html`
3. **Shared** Static assets (CSS, JavaScript, images) located in `resources/static`

### 🔑 Key Delphi Units
- `WebStencilsDemo.dpr`: The main project file that includes WebBroker
- `Modules.Main.pas`: Handles web requests and sets up the WebStencils engine (renamed from MainWebModuleU.pas)
- `Controllers.Base.pas`: Base controller with form session management and validation
- `Controllers.Customers.pas`: Enhanced customer management with CRUD operations
- `Controllers.Tasks.pas`: Implement the Tasks demo functionality (Tasks are stored in memory using a singleton)
- `Models.Tasks.pas`: Task data model and management
- `Utils.FormSession.pas`: Form data persistence and validation across requests
- `Utils.Search.pas`: Configurable search functionality
- `Utils.PaginationParams.pas`: Reusable pagination system for WebStencils
- `Utils.Logger.pas`: Comprehensive logging system
- `Helpers.Messages.pas`: Flash message system for user feedback
- `Helpers.FDQuery.pas`: FireDAC query helpers for pagination
- `Services.CodeExamples.pas`: Code examples used in the demo pages
- `Constants.Server.pas`: Server-related constants and messages

### 📄 HTML Templates
Located in the **shared** `resources/html` directory. This demonstrates how the same templates can be used across different projects (Delphi/C++, WebBroker/RAD Server).
- `layouts/mainLayout.html`: The main layout template
- `layouts/baseLayout.html`: Base layout for authentication pages
- `layouts/authMenu.html`: Authentication menu component
- Various content pages (e.g., `home.html`, `basics.html`, `keywords.html`)
- `login.html`, `logout.html`: Authentication templates
- `customers/`: Enhanced customer management templates (add, edit, index, bigtable)
- Partial templates reusable across pages in the `partials/` directory
- `partials/customers/`: Templates for customer CRUD operations
- `partials/messages.html`: Flash message display
- `partials/dynamicInput.html`: Dynamic form input generation
- `partials/tasks`: Templates specific to the HTMX Tasks demo

> **IMPORTANT**
> The `codeBlock` template has a `copy` button. Due to browser security limitations, this only works if the URL is `localhost` or if it's being run under https. If the demo is accessed through the network, the button is not functional.

## ✨ Environment Variables & Data Sources
- The `Modules.Main` demonstrates handling custom environment variables accessible in templates via `@env.VARIABLE_NAME` (e.g., `@env.APP_NAME`, `@env.APP_EDITION`).
- It also shows handling system variables like `@system.year` and `@system.timestamp` via the WebStencilsEngine `OnValue` event.
- The `DEBUG_MODE` variable is automatically set based on the build configuration.
- **Data Sources:** This project uses an in-memory list for Tasks and an SQLite database (`resources/data/database.sqlite3`) for Customer data, showcasing WebStencils' independence from specific data storage mechanisms.

## 💬 Flash Message System
The application includes a comprehensive flash message system:
- **Message Types**: Success, warning, error, and info messages
- **Session Persistence**: Messages survive page redirects
- **Bootstrap Integration**: Styled message display with icons
- **Automatic Cleanup**: Messages are cleared after display

## 🔍 Search & Pagination Features
- **Multi-Field Search**: Search across customer name, email, company, phone, and city
- **URL Parameter Handling**: Search terms preserved in URLs
- **Configurable Pagination**: Adjustable page sizes with validation
- **Search Result Highlighting**: Visual feedback for search results

## 💻 Web Tech Used
- Bootstrap 5.3
- Bootstrap icons
- HTMX 2.0.2
- Minor custom CSS and JS

All the external dependencies are loaded directly from CDNs. The custom CSS and JS can be found in the `static` folder.
