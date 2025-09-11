# WebStencils Demos

![WebStencils screenshot](.github/images/WebStencils-Screenshot.png)

Embarcadero official WebStencils demo repository. Note that you will need RAD Studio (with Delphi or C++Builder) 13.0 or higher to compile the demos.

## Sample Projects included

### WebBroker Project
Demonstrates using WebStencils with WebBroker technology. Available in both Delphi and C++Builder.
- **Data Sources:** Uses an in-memory list for the Tasks demo and an SQLite database (`resources/data/database.sqlite3`) for the Customers demos (Big Table & Pagination).
- **Templates:** Uses dedicated HTML templates from the `resources/html` directory.

[More info - Delphi](https://github.com/Embarcadero/WebStencilsDemos/tree/main/WebBrokerProject/Delphi) | [More info - C++Builder](https://github.com/Embarcadero/WebStencilsDemos/tree/main/WebBrokerProject/C++)

### RAD Server Project
Demonstrates using WebStencils within Embarcadero's RAD Server REST API backend framework. Available in both Delphi and C++Builder.
- **Data Sources:** Uses an InterBase database (`resources/data/tasks.ib`) for both the Tasks and Customers demos.
- **Templates:** Uses dedicated HTML templates from the `resources/html` directory.

[More info - Delphi](https://github.com/Embarcadero/WebStencilsDemos/tree/main/RADServerProject/Delphi) | [More info - C++Builder](https://github.com/Embarcadero/WebStencilsDemos/tree/main/RADServerProject/C++)

> **Note:** Both WebBroker and RAD Server projects render the same website but leverage different backend technologies and data storage mechanisms. This highlights the flexibility of WebStencils, which remains independent of the backend framework (WebBroker, RAD Server, etc.), programming language (Delphi, C++), and data access layer (In-memory, SQLite, InterBase via FireDAC). Each project type uses its own dedicated WebStencils templates.

### FMX Showcase
This demo shows the core functionality of WebStencils in a FireMonkey (FMX) application through a highly visual interface. The default templates are stored as global constants in a dedicated unit, and the demo allows editing the templates on the memo fields to allow interactive visualization of the generated output. 

[More info](https://github.com/Embarcadero/WebStencilsDemos/tree/main/FMXShowcase)

## Copyright
This software is Copyright 2025 Embarcadero Technologies, Inc.

You may only use this software if you are an authorized licensee of an Embarcadero developer tools product. This software is considered a Redistributable as defined in the software license agreement that comes with the Embarcadero Products and is governed by the terms of such software license agreement (https://www.embarcadero.com/products/rad-studio/rad-studio-eula).
