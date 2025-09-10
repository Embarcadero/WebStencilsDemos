#include <vcl.h>
#pragma hdrstop

#include "ControllerTasks.h"
#include <System.NetEncoding.hpp>
#include <System.SysUtils.hpp>
#include <System.IOUtils.hpp>
#pragma package(smart_init)

TTasksController::TTasksController(TWebStencilsEngine* AWebStencilsEngine) {
    if (!AWebStencilsEngine) {
        throw Exception("WebStencilsEngine cannot be null");
    }
    try {
        FWebStencilsEngine = AWebStencilsEngine;
        FWebStencilsProcessor = new TWebStencilsProcessor(nullptr);
        FWebStencilsProcessor->Engine = FWebStencilsEngine;
        FTasks = TTasks::GetInstance();
        FWebStencilsEngine->AddVar("Tasks", FTasks);
    }
    catch (Exception& E) {
        if (FWebStencilsProcessor) {
            delete FWebStencilsProcessor;
            FWebStencilsProcessor = nullptr;
        }
        throw;
    }
}

TTasksController::~TTasksController() {
    if (FWebStencilsProcessor) {
        delete FWebStencilsProcessor;
        FWebStencilsProcessor = nullptr;
    }
}

String TTasksController::RenderTemplate(const String& ATemplate, TTaskItem* ATask) {
    String templatePath = TPath::Combine(FWebStencilsEngine->RootDirectory, "partials/tasks/" + ATemplate + ".html");
    
    if (!FileExists(templatePath)) {
        throw Exception("Template file not found: " + templatePath);
    }

    FWebStencilsProcessor->InputFileName = templatePath;

    try {
        if (ATask) {
            FWebStencilsProcessor->AddVar("Task", ATask, false);
        }

        String result = FWebStencilsProcessor->Content();
        return result;
    }
    __finally {
        if (ATask) {
            FWebStencilsProcessor->DataVars->Remove("Task");
        }
    }
}

void TTasksController::CreateTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled) {
    try {
        String task = Request->ContentFields->Values["task"];
        if (task.IsEmpty()) {
            throw Exception("Task description is required");
        }
        task = TNetEncoding::HTML->Encode(task);
        TTaskItem* newTask = FTasks->AddTask(task);
        Response->Content = RenderTemplate("card");
        Handled = true;
    }
    catch (Exception& E) {
        Response->Content = "Error: " + E.Message;
        Response->StatusCode = 400;
        Handled = true;
    }
}

void TTasksController::DeleteTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled) {
    try {
        String id = Request->QueryFields->Values["id"];
        if (id.IsEmpty()) {
            throw Exception("Task ID is required");
        }
        int taskId = id.ToInt();
        if (taskId <= 0) {
            throw Exception("Invalid task ID");
        }
        FTasks->DeleteTask(taskId);
        Response->Content = RenderTemplate("card");
        Handled = true;
    }
    catch (Exception& E) {
        Response->Content = "Error: " + E.Message;
        Response->StatusCode = 400;
        Handled = true;
    }
}

void TTasksController::EditTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled) {
    try {
        String id = Request->QueryFields->Values["id"];
        String task = Request->ContentFields->Values["task"];
        
        if (id.IsEmpty()) {
            throw Exception("Task ID is required");
        }
        if (task.IsEmpty()) {
            throw Exception("Task description is required");
        }
        
        int taskId = id.ToInt();
        if (taskId <= 0) {
            throw Exception("Invalid task ID");
        }
        
        task = TNetEncoding::HTML->Encode(task);
        FTasks->EditTask(taskId, task);
        Response->Content = RenderTemplate("card");
        Handled = true;
    }
    catch (Exception& E) {
        Response->Content = "Error: " + E.Message;
        Response->StatusCode = 400;
        Handled = true;
    }
}

void TTasksController::GetEditTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled) {
    try {
        String id = Request->QueryFields->Values["id"];
        if (id.IsEmpty()) {
            throw Exception("Task ID is required");
        }
        int taskId = id.ToInt();
        if (taskId <= 0) {
            throw Exception("Invalid task ID");
        }
        
        TTaskItem* task = FTasks->FindTaskById(taskId);
        if (!task) {
            throw Exception("Task not found");
        }
        
        Response->Content = RenderTemplate("itemEdit", task);
        Handled = true;
    }
    catch (Exception& E) {
        Response->Content = "Error: " + E.Message;
        Response->StatusCode = 400;
        Handled = true;
    }
}

void TTasksController::ToggleCompletedTask(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled) {
    try {
        String id = Request->QueryFields->Values["id"];
        if (id.IsEmpty()) {
            throw Exception("Task ID is required");
        }
        int taskId = id.ToInt();
        if (taskId <= 0) {
            throw Exception("Invalid task ID");
        }
        
        TTaskItem* task = FTasks->ToggleCompletedTask(taskId);
        if (!task) {
            throw Exception("Task not found");
        }
        
        Response->Content = RenderTemplate("card");
        Handled = true;
    }
    catch (Exception& E) {
        Response->Content = "Error: " + E.Message;
        Response->StatusCode = 400;
        Handled = true;
    }
}