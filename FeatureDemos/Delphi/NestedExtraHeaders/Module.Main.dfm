object WebModuleMain: TWebModuleMain
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModuleMainDefaultHandlerAction
    end
    item
      Name = 'WebActionItem1'
      PathInfo = '/link1'
      OnAction = WebModuleMainWebActionItem1Action
    end
    item
      Name = 'WebActionItem2'
      PathInfo = '/link2'
      OnAction = WebModuleMainWebActionItem2Action
    end>
  Height = 230
  Width = 415
  object WSProcessor: TWebStencilsProcessor
    InputFileName = '../../templates/BaseLayout.html'
    Left = 192
    Top = 80
  end
end

