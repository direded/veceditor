unit About;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TaboutForm }
  TaboutForm = class(TForm)
    logoImg: TImage;
    authorLabel: TLabel;
    authorNameLabel: TLabel;
    educationGroupLabel: TLabel;
    educationGroupNameLabel: TLabel;
    textPanel: TPanel;
  end;

var
  aboutForm: TaboutForm;

implementation

{$R *.lfm}

end.

