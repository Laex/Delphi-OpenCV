unit highgui;

interface

Uses WinApi.Windows, Mat;

Type
  IVideoCapture = interface
    ['{3F605CF0-ECAC-4230-B30B-AF9BFD516C4F}']
    function open(device: Integer): bool; stdcall;
    function isOpened(): bool; stdcall;
    procedure release(); stdcall;

    function grab(): bool; stdcall;
    function retrieve(image: IMat; flag: Integer): bool; stdcall;
    function read(image: IMat): bool; stdcall;

    function setValue(propId: Integer; value: double): bool; stdcall;
    function getValue(propId: Integer): double; stdcall;
  end;

function CreateVideoCapture: IVideoCapture; overload; safecall;
function CreateVideoCapture(device: Integer): IVideoCapture; overload; safecall;

implementation

Uses uLibName;

function CreateVideoCapture: IVideoCapture; external OpenCV_Classes_DLL index 100;
function CreateVideoCapture(device: Integer): IVideoCapture; external OpenCV_Classes_DLL index 101;

end.
