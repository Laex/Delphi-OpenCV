unit opencv_classes;

interface

Uses WinApi.Windows;

Type
  IMat = interface
    ['{9C458D5C-F577-4A2D-89A0-FC426B80CC56}']
    // ! returns element type, similar to CV_MAT_TYPE(cvmat->type)
    function _type: Integer; stdcall;
    // ! returns element type, similar to CV_MAT_DEPTH(cvmat->type)
    function depth: Integer; stdcall;
    // ! returns element type, similar to CV_MAT_CN(cvmat->type)
    function channels: Integer; stdcall;
    // ! returns true if matrix data is NULL
    function empty: bool; stdcall;
  end;

  // ! default constructor
function CreateMat: IMat; overload; safecall;
// ! constructs 2D matrix of the specified size and type
// (_type is CV_8UC1, CV_64FC3, CV_32SC(12) etc.)
function CreateMat(rows, cols, _type: Integer): IMat; overload; safecall;

implementation

Uses uLibName;

function CreateMat: IMat; external OpenCV_Classes_DLL index 1;
function CreateMat(rows, cols, _type: Integer): IMat; external OpenCV_Classes_DLL index 2;

end.
