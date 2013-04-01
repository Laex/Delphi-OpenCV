unit types_c;

interface

const
  // Ќаименьшее число дл€ которого выполн€етс€ условие 1.0+DBL_EPSILON != 1.0
  DBL_EPSILON = 2.2204460492503131E-016;

Type
  pCvPoint2D32f = ^TCvPoint2D32f;

  TCvPoint2D32f = packed record
    x: Single;
    y: Single;
  end;

  pCvPoint2D32fArray = ^TCvPoint2D32fArray;
  TCvPoint2D32fArray = array [0 .. 1] of TCvPoint2D32f;

  // *********************************** CvTermCriteria *************************************/

const
  CV_TERMCRIT_ITER = 1;
  CV_TERMCRIT_NUMBER = CV_TERMCRIT_ITER;
  CV_TERMCRIT_EPS = 2;

Type
  pCvTermCriteria = ^TCvTermCriteria;

  TCvTermCriteria = packed record
    _type: Integer;
    // may be combination of
    // CV_TERMCRIT_ITER
    // CV_TERMCRIT_EPS
    max_iter: Integer;
    epsilon: double;
  end;

function CvTermCriteria(_type: Integer; max_iter: Integer; epsilon: double): TCvTermCriteria; inline;

implementation

function CvTermCriteria(_type: Integer; max_iter: Integer; epsilon: double): TCvTermCriteria;
begin
  Result._type := _type;
  Result.max_iter := max_iter;
  Result.epsilon := epsilon;
end;

end.
