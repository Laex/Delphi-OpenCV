// Created uCompressHaar.exe

Type
  TocvHaarCascadeRecord = record
    Name: String;
    FileName: String;
  end;

const
CascadeRecourse: array [TocvHaarCascadeType] of TocvHaarCascadeRecord =
(
(Name: 'HEYE'; FileName: 'haarcascade_eye.xml'),
(Name: 'HEYETREEEYEGLASSES'; FileName: 'haarcascade_eye_tree_eyeglasses.xml'),
(Name: 'HFFALT'; FileName: 'haarcascade_frontalface_alt.xml'),
(Name: 'HFFALT2'; FileName: 'haarcascade_frontalface_alt2.xml'),
(Name: 'HFFALTTREE'; FileName: 'haarcascade_frontalface_alt_tree.xml'),
(Name: 'HFFDEFAULT'; FileName: 'haarcascade_frontalface_default.xml'),
(Name: 'HFULLBODY'; FileName: 'haarcascade_fullbody.xml'),
(Name: 'HLEYE2SPLITS'; FileName: 'haarcascade_lefteye_2splits.xml'),
(Name: 'HLOWERBODY'; FileName: 'haarcascade_lowerbody.xml'),
(Name: 'HMEYEPAIRBIG'; FileName: 'haarcascade_mcs_eyepair_big.xml'),
(Name: 'HMEYEPAIRSMALL'; FileName: 'haarcascade_mcs_eyepair_small.xml'),
(Name: 'HMLEAR'; FileName: 'haarcascade_mcs_leftear.xml'),
(Name: 'HMLEYE'; FileName: 'haarcascade_mcs_lefteye.xml'),
(Name: 'HMMOUTH'; FileName: 'haarcascade_mcs_mouth.xml'),
(Name: 'HMNOSE'; FileName: 'haarcascade_mcs_nose.xml'),
(Name: 'HMREAR'; FileName: 'haarcascade_mcs_rightear.xml'),
(Name: 'HMREYE'; FileName: 'haarcascade_mcs_righteye.xml'),
(Name: 'HMUPPERBODY'; FileName: 'haarcascade_mcs_upperbody.xml'),
(Name: 'HPROFILEFACE'; FileName: 'haarcascade_profileface.xml'),
(Name: 'HREYE2SPLITS'; FileName: 'haarcascade_righteye_2splits.xml'),
(Name: 'HSMILE'; FileName: 'haarcascade_smile.xml'),
(Name: 'HUPPERBODY'; FileName: 'haarcascade_upperbody.xml')
);
