// --------------------------------- OpenCV license.txt ---------------------------
(* //    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.
  //
  //    By downloading, copying, installing or using the software you agree to this license.
  //    If you do not agree to this license, do not download, install,
  //    copy or use the software.
  //
  //
  //                             License Agreement
  //                  For Open Source Computer Vision Library
  //
  //   Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
  //   Copyright (C) 2009, Willow Garage Inc., all rights reserved.
  //   Third party copyrights are property of their respective owners.
  //
  //   Redistribution and use in source and binary forms, with or without modification,
  //   are permitted provided that the following conditions are met:
  //
  //     * Redistribution's of source code must retain the above copyright notice,
  //       this list of conditions and the following disclaimer.
  //
  //     * Redistribution's in binary form must reproduce the above copyright notice,
  //       this list of conditions and the following disclaimer in the documentation
  //       and/or other materials provided with the distribution.
  //
  //     * The name of the copyright holders may not be used to endorse or promote products
  //       derived from this software without specific prior written permission.
  //
  //   This software is provided by the copyright holders and contributors "as is" and
  //   any express or implied warranties, including, but not limited to, the implied
  //   warranties of merchantability and fitness for a particular purpose are disclaimed.
  //   In no event shall the Intel Corporation or contributors be liable for any direct,
  //   indirect, incidental, special, exemplary, or consequential damages
  //   (including, but not limited to, procurement of substitute goods or services;
  //   loss of use, data, or profits; or business interruption) however caused
  //   and on any theory of liability, whether in contract, strict liability,
  //   or tort (including negligence or otherwise) arising in any way out of
  //   the use of this software, even if advised of the possibility of such damage. *)

(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  laentir Valetov
  //  email:laex@bk.ru
  //  **************************************************************************************************
  //  You may retrieve the latest version of this file at the GitHub,
  //  located at git://github.com/Laex/Delphi-OpenCV.git
  //  **************************************************************************************************
  //  License:
  //  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  //  you may not use this file except in compliance with the License. You may obtain a copy of the
  //  License at http://www.mozilla.org/MPL/
  //
  //  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  //  ANY KIND, either express or implied. See the License for the specific language governing rights
  //  and limitations under the License.
  //
  //  Alternatively, the contents of this file may be used under the terms of the
  //  GNU Lesser General Public License (the  "LGPL License"), in which case the
  //  provisions of the LGPL License are applicable instead of those above.
  //  If you wish to allow use of your version of this file only under the terms
  //  of the LGPL License and not to allow others to use your version of this file
  //  under the MPL, indicate your decision by deleting  the provisions above and
  //  replace  them with the notice and other provisions required by the LGPL
  //  License.  If you do not delete the provisions above, a recipient may use
  //  your version of this file under either the MPL or the LGPL License.
  //
  //  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  //  **************************************************************************************************
  //  Warning: Using Delphi XE3 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  **************************************************************************************************
  //  Original file:
  //  opencv\modules\legacy\include\opencv2\legacy\blobtrack.hpp
  //  ************************************************************************************************* *)

{$IFDEF DEBUG}
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$ELSE}
{$A8,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$ENDIF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$POINTERMATH ON}
unit blobtrack;

interface

Uses Windows, core_c, core.types_c, imgproc_c;

type
  // struct DefParam;
  pCvDefParam = ^TCvDefParam;

  TCvDefParam = packed record
    next: pCvDefParam;
    pName: pCvChar;
    pComment: pCvChar;
    pDouble: pDouble;
    Double: Double;
    pFloat: pSingle;
    Float: Single;
    pInt: pInteger;
    Int: Integer;
    pStr: ppCVChar;
    Str: pCvChar;
  end;

  TCvVSModule = class
  private // * Internal data: */
    m_pParamList: pCvDefParam;
    m_pModuleTypeName: pCvChar;
    m_pModuleName: pCvChar;
    m_pNickName: pCvChar;
  protected
    m_Wnd: Integer;
  public
    // * Constructor and destructor: * /
    constructor Create;
    destructor Destroy; override;
  private
    // * Internal functions: * /
    procedure FreeParam(var pp: pCvDefParam);
    function NewParam(const name: pCvChar): pCvDefParam;
    function GetParamPtr(index: Integer): pCvDefParam; overload;
    function GetParamPtr(const name: pCvChar): pCvDefParam; overload;
  protected
    // * Internal INTERFACE * /
    function IsParam(const name: pCvChar): Integer;
    procedure AddParam(const name: pCvChar; pAddr: pDouble); overload;
    procedure AddParam(const name: pCvChar; pAddr: pSingle); overload;
    procedure AddParam(const name: pCvChar; pAddr: pInteger); overload;
    procedure AddParam(const name: pCvChar; pAddr: pCvChar); overload;
    procedure AddParam(const name: pCvChar); overload;
    procedure CommentParam(const name: pCvChar; const pComment: pCvChar);
    procedure SetTypeName(const name: pCvChar);
    procedure SetModuleName(const name: pCvChar);
    procedure DelParam(const name: pCvChar);
  public
    // * EXTERNAL INTERFACE * /
    function GetParamName(index: Integer): pCvChar;
    function GetParamComment(const name: pCvChar): pCvChar;
    function GetParam(const name: pCvChar): Double;
    function GetParamStr(const name: pCvChar): pCvChar;
    procedure SetParam(const name: pCvChar; val: Double);
    procedure SetParamStr(const name: pCvChar; const Str: pCvChar);
    procedure TransferParamsFromChild(pM: TCvVSModule; const prefix: pCvChar = nil);
    procedure TransferParamsToChild(pM: TCvVSModule; prefix: pCvChar = nil);
    procedure ParamUpdate; virtual;
    function GetTypeName(): pCvChar;
    function IsModuleTypeName(const name: pCvChar): Integer;
    function GetModuleName(): pCvChar;
    function IsModuleName(const name: pCvChar): Integer;
    procedure SetNickName(const pStr: pCvChar);
    function GetNickName(): pCvChar;
    procedure SaveState(FileStorage: pCvFileStorage); virtual;
    procedure LoadState(FileStorage: pCvFileStorage; FileNode: pCvFileNode); virtual;
    procedure Release(); virtual; abstract;
  end;

  // CV_EXPORTS void cvWriteStruct(CvFileStorage* fs, const char* name, void* addr, const char* desc, int num=1);
  // CV_EXPORTS void cvReadStructByName(CvFileStorage* fs, CvFileNode* node, const char* name, void* addr, const char* desc);

  /// * FOREGROUND DETECTOR INTERFACE */
  TCvFGDetector = class(TCvVSModule)
  public
    constructor Create;
    function GetMask: pIplImage; virtual; abstract;
    // * Process current image: */
    procedure Process(pImg: pIplImage); virtual; abstract;
    // * Release foreground detector: */
    procedure Release; virtual; abstract;
  end;

  // * BLOB STRUCTURE*/
  pCvBlob = ^TCvBlob;

  TCvBlob = packed record
    x, y: Single; // * blob position   */
    w, h: Single; // * blob sizes      */
    ID: Integer; // * blob ID         */
  end;

function cvBlob(x, y, w, h: Single): TCvBlob; inline;

const
  CV_BLOB_MINW = 5;
  CV_BLOB_MINH = 5;

function CV_BLOB_ID(pB: pCvBlob): Integer; inline;
// #define CV_BLOB_CENTER(pB) cvPoint2D32f(((CvBlob*)(pB))->x,((CvBlob*)(pB))->y)
function CV_BLOB_X(pB: pCvBlob): Single; inline;
function CV_BLOB_Y(pB: pCvBlob): Single; inline;
function CV_BLOB_WX(pB: pCvBlob): Single; inline;
function CV_BLOB_WY(pB: pCvBlob): Single; inline;
function CV_BLOB_RX(pB: pCvBlob): Single; inline;
function CV_BLOB_RY(pB: pCvBlob): Single; inline;
// #define CV_BLOB_RECT(pB) cvRect(cvRound(((CvBlob*)(pB))->x-CV_BLOB_RX(pB)),cvRound(((CvBlob*)(pB))->y-CV_BLOB_RY(pB)),cvRound(CV_BLOB_WX(pB)),cvRound(CV_BLOB_WY(pB)))
/// * END BLOB STRUCTURE*/

type
  // * simple BLOBLIST */
  TCvBlobSeq = class
  public
    constructor Create(BlobSize: Integer = sizeof(TCvBlob));
    destructor Destroy; override;
    function GetBlob(BlobIndex: Integer): pCvBlob; virtual;
    function GetBlobByID(BlobID: Integer): pCvBlob; virtual;
    procedure DelBlob(BlobIndex: Integer); virtual;
    procedure DelBlobByID(BlobID: Integer); virtual;
    procedure Clear(); virtual;
    procedure AddBlob(pB: pCvBlob); virtual;
    function GetBlobNum(): Integer; virtual;
    procedure Write(fs: pCvFileStorage; const name: pCvChar); virtual;
    procedure Load(fs: pCvFileStorage; node: pCvFileNode); virtual;
    procedure AddFormat(const Str: pCvChar);
  protected
    m_pMem: pCvMemStorage;
    m_pSeq: pCvSeq;
    m_pElemFormat: array [0 .. 1024 - 1] of CvChar;
  end;


  // * simple BLOBLIST */
  //
  /// * simple TRACKLIST */
  // struct CvBlobTrack
  // {
  // int         TrackID;
  // int         StartFrame;
  // CvBlobSeq*  pBlobSeq;
  // };
  //
  // class CV_EXPORTS CvBlobTrackSeq
  // {
  // public:
  // CvBlobTrackSeq(int TrackSize = sizeof(CvBlobTrack));
  // virtual ~CvBlobTrackSeq();
  // virtual CvBlobTrack* GetBlobTrack(int TrackIndex);
  // virtual CvBlobTrack* GetBlobTrackByID(int TrackID);
  // virtual void DelBlobTrack(int TrackIndex);
  // virtual void DelBlobTrackByID(int TrackID);
  // virtual void Clear();
  // virtual void AddBlobTrack(int TrackID, int StartFrame = 0);
  // virtual int GetBlobTrackNum();
  // protected:
  // CvMemStorage*   m_pMem;
  // CvSeq*          m_pSeq;
  // };

  /// * simple TRACKLIST */
Type
  // * BLOB DETECTOR INTERFACE */
  TCvBlobDetector = class(TCvVSModule)
  public
    constructor Craete;
    // * Try to detect new blob entrance based on foreground mask. */
    // * pFGMask - image of foreground mask */
    // * pNewBlob - pointer to CvBlob structure which will be filled if new blob entrance detected */
    // * pOldBlobList - pointer to blob list which already exist on image */
    function DetectNewBlob(pImg: pIplImage; pImgFG: pIplImage; pNewBlobList: TCvBlobSeq; pOldBlobList: TCvBlobSeq)
      : Integer; virtual; abstract;
    // * release blob detector */
    procedure Release(); virtual; abstract;
  end;

  /// * Release any blob detector: */
  // CV_EXPORTS void cvReleaseBlobDetector(CvBlobDetector** ppBD);
  //
  /// * Declarations of constructors of implemented modules: */
  /// * Blob detector creator (sole interface function for this file) */
  // CvBlobDetector* cvCreateBlobDetectorSimple()
function cvCreateBlobDetectorSimple(): TCvBlobDetector;
/// * Blob detector creator (sole interface function for this file): */
function cvCreateBlobDetectorCC(): TCvBlobDetector;

Type
  TCvDetectedBlob = packed record
    x, y: Single; // * blob position   */
    w, h: Single; // * blob sizes      */
    ID: Integer; // * blob ID         */
    response: Single;
  end;

  // CV_INLINE CvDetectedBlob cvDetectedBlob( float x, float y, float w, float h, int ID = 0, float response = 0.0F )
  // {
  // CvDetectedBlob b;
  // b.x = x; b.y = y; b.w = w; b.h = h; b.ID = ID; b.response = response;
  // return b;
  // }

type
  TCvObjectDetector = class
  public
    constructor Create(const detector_file_name: pCvChar = nil);
    destructor Destroy; override;
    // /*
    // * Release the current detector and load new detector from file
    // * (if detector_file_name is not 0)
    // * Return true on success:
    // */
    function Load(const detector_file_name: pCvChar = nil): boolean;
    // /* Return min detector window size: */
    function GetMinWindowSize(): TCvSize;
    // /* Return max border: */
    function GetMaxBorderSize(): Integer;
    // /*
    // * Detect the object on the image and push the detected
    // * blobs into <detected_blob_seq> which must be the sequence of <CvDetectedBlob>s
    // */
    procedure Detect(const img: pCvArr; out detected_blob_seq: TCvBlobSeq);
    // protected
    // impl:TCvObjectDetectorImpl;
  end;


  // CV_INLINE CvRect cvRectIntersection( const CvRect r1, const CvRect r2 )
  // {
  // CvRect r = cvRect( MAX(r1.x, r2.x), MAX(r1.y, r2.y), 0, 0 );
  //
  // r.width  = MIN(r1.x + r1.width, r2.x + r2.width) - r.x;
  // r.height = MIN(r1.y + r1.height, r2.y + r2.height) - r.y;
  //
  // return r;
  // }
  //
  //
  /// *
  // * CvImageDrawer
  // *
  // * Draw on an image the specified ROIs from the source image and
  // * given blobs as ellipses or rectangles:
  // */
  //
  // struct CvDrawShape
  // {
  // enum {RECT, ELLIPSE} shape;
  // CvScalar color;
  // };
  //
  /// *extern const CvDrawShape icv_shape[] =
  // {
  // { CvDrawShape::ELLIPSE, CV_RGB(255,0,0) },
  // { CvDrawShape::ELLIPSE, CV_RGB(0,255,0) },
  // { CvDrawShape::ELLIPSE, CV_RGB(0,0,255) },
  // { CvDrawShape::ELLIPSE, CV_RGB(255,255,0) },
  // { CvDrawShape::ELLIPSE, CV_RGB(0,255,255) },
  // { CvDrawShape::ELLIPSE, CV_RGB(255,0,255) }
  // };*/
  //
  // class CV_EXPORTS CvImageDrawer
  // {
  // public:
  // CvImageDrawer() : m_image(0) {}
  // ~CvImageDrawer() { cvReleaseImage( &m_image ); }
  // void SetShapes( const CvDrawShape* shapes, int num );
  // /* <blob_seq> must be the sequence of <CvDetectedBlob>s */
  // IplImage* Draw( const CvArr* src, CvBlobSeq* blob_seq = 0, const CvSeq* roi_seq = 0 );
  // IplImage* GetImage() { return m_image; }
  // protected:
  // //static const int MAX_SHAPES = sizeof(icv_shape) / sizeof(icv_shape[0]);;
  //
  // IplImage* m_image;
  // CvDrawShape m_shape[16];
  // };
  //
  //
  //
  /// * Trajectory generation module: */
  // class CV_EXPORTS CvBlobTrackGen: public CvVSModule
  // {
  // public:
  // CvBlobTrackGen(){SetTypeName("BlobTrackGen");};
  // virtual void    SetFileName(char* pFileName) = 0;
  // virtual void    AddBlob(CvBlob* pBlob) = 0;
  // virtual void    Process(IplImage* pImg = NULL, IplImage* pFG = NULL) = 0;
  // virtual void    Release() = 0;
  // };
  //
  // inline void cvReleaseBlobTrackGen(CvBlobTrackGen** pBTGen)
  // {
  // if(*pBTGen)(*pBTGen)->Release();
  // *pBTGen = 0;
  // }
  //
  /// * Declarations of constructors of implemented modules: */
  // CV_EXPORTS CvBlobTrackGen* cvCreateModuleBlobTrackGen1();
  // CV_EXPORTS CvBlobTrackGen* cvCreateModuleBlobTrackGenYML();

type
  /// * BLOB TRACKER INTERFACE */
  TCvBlobTracker = class(TCvVSModule)
  public
    constructor Create;
    // /* Add new blob to track it and assign to this blob personal ID */
    // /* pBlob - pointer to structure with blob parameters (ID is ignored)*/
    // /* pImg - current image */
    // /* pImgFG - current foreground mask */
    // /* Return pointer to new added blob: */
    function AddBlob(pBlob: pCvBlob; pImg: pIplImage; pImgFG: pIplImage = NiL): pCvBlob; virtual; abstract;
    // /* Return number of currently tracked blobs: */
    function GetBlobNum(): Integer; virtual; abstract;
    // /* Return pointer to specified by index blob: */
    function GetBlob(BlobIndex: Integer): pCvBlob; virtual; abstract;
    // /* Delete blob by its index: */
    procedure DelBlob(BlobIndex: Integer); virtual; abstract;
    // /* Process current image and track all existed blobs: */
    // virtual void    Process(IplImage* pImg, IplImage* pImgFG = NULL) = 0;
    procedure Process(pImg: pIplImage; pImgFG: pIplImage = NiL); virtual; abstract;
    // /* Release blob tracker: */
    procedure Release(); virtual; abstract;
    // /* Process one blob (for multi hypothesis tracing): */
    procedure ProcessBlob(BlobIndex: Integer; pBlob: pCvBlob; pImg: pIplImage; pImgFG: pIplImage = NiL);
      virtual; abstract;
    // /* Get confidence/wieght/probability (0-1) for blob: */
    // virtual double  GetConfidence(int /*BlobIndex*/, CvBlob* /*pBlob*/, IplImage* /*pImg*/, IplImage* /*pImgFG*/ = NULL);
    function GetConfidence(int /*BlobIndex*/, CvBlob* /*pBlob*/, IplImage* /*pImg*/, IplImage* /*pImgFG*/ = NULL):double; virtual;
    // virtual double GetConfidenceList(CvBlobSeq* pBlobList, IplImage* pImg, IplImage* pImgFG = NULL);
    //
    // virtual void UpdateBlob(int /*BlobIndex*/, CvBlob* /*pBlob*/, IplImage* /*pImg*/, IplImage* /*pImgFG*/ = NULL);
    //
    // /* Update all blob models: */
    // virtual void Update(IplImage* pImg, IplImage* pImgFG = NULL);
    //
    // /* Return pointer to blob by its unique ID: */
    // virtual int     GetBlobIndexByID(int BlobID);
    //
    // /* Return pointer to blob by its unique ID: */
    // virtual CvBlob* GetBlobByID(int BlobID);
    //
    // /* Delete blob by its ID: */
    // virtual void    DelBlobByID(int BlobID);
    //
    // /* Set new parameters for specified (by index) blob: */
    // virtual void    SetBlob(int /*BlobIndex*/, CvBlob* /*pBlob*/);
    //
    // /* Set new parameters for specified (by ID) blob: */
    // virtual void    SetBlobByID(int BlobID, CvBlob* pBlob);
    //
    // /*  ===============  MULTI HYPOTHESIS INTERFACE ==================  */
    //
    // /* Return number of position hyposetis of currently tracked blob: */
    // virtual int     GetBlobHypNum(int /*BlobIdx*/);
    //
    // /* Return pointer to specified blob hypothesis by index blob: */
    // virtual CvBlob* GetBlobHyp(int BlobIndex, int /*hypothesis*/);
    //
    // /* Set new parameters for specified (by index) blob hyp
    // * (can be called several times for each hyp ):
    // */
    // virtual void    SetBlobHyp(int /*BlobIndex*/, CvBlob* /*pBlob*/);
  end;

  // CV_EXPORTS void cvReleaseBlobTracker(CvBlobTracker**ppT );
  /// * BLOB TRACKER INTERFACE */
  //
  /// *BLOB TRACKER ONE INTERFACE */
  // class CV_EXPORTS CvBlobTrackerOne : public CvVSModule
  // {
  // public:
  // virtual void Init(CvBlob* pBlobInit, IplImage* pImg, IplImage* pImgFG = NULL) = 0;
  // virtual CvBlob* Process(CvBlob* pBlobPrev, IplImage* pImg, IplImage* pImgFG = NULL) = 0;
  // virtual void Release() =  0;
  //
  // /* Non-required methods: */
  // virtual void SkipProcess(CvBlob* /*pBlobPrev*/, IplImage* /*pImg*/, IplImage* /*pImgFG*/ = NULL){};
  // virtual void Update(CvBlob* /*pBlob*/, IplImage* /*pImg*/, IplImage* /*pImgFG*/ = NULL){};
  // virtual void SetCollision(int /*CollisionFlag*/){}; /* call in case of blob collision situation*/
  // virtual double GetConfidence(CvBlob* /*pBlob*/, IplImage* /*pImg*/,
  // IplImage* /*pImgFG*/ = NULL, IplImage* /*pImgUnusedReg*/ = NULL)
  // {
  // return 1;
  // };
  // };
  // inline void cvReleaseBlobTrackerOne(CvBlobTrackerOne **ppT )
  // {
  // ppT[0]->Release();
  // ppT[0] = 0;
  // }
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerList(CvBlobTrackerOne* (*create)());
  /// *BLOB TRACKER ONE INTERFACE */
  //
  /// * Declarations of constructors of implemented modules: */
  //
  /// * Some declarations for specific MeanShift tracker: */
  // #define PROFILE_EPANECHNIKOV    0
  // #define PROFILE_DOG             1
  // struct CvBlobTrackerParamMS
  // {
  // int     noOfSigBits;
  // int     appearance_profile;
  // int     meanshift_profile;
  // float   sigma;
  // };
  //
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMS1(CvBlobTrackerParamMS* param);
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMS2(CvBlobTrackerParamMS* param);
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMS1ByList();
  //
  /// * Some declarations for specific Likelihood tracker: */
  // struct CvBlobTrackerParamLH
  // {
  // int     HistType; /* see Prob.h */
  // int     ScaleAfter;
  // };
  //
  /// * Without scale optimization: */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerLHR(CvBlobTrackerParamLH* /*param*/ = NULL);
  //
  /// * With scale optimization: */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerLHRS(CvBlobTrackerParamLH* /*param*/ = NULL);
  //
  /// * Simple blob tracker based on connected component tracking: */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerCC();
  //
  /// * Connected component tracking and mean-shift particle filter collion-resolver: */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerCCMSPF();
  //
  /// * Blob tracker that integrates meanshift and connected components: */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMSFG();
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMSFGS();
  //
  /// * Meanshift without connected-components */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMS();
  //
  /// * Particle filtering via Bhattacharya coefficient, which        */
  /// * is roughly the dot-product of two probability densities.      */
  /// * See: Real-Time Tracking of Non-Rigid Objects using Mean Shift */
  /// *      Comanicius, Ramesh, Meer, 2000, 8p                       */
  /// *      http://citeseer.ist.psu.edu/321441.html                  */
  // CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerMSPF();
  //
  /// * =========== tracker integrators trackers =============*/
  //
  /// * Integrator based on Particle Filtering method: */
  /// /CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerIPF();
  //
  /// * Rule based integrator: */
  /// /CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerIRB();
  //
  /// * Integrator based on data fusion using particle filtering: */
  /// /CV_EXPORTS CvBlobTracker* cvCreateBlobTrackerIPFDF();
  //
  //
  //
  //
  /// * Trajectory postprocessing module: */
  // class CV_EXPORTS CvBlobTrackPostProc: public CvVSModule
  // {
  // public:
  // CvBlobTrackPostProc(){SetTypeName("BlobTrackPostProc");};
  // virtual void    AddBlob(CvBlob* pBlob) = 0;
  // virtual void    Process() = 0;
  // virtual int     GetBlobNum() = 0;
  // virtual CvBlob* GetBlob(int index) = 0;
  // virtual void    Release() = 0;
  //
  // /* Additional functionality: */
  // virtual CvBlob* GetBlobByID(int BlobID)
  // {
  // int i;
  // for(i=GetBlobNum();i>0;i--)
  // {
  // CvBlob* pB=GetBlob(i-1);
  // if(pB->ID==BlobID) return pB;
  // }
  // return NULL;
  // };
  // };
  //
  // inline void cvReleaseBlobTrackPostProc(CvBlobTrackPostProc** pBTPP)
  // {
  // if(pBTPP == NULL) return;
  // if(*pBTPP)(*pBTPP)->Release();
  // *pBTPP = 0;
  // }
  //
  /// * Trajectory generation module: */
  // class CV_EXPORTS CvBlobTrackPostProcOne: public CvVSModule
  // {
  // public:
  // CvBlobTrackPostProcOne(){SetTypeName("BlobTrackPostOne");};
  // virtual CvBlob* Process(CvBlob* pBlob) = 0;
  // virtual void    Release() = 0;
  // };
  //
  /// * Create blob tracking post processing module based on simle module: */
  // CV_EXPORTS CvBlobTrackPostProc* cvCreateBlobTrackPostProcList(CvBlobTrackPostProcOne* (*create)());
  //
  //
  /// * Declarations of constructors of implemented modules: */
  // CV_EXPORTS CvBlobTrackPostProc* cvCreateModuleBlobTrackPostProcKalman();
  // CV_EXPORTS CvBlobTrackPostProc* cvCreateModuleBlobTrackPostProcTimeAverRect();
  // CV_EXPORTS CvBlobTrackPostProc* cvCreateModuleBlobTrackPostProcTimeAverExp();
  //
  //
  /// * PREDICTORS */
  /// * blob PREDICTOR */
  // class CvBlobTrackPredictor: public CvVSModule
  // {
  // public:
  // CvBlobTrackPredictor(){SetTypeName("BlobTrackPredictor");};
  // virtual CvBlob* Predict() = 0;
  // virtual void    Update(CvBlob* pBlob) = 0;
  // virtual void    Release() = 0;
  // };
  // CV_EXPORTS CvBlobTrackPredictor* cvCreateModuleBlobTrackPredictKalman();
  //
  //
  //
  /// * Trajectory analyser module: */
  // class CV_EXPORTS CvBlobTrackAnalysis: public CvVSModule
  // {
  // public:
  // CvBlobTrackAnalysis(){SetTypeName("BlobTrackAnalysis");};
  // virtual void    AddBlob(CvBlob* pBlob) = 0;
  // virtual void    Process(IplImage* pImg, IplImage* pFG) = 0;
  // virtual float   GetState(int BlobID) = 0;
  // /* return 0 if trajectory is normal
  // return >0 if trajectory abnormal */
  // virtual const char*   GetStateDesc(int /*BlobID*/){return NULL;};
  // virtual void    SetFileName(char* /*DataBaseName*/){};
  // virtual void    Release() = 0;
  // };
  //
  //
  // inline void cvReleaseBlobTrackAnalysis(CvBlobTrackAnalysis** pBTPP)
  // {
  // if(pBTPP == NULL) return;
  // if(*pBTPP)(*pBTPP)->Release();
  // *pBTPP = 0;
  // }
  //
  /// * Feature-vector generation module: */
  // class CV_EXPORTS CvBlobTrackFVGen : public CvVSModule
  // {
  // public:
  // CvBlobTrackFVGen(){SetTypeName("BlobTrackFVGen");};
  // virtual void    AddBlob(CvBlob* pBlob) = 0;
  // virtual void    Process(IplImage* pImg, IplImage* pFG) = 0;
  // virtual void    Release() = 0;
  // virtual int     GetFVSize() = 0;
  // virtual int     GetFVNum() = 0;
  // virtual float*  GetFV(int index, int* pFVID) = 0; /* Returns pointer to FV, if return 0 then FV not created */
  // virtual float*  GetFVVar(){return NULL;}; /* Returns pointer to array of variation of values of FV, if returns 0 then FVVar does not exist. */
  // virtual float*  GetFVMin() = 0; /* Returns pointer to array of minimal values of FV, if returns 0 then FVrange does not exist */
  // virtual float*  GetFVMax() = 0; /* Returns pointer to array of maximal values of FV, if returns 0 then FVrange does not exist */
  // };
  //
  //
  /// * Trajectory Analyser module: */
  // class CV_EXPORTS CvBlobTrackAnalysisOne
  // {
  // public:
  // virtual ~CvBlobTrackAnalysisOne() {};
  // virtual int     Process(CvBlob* pBlob, IplImage* pImg, IplImage* pFG) = 0;
  // /* return 0 if trajectory is normal
  // return >0 if trajectory abnormal */
  // virtual void    Release() = 0;
  // };
  //
  /// * Create blob tracking post processing module based on simle module: */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateBlobTrackAnalysisList(CvBlobTrackAnalysisOne* (*create)());
  //
  /// * Declarations of constructors of implemented modules: */
  //
  /// * Based on histogram analysis of 2D FV (x,y): */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisHistP();
  //
  /// * Based on histogram analysis of 4D FV (x,y,vx,vy): */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisHistPV();
  //
  /// * Based on histogram analysis of 5D FV (x,y,vx,vy,state): */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisHistPVS();
  //
  /// * Based on histogram analysis of 4D FV (startpos,stoppos): */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisHistSS();
  //
  //
  //
  /// * Based on SVM classifier analysis of 2D FV (x,y): */
  /// /CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisSVMP();
  //
  /// * Based on SVM classifier analysis of 4D FV (x,y,vx,vy): */
  /// /CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisSVMPV();
  //
  /// * Based on SVM classifier analysis of 5D FV (x,y,vx,vy,state): */
  /// /CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisSVMPVS();
  //
  /// * Based on SVM classifier analysis of 4D FV (startpos,stoppos): */
  /// /CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisSVMSS();
  //
  /// * Track analysis based on distance between tracks: */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisTrackDist();
  //
  /// * Analyzer based on reation Road and height map: */
  /// /CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysis3DRoadMap();
  //
  /// * Analyzer that makes OR decision using set of analyzers: */
  // CV_EXPORTS CvBlobTrackAnalysis* cvCreateModuleBlobTrackAnalysisIOR();
  //
  /// * Estimator of human height: */
  // class CV_EXPORTS CvBlobTrackAnalysisHeight: public CvBlobTrackAnalysis
  // {
  // public:
  // virtual double  GetHeight(CvBlob* pB) = 0;
  // };
  /// /CV_EXPORTS CvBlobTrackAnalysisHeight* cvCreateModuleBlobTrackAnalysisHeightScale();
  //
  //
  //
  /// * AUTO BLOB TRACKER INTERFACE -- pipeline of 3 modules: */
  // class CV_EXPORTS CvBlobTrackerAuto: public CvVSModule
  // {
  // public:
  // CvBlobTrackerAuto(){SetTypeName("BlobTrackerAuto");};
  // virtual void        Process(IplImage* pImg, IplImage* pMask = NULL) = 0;
  // virtual CvBlob*     GetBlob(int index) = 0;
  // virtual CvBlob*     GetBlobByID(int ID) = 0;
  // virtual int         GetBlobNum() = 0;
  // virtual IplImage*   GetFGMask(){return NULL;};
  // virtual float       GetState(int BlobID) = 0;
  // virtual const char*       GetStateDesc(int BlobID) = 0;
  // /* return 0 if trajectory is normal;
  // * return >0 if trajectory abnormal. */
  // virtual void    Release() = 0;
  // };
  // inline void cvReleaseBlobTrackerAuto(CvBlobTrackerAuto** ppT)
  // {
  // ppT[0]->Release();
  // ppT[0] = 0;
  // }
  /// * END AUTO BLOB TRACKER INTERFACE */
  //
  //
  /// * Constructor functions and data for specific BlobTRackerAuto modules: */
  //
  /// * Parameters of blobtracker auto ver1: */
  // struct CvBlobTrackerAutoParam1
  // {
  // int                     FGTrainFrames; /* Number of frames needed for FG (foreground) detector to train.        */
  //
  // CvFGDetector*           pFG;           /* FGDetector module. If this field is NULL the Process FG mask is used. */
  //
  // CvBlobDetector*         pBD;           /* Selected blob detector module. 					    */
  // /* If this field is NULL default blobdetector module will be created.    */
  //
  // CvBlobTracker*          pBT;           /* Selected blob tracking module.					    */
  // /* If this field is NULL default blobtracker module will be created.     */
  //
  // CvBlobTrackGen*         pBTGen;        /* Selected blob trajectory generator.				    */
  // /* If this field is NULL no generator is used.                           */
  //
  // CvBlobTrackPostProc*    pBTPP;         /* Selected blob trajectory postprocessing module.			    */
  // /* If this field is NULL no postprocessing is done.                      */
  //
  // int                     UsePPData;
  //
  // CvBlobTrackAnalysis*    pBTA;          /* Selected blob trajectory analysis module.                             */
  // /* If this field is NULL no track analysis is done.                      */
  // };
  //
  /// * Create blob tracker auto ver1: */
  // CV_EXPORTS CvBlobTrackerAuto* cvCreateBlobTrackerAuto1(CvBlobTrackerAutoParam1* param = NULL);
  //
  /// * Simple loader for many auto trackers by its type : */
  // inline CvBlobTrackerAuto* cvCreateBlobTrackerAuto(int type, void* param)
  // {
  // if(type == 0) return cvCreateBlobTrackerAuto1((CvBlobTrackerAutoParam1*)param);
  // return 0;
  // }
  //
  //
  //
  // struct CvTracksTimePos
  // {
  // int len1,len2;
  // int beg1,beg2;
  // int end1,end2;
  // int comLen; //common length for two tracks
  // int shift1,shift2;
  // };
  //
  /// *CV_EXPORTS int cvCompareTracks( CvBlobTrackSeq *groundTruth,
  // CvBlobTrackSeq *result,
  // FILE *file);*/
  //
  //
  /// * Constructor functions:  */
  //
  // CV_EXPORTS void cvCreateTracks_One(CvBlobTrackSeq *TS);
  // CV_EXPORTS void cvCreateTracks_Same(CvBlobTrackSeq *TS1, CvBlobTrackSeq *TS2);
  // CV_EXPORTS void cvCreateTracks_AreaErr(CvBlobTrackSeq *TS1, CvBlobTrackSeq *TS2, int addW, int addH);
  //
  //
  /// * HIST API */
  // class CV_EXPORTS CvProb
  // {
  // public:
  // virtual ~CvProb() {};
  //
  // /* Calculate probability value: */
  // virtual double Value(int* /*comp*/, int /*x*/ = 0, int /*y*/ = 0){return -1;};
  //
  // /* Update histograpp Pnew = (1-W)*Pold + W*Padd*/
  // /* W weight of new added prob */
  // /* comps - matrix of new fetature vectors used to update prob */
  // virtual void AddFeature(float W, int* comps, int x =0, int y = 0) = 0;
  // virtual void Scale(float factor = 0, int x = -1, int y = -1) = 0;
  // virtual void Release() = 0;
  // };
  // inline void cvReleaseProb(CvProb** ppProb){ppProb[0]->Release();ppProb[0]=NULL;}
  /// * HIST API */
  //
  /// * Some Prob: */
  // CV_EXPORTS CvProb* cvCreateProbS(int dim, CvSize size, int sample_num);
  // CV_EXPORTS CvProb* cvCreateProbMG(int dim, CvSize size, int sample_num);
  // CV_EXPORTS CvProb* cvCreateProbMG2(int dim, CvSize size, int sample_num);
  // CV_EXPORTS CvProb* cvCreateProbHist(int dim, CvSize size);
  //
  // #define CV_BT_HIST_TYPE_S     0
  // #define CV_BT_HIST_TYPE_MG    1
  // #define CV_BT_HIST_TYPE_MG2   2
  // #define CV_BT_HIST_TYPE_H     3
  // inline CvProb* cvCreateProb(int type, int dim, CvSize size = cvSize(1,1), void* /*param*/ = NULL)
  // {
  // if(type == CV_BT_HIST_TYPE_S) return cvCreateProbS(dim, size, -1);
  // if(type == CV_BT_HIST_TYPE_MG) return cvCreateProbMG(dim, size, -1);
  // if(type == CV_BT_HIST_TYPE_MG2) return cvCreateProbMG2(dim, size, -1);
  // if(type == CV_BT_HIST_TYPE_H) return cvCreateProbHist(dim, size);
  // return NULL;
  // }
  //
  //
  //
  /// * Noise type definitions: */
  // #define CV_NOISE_NONE               0
  // #define CV_NOISE_GAUSSIAN           1
  // #define CV_NOISE_UNIFORM            2
  // #define CV_NOISE_SPECKLE            3
  // #define CV_NOISE_SALT_AND_PEPPER    4
  //
  /// * Add some noise to image: */
  /// * pImg - (input) image without noise */
  /// * pImg - (output) image with noise */
  /// * noise_type - type of added noise */
  /// *  CV_NOISE_GAUSSIAN - pImg += n , n - is gaussian noise with Ampl standart deviation */
  /// *  CV_NOISE_UNIFORM - pImg += n , n - is uniform noise with Ampl standart deviation */
  /// *  CV_NOISE_SPECKLE - pImg += n*pImg , n - is gaussian noise with Ampl standart deviation */
  /// *  CV_NOISE_SALT_AND_PAPPER - pImg = pImg with blacked and whited pixels,
  // Ampl is density of brocken pixels (0-there are not broken pixels, 1 - all pixels are broken)*/
  /// * Ampl - "amplitude" of noise */
  /// /CV_EXPORTS void cvAddNoise(IplImage* pImg, int noise_type, double Ampl, CvRNG* rnd_state = NULL);
  //
  /// *================== GENERATOR OF TEST VIDEO SEQUENCE ===================== */
  // typedef void CvTestSeq;
  //
  /// * pConfigfile - Name of file (yml or xml) with description of test sequence */
  /// * videos - array of names of test videos described in "pConfigfile" file */
  /// * numvideos - size of "videos" array */
  // CV_EXPORTS CvTestSeq* cvCreateTestSeq(char* pConfigfile, char** videos, int numvideo, float Scale = 1, int noise_type = CV_NOISE_NONE, double noise_ampl = 0);
  // CV_EXPORTS void cvReleaseTestSeq(CvTestSeq** ppTestSeq);
  //
  /// * Generate next frame from test video seq and return pointer to it: */
  // CV_EXPORTS IplImage* cvTestSeqQueryFrame(CvTestSeq* pTestSeq);
  //
  /// * Return pointer to current foreground mask: */
  // CV_EXPORTS IplImage* cvTestSeqGetFGMask(CvTestSeq* pTestSeq);
  //
  /// * Return pointer to current image: */
  // CV_EXPORTS IplImage* cvTestSeqGetImage(CvTestSeq* pTestSeq);
  //
  /// * Return frame size of result test video: */
  // CV_EXPORTS CvSize cvTestSeqGetImageSize(CvTestSeq* pTestSeq);
  //
  /// * Return number of frames result test video: */
  // CV_EXPORTS int cvTestSeqFrameNum(CvTestSeq* pTestSeq);
  //
  /// * Return number of existing objects.
  // * This is general number of any objects.
  // * For example number of trajectories may be equal or less than returned value:
  // */
  // CV_EXPORTS int cvTestSeqGetObjectNum(CvTestSeq* pTestSeq);
  //
  /// * Return 0 if there is not position for current defined on current frame */
  /// * Return 1 if there is object position and pPos was filled */
  // CV_EXPORTS int cvTestSeqGetObjectPos(CvTestSeq* pTestSeq, int ObjIndex, CvPoint2D32f* pPos);
  // CV_EXPORTS int cvTestSeqGetObjectSize(CvTestSeq* pTestSeq, int ObjIndex, CvPoint2D32f* pSize);
  //
  /// * Add noise to final image: */
  // CV_EXPORTS void cvTestSeqAddNoise(CvTestSeq* pTestSeq, int noise_type = CV_NOISE_NONE, double noise_ampl = 0);
  //
  /// * Add Intensity variation: */
  // CV_EXPORTS void cvTestSeqAddIntensityVariation(CvTestSeq* pTestSeq, float DI_per_frame, float MinI, float MaxI);
  // CV_EXPORTS void cvTestSeqSetFrame(CvTestSeq* pTestSeq, int n);
  //
  // #endif

implementation

Uses System.SysUtils, enteringblobdetection;

{ TCvVSModule }

procedure TCvVSModule.AddParam(const name: pCvChar; pAddr: pInteger);
begin
  NewParam(name)^.pInt := pAddr;
end;

procedure TCvVSModule.AddParam(const name: pCvChar; pAddr: pCvChar);
var
  pp: pCvDefParam;
  p: pCvChar;
begin
  pp := NewParam(name);
  if Assigned(pAddr) then
    p := pAddr
  else
    p := nil;
  if Assigned(pAddr) then
    pp^.pStr := @pAddr
  else
    pp^.pStr := @pp^.Str;

  if Assigned(p) then
  begin
    pp^.Str := strdup(p);
    pp^.pStr := @pp^.Str;
  end;
end;

procedure TCvVSModule.AddParam(const name: pCvChar; pAddr: pSingle);
begin
  NewParam(name)^.pFloat := pAddr;
end;

procedure TCvVSModule.AddParam(const name: pCvChar; pAddr: pDouble);
begin
  NewParam(name)^.pDouble := pAddr;
end;

procedure TCvVSModule.AddParam(const name: pCvChar);
var
  p: pCvDefParam;
begin
  p := NewParam(name);
  p^.pDouble := @p^.Double;
end;

procedure TCvVSModule.CommentParam(const name, pComment: pCvChar);
var
  p: pCvDefParam;
begin
  if Assigned(p) then
    if Assigned(pComment) then
      p^.pComment := strdup(pComment)
    else
      p^.pComment := nil;
end;

constructor TCvVSModule.Create;
begin
  inherited;
  m_pNickName := nil;
  m_pParamList := nil;
  m_pModuleTypeName := nil;
  m_pModuleName := nil;
  m_Wnd := 0;
  AddParam('DebugWnd', pInteger(@m_Wnd));
end;

procedure TCvVSModule.DelParam(const name: pCvChar);
Var
  p: pCvDefParam;
  pPrev: pCvDefParam;
begin
  p := m_pParamList;
  pPrev := NiL;
  while Assigned(p) do
  begin
    if cv_stricmp(p^.pName, name) = 0 then
      break;
    pPrev := p;
    p := p^.next;
  end;
  if Assigned(p) then
  begin
    if Assigned(pPrev) then
    begin
      pPrev^.next := p^.next;
    end
    else
    begin
      m_pParamList := p^.next;
    end;
    FreeParam(p);
  end;
end;

destructor TCvVSModule.Destroy;
Var
  p: pCvDefParam;
  pf: pCvDefParam;
begin
  p := m_pParamList;
  while Assigned(p) do
  begin
    pf := p;
    p := p^.next;
    FreeParam(pf);
  end;
  m_pParamList := Nil;
  if Assigned(m_pModuleTypeName) then
    freemem(m_pModuleTypeName);
  if Assigned(m_pModuleName) then
    freemem(m_pModuleName);
  inherited;
end;

procedure TCvVSModule.FreeParam(var pp: pCvDefParam);
begin
  if Assigned(pp^.pName) then
    freemem(pp^.pName);
  if Assigned(pp^.pComment) then
    freemem(pp^.pComment);
  cvFree(pp);
end;

function TCvVSModule.GetModuleName: pCvChar;
begin
  Result := m_pModuleName;
end;

function TCvVSModule.GetNickName: pCvChar;
Const
  unknown: pCvChar = 'unknown';
begin
  Result := iif(Assigned(m_pNickName), m_pNickName, unknown);
end;

function TCvVSModule.GetParam(const name: pCvChar): Double;
Var
  p: pCvDefParam;
begin
  p := GetParamPtr(name);
  if Assigned(p) then
  begin
    if Assigned(p^.pDouble) then
      Exit(p^.pDouble[0]);
    if Assigned(p^.pFloat) then
      Exit(p^.pFloat[0]);
    if Assigned(p^.pInt) then
      Exit(p^.pInt[0]);
  end
  else
    Result := 0;
end;

function TCvVSModule.GetParamComment(const name: pCvChar): pCvChar;
Var
  p: pCvDefParam;
begin
  p := GetParamPtr(name);
  if Assigned(p) and Assigned(p^.pComment) then
    Exit(p^.pComment)
  else
    Exit(nil);
end;

function TCvVSModule.GetParamName(index: Integer): pCvChar;
Var
  p: pCvDefParam;
begin
  p := GetParamPtr(index);
  Result := iif(Assigned(p), p^.pName, nil);
end;

function TCvVSModule.GetParamPtr(index: Integer): pCvDefParam;
Var
  p: pCvDefParam;
begin
  p := m_pParamList;
  while (index > 0) and Assigned(p) do
  begin
    Dec(index);
    p := p^.next;
  end;
  Result := p;
end;

function TCvVSModule.GetParamPtr(const name: pCvChar): pCvDefParam;
Var
  p: pCvDefParam;
begin
  p := m_pParamList;
  while Assigned(p) do
  begin
    if cv_stricmp(p^.pName, name) = 0 then
      break;
    p := p^.next;
  end;
  Result := p;
end;

function TCvVSModule.GetParamStr(const name: pCvChar): pCvChar;
Var
  p: pCvDefParam;
begin
  p := GetParamPtr(name);
  Result := iif(Assigned(p), p^.Str, nil);
end;

function TCvVSModule.GetTypeName: pCvChar;
begin
  Result := m_pModuleTypeName;
end;

function TCvVSModule.IsModuleName(const name: pCvChar): Integer;
begin
  Result := iif(Assigned(m_pModuleName), (cv_stricmp(m_pModuleName, name) = 0), 0);
end;

function TCvVSModule.IsModuleTypeName(const name: pCvChar): Integer;
begin
  Result := iif(Assigned(m_pModuleTypeName), (cv_stricmp(m_pModuleTypeName, name) = 0), 0);
end;

function TCvVSModule.IsParam(const name: pCvChar): Integer;
begin
  Result := iif(Assigned(GetParamPtr(name)), 1, 0);
end;

procedure TCvVSModule.LoadState(FileStorage: pCvFileStorage; FileNode: pCvFileNode);
begin

end;

function TCvVSModule.NewParam(const name: pCvChar): pCvDefParam;
Var
  pNew: pCvDefParam;
  p: pCvDefParam;
begin
  pNew := { (CvDefParam*) } cvAlloc(sizeof(TCvDefParam));
  ZeroMemory(pNew, sizeof(TCvDefParam));
  pNew^.pName := strdup(name);
  if (m_pParamList = NiL) then
    m_pParamList := pNew
  else
  begin
    p := m_pParamList;
    while Assigned(p^.next) do
    begin
      p^.next := pNew;
      p := p^.next;
    end;
  end;
  Result := pNew;
end;

procedure TCvVSModule.ParamUpdate;
begin

end;

procedure TCvVSModule.SaveState(FileStorage: pCvFileStorage);
begin

end;

procedure TCvVSModule.SetModuleName(const name: pCvChar);
begin
  m_pModuleName := strdup(name);
end;

procedure TCvVSModule.SetNickName(const pStr: pCvChar);
begin
  if Assigned(m_pNickName) then
    freemem(m_pNickName);
  m_pNickName := NiL;
  if Assigned(pStr) then
    m_pNickName := strdup(pStr);
end;

procedure TCvVSModule.SetParam(const name: pCvChar; val: Double);
var
  p: pCvDefParam;
begin
  p := m_pParamList;
  while Assigned(p) do
  begin
    if (cv_stricmp(p^.pName, name) <> 0) then
      continue;
    if Assigned(p^.pDouble) then
      p^.pDouble[0] := val;
    if Assigned(p^.pFloat) then
      p^.pFloat[0] := val;
    if Assigned(p^.pInt) then
      p^.pInt[0] := cvRound(val);

    p := p^.next
  end;
end;

procedure TCvVSModule.SetParamStr(const name, Str: pCvChar);
var
  p: pCvDefParam;
begin
  p := m_pParamList;
  while Assigned(p) do
  begin
    if (cv_stricmp(p^.pName, name) <> 0) then
      continue;
    if Assigned(p^.pStr) then
    begin
      if Assigned(p^.Str) then
        freemem(p^.Str);
      p^.Str := NiL;
      if Assigned(Str) then
        p^.Str := strdup(Str);
      p^.pStr[0] := p^.Str;
    end;
    p := p^.next;
  end;
  // * Convert to double and set: */
  if Assigned(Str) then
    SetParam(name, StrToFloat(Str));
end;

procedure TCvVSModule.SetTypeName(const name: pCvChar);
begin
  m_pModuleTypeName := strdup(name);
end;

procedure TCvVSModule.TransferParamsFromChild(pM: TCvVSModule; const prefix: pCvChar);
Var
  tmp: pCvChar;
  FN: pCvChar;
  i: Integer;
  N: pCvChar;
  val: pCvChar;
  dval: Double;
begin
  FN := Nil;
  i := 0;
  while True do
  begin
    N := pM.GetParamName(i);
    if (N = nil) then
      break;
    FN := N;
    if Assigned(prefix) then
    begin
      strcpy(tmp, prefix);
      strcat(tmp, '_');
      FN := strcat(tmp, N);
    end;

    if IsParam(FN) = 0 then
    begin
      if Assigned(pM.GetParamStr(N)) then
      begin
        AddParam(FN, pCvChar(nil));
      end
      else
      begin
        AddParam(FN);
      end;
    end;
    if Assigned(pM.GetParamStr(N)) then
    begin
      val := pM.GetParamStr(N);
      SetParamStr(FN, val);
    end
    else
    begin
      dval := pM.GetParam(N);
      SetParam(FN, dval);
    end;
    CommentParam(FN, pM.GetParamComment(N));
    inc(i);
  end; // * transfer next param */
end;

procedure TCvVSModule.TransferParamsToChild(pM: TCvVSModule; prefix: pCvChar);
Var
  tmp: pCvChar;
  i: Integer;
  N: pCvChar;
begin
  i := 0;
  while True do
  begin
    N := pM.GetParamName(i);
    if (N = nil) then
      break;
    if Assigned(prefix) then
    begin
      strcpy(tmp, prefix);
      strcat(tmp, '_');
      strcat(tmp, N);
    end
    else
    begin
      strcpy(tmp, N);
    end;

    if IsParam(tmp) <> 0 then
    begin
      if Assigned(GetParamStr(tmp)) then
        pM.SetParamStr(N, GetParamStr(tmp))
      else
        pM.SetParam(N, GetParam(tmp));
    end;
    inc(i);
  end; // * Transfer next parameter */
  pM.ParamUpdate();
end;

{ TCvFGDetector }

constructor TCvFGDetector.Create;
begin
  inherited;
  SetTypeName('FGDetector');
end;

{ TCvBlobDetector }

constructor TCvBlobDetector.Craete;
begin
  SetTypeName('BlobDetector');
end;

{ TCvBlobSeq }

procedure TCvBlobSeq.AddBlob(pB: pCvBlob);
begin
  cvSeqPush(m_pSeq, pB);
end;

procedure TCvBlobSeq.AddFormat(const Str: pCvChar);
begin
  strcat(m_pElemFormat, Str);
end;

procedure TCvBlobSeq.Clear;
begin
  cvClearSeq(m_pSeq);
end;

constructor TCvBlobSeq.Create(BlobSize: Integer);
begin
  m_pMem := cvCreateMemStorage();
  m_pSeq := cvCreateSeq(0, sizeof(TCvSeq), BlobSize, m_pMem);
  m_pElemFormat := 'ffffi';
end;

procedure TCvBlobSeq.DelBlob(BlobIndex: Integer);
begin
  cvSeqRemove(m_pSeq, BlobIndex);
end;

procedure TCvBlobSeq.DelBlobByID(BlobID: Integer);
Var
  i: Integer;
begin
  for i := 0 to m_pSeq^.total - 1 do
    if (BlobID = CV_BLOB_ID(GetBlob(i))) then
    begin
      DelBlob(i);
      Exit;
    end;
end;

destructor TCvBlobSeq.Destroy;
begin
  cvReleaseMemStorage(m_pMem);
  inherited;
end;

function TCvBlobSeq.GetBlob(BlobIndex: Integer): pCvBlob;
begin
  Result := pCvBlob(cvGetSeqElem(m_pSeq, BlobIndex));
end;

function TCvBlobSeq.GetBlobByID(BlobID: Integer): pCvBlob;
Var
  i: Integer;
begin
  for i := 0 to m_pSeq^.total - 1 do
    if (BlobID = CV_BLOB_ID(GetBlob(i))) then
      Exit(GetBlob(i));
  Result := Nil;
end;

function TCvBlobSeq.GetBlobNum: Integer;
begin
  Result := m_pSeq^.total;
end;

procedure TCvBlobSeq.Load(fs: pCvFileStorage; node: pCvFileNode);
Var
  pSeq: pCvSeq;
  i: Integer;
  pB: Pointer;
begin
  if (fs = NiL) then
    Exit;
  pSeq := pCvSeq(cvRead(fs, node));
  if Assigned(pSeq) then
  begin
    cvClearSeq(m_pSeq);
    for i := 0 to pSeq^.total - 1 do
    begin
      pB := cvGetSeqElem(pSeq, i);
      cvSeqPush(m_pSeq, pB);
    end
  end;
end;

procedure TCvBlobSeq.Write(fs: pCvFileStorage; const name: pCvChar);
Var
  attr: array [0 .. 2] of pCvChar;
begin
  attr[0] := 'dt';
  strcpy(attr[1], m_pElemFormat);
  attr[2] := nil;
  if Assigned(fs) then
    cvWrite(fs, name, m_pSeq, cvAttrList(@attr, NiL));
end;

function CV_BLOB_ID(pB: pCvBlob): Integer; inline;
begin
  Result := pB^.ID;
end;

{ TCvObjectDetector }

constructor TCvObjectDetector.Create(const detector_file_name: pCvChar);
begin

end;

destructor TCvObjectDetector.Destroy;
begin

  inherited;
end;

procedure TCvObjectDetector.Detect(const img: pCvArr; out detected_blob_seq: TCvBlobSeq);
begin

end;

function TCvObjectDetector.GetMaxBorderSize: Integer;
begin
  Result := 0;
end;

function TCvObjectDetector.GetMinWindowSize: TCvSize;
begin
  Result := cvSize(0, 0);
end;

function TCvObjectDetector.Load(const detector_file_name: pCvChar): boolean;
begin
  Result := false;
end;

function cvBlob(x, y, w, h: Single): TCvBlob; inline;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

function CV_BLOB_RX(pB: pCvBlob): Single; inline;
begin
  Result := 0.5 * CV_BLOB_WX(pB);
end;

function CV_BLOB_WX(pB: pCvBlob): Single; inline;
begin
  Result := pB^.w;
end;

function CV_BLOB_Y(pB: pCvBlob): Single; inline;
begin
  Result := pB^.y;
end;

function CV_BLOB_WY(pB: pCvBlob): Single; inline;
begin
  Result := pB^.h;
end;

function CV_BLOB_RY(pB: pCvBlob): Single; inline;
begin
  Result := 0.5 * CV_BLOB_WY(pB);
end;

function CV_BLOB_X(pB: pCvBlob): Single; inline;
begin
  Result := pB^.x;
end;

function cvCreateBlobDetectorCC(): TCvBlobDetector;
begin
  Result := TCvBlobDetectorCC.Create;
end;

function cvCreateBlobDetectorSimple(): TCvBlobDetector;
begin
  Result := TCvBlobDetectorSimple.Create;
end;

end.
