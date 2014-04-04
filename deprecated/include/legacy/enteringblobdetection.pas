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
  //  opencv\modules\legacy\src\enteringblobdetection.cpp
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
{$TYPEDADDRESS ON}
unit enteringblobdetection;

interface

Uses Windows, core_c, core.types_c, imgproc_c, blobtrack;

// static int CompareContour(const void* a, const void* b, void* )
// {
// float           dx,dy;
// float           h,w,ht,wt;
// CvPoint2D32f    pa,pb;
// CvRect          ra,rb;
// CvSeq*          pCA = *(CvSeq**)a;
// CvSeq*          pCB = *(CvSeq**)b;
// ra = ((CvContour*)pCA)->rect;
// rb = ((CvContour*)pCB)->rect;
// pa.x = ra.x + ra.width*0.5f;
// pa.y = ra.y + ra.height*0.5f;
// pb.x = rb.x + rb.width*0.5f;
// pb.y = rb.y + rb.height*0.5f;
// w = (ra.width+rb.width)*0.5f;
// h = (ra.height+rb.height)*0.5f;
//
// dx = (float)(fabs(pa.x - pb.x)-w);
// dy = (float)(fabs(pa.y - pb.y)-h);
//
// //wt = MAX(ra.width,rb.width)*0.1f;
// wt = 0;
// ht = MAX(ra.height,rb.height)*0.3f;
// return (dx < wt && dy < ht);
// }
//
procedure cvFindBlobsByCCClasters(pFG: pIplImage; pBlobs: TCvBlobSeq; storage: pCvMemStorage);

const
  /// * Simple blob detector.  */
  /// * Number of successive frame to analyse: */
  EBD_FRAME_NUM = 5;

type
  TCvBlobDetectorSimple = class(TCvBlobDetector)
  public
    constructor Create;
    destructor Destroy; override;
    function DetectNewBlob(pImg: pIplImage; pFGMask: pIplImage; pNewBlobList: TCvBlobSeq; pOldBlobList: TCvBlobSeq)
      : Integer; override;
    procedure Release(); override;
    { delete this; }
  protected
    m_pMaskBlobNew: pIplImage;
    m_pMaskBlobExist: pIplImage;
    // * Lists of connected components detected on previous frames: */
    m_pBlobLists: array [0 .. EBD_FRAME_NUM - 1] of TCvBlobSeq;
  end;

/// * Constructor of BlobDetector: */
// CvBlobDetectorSimple::CvBlobDetectorSimple()
// {
// int i = 0;
// m_pMaskBlobNew = NULL;
// m_pMaskBlobExist = NULL;
// for(i=0;i<EBD_FRAME_NUM;++i)m_pBlobLists[i] = NULL;
//
// SetModuleName("Simple");
// }
//
/// * Destructor of BlobDetector: */
// CvBlobDetectorSimple::~CvBlobDetectorSimple()
// {
// int i;
// if(m_pMaskBlobExist) cvReleaseImage(&m_pMaskBlobExist);
// if(m_pMaskBlobNew) cvReleaseImage(&m_pMaskBlobNew);
// for(i=0; i<EBD_FRAME_NUM; ++i) delete m_pBlobLists[i];
// }   /* cvReleaseBlobDetector */
//
/// * cvDetectNewBlobs
// * return 1 and fill blob pNewBlob by blob parameters
// * if new blob is detected:
// */
// int CvBlobDetectorSimple::DetectNewBlob(IplImage* /*pImg*/, IplImage* pFGMask, CvBlobSeq* pNewBlobList, CvBlobSeq* pOldBlobList)
// {
// int         result = 0;
// CvSize      S = cvSize(pFGMask->width,pFGMask->height);
// if(m_pMaskBlobNew == NULL ) m_pMaskBlobNew = cvCreateImage(S,IPL_DEPTH_8U,1);
// if(m_pMaskBlobExist == NULL ) m_pMaskBlobExist = cvCreateImage(S,IPL_DEPTH_8U,1);
//
// /* Shift blob list: */
// {
// int     i;
// if(m_pBlobLists[0]) delete m_pBlobLists[0];
// for(i=1;i<EBD_FRAME_NUM;++i)m_pBlobLists[i-1]=m_pBlobLists[i];
// m_pBlobLists[EBD_FRAME_NUM-1] = new CvBlobSeq;
// }   /* Shift blob list. */
//
// /* Create exist blob mask: */
// cvCopy(pFGMask, m_pMaskBlobNew);
//
// /* Create contours and add new blobs to blob list: */
// {   /* Create blobs: */
// CvBlobSeq       Blobs;
// CvMemStorage*   storage = cvCreateMemStorage();
//
// #if 1
// {   /* Glue contours: */
// cvFindBlobsByCCClasters(m_pMaskBlobNew, &Blobs, storage );
// }   /* Glue contours. */
// #else
// {   /**/
// IplImage*       pIB = cvCloneImage(m_pMaskBlobNew);
// CvSeq*          cnts = NULL;
// CvSeq*          cnt = NULL;
// cvThreshold(pIB,pIB,128,255,CV_THRESH_BINARY);
// cvFindContours(pIB,storage, &cnts, sizeof(CvContour), CV_RETR_EXTERNAL);
//
// /* Process each contour: */
// for(cnt = cnts; cnt; cnt=cnt->h_next)
// {
// CvBlob  NewBlob;
//
// /* Image moments: */
// double      M00,X,Y,XX,YY;
// CvMoments   m;
// CvRect      r = ((CvContour*)cnt)->rect;
// CvMat       mat;
//
// if(r.height < S.height*0.02 || r.width < S.width*0.02) continue;
//
// cvMoments( cvGetSubRect(m_pMaskBlobNew,&mat,r), &m, 0 );
// M00 = cvGetSpatialMoment( &m, 0, 0 );
//
// if(M00 <= 0 ) continue;
//
// X  = cvGetSpatialMoment( &m, 1, 0 )/M00;
// Y  = cvGetSpatialMoment( &m, 0, 1 )/M00;
//
// XX = (cvGetSpatialMoment( &m, 2, 0 )/M00) - X*X;
// YY = (cvGetSpatialMoment( &m, 0, 2 )/M00) - Y*Y;
//
// NewBlob = cvBlob(r.x+(float)X,r.y+(float)Y,(float)(4*sqrt(XX)),(float)(4*sqrt(YY)));
//
// Blobs.AddBlob(&NewBlob);
//
// }   /* Next contour. */
//
// cvReleaseImage(&pIB);
//
// }   /* One contour - one blob. */
// #endif
//
// {   /* Delete small and intersected blobs: */
// int i;
// for(i=Blobs.GetBlobNum(); i>0; i--)
// {
// CvBlob* pB = Blobs.GetBlob(i-1);
//
// if(pB->h < S.height*0.02 || pB->w < S.width*0.02)
// {
// Blobs.DelBlob(i-1);
// continue;
// }
// if(pOldBlobList)
// {
// int j;
// for(j=pOldBlobList->GetBlobNum(); j>0; j--)
// {
// CvBlob* pBOld = pOldBlobList->GetBlob(j-1);
// if((fabs(pBOld->x-pB->x) < (CV_BLOB_RX(pBOld)+CV_BLOB_RX(pB))) &&
// (fabs(pBOld->y-pB->y) < (CV_BLOB_RY(pBOld)+CV_BLOB_RY(pB))))
// {   /* Intersection is present, so delete blob from list: */
// Blobs.DelBlob(i-1);
// break;
// }
// }   /* Check next old blob. */
// }   /*  if pOldBlobList */
// }   /* Check next blob. */
// }   /*  Delete small and intersected blobs. */
//
// {   /* Bubble-sort blobs by size: */
// int N = Blobs.GetBlobNum();
// int i,j;
// for(i=1; i<N; ++i)
// {
// for(j=i; j>0; --j)
// {
// CvBlob  temp;
// float   AreaP, AreaN;
// CvBlob* pP = Blobs.GetBlob(j-1);
// CvBlob* pN = Blobs.GetBlob(j);
// AreaP = CV_BLOB_WX(pP)*CV_BLOB_WY(pP);
// AreaN = CV_BLOB_WX(pN)*CV_BLOB_WY(pN);
// if(AreaN < AreaP)break;
// temp = pN[0];
// pN[0] = pP[0];
// pP[0] = temp;
// }
// }
//
// /* Copy only first 10 blobs: */
// for(i=0; i<MIN(N,10); ++i)
// {
// m_pBlobLists[EBD_FRAME_NUM-1]->AddBlob(Blobs.GetBlob(i));
// }
//
// }   /* Sort blobs by size. */
//
// cvReleaseMemStorage(&storage);
//
// }   /* Create blobs. */
//
// /* Analyze blob list to find best blob trajectory: */
// {
// int     Count = 0;
// int     pBLIndex[EBD_FRAME_NUM];
// int     pBL_BEST[EBD_FRAME_NUM];
// int     i;
// int     finish = 0;
// double  BestError = -1;
// int     Good = 1;
//
// for(i=0; i<EBD_FRAME_NUM; ++i)
// {
// pBLIndex[i] = 0;
// pBL_BEST[i] = 0;
// }
//
// /* Check configuration exist: */
// for(i=0; Good && (i<EBD_FRAME_NUM); ++i)
// if(m_pBlobLists[i] == NULL || m_pBlobLists[i]->GetBlobNum() == 0)
// Good = 0;
//
// if(Good)
// do{ /* For each configuration: */
// CvBlob* pBL[EBD_FRAME_NUM];
// int     good = 1;
// double  Error = 0;
// CvBlob* pBNew = m_pBlobLists[EBD_FRAME_NUM-1]->GetBlob(pBLIndex[EBD_FRAME_NUM-1]);
//
// for(i=0; i<EBD_FRAME_NUM; ++i)  pBL[i] = m_pBlobLists[i]->GetBlob(pBLIndex[i]);
//
// Count++;
//
// /* Check intersection last blob with existed: */
// if(good && pOldBlobList)
// {   /* Check intersection last blob with existed: */
// int     k;
// for(k=pOldBlobList->GetBlobNum(); k>0; --k)
// {
// CvBlob* pBOld = pOldBlobList->GetBlob(k-1);
// if((fabs(pBOld->x-pBNew->x) < (CV_BLOB_RX(pBOld)+CV_BLOB_RX(pBNew))) &&
// (fabs(pBOld->y-pBNew->y) < (CV_BLOB_RY(pBOld)+CV_BLOB_RY(pBNew))))
// good = 0;
// }
// }   /* Check intersection last blob with existed. */
//
// /* Check distance to image border: */
// if(good)
// {   /* Check distance to image border: */
// CvBlob*  pB = pBNew;
// float    dx = MIN(pB->x,S.width-pB->x)/CV_BLOB_RX(pB);
// float    dy = MIN(pB->y,S.height-pB->y)/CV_BLOB_RY(pB);
//
// if(dx < 1.1 || dy < 1.1) good = 0;
// }   /* Check distance to image border. */
//
// /* Check uniform motion: */
// if(good)
// {
// int     N = EBD_FRAME_NUM;
// float   sum[2] = {0,0};
// float   jsum[2] = {0,0};
// float   a[2],b[2]; /* estimated parameters of moving x(t) = a*t+b*/
//
// int j;
// for(j=0; j<N; ++j)
// {
// float   x = pBL[j]->x;
// float   y = pBL[j]->y;
// sum[0] += x;
// jsum[0] += j*x;
// sum[1] += y;
// jsum[1] += j*y;
// }
//
// a[0] = 6*((1-N)*sum[0]+2*jsum[0])/(N*(N*N-1));
// b[0] = -2*((1-2*N)*sum[0]+3*jsum[0])/(N*(N+1));
// a[1] = 6*((1-N)*sum[1]+2*jsum[1])/(N*(N*N-1));
// b[1] = -2*((1-2*N)*sum[1]+3*jsum[1])/(N*(N+1));
//
// for(j=0; j<N; ++j)
// {
// Error +=
// pow(a[0]*j+b[0]-pBL[j]->x,2)+
// pow(a[1]*j+b[1]-pBL[j]->y,2);
// }
//
// Error = sqrt(Error/N);
//
// if( Error > S.width*0.01 ||
// fabs(a[0])>S.width*0.1 ||
// fabs(a[1])>S.height*0.1)
// good = 0;
//
// }   /* Check configuration. */
//
//
// /* New best trajectory: */
// if(good && (BestError == -1 || BestError > Error))
// {
// for(i=0; i<EBD_FRAME_NUM; ++i)
// {
// pBL_BEST[i] = pBLIndex[i];
// }
// BestError = Error;
// }   /* New best trajectory. */
//
// /* Set next configuration: */
// for(i=0; i<EBD_FRAME_NUM; ++i)
// {
// pBLIndex[i]++;
// if(pBLIndex[i] != m_pBlobLists[i]->GetBlobNum()) break;
// pBLIndex[i]=0;
// }   /* Next time shift. */
//
// if(i==EBD_FRAME_NUM)finish=1;
//
// } while(!finish);   /* Check next time configuration of connected components. */
//
// #if 0
// {/**/
// printf("BlobDetector configurations = %d [",Count);
// int i;
// for(i=0; i<EBD_FRAME_NUM; ++i)
// {
// printf("%d,",m_pBlobLists[i]?m_pBlobLists[i]->GetBlobNum():0);
// }
// printf("]\n");
//
// }
// #endif
//
// if(BestError != -1)
// {   /* Write new blob to output and delete from blob list: */
// CvBlob* pNewBlob = m_pBlobLists[EBD_FRAME_NUM-1]->GetBlob(pBL_BEST[EBD_FRAME_NUM-1]);
// pNewBlobList->AddBlob(pNewBlob);
//
// for(i=0; i<EBD_FRAME_NUM; ++i)
// {   /* Remove blob from each list: */
// m_pBlobLists[i]->DelBlob(pBL_BEST[i]);
// }   /* Remove blob from each list. */
//
// result = 1;
//
// }   /* Write new blob to output and delete from blob list. */
// }   /*  Analyze blob list to find best blob trajectory. */
//
// return result;
//
// }   /* cvDetectNewBlob */

const
  /// * Simple blob detector2.  */
  /// * Number of successive frames to analyse: */
  SEQ_SIZE_MAX = 30;
  SEQ_NUM = 1000;

Type
  pDefSeq = ^TDefSeq;

  TDefSeq = packed record
    size: Integer;
    pBlobs: array [0 .. SEQ_SIZE_MAX - 1] of pCvBlob;
  end;

Type
  TCvBlobDetectorCC = class(TCvBlobDetector)
  public
    constructor Create;
    destructor Destroy; override;
    function DetectNewBlob(pImg: pIplImage; pFGMask: pIplImage; pNewBlobList: TCvBlobSeq; pOldBlobList: TCvBlobSeq)
      : Integer; override;
    procedure Release(); override;
    { delete this; }
    procedure ParamUpdate(); override;
    // {
    // if(SEQ_SIZE<1)SEQ_SIZE=1;
    // if(SEQ_SIZE>SEQ_SIZE_MAX)SEQ_SIZE=SEQ_SIZE_MAX;
    //
    // #ifdef USE_OBJECT_DETECTOR
    // if( m_param_split_detector_file_name )
    // {
    // m_split_detector = new CvObjectDetector();
    // if( !m_split_detector->Load( m_param_split_detector_file_name ) )
    // {
    // delete m_split_detector;
    // m_split_detector = 0;
    // }
    // else
    // {
    // m_min_window_size = m_split_detector->GetMinWindowSize();
    // m_max_border = m_split_detector->GetMaxBorderSize();
    // }
    // }
    // #endif
    // }
  private
    // * Lists of connected components detected on previous frames: */
    m_pBlobLists: array [0 .. SEQ_SIZE_MAX - 1] of TCvBlobSeq;
    m_TrackSeq: array [0 .. SEQ_NUM - 1] of TDefSeq;
    m_TrackNum: Integer;
    m_HMin: Single;
    m_WMin: Single;
    m_MinDistToBorder: Single;
    m_Clastering: Integer;
    SEQ_SIZE: Integer;
    // /* If not 0 then the detector is loaded from the specified file
    // * and it is applied for splitting blobs which actually correspond
    // * to groups of objects:
    // */
    m_param_split_detector_file_name: pCvChar;
    m_param_roi_scale: Single;
    m_param_only_roi: Integer;
    m_split_detector: TCvObjectDetector;
    m_min_window_size: TCvSize;
    m_max_border: Integer;

    m_detected_blob_seq: TCvBlobSeq;
    m_roi_seq: pCvSeq;

    m_debug_blob_seq: TCvBlobSeq;
  end;

/// * Constructor for BlobDetector: */
// CvBlobDetectorCC::CvBlobDetectorCC() :
// m_split_detector(0),
// m_detected_blob_seq(sizeof(CvDetectedBlob)),
// m_roi_seq(0),
// m_debug_blob_seq(sizeof(CvDetectedBlob))
// {
// /*CvDrawShape shapes[] =
// {
// { CvDrawShape::RECT,    {{255,255,255}} },
// { CvDrawShape::RECT,    {{0,0,255}} },
// { CvDrawShape::ELLIPSE, {{0,255,0}} }
// };
// int num_shapes = sizeof(shapes) / sizeof(shapes[0]);*/
//
// int i = 0;
// SEQ_SIZE = 10;
// AddParam("Latency",&SEQ_SIZE);
// for(i=0;i<SEQ_SIZE_MAX;++i)m_pBlobLists[i] = NULL;
// for(i=0;i<SEQ_NUM;++i)m_TrackSeq[i].size = 0;
// m_TrackNum = 0;
//
// m_HMin = 0.02f;
// m_WMin = 0.01f;
// AddParam("HMin",&m_HMin);
// AddParam("WMin",&m_WMin);
// m_MinDistToBorder = 1.1f;
// AddParam("MinDistToBorder",&m_MinDistToBorder);
// CommentParam("MinDistToBorder","Minimal allowed distance from blob center to image border in blob sizes");
//
// m_Clastering=1;
// AddParam("Clastering",&m_Clastering);
// CommentParam("Clastering","Minimal allowed distance from blob center to image border in blob sizes");
//
// m_param_split_detector_file_name = 0;
// #ifdef USE_OBJECT_DETECTOR
// AddParam("Detector", &m_param_split_detector_file_name);
// CommentParam("Detector", "Detector file name");
// #endif
//
// m_param_roi_scale = 1.5F;
// AddParam("ROIScale", &m_param_roi_scale);
// CommentParam("ROIScale", "Determines the size of search window around a blob");
//
// m_param_only_roi = 1;
// AddParam("OnlyROI", &m_param_only_roi);
// CommentParam("OnlyROI", "Shows the whole debug image (0) or only ROIs where the detector was applied (1)");
//
// m_min_window_size = cvSize(0,0);
// m_max_border = 0;
// m_roi_seq = cvCreateSeq( 0, sizeof(*m_roi_seq), sizeof(CvRect), cvCreateMemStorage() );
//
// SetModuleName("CC");
// }
//
/// * Destructor for BlobDetector: */
// CvBlobDetectorCC::~CvBlobDetectorCC()
// {
// int i;
// for(i=0; i<SEQ_SIZE_MAX; ++i)
// {
// if(m_pBlobLists[i])
// delete m_pBlobLists[i];
// }
//
// if( m_roi_seq )
// {
// cvReleaseMemStorage( &m_roi_seq->storage );
// m_roi_seq = 0;
// }
// //cvDestroyWindow( "EnteringBlobDetectionDebug" );
// }   /* cvReleaseBlobDetector */
//
//
/// * cvDetectNewBlobs
// * Return 1 and fill blob pNewBlob  with
// * blob parameters if new blob is detected:
// */
// int CvBlobDetectorCC::DetectNewBlob(IplImage* /*pImg*/, IplImage* pFGMask, CvBlobSeq* pNewBlobList, CvBlobSeq* pOldBlobList)
// {
// int         result = 0;
// CvSize      S = cvSize(pFGMask->width,pFGMask->height);
//
// /* Shift blob list: */
// {
// int     i;
// if(m_pBlobLists[SEQ_SIZE-1]) delete m_pBlobLists[SEQ_SIZE-1];
//
// for(i=SEQ_SIZE-1; i>0; --i)  m_pBlobLists[i] = m_pBlobLists[i-1];
//
// m_pBlobLists[0] = new CvBlobSeq;
//
// }   /* Shift blob list. */
//
// /* Create contours and add new blobs to blob list: */
// {   /* Create blobs: */
// CvBlobSeq       Blobs;
// CvMemStorage*   storage = cvCreateMemStorage();
//
// if(m_Clastering)
// {   /* Glue contours: */
// cvFindBlobsByCCClasters(pFGMask, &Blobs, storage );
// }   /* Glue contours. */
// else
// { /**/
// IplImage*       pIB = cvCloneImage(pFGMask);
// CvSeq*          cnts = NULL;
// CvSeq*          cnt = NULL;
// cvThreshold(pIB,pIB,128,255,CV_THRESH_BINARY);
// cvFindContours(pIB,storage, &cnts, sizeof(CvContour), CV_RETR_EXTERNAL);
//
// /* Process each contour: */
// for(cnt = cnts; cnt; cnt=cnt->h_next)
// {
// CvBlob  NewBlob;
// /* Image moments: */
// double      M00,X,Y,XX,YY;
// CvMoments   m;
// CvRect      r = ((CvContour*)cnt)->rect;
// CvMat       mat;
// if(r.height < S.height*m_HMin || r.width < S.width*m_WMin) continue;
// cvMoments( cvGetSubRect(pFGMask,&mat,r), &m, 0 );
// M00 = cvGetSpatialMoment( &m, 0, 0 );
// if(M00 <= 0 ) continue;
// X = cvGetSpatialMoment( &m, 1, 0 )/M00;
// Y = cvGetSpatialMoment( &m, 0, 1 )/M00;
// XX = (cvGetSpatialMoment( &m, 2, 0 )/M00) - X*X;
// YY = (cvGetSpatialMoment( &m, 0, 2 )/M00) - Y*Y;
// NewBlob = cvBlob(r.x+(float)X,r.y+(float)Y,(float)(4*sqrt(XX)),(float)(4*sqrt(YY)));
// Blobs.AddBlob(&NewBlob);
//
// }   /* Next contour. */
//
// cvReleaseImage(&pIB);
//
// }   /* One contour - one blob. */
//
// {   /* Delete small and intersected blobs: */
// int i;
// for(i=Blobs.GetBlobNum(); i>0; i--)
// {
// CvBlob* pB = Blobs.GetBlob(i-1);
//
// if(pB->h < S.height*m_HMin || pB->w < S.width*m_WMin)
// {
// Blobs.DelBlob(i-1);
// continue;
// }
//
// if(pOldBlobList)
// {
// int j;
// for(j=pOldBlobList->GetBlobNum(); j>0; j--)
// {
// CvBlob* pBOld = pOldBlobList->GetBlob(j-1);
// if((fabs(pBOld->x-pB->x) < (CV_BLOB_RX(pBOld)+CV_BLOB_RX(pB))) &&
// (fabs(pBOld->y-pB->y) < (CV_BLOB_RY(pBOld)+CV_BLOB_RY(pB))))
// {   /* Intersection detected, delete blob from list: */
// Blobs.DelBlob(i-1);
// break;
// }
// }   /* Check next old blob. */
// }   /*  if pOldBlobList. */
// }   /*  Check next blob. */
// }   /*  Delete small and intersected blobs. */
//
// {   /* Bubble-sort blobs by size: */
// int N = Blobs.GetBlobNum();
// int i,j;
// for(i=1; i<N; ++i)
// {
// for(j=i; j>0; --j)
// {
// CvBlob  temp;
// float   AreaP, AreaN;
// CvBlob* pP = Blobs.GetBlob(j-1);
// CvBlob* pN = Blobs.GetBlob(j);
// AreaP = CV_BLOB_WX(pP)*CV_BLOB_WY(pP);
// AreaN = CV_BLOB_WX(pN)*CV_BLOB_WY(pN);
// if(AreaN < AreaP)break;
// temp = pN[0];
// pN[0] = pP[0];
// pP[0] = temp;
// }
// }
//
// /* Copy only first 10 blobs: */
// for(i=0; i<MIN(N,10); ++i)
// {
// m_pBlobLists[0]->AddBlob(Blobs.GetBlob(i));
// }
//
// }   /* Sort blobs by size. */
//
// cvReleaseMemStorage(&storage);
//
// }   /* Create blobs. */
//
// {   /* Shift each track: */
// int j;
// for(j=0; j<m_TrackNum; ++j)
// {
// int     i;
// DefSeq* pTrack = m_TrackSeq+j;
//
// for(i=SEQ_SIZE-1; i>0; --i)
// pTrack->pBlobs[i] = pTrack->pBlobs[i-1];
//
// pTrack->pBlobs[0] = NULL;
// if(pTrack->size == SEQ_SIZE)pTrack->size--;
// }
// }   /* Shift each track. */
//
// /* Analyze blob list to find best blob trajectory: */
// {
// double      BestError = -1;
// int         BestTrack = -1;;
// CvBlobSeq*  pNewBlobs = m_pBlobLists[0];
// int         i;
// int         NewTrackNum = 0;
// for(i=pNewBlobs->GetBlobNum(); i>0; --i)
// {
// CvBlob* pBNew = pNewBlobs->GetBlob(i-1);
// int     j;
// int     AsignedTrack = 0;
// for(j=0; j<m_TrackNum; ++j)
// {
// double  dx,dy;
// DefSeq* pTrack = m_TrackSeq+j;
// CvBlob* pLastBlob = pTrack->size>0?pTrack->pBlobs[1]:NULL;
// if(pLastBlob == NULL) continue;
// dx = fabs(CV_BLOB_X(pLastBlob)-CV_BLOB_X(pBNew));
// dy = fabs(CV_BLOB_Y(pLastBlob)-CV_BLOB_Y(pBNew));
// if(dx > 2*CV_BLOB_WX(pLastBlob) || dy > 2*CV_BLOB_WY(pLastBlob)) continue;
// AsignedTrack++;
//
// if(pTrack->pBlobs[0]==NULL)
// {   /* Fill existed track: */
// pTrack->pBlobs[0] = pBNew;
// pTrack->size++;
// }
// else if((m_TrackNum+NewTrackNum)<SEQ_NUM)
// {   /* Duplicate existed track: */
// m_TrackSeq[m_TrackNum+NewTrackNum] = pTrack[0];
// m_TrackSeq[m_TrackNum+NewTrackNum].pBlobs[0] = pBNew;
// NewTrackNum++;
// }
// }   /* Next track. */
//
// if(AsignedTrack==0 && (m_TrackNum+NewTrackNum)<SEQ_NUM )
// {   /* Initialize new track: */
// m_TrackSeq[m_TrackNum+NewTrackNum].size = 1;
// m_TrackSeq[m_TrackNum+NewTrackNum].pBlobs[0] = pBNew;
// NewTrackNum++;
// }
// }   /* Next new blob. */
//
// m_TrackNum += NewTrackNum;
//
// /* Check each track: */
// for(i=0; i<m_TrackNum; ++i)
// {
// int     Good = 1;
// DefSeq* pTrack = m_TrackSeq+i;
// CvBlob* pBNew = pTrack->pBlobs[0];
// if(pTrack->size != SEQ_SIZE) continue;
// if(pBNew == NULL ) continue;
//
// /* Check intersection last blob with existed: */
// if(Good && pOldBlobList)
// {
// int k;
// for(k=pOldBlobList->GetBlobNum(); k>0; --k)
// {
// CvBlob* pBOld = pOldBlobList->GetBlob(k-1);
// if((fabs(pBOld->x-pBNew->x) < (CV_BLOB_RX(pBOld)+CV_BLOB_RX(pBNew))) &&
// (fabs(pBOld->y-pBNew->y) < (CV_BLOB_RY(pBOld)+CV_BLOB_RY(pBNew))))
// Good = 0;
// }
// }   /* Check intersection last blob with existed. */
//
// /* Check distance to image border: */
// if(Good)
// {   /* Check distance to image border: */
// float    dx = MIN(pBNew->x,S.width-pBNew->x)/CV_BLOB_RX(pBNew);
// float    dy = MIN(pBNew->y,S.height-pBNew->y)/CV_BLOB_RY(pBNew);
// if(dx < m_MinDistToBorder || dy < m_MinDistToBorder) Good = 0;
// }   /* Check distance to image border. */
//
// /* Check uniform motion: */
// if(Good)
// {   /* Check uniform motion: */
// double      Error = 0;
// int         N = pTrack->size;
// CvBlob**    pBL = pTrack->pBlobs;
// float       sum[2] = {0,0};
// float       jsum[2] = {0,0};
// float       a[2],b[2]; /* estimated parameters of moving x(t) = a*t+b*/
// int         j;
//
// for(j=0; j<N; ++j)
// {
// float   x = pBL[j]->x;
// float   y = pBL[j]->y;
// sum[0] += x;
// jsum[0] += j*x;
// sum[1] += y;
// jsum[1] += j*y;
// }
//
// a[0] = 6*((1-N)*sum[0]+2*jsum[0])/(N*(N*N-1));
// b[0] = -2*((1-2*N)*sum[0]+3*jsum[0])/(N*(N+1));
// a[1] = 6*((1-N)*sum[1]+2*jsum[1])/(N*(N*N-1));
// b[1] = -2*((1-2*N)*sum[1]+3*jsum[1])/(N*(N+1));
//
// for(j=0; j<N; ++j)
// {
// Error +=
// pow(a[0]*j+b[0]-pBL[j]->x,2)+
// pow(a[1]*j+b[1]-pBL[j]->y,2);
// }
//
// Error = sqrt(Error/N);
//
// if( Error > S.width*0.01 ||
// fabs(a[0])>S.width*0.1 ||
// fabs(a[1])>S.height*0.1)
// Good = 0;
//
// /* New best trajectory: */
// if(Good && (BestError == -1 || BestError > Error))
// {   /* New best trajectory: */
// BestTrack = i;
// BestError = Error;
// }   /* New best trajectory. */
// }   /*  Check uniform motion. */
// }   /*  Next track. */
//
// #if 0
// {   /**/
// printf("BlobDetector configurations = %d [",m_TrackNum);
// int i;
// for(i=0; i<SEQ_SIZE; ++i)
// {
// printf("%d,",m_pBlobLists[i]?m_pBlobLists[i]->GetBlobNum():0);
// }
// printf("]\n");
// }
// #endif
//
// if(BestTrack >= 0)
// {   /* Put new blob to output and delete from blob list: */
// assert(m_TrackSeq[BestTrack].size == SEQ_SIZE);
// assert(m_TrackSeq[BestTrack].pBlobs[0]);
// pNewBlobList->AddBlob(m_TrackSeq[BestTrack].pBlobs[0]);
// m_TrackSeq[BestTrack].pBlobs[0] = NULL;
// m_TrackSeq[BestTrack].size--;
// result = 1;
// }   /* Put new blob to output and mark in blob list to delete. */
// }   /*  Analyze blod list to find best blob trajectory. */
//
// {   /* Delete bad tracks: */
// int i;
// for(i=m_TrackNum-1; i>=0; --i)
// {   /* Delete bad tracks: */
// if(m_TrackSeq[i].pBlobs[0]) continue;
// if(m_TrackNum>0)
// m_TrackSeq[i] = m_TrackSeq[--m_TrackNum];
// }   /* Delete bad tracks: */
// }
//
// #ifdef USE_OBJECT_DETECTOR
// if( m_split_detector && pNewBlobList->GetBlobNum() > 0 )
// {
// int num_new_blobs = pNewBlobList->GetBlobNum();
// int i = 0;
//
// if( m_roi_seq ) cvClearSeq( m_roi_seq );
// m_debug_blob_seq.Clear();
// for( i = 0; i < num_new_blobs; ++i )
// {
// CvBlob* b = pNewBlobList->GetBlob(i);
// CvMat roi_stub;
// CvMat* roi_mat = 0;
// CvMat* scaled_roi_mat = 0;
//
// CvDetectedBlob d_b = cvDetectedBlob( CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 0 );
// m_debug_blob_seq.AddBlob(&d_b);
//
// float scale = m_param_roi_scale * m_min_window_size.height / CV_BLOB_WY(b);
//
// float b_width =   MAX(CV_BLOB_WX(b), m_min_window_size.width / scale)
// + (m_param_roi_scale - 1.0F) * (m_min_window_size.width / scale)
// + 2.0F * m_max_border / scale;
// float b_height = CV_BLOB_WY(b) * m_param_roi_scale + 2.0F * m_max_border / scale;
//
// CvRect roi = cvRectIntersection( cvRect( cvFloor(CV_BLOB_X(b) - 0.5F*b_width),
// cvFloor(CV_BLOB_Y(b) - 0.5F*b_height),
// cvCeil(b_width), cvCeil(b_height) ),
// cvRect( 0, 0, pImg->width, pImg->height ) );
// if( roi.width <= 0 || roi.height <= 0 )
// continue;
//
// if( m_roi_seq ) cvSeqPush( m_roi_seq, &roi );
//
// roi_mat = cvGetSubRect( pImg, &roi_stub, roi );
// scaled_roi_mat = cvCreateMat( cvCeil(scale*roi.height), cvCeil(scale*roi.width), CV_8UC3 );
// cvResize( roi_mat, scaled_roi_mat );
//
// m_detected_blob_seq.Clear();
// m_split_detector->Detect( scaled_roi_mat, &m_detected_blob_seq );
// cvReleaseMat( &scaled_roi_mat );
//
// for( int k = 0; k < m_detected_blob_seq.GetBlobNum(); ++k )
// {
// CvDetectedBlob* b = (CvDetectedBlob*) m_detected_blob_seq.GetBlob(k);
//
// /* scale and shift each detected blob back to the original image coordinates */
// CV_BLOB_X(b) = CV_BLOB_X(b) / scale + roi.x;
// CV_BLOB_Y(b) = CV_BLOB_Y(b) / scale + roi.y;
// CV_BLOB_WX(b) /= scale;
// CV_BLOB_WY(b) /= scale;
//
// CvDetectedBlob d_b = cvDetectedBlob( CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 1,
// b->response );
// m_debug_blob_seq.AddBlob(&d_b);
// }
//
// if( m_detected_blob_seq.GetBlobNum() > 1 )
// {
// /*
// * Split blob.
// * The original blob is replaced by the first detected blob,
// * remaining detected blobs are added to the end of the sequence:
// */
// CvBlob* first_b = m_detected_blob_seq.GetBlob(0);
// CV_BLOB_X(b)  = CV_BLOB_X(first_b);  CV_BLOB_Y(b)  = CV_BLOB_Y(first_b);
// CV_BLOB_WX(b) = CV_BLOB_WX(first_b); CV_BLOB_WY(b) = CV_BLOB_WY(first_b);
//
// for( int j = 1; j < m_detected_blob_seq.GetBlobNum(); ++j )
// {
// CvBlob* detected_b = m_detected_blob_seq.GetBlob(j);
// pNewBlobList->AddBlob(detected_b);
// }
// }
// }   /* For each new blob. */
//
// for( i = 0; i < pNewBlobList->GetBlobNum(); ++i )
// {
// CvBlob* b = pNewBlobList->GetBlob(i);
// CvDetectedBlob d_b = cvDetectedBlob( CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 2 );
// m_debug_blob_seq.AddBlob(&d_b);
// }
// }   // if( m_split_detector )
// #endif
//
// return result;
//
// }   /* cvDetectNewBlob */

implementation

Uses imgproc.types_c, Math;

{ TCvBlobDetectorSimple }

constructor TCvBlobDetectorSimple.Create;
var
  i: Integer;
begin
  m_pMaskBlobNew := NiL;
  m_pMaskBlobExist := NiL;
  for i := 0 to EBD_FRAME_NUM - 1 do
    m_pBlobLists[i] := NiL;

  SetModuleName('Simple');
end;

destructor TCvBlobDetectorSimple.Destroy;
var
  i: Integer;
begin
  if Assigned(m_pMaskBlobExist) then
    cvReleaseImage(m_pMaskBlobExist);
  if Assigned(m_pMaskBlobNew) then
    cvReleaseImage(m_pMaskBlobNew);
  for i := 0 to EBD_FRAME_NUM - 1 do
    m_pBlobLists[i].Free;
  inherited;
end;

function TCvBlobDetectorSimple.DetectNewBlob(pImg, pFGMask: pIplImage; pNewBlobList, pOldBlobList: TCvBlobSeq): Integer;
Var
  S: TCvSize;
  i: Integer;
  Blobs: TCvBlobSeq;
  storage: pCvMemStorage;
  pIB: pIplImage;
  cnts: pCvSeq;
  cnt: pCvSeq;
  NewBlob: TCvBlob;

  M00, X, Y, XX, YY: double;
  m: TCvMoments;
  r: TCvRect;
  mat: TCvMat;
  pB: pCvBlob;
  j: Integer;
  pBOld: pCvBlob;
  N: Integer;
  temp: TCvBlob;
  AreaP, AreaN: Single;
  pP: pCvBlob;
  pN: pCvBlob;

  Count: Integer;
  pBLIndex: array [0 .. EBD_FRAME_NUM - 1] of Integer;
  pBL_BEST: array [0 .. EBD_FRAME_NUM - 1] of Integer;
  finish: Integer;
  BestError: double;
  Good: Integer;

  pBL: array [0 .. EBD_FRAME_NUM - 1] of pCvBlob;
  Error: double;
  pBNew: pCvBlob;
  k: Integer;
  dx, dy: Single;

  sum, jsum,
  // estimated parameters of moving x(t) := a*t+b*/
  a, b: array [0 .. 1] of Single;

  pNewBlob: pCvBlob;
begin
  Result := 0;
  S := cvSize(pFGMask^.width, pFGMask^.height);
  if (m_pMaskBlobNew = nil) then
    m_pMaskBlobNew := cvCreateImage(S, IPL_DEPTH_8U, 1);
  if (m_pMaskBlobExist = nil) then
    m_pMaskBlobExist := cvCreateImage(S, IPL_DEPTH_8U, 1);

  // Shift blob list: */
  if Assigned(m_pBlobLists[0]) then
    m_pBlobLists[0].Free;
  for i := 1 to EBD_FRAME_NUM - 1 do
    m_pBlobLists[i - 1] := m_pBlobLists[i];
  m_pBlobLists[EBD_FRAME_NUM - 1] := TCvBlobSeq.Create;
  // Shift blob list. */

  // Create exist blob mask: */
  cvCopy(pFGMask, m_pMaskBlobNew);

  // Create contours and add new blobs to blob list: */
  begin // Create blobs: */
    storage := cvCreateMemStorage();

{$IFDEF 1}
    // Glue contours: */
    cvFindBlobsByCCClasters(m_pMaskBlobNew, &Blobs, storage);
    // Glue contours. */
{$ELSE}
    // */
    pIB := cvCloneImage(m_pMaskBlobNew);
    cnts := nil;
    cnt := nil;
    cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
    cvFindContours(pIB, storage, cnts, sizeof(TCvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
    // Process each contour: */
    cnt := cnts;
    while Assigned(cnt) do
    // cnt = cnt.h_next)
    begin
      // Image moments: */
      r := pCvContour(cnt).rect;

      if (r.height < S.height * 0.02) or (r.width < S.width * 0.02) then
        continue;

      CvMoments(cvGetSubRect(m_pMaskBlobNew, @mat, r), @m, 0);
      M00 := cvGetSpatialMoment(@m, 0, 0);

      if (M00 <= 0) then
        continue;

      X := cvGetSpatialMoment(@m, 1, 0) / M00;
      Y := cvGetSpatialMoment(@m, 0, 1) / M00;

      XX := (cvGetSpatialMoment(@m, 2, 0) / M00) - X * X;
      YY := (cvGetSpatialMoment(@m, 0, 2) / M00) - Y * Y;

      NewBlob := CvBlob(r.X + X, r.Y + Y, (4 * sqrt(XX)), (4 * sqrt(YY)));

      Blobs.AddBlob(@NewBlob);
      cnt := cnt.h_next
    end; // Next contour. */

    cvReleaseImage(&pIB);

    // One contour - one blob. */
{$ENDIF}
    begin // Delete small and intersected blobs: */
      for i := Blobs.GetBlobNum() downto 1 do
      begin
        pB := Blobs.GetBlob(i - 1);

        if (pB.h < S.height * 0.02) or (pB.w < S.width * 0.02) then
        begin
          Blobs.DelBlob(i - 1);
          continue;
        end;
        if Assigned(pOldBlobList) then
        begin
          for j := pOldBlobList.GetBlobNum() downto 1 do
          begin
            pBOld := pOldBlobList.GetBlob(j - 1);
            if ((abs(pBOld.X - pB.X) < (CV_BLOB_RX(pBOld) + CV_BLOB_RX(pB)))) and
              ((abs(pBOld.Y - pB.Y) < (CV_BLOB_RY(pBOld) + CV_BLOB_RY(pB)))) then
            begin
              // Intersection is present, so delete blob from list: */
              Blobs.DelBlob(i - 1);
              break;
            end;
          end; // Check next old blob. */
        end; // if pOldBlobList */
      end; // Check next blob. */
    end; // Delete small and intersected blobs. */

    begin // Bubble-sort blobs by size: */
      N := Blobs.GetBlobNum();
      for i := 1 to N - 1 do
      begin
        for j := i downto 1 do
        begin
          pP := Blobs.GetBlob(j - 1);
          pN := Blobs.GetBlob(j);
          AreaP := CV_BLOB_WX(pP) * CV_BLOB_WY(pP);
          AreaN := CV_BLOB_WX(pN) * CV_BLOB_WY(pN);
          if (AreaN < AreaP) then
            break;
          temp := pN[0];
          pN[0] := pP[0];
          pP[0] := temp;
        end;
      end;

      // Copy only first 10 blobs: */
      for i := 0 to MIN(N, 10) - 1 do
      begin
        m_pBlobLists[EBD_FRAME_NUM - 1].AddBlob(Blobs.GetBlob(i));
      end;
    end; // Sort blobs by size. */
    cvReleaseMemStorage(storage);
  end; // Create blobs. */

  // Analyze blob list to find best blob trajectory: */
  begin
    Count := 0;
    finish := 0;
    BestError := -1;
    Good := 1;

    for i := 0 to EBD_FRAME_NUM - 1 do
    begin
      pBLIndex[i] := 0;
      pBL_BEST[i] := 0;
    end;

    // Check configuration exist: */
    i := 0;
    while (Good <> 0) and (i < EBD_FRAME_NUM) do
    begin
      if (m_pBlobLists[i] = nil) or (m_pBlobLists[i].GetBlobNum() = 0) then
        Good := 0;
      Inc(i);
    end;

    if (Good <> 0) then
      repeat
      begin // For each configuration: */
        Good := 1;
        Error := 0;
        pBNew := m_pBlobLists[EBD_FRAME_NUM - 1].GetBlob(pBLIndex[EBD_FRAME_NUM - 1]);

        for i := 0 to EBD_FRAME_NUM - 1 do
          pBL[i] := m_pBlobLists[i].GetBlob(pBLIndex[i]);

        Inc(Count);

        // Check intersection last blob with existed: */
        if (Good <> 0) and Assigned(pOldBlobList) then
        begin // Check intersection last blob with existed: */
          for k := pOldBlobList.GetBlobNum() downto 1 do
          begin
            pBOld := pOldBlobList.GetBlob(k - 1);
            if ((abs(pBOld.X - pBNew.X) < (CV_BLOB_RX(pBOld) + CV_BLOB_RX(pBNew)))) and
              ((abs(pBOld.Y - pBNew.Y) < (CV_BLOB_RY(pBOld) + CV_BLOB_RY(pBNew)))) then
              Good := 0;
          end;
        end; // Check intersection last blob with existed. */

        // Check distance to image border: */
        if (Good <> 0) then
        begin // Check distance to image border: */
          pB := pBNew;
          dx := MIN(pB.X, S.width - pB.X) / CV_BLOB_RX(pB);
          dy := MIN(pB.Y, S.height - pB.Y) / CV_BLOB_RY(pB);

          if (dx < 1.1) or (dy < 1.1) then
            Good := 0;
        end; // Check distance to image border. */

        // Check uniform motion: */
        if (Good <> 0) then
        begin
          N := EBD_FRAME_NUM;
          sum[0] := 0;
          sum[1] := 0;
          jsum := sum;

          for j := 0 to N - 1 do
          begin
            X := pBL[j].X;
            Y := pBL[j].Y;
            sum[0] := sum[0] + X;
            jsum[0] := jsum[0] + j * X;
            sum[1] := sum[1] + Y;
            jsum[1] := jsum[1] + j * Y;
          end;

          a[0] := 6 * ((1 - N) * sum[0] + 2 * jsum[0]) / (N * (N * N - 1));
          b[0] := -2 * ((1 - 2 * N) * sum[0] + 3 * jsum[0]) / (N * (N + 1));
          a[1] := 6 * ((1 - N) * sum[1] + 2 * jsum[1]) / (N * (N * N - 1));
          b[1] := -2 * ((1 - 2 * N) * sum[1] + 3 * jsum[1]) / (N * (N + 1));

          for j := 0 to N - 1 do
          begin
            Error := Error + power(a[0] * j + b[0] - pBL[j].X, 2) + power(a[1] * j + b[1] - pBL[j].Y, 2);
          end;

          Error := sqrt(Error / N);

          if (Error > S.width * 0.01) or (abs(a[0]) > S.width * 0.1) or (abs(a[1]) > S.height * 0.1) then
            Good := 0;

        end; // Check configuration. */

        // New best trajectory: */
        if (Good <> 0) and ((BestError = -1) or (BestError > Error)) then
        begin
          for i := 0 to EBD_FRAME_NUM - 1 do
          begin
            pBL_BEST[i] := pBLIndex[i];
          end;
          BestError := Error;
        end; // New best trajectory. */

        // Set next configuration: */
        for i := 0 to EBD_FRAME_NUM - 1 do
        begin
          Inc(pBLIndex[i]);
          if (pBLIndex[i] <> m_pBlobLists[i].GetBlobNum()) then
            break;
          pBLIndex[i] := 0;
        end; // Next time shift. */

        if (i = EBD_FRAME_NUM) then
          finish := 1;

      end;
      until finish = 1; // Check next time configuration of connected components. */
{$IFDEF 0}
    begin // */
      WriteLn('BlobDetector configurations =  ', Count);
      for i := 0 to EBD_FRAME_NUM - 1 do
      begin
        WriteLn(iif(Assigned(m_pBlobLists[i]), m_pBlobLists[i].GetBlobNum(), 0));
      end;
      WriteLn;
    end;
{$ENDIF}
    if (BestError <> -1) then
    begin // Write new blob to output and delete from blob list: */
      pNewBlob := m_pBlobLists[EBD_FRAME_NUM - 1].GetBlob(pBL_BEST[EBD_FRAME_NUM - 1]);
      pNewBlobList.AddBlob(pNewBlob);

      for i := 0 to EBD_FRAME_NUM - 1 do
      begin // Remove blob from each list: */
        m_pBlobLists[i].DelBlob(pBL_BEST[i]);
      end; // Remove blob from each list. */

      Result := 1;

    end; // Write new blob to output and delete from blob list. */
  end; // Analyze blob list to find best blob trajectory. */
end;

procedure TCvBlobDetectorSimple.Release;
begin
  inherited;

end;

{ TCvBlobDetectorCC }

constructor TCvBlobDetectorCC.Create;
var
  i: Integer;
begin
  inherited;
  // /*CvDrawShape shapes[] =
  // {
  // { CvDrawShape::RECT,    {{255,255,255}} },
  // { CvDrawShape::RECT,    {{0,0,255}} },
  // { CvDrawShape::ELLIPSE, {{0,255,0}} }
  // };
  // int num_shapes = sizeof(shapes) / sizeof(shapes[0]);*/
  i := 0;
  SEQ_SIZE := 10;
  AddParam('Latency', @SEQ_SIZE);
  for i := 0 to SEQ_SIZE_MAX - 1 do
    m_pBlobLists[i] := NiL;
  for i := 0 to SEQ_NUM - 1 do
    m_TrackSeq[i].size := 0;
  m_TrackNum := 0;

  m_HMin := 0.02;
  m_WMin := 0.01;
  AddParam('HMin', @m_HMin);
  AddParam('WMin', @m_WMin);
  m_MinDistToBorder := 1.1;
  AddParam('MinDistToBorder', @m_MinDistToBorder);
  CommentParam('MinDistToBorder', 'Minimal allowed distance from blob center to image border in blob sizes');

  m_Clastering := 1;
  AddParam('Clastering', @m_Clastering);
  CommentParam('Clastering', 'Minimal allowed distance from blob center to image border in blob sizes');

  m_param_split_detector_file_name := 0;
{$IFDEF USE_OBJECT_DETECTOR}
  AddParam('Detector', @m_param_split_detector_file_name);
  CommentParam('Detector', 'Detector file name');
{$ENDIF}
  m_param_roi_scale := 1.5;
  AddParam('ROIScale', @m_param_roi_scale);
  CommentParam('ROIScale', 'Determines the size of search window around a blob');

  m_param_only_roi := 1;
  AddParam('OnlyROI', @m_param_only_roi);
  CommentParam('OnlyROI', 'Shows the whole debug image (0) or only ROIs where the detector was applied (1)');

  m_min_window_size := cvSize(0, 0);
  m_max_border := 0;
  m_roi_seq := cvCreateSeq(0, sizeof(m_roi_seq^), sizeof(TCvRect), cvCreateMemStorage());

  SetModuleName('CC');
end;

destructor TCvBlobDetectorCC.Destroy;
Var
  i: Integer;
begin
  for i := 0 to SEQ_SIZE_MAX - 1 do

    if Assigned(m_pBlobLists[i]) then
      m_pBlobLists[i].Free;

  if Assigned(m_roi_seq) then
  begin
    cvReleaseMemStorage(m_roi_seq^.storage);
    m_roi_seq := 0;
  end;
  // cvDestroyWindow( "EnteringBlobDetectionDebug" );
  inherited;
end;

function CompareContour(const a: Pointer; const b: Pointer; userdata: Pointer): Integer; cdecl;
Var
  dx, dy, h, w, ht, wt: Single;
  pa, pB: TCvPoint2D32f;
  ra, rb: TCvRect;
  pCA: pCvSeq;
  pCB: pCvSeq;
begin
  pCA := pCvSeq(a);
  pCB := pCvSeq(b);
  ra := pCvContour(pCA).rect;
  rb := pCvContour(pCB).rect;
  pa.X := ra.X + ra.width * 0.5;
  pa.Y := ra.Y + ra.height * 0.5;
  pB.X := rb.X + rb.width * 0.5;
  pB.Y := rb.Y + rb.height * 0.5;
  w := (ra.width + rb.width) * 0.5;
  h := (ra.height + rb.height) * 0.5;

  dx := abs(pa.X - pB.X) - w;
  dy := abs(pa.Y - pB.Y) - h;

  // wt = MAX(ra.width,rb.width)*0.1f;
  wt := 0;
  ht := MAX(ra.height, rb.height) * 0.3;
  Result := Integer((dx < wt) and (dy < ht));
end;

function TCvBlobDetectorCC.DetectNewBlob(pImg, pFGMask: pIplImage; pNewBlobList, pOldBlobList: TCvBlobSeq): Integer;
Var
  S: TCvSize;
  i: Integer;
  Blobs: TCvBlobSeq;
  storage: pCvMemStorage;

  pIB: pIplImage;
  cnts: pCvSeq;
  cnt: pCvSeq;

  NewBlob: TCvBlob;

  M00, X, Y, XX, YY: double;
  m: TCvMoments;
  r: TCvRect;
  mat: TCvMat;
  pB: pCvBlob;

  j: Integer;
  pBOld: pCvBlob;

  N: Integer;

  temp: TCvBlob;
  AreaP, AreaN: Single;
  pP: pCvBlob;
  pN: pCvBlob;

  pTrack: pDefSeq;

  BestError: double;
  BestTrack: Integer;
  pNewBlobs: TCvBlobSeq;
  NewTrackNum: Integer;

  pBNew: pCvBlob;
  AsignedTrack: Integer;

  dx, dy: double;
  pLastBlob: pCvBlob;

  Good: Integer;

  k: Integer;

  Error: double;
  pBL: pCvBlob;
  sum, jsum,
  // estimated parameters of moving x(t) := a*t+b*/
  a, b: array [0 .. 1] of Single;

  num_new_blobs: Integer;

  bb: pCvBlob;
  roi_stub: TCvMat;
  roi_mat: TCvMat;
  scaled_roi_mat: TCvMat;

  d_b: TCvDetectedBlob;

  pI: pIplImage;

  cnt_cur: Integer;
  color: TCvScalar;

begin
  Result := 0;
  S := cvSize(pFGMask.width, pFGMask.height);

  // Shift blob list: */
  begin
    if Assigned(m_pBlobLists[SEQ_SIZE - 1]) then
      m_pBlobLists[SEQ_SIZE - 1].Free;

    for i := SEQ_SIZE - 1 downto 1 do
      m_pBlobLists[i] := m_pBlobLists[i - 1];
    m_pBlobLists[0] := TCvBlobSeq.Create;
  end; // Shift blob list. */

  // Create contours and add new blobs to blob list: */
  begin // Create blobs: */

    storage := cvCreateMemStorage();

    if (m_Clastering <> 0) then
    begin // Glue contours: */
      cvFindBlobsByCCClasters(pFGMask, &Blobs, storage);
    end // Glue contours. */
    else
    begin // */
      pIB := cvCloneImage(pFGMask);
      cnts := nil;
      cnt := nil;
      cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
      cvFindContours(pIB, storage, cnts, sizeof(TCvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
      // Process each contour: */
      cnt := cnts;
      while Assigned(cnt) do
      // cnt := cnt.h_next)
      begin
        // Image moments: */
        r := pCvContour(cnt).rect;
        if (r.height < S.height * m_HMin) or (r.width < S.width * m_WMin) then
          continue;
        CvMoments(cvGetSubRect(pFGMask, @mat, r), @m, 0);
        M00 := cvGetSpatialMoment(@m, 0, 0);
        if (M00 <= 0) then
          continue;
        X := cvGetSpatialMoment(@m, 1, 0) / M00;
        Y := cvGetSpatialMoment(@m, 0, 1) / M00;
        XX := (cvGetSpatialMoment(@m, 2, 0) / M00) - X * X;
        YY := (cvGetSpatialMoment(@m, 0, 2) / M00) - Y * Y;
        NewBlob := CvBlob(r.X + X, r.Y + Y, (4 * sqrt(XX)), (4 * sqrt(YY)));
        Blobs.AddBlob(@NewBlob);
      end; // Next contour. */
      cvReleaseImage(pIB);
    end; // One contour - one blob. */

    begin // Delete small and intersected blobs: */

      for i := Blobs.GetBlobNum() downto 1 do
      begin
        pB := Blobs.GetBlob(i - 1);

        if (pB.h < S.height * m_HMin) or (pB.w < S.width * m_WMin) then
        begin
          Blobs.DelBlob(i - 1);
          continue;
        end;

        if Assigned(pOldBlobList) then
        begin
          for j := pOldBlobList.GetBlobNum() downto 1 do
          begin
            pBOld := pOldBlobList.GetBlob(j - 1);
            if ((abs(pBOld.X - pB.X) < (CV_BLOB_RX(pBOld) + CV_BLOB_RX(pB)))) and
              ((abs(pBOld.Y - pB.Y) < (CV_BLOB_RY(pBOld) + CV_BLOB_RY(pB)))) then
            begin
              // Intersection detected, delete blob from list: */
              Blobs.DelBlob(i - 1);
              break;
            end;
          end; // Check next old blob. */
        end; // if pOldBlobList. */
      end; // Check next blob. */
    end; // Delete small and intersected blobs. */

    begin // Bubble-sort blobs by size: */
      N := Blobs.GetBlobNum();
      for i := 1 to N - 1 do
      begin
        for j := i downto 1 do
        begin
          pP := Blobs.GetBlob(j - 1);
          pN := Blobs.GetBlob(j);
          AreaP := CV_BLOB_WX(pP) * CV_BLOB_WY(pP);
          AreaN := CV_BLOB_WX(pN) * CV_BLOB_WY(pN);
          if (AreaN < AreaP) then
            break;
          temp := pN[0];
          pN[0] := pP[0];
          pP[0] := temp;
        end;
      end;

      // Copy only first 10 blobs: */
      for i := 0 to MIN(N, 10) - 1 do
      begin
        m_pBlobLists[0].AddBlob(Blobs.GetBlob(i));
      end;

    end; // Sort blobs by size. */

    cvReleaseMemStorage(storage);

  end; // Create blobs. */

  begin // Shift each track: */
    for j := 0 to m_TrackNum - 1 do
    begin
      pTrack := @m_TrackSeq[j];

      for i := SEQ_SIZE - 1 downto 1 do
        pTrack.pBlobs[i] := pTrack.pBlobs[i - 1];

      pTrack.pBlobs[0] := nil;
      if (pTrack.size = SEQ_SIZE) then
        Dec(pTrack.size);
    end;
  end; // Shift each track. */

  // Analyze blob list to find best blob trajectory: */
  begin
    BestError := -1;
    BestTrack := -1;;
    pNewBlobs := m_pBlobLists[0];
    NewTrackNum := 0;
    for i := pNewBlobs.GetBlobNum() downto 1 do
    begin
      pBNew := pNewBlobs.GetBlob(i - 1);
      AsignedTrack := 0;
      for j := 0 to m_TrackNum - 1 do
      begin
        pTrack := @m_TrackSeq[j];
        pLastBlob := iif(pTrack.size > 0, pTrack.pBlobs[1], nil);
        if (pLastBlob = nil) then
          continue;
        dx := abs(CV_BLOB_X(pLastBlob) - CV_BLOB_X(pBNew));
        dy := abs(CV_BLOB_Y(pLastBlob) - CV_BLOB_Y(pBNew));
        if (dx > 2 * CV_BLOB_WX(pLastBlob)) or (dy > 2 * CV_BLOB_WY(pLastBlob)) then
          continue;
        Inc(AsignedTrack);

        if (pTrack.pBlobs[0] = nil) then
        begin // Fill existed track: */
          pTrack.pBlobs[0] := pBNew;
          Inc(pTrack.size);
        end
        else if ((m_TrackNum + NewTrackNum) < SEQ_NUM) then
        begin // Duplicate existed track: */
          m_TrackSeq[m_TrackNum + NewTrackNum] := pTrack[0];
          m_TrackSeq[m_TrackNum + NewTrackNum].pBlobs[0] := pBNew;
          Inc(NewTrackNum);
        end;
      end; // Next track. */

      if (AsignedTrack = 0) and ((m_TrackNum + NewTrackNum) < SEQ_NUM) then
      begin // Initialize new track: */
        m_TrackSeq[m_TrackNum + NewTrackNum].size := 1;
        m_TrackSeq[m_TrackNum + NewTrackNum].pBlobs[0] := pBNew;
        Inc(NewTrackNum);
      end;
    end; // Next new blob. */

    m_TrackNum := m_TrackNum + NewTrackNum;

    // Check each track: */
    for i := 0 to m_TrackNum - 1 do
    begin
      Good := 1;
      pTrack := @m_TrackSeq[i];
      pBNew := pTrack.pBlobs[0];
      if (pTrack.size <> SEQ_SIZE) then
        continue;
      if (pBNew = nil) then
        continue;

      // Check intersection last blob with existed: */
      if (Good <> 0) and Assigned(pOldBlobList) then
      begin
        for k := pOldBlobList.GetBlobNum() downto 1 do
        begin
          pBOld := pOldBlobList.GetBlob(k - 1);
          if ((abs(pBOld.X - pBNew.X) < (CV_BLOB_RX(pBOld) + CV_BLOB_RX(pBNew)))) and
            ((abs(pBOld.Y - pBNew.Y) < (CV_BLOB_RY(pBOld) + CV_BLOB_RY(pBNew)))) then
            Good := 0;
        end;
      end; // Check intersection last blob with existed. */

      // Check distance to image border: */
      if (Good <> 0) then
      begin // Check distance to image border: */
        dx := MIN(pBNew.X, S.width - pBNew.X) / CV_BLOB_RX(pBNew);
        dy := MIN(pBNew.Y, S.height - pBNew.Y) / CV_BLOB_RY(pBNew);
        if (dx < m_MinDistToBorder) or (dy < m_MinDistToBorder) then
          Good := 0;
      end; // Check distance to image border. */

      // Check uniform motion: */
      if (Good <> 0) then
      begin // Check uniform motion: */
        Error := 0;
        N := pTrack.size;
        pBL := pTrack.pBlobs[0];
        sum[0] := 0;
        sum[1] := 0;
        jsum := sum;
        for j := 0 to N - 1 do
        begin
          X := pBL[j].X;
          Y := pBL[j].Y;
          sum[0] := sum[0] + X;
          jsum[0] := jsum[0] + j * X;
          sum[1] := sum[1] + Y;
          jsum[1] := jsum[1] + j * Y;
        end;

        a[0] := 6 * ((1 - N) * sum[0] + 2 * jsum[0]) / (N * (N * N - 1));
        b[0] := -2 * ((1 - 2 * N) * sum[0] + 3 * jsum[0]) / (N * (N + 1));
        a[1] := 6 * ((1 - N) * sum[1] + 2 * jsum[1]) / (N * (N * N - 1));
        b[1] := -2 * ((1 - 2 * N) * sum[1] + 3 * jsum[1]) / (N * (N + 1));

        for j := 0 to N - 1 do
        begin
          Error := Error + power(a[0] * j + b[0] - pBL[j].X, 2) + power(a[1] * j + b[1] - pBL[j].Y, 2);
        end;

        Error := sqrt(Error / N);

        if (Error > S.width * 0.01) or (abs(a[0]) > S.width * 0.1) or (abs(a[1]) > S.height * 0.1) then
          Good := 0;

        // New best trajectory: */
        if (Good <> 0) and ((BestError = -1) or (BestError > Error)) then
        begin // New best trajectory: */
          BestTrack := i;
          BestError := Error;
        end; // New best trajectory. */
      end; // Check uniform motion. */
    end; // Next track. */

{$IFDEF 0}
    begin // */
      WriteLn('BlobDetector configurations := ', m_TrackNum);
      for i := 0 to SEQ_SIZE - 1 do
      begin
        WriteLn(iif(Assigned(m_pBlobLists[i]), m_pBlobLists[i].GetBlobNum(), 0));
      end;
      WriteLn;
    end;
{$ENDIF}
    if (BestTrack >= 0) then
    begin // Put new blob to output and delete from blob list: */
      assert(m_TrackSeq[BestTrack].size = SEQ_SIZE);
      assert(Assigned(m_TrackSeq[BestTrack].pBlobs[0]));
      pNewBlobList.AddBlob(m_TrackSeq[BestTrack].pBlobs[0]);
      m_TrackSeq[BestTrack].pBlobs[0] := nil;
      Dec(m_TrackSeq[BestTrack].size);
      Result := 1;
    end; // Put new blob to output and mark in blob list to delete. */
  end; // Analyze blod list to find best blob trajectory. */

  begin // Delete bad tracks: */
    for i := m_TrackNum - 1 downto 0 do
    begin // Delete bad tracks: */
      if Assigned(m_TrackSeq[i].pBlobs[0]) then
        continue;
      if (m_TrackNum > 0) then
      begin
        Dec(m_TrackNum);
        m_TrackSeq[i] := m_TrackSeq[m_TrackNum];
      end;
    end;
    // Delete bad tracks: */
  end;

{$IFDEF USE_OBJECT_DETECTOR}
  if (m_split_detector) and (pNewBlobList.GetBlobNum() > 0) then
  begin
    num_new_blobs := pNewBlobList.GetBlobNum();

    if Assigned(m_roi_seq) then
      cvClearSeq(m_roi_seq)m_debug_blob_seq.Clear();
    for i := 0 to num_new_blobs - 1 do
    begin
      b := pNewBlobList.GetBlob(i);
      roi_mat := nil;
      scaled_roi_mat := nil;

      d_b := CvDetectedBlob(CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 0);
      m_debug_blob_seq.AddBlob(&d_b);

      float scale := m_param_roi_scale * m_min_window_size.height / CV_BLOB_WY(b);

      float b_width := MAX(CV_BLOB_WX(b), m_min_window_size.width / scale) + (m_param_roi_scale - 1.0) *
        (m_min_window_size.width / scale) + 2.0 F * m_max_border / scale;
      float b_height := CV_BLOB_WY(b) * m_param_roi_scale + 2.0 F * m_max_border / scale;

      CvRect roi := cvRectIntersection(CvRect(cvFloor(CV_BLOB_X(b) - 0.5 F * b_width),
        cvFloor(CV_BLOB_Y(b) - 0.5 F * b_height), cvCeil(b_width), cvCeil(b_height)),
        CvRect(0, 0, pImg.width, pImg.height));
      if (roi.width < := 0) or (roi.height < := 0) then
        continue;

      if (m_roi_seq) then
        cvSeqPush(m_roi_seq, &roi);

      roi_mat := cvGetSubRect(pImg, &roi_stub, roi);
      scaled_roi_mat := cvCreateMat(cvCeil(scale * roi.height), cvCeil(scale * roi.width), CV_8UC3);
      cvResize(roi_mat, scaled_roi_mat);

      m_detected_blob_seq.Clear();
      m_split_detector.Detect(scaled_roi_mat, &m_detected_blob_seq);
      cvReleaseMat(&scaled_roi_mat);

      for (int k := 0; k < m_detected_blob_seq.GetBlobNum(); + + k) do
      begin
        CvDetectedBlob * b := (CvDetectedBlob * )m_detected_blob_seq.GetBlob(k);

        // scale and shift each detected blob back to the original image coordinates */
        CV_BLOB_X(b) := CV_BLOB_X(b) / scale + roi.X;
        CV_BLOB_Y(b) := CV_BLOB_Y(b) / scale + roi.Y;
        CV_BLOB_WX(b) / := scale;
        CV_BLOB_WY(b) / := scale;

        CvDetectedBlob d_b := CvDetectedBlob(CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 1, b.response);
        m_debug_blob_seq.AddBlob(&d_b);
      end;

      if (m_detected_blob_seq.GetBlobNum() > 1) then
      begin
        // * Split blob.
        // * The original blob is replaced by the first detected blob,
        // * remaining detected blobs are added to the end of the sequence:
        // */
        CvBlob * first_b := m_detected_blob_seq.GetBlob(0);
        CV_BLOB_X(b) := CV_BLOB_X(first_b);
        CV_BLOB_Y(b) := CV_BLOB_Y(first_b);
        CV_BLOB_WX(b) := CV_BLOB_WX(first_b);
        CV_BLOB_WY(b) := CV_BLOB_WY(first_b);

        for (int j := 1; j < m_detected_blob_seq.GetBlobNum(); + + j) do
        begin
          CvBlob * detected_b := m_detected_blob_seq.GetBlob(j);
          pNewBlobList.AddBlob(detected_b);
        end;
      end;
    end;
    // For each new blob. */

    for (i := 0; i < pNewBlobList.GetBlobNum(); + + i) do
    begin
      CvBlob * b := pNewBlobList.GetBlob(i);
      CvDetectedBlob d_b := CvDetectedBlob(CV_BLOB_X(b), CV_BLOB_Y(b), CV_BLOB_WX(b), CV_BLOB_WY(b), 2);
      m_debug_blob_seq.AddBlob(&d_b);
    end;
  end; // if( m_split_detector )
{$ENDIF}
end;

procedure TCvBlobDetectorCC.ParamUpdate;
begin
  inherited;

end;

procedure TCvBlobDetectorCC.Release;
begin
  inherited;

end;

procedure cvFindBlobsByCCClasters(pFG: pIplImage; pBlobs: TCvBlobSeq; storage: pCvMemStorage);
Var
  pIB: pIplImage;
  cnt: pCvSeq;
  cnt_list: pCvSeq;
  clasters: pCvSeq;
  claster_cur, claster_num: Integer;

  cnt_cur: Integer;
  NewBlob: TCvBlob;
  M00, X, Y, XX, YY: double; // image moments */
  m: TCvMoments;
  rect_res: TCvRect;
  mat: TCvMat;

  rect: TCvRect;
  cont: pCvSeq;
  k: Integer;

  x0, x1, y0, y1: Integer;

begin // Create contours: */
  pIB := nil;
  cnt := nil;
  cnt_list := cvCreateSeq(0, sizeof(TCvSeq), sizeof(TCvSeq), storage);
  clasters := nil;

  pIB := cvCloneImage(pFG);
  cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
  cvFindContours(pIB, storage, cnt, sizeof(TCvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE, cvPoint(0, 0));
  cvReleaseImage(pIB);

  // Create cnt_list.      */
  // Process each contour: */
  while Assigned(cnt) do
  begin
    cvSeqPush(cnt_list, cnt);
    cnt := cnt^.h_next;
  end;

  claster_num := cvSeqPartition(cnt_list, storage, clasters, CompareContour, nil);

  for claster_cur := 0 to claster_num - 1 do
  begin
    rect_res := CvRect(-1, -1, -1, -1);
    for cnt_cur := 0 to clasters.total - 1 do
    begin
      k := pInteger(cvGetSeqElem(clasters, cnt_cur))^;
      if (k <> claster_cur) then
        continue;
      cont := pCvSeq(cvGetSeqElem(cnt_list, cnt_cur));
      rect := pCvContour(cont).rect;

      if (rect_res.height < 0) then
      begin
        rect_res := rect;
      end
      else
      begin // Unite rects: */
        x0 := MIN(rect_res.X, rect.X);
        y0 := MIN(rect_res.Y, rect.Y);
        x1 := MAX(rect_res.X + rect_res.width, rect.X + rect.width);
        y1 := MAX(rect_res.Y + rect_res.height, rect.Y + rect.height);
        rect_res.X := x0;
        rect_res.Y := y0;
        rect_res.width := x1 - x0;
        rect_res.height := y1 - y0;
      end;
    end;

    if (rect_res.height < 1) or (rect_res.width < 1) then
    begin
      X := 0;
      Y := 0;
      XX := 0;
      YY := 0;
    end
    else
    begin
      CvMoments(cvGetSubRect(pFG, @mat, rect_res), @m, 0);
      M00 := cvGetSpatialMoment(@m, 0, 0);
      if (M00 <= 0) then
        continue;
      X := cvGetSpatialMoment(@m, 1, 0) / M00;
      Y := cvGetSpatialMoment(@m, 0, 1) / M00;
      XX := (cvGetSpatialMoment(@m, 2, 0) / M00) - X * X;
      YY := (cvGetSpatialMoment(@m, 0, 2) / M00) - Y * Y;
    end;
    NewBlob := CvBlob(rect_res.X + X, rect_res.Y + Y, (4 * sqrt(XX)), (4 * sqrt(YY)));
    pBlobs.AddBlob(@NewBlob);
  end; // Next cluster. */

{$IFDEF 0}
  begin // Debug info:
    pI := cvCreateImage(cvSize(pFG.width, pFG.height), IPL_DEPTH_8U, 3);
    cvZero(pI);
    for claster_cur := 0 to claster_num - 1 do
    begin
      color := CV_RGB(random(256), random(256), random(256));

      for cnt_cur := 0 to clasters.total - 1 do
      begin
        k := pInteger(cvGetSeqElem(clasters, cnt_cur))^;
        if (k <> claster_cur) then
          continue;
        cnt := pCvSeq(cvGetSeqElem(cnt_list, cnt_cur));
        cvDrawContours(pI, cnt, color, color, 0, 1, 8);
      end;

      pB := pBlobs.GetBlob(claster_cur);
      X := cvRound(CV_BLOB_RX(pB));
      Y := cvRound(CV_BLOB_RY(pB));
      cvEllipse(pI, cvPointFrom32f(CV_BLOB_CENTER(pB)), cvSize(MAX(1, X), MAX(1, Y)), 0, 0, 360, color, 1);
    end;

    cvNamedWindow('Clusters', 0);
    cvShowImage('Clusters', pI);

    cvReleaseImage(pI);

  end; // Debug info. */
{$ENDIF}
end; // cvFindBlobsByCCClasters */

end.
