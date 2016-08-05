(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)

(* *************** License:**************************
  Oct. 3, 2008
  Right to use this code in any way you want without warrenty, support or any guarentee of it working.

  BOOK: It would be nice if you cited it:
  Learning OpenCV: Computer Vision with the OpenCV Library
  by Gary Bradski and Adrian Kaehler
  Published by O'Reilly Media, October 3, 2008

  AVAILABLE AT:
  http://www.amazon.com/Learning-OpenCV-Computer-Vision-Library/dp/0596516134
  Or: http://oreilly.com/catalog/9780596516130/
  ISBN-10: 0596516134 or: ISBN-13: 978-0596516130

  OTHER OPENCV SITES:
  * The source code is on sourceforge at:
  http://sourceforge.net/projects/opencvlibrary/
  * The OpenCV wiki page (As of Oct 1, 2008 this is down for changing over servers, but should come back):
  http://opencvlibrary.sourceforge.net/
  * An active user group is at:
  http://tech.groups.yahoo.com/group/OpenCV/
  * The minutes of weekly OpenCV development meetings are at:
  http://pr.willowgarage.com/wiki/OpenCV
  **************************************************
*)

program ch7_ex7_3_expanded;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Character,
  ocv.utils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.tracking_c,
  ocv.compat;

procedure help();
begin
  WriteLn('Call is:');
  WriteLn('ch7_ex7_3_expanded modelImage0 testImage1 testImage2 badImage3');
  WriteLn('  Note that the model image is split in half.  Top half(0) makes model.  It''s then tested');
  WriteLn('  against its lower half(0), testImages 1 and 2 in different lighting and different object 3');
end;

// Compare 3 images histograms together,
// the first is divided in half along y to test its other half
// Call is:
// ch7HistCmp modelImage0 testImage1 testImage2 badImage3
// Note that the model image is split in half.  Top half(0) makes model.  It's then tested
// against its lower half(0), testImages 1 and 2 in different lighting and different object 3
//

Var
  src:array[0..4] of pIplImage;
  tmp:pIplImage;
	i:integer;
  size:TCvSize;
  width ,height ,halfheight :Integer;
  widthStep :Integer;
	p, sp:PByte;

  hsv, h_plane,s_plane,v_plane:array[0..4] of pIplImage;
  ,*planes[5][2];
       	IplImage* hist_img[5];
		CvHistogram* hist[5];
	    int h_bins = 8, s_bins = 8;
        int    hist_size[] = begin h_bins, s_bins end;;
        float  h_ranges[]  = begin 0, 180 end;;          // hue is [0,180]
        float  s_ranges[]  = begin 0, 255 end;;
        float* ranges[]    = begin h_ranges, s_ranges end;;
		int scale = 10;


begin
  try
    if ParamCount=5 then
    begin
    tmp := cvLoadImage(ParamStr(1).AsPAnsiChar, 1);
		if not Assigned(tmp) then
    begin //We're going to split this one in half
			WriteLn('Error on reading image 1, ',ParamStr(1));
			help();
			Halt(-1);
		end;
		//Parse the first image into two image halves divided halfway on y
		WriteLn('Getting size [[',tmp^.width,'] [',tmp^.height,']]');
		size := cvGetSize(tmp);
		WriteLn('Get size ',size.width,'  ',size.height);
		width := size.width;
		height := size.height;
		halfheight := height shr 1;
		src[0] := cvCreateImage(cvSize(width,halfheight), 8, 3);
		src[1] := cvCreateImage(cvSize(width,halfheight), 8, 3);
		if src[0]^.widthStep <> tmp^.widthStep then
    begin
			WriteLn('Error, Withstep of alloated src doesn''t equal withStep of loaded image [',src[0]^.widthStep,' vs ',tmp^.widthStep,']'
					);
			help();
			Halt(-2);
		end;
		widthStep := tmp^.widthStep;
		p := tmp^.imageData;
		sp := src[0]^.imageData;
		for i:=0 to widthStep*halfheight-1 do
    begin//Top half
			sp^ = p^;
      Inc(sp);
      Inc(p);
		end;
		sp := src[1]^.imageData;
		for i:=0 to widthStep*halfheight-1 do
     begin//Bottom half
	sp^ = p^;
      Inc(sp);
      Inc(p);
		end;
		//LOAD THE OTHER THREE IMAGES
		for i := 2 to 4 do
    begin
      src[i]:=cvLoadImage(ParamStr(i).AsPAnsiChar, 1);
			if not Assigned(src[i])  then
      begin
				WriteLn('Error on reading image ',i,' ',ParamStr(i));
				help();
				Halt(-1);
			end;
		end;

        // Compute the HSV image, and decompose it into separate planes.
        //
        IplImage *hsv[5], *h_plane[5],*s_plane[5],*v_plane[5],*planes[5][2];
       	IplImage* hist_img[5];
		CvHistogram* hist[5];
	    int h_bins = 8, s_bins = 8;
        int    hist_size[] = begin h_bins, s_bins end;;
        float  h_ranges[]  = begin 0, 180 end;;          // hue is [0,180]
        float  s_ranges[]  = begin 0, 255 end;;
        float* ranges[]    = begin h_ranges, s_ranges end;;
		int scale = 10;
 		for(i = 0; i<5; ++i)begin
			hsv[i] = cvCreateImage( cvGetSize(src[i]), 8, 3 );
        	cvCvtColor( src[i], hsv[i], CV_BGR2HSV );

			h_plane[i]  = cvCreateImage( cvGetSize(src[i]), 8, 1 );
        	s_plane[i]  = cvCreateImage( cvGetSize(src[i]), 8, 1 );
        	v_plane[i]  = cvCreateImage( cvGetSize(src[i]), 8, 1 );
        	planes[i][0] = h_plane[i];
			planes[i][1] = s_plane[i];
        	cvCvtPixToPlane( hsv[i], h_plane[i], s_plane[i], v_plane[i], 0 );
	        // Build the histogram and compute its contents.
    	    //
            begin
         		hist[i] = cvCreateHist(
            	2,
            	hist_size,
            	CV_HIST_ARRAY,
            	ranges,
            	1
          		);
        	end;
        	cvCalcHist( planes[i], hist[i], 0, 0 );
			cvNormalizeHist( hist[i], 1.0 );



			// Create an image to use to visualize our histogram.
  	        //
         	hist_img[i] = cvCreateImage(
          		cvSize( h_bins * scale, s_bins * scale ),
          		8,
          		3
        	);
        	cvZero( hist_img[i] );

        	// populate our visualization with little gray squares.
        	//
        	float max_value = 0;
			float *fp,fval;
        	cvGetMinMaxHistValue( hist[i], 0, &max_value, 0, 0 );

	        for( int h = 0; h < h_bins; h++ ) begin
    	        for( int s = 0; s < s_bins; s++ ) begin
        	        float bin_val = cvQueryHistValue_2D( hist[i], h, s );
	               	int intensity = cvRound( bin_val * 255 / max_value );
                	cvRectangle(
                  		hist_img[i],
                  		cvPoint( h*scale, s*scale ),
                  		cvPoint( (h+1)*scale - 1, (s+1)*scale - 1),
                  		CV_RGB(intensity,intensity,intensity),
                  		CV_FILLED
                	);
            	end;
        	end;
		end;//For the 5 images

        //DISPLAY
		cvNamedWindow( 'Source0', 1 );
        cvShowImage(   'Source0', src[0] );
        cvNamedWindow( 'H-S Histogram0', 1 );
        cvShowImage(   'H-S Histogram0', hist_img[0] );

		cvNamedWindow( 'Source1', 1 );
        cvShowImage(   'Source1', src[1] );
        cvNamedWindow( 'H-S Histogram1', 1 );
        cvShowImage(   'H-S Histogram1', hist_img[1] );

		cvNamedWindow( 'Source2', 1 );
        cvShowImage(   'Source2', src[2] );
        cvNamedWindow( 'H-S Histogram2', 1 );
        cvShowImage(   'H-S Histogram2', hist_img[2] );

		cvNamedWindow( 'Source3', 1 );
        cvShowImage(   'Source3', src[3] );
        cvNamedWindow( 'H-S Histogram3', 1 );
        cvShowImage(   'H-S Histogram3', hist_img[3] );

		cvNamedWindow( 'Source4', 1 );
        cvShowImage(   'Source4', src[4] );
        cvNamedWindow( 'H-S Histogram4', 1 );
        cvShowImage(   'H-S Histogram4', hist_img[4] );

		//Compare the histogram src0 vs 1, vs 2, vs 3, vs 4
		WriteLn('Comparison                         Corr                    Chi                     Intersect               Bhat\n');
		for(i=1; i<5; ++i)begin//For histogram
			WriteLn('Hist[0] vs: Hist[%d]: ',i);
			for(int j=0; j<4; ++j) begin //For comparision type
				WriteLn('CmpMethod[%d]: %lf; ',j,cvCompareHist(hist[0],hist[i],j));
			end;
			WriteLn('\n');
		end;
		//Oi Vey, parse histogram to earth movers signatures
//		CvRNG rng_state = cvRNG(0xffffffff); //Tested random bins
		CvMat* sig[5];
		int numrows = h_bins*s_bins;
		int numcols = 3; //value,i,j
		for(i=0; i<5; ++i)begin
			sig[i] = cvCreateMat(numrows, 3, CV_32FC1);
			//fill it
			float sum = 0.0;
	        for( int h = 0; h < h_bins; h++ ) begin
    	        for( int s = 0; s < s_bins; s++ ) begin
        	        float bin_val = cvQueryHistValue_2D( hist[i], h, s );
					cvSet2D(sig[i],h*s_bins + s,0,cvScalar(bin_val,bin_val,bin_val)); //Point weight
					cvSet2D(sig[i],h*s_bins + s,1,cvScalar(h)); //Coord 1
					cvSet2D(sig[i],h*s_bins + s,2,cvScalar(s)); //Coord 2
				end;
			end;
		end;
		//Do EMD AND REPORT
		WriteLn('EMD: ');
		for(i=1; i<5; ++i)begin
			float emd = cvCalcEMD2(sig[0],sig[i],CV_DIST_L2);
			WriteLn('%f; \n',emd);
		end;
		WriteLn('\n');

        cvWaitKey(0);
    end;
	else begin WriteLn('Error: Wrong number of arguments\n'); help();end;


  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
