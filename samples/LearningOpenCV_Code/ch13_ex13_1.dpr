program ch13_ex13_1;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  ocv.cvutils,
  ocv.core_c,
  ocv.highgui_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c;

const
  MAX_CLUSTERS = 5;

Var
  color_tab: array [0 .. MAX_CLUSTERS - 1] of TCvScalar;
  img: PIplImage;
  rng: TCvRNG;

  k, cluster_count: Integer;
  i, sample_count: Integer;
  points: PCvMat;
  clusters: PCvMat;
  center: TCvPoint;
  point_chunk: TCvMat;
  pt1: pCvPoint2D32f;
  pt2: pCvPoint2D32f;
  temp: TCvPoint2D32f;
  pt: TCvPoint2D32f;
  cluster_idx: Integer;
  key: Integer;

begin
  try
    img := cvCreateImage(cvSize(500, 500), 8, 3);
    rng := cvRNG($FFFFFFFF);

    color_tab[0] := CV_RGB(255, 0, 0);
    color_tab[1] := CV_RGB(0, 255, 0);
    color_tab[2] := CV_RGB(100, 100, 255);
    color_tab[3] := CV_RGB(255, 0, 255);
    color_tab[4] := CV_RGB(255, 255, 0);

    cvNamedWindow('clusters', 1);

    While true do
    begin
      cluster_count := (cvRandInt(rng) mod MAX_CLUSTERS) + 1;
      sample_count := (cvRandInt(rng) mod 1000) + 1;
      points := cvCreateMat(sample_count, 1, CV_32FC2);
      clusters := cvCreateMat(sample_count, 1, CV_32SC1);

      (* generate random sample from multivariate
        Gaussian distribution *)
      for k := 0 to cluster_count - 1 do
      begin
        center.x := cvRandInt(rng) mod img^.width;
        center.y := cvRandInt(rng) mod img^.height;
        cvGetRows(points, @point_chunk, (k * sample_count) div cluster_count, iif(k = cluster_count - 1, sample_count,
          (k + 1) * sample_count div cluster_count));
        cvRandArr(@rng, @point_chunk, CV_RAND_NORMAL, cvScalar(center.x, center.y, 0, 0), cvScalar(img^.width / 6, img^.height / 6, 0, 0));
      end;

      (* shuffle samples *)
      for i := 0 to sample_count div 2 do
      begin
        pt1 := pCvPoint2D32f(@(pFloat(points^.data)[2*(cvRandInt(rng) mod sample_count)]));
        pt2 := pCvPoint2D32f(@(pFloat(points^.data)[2*(cvRandInt(rng) mod sample_count)]));
        temp := pt1^;
        pt1^ := pt2^;
        pt2^ := temp;
      end;

      cvKMeans2(points, cluster_count, clusters, cvTermCriteria(CV_TERMCRIT_EPS or CV_TERMCRIT_ITER, 10, 1.0));
      cvZero(img);
      for i := 0 to sample_count - 1 do
      begin
        pt := PCvPoint2D32f(@pFloat(points^.data)[i*2])^;
        cluster_idx := pInteger(clusters^.data)[i];
        cvCircle(img, cvPointFrom32f(pt), 2, color_tab[cluster_idx], CV_FILLED);
      end;

      cvReleaseMat(points);
      cvReleaseMat(clusters);

      cvShowImage('clusters', img);

      key := cvWaitKey(0);
      if (key = 27) then // 'ESC'
        break;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
