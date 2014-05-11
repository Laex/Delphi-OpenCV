unit leptonica.pix;

interface

Type

  l_uint32 = Cardinal;
  pl_uint32 = ^l_uint32;
  l_int32 = Integer;

  pPixColormap = ^TPixColormap;

  TPixColormap = packed record
    array_: Pointer; (*colormap table (array of RGBA_QUAD)*)
    depth: l_int32; (*of pix (1, 2, 4 or 8 bpp)*)
    nalloc: l_int32; (*number of color entries allocated*)
    n: l_int32; (*number of color entries used*)
  end;

  pPix = ^TPix;

  TPix = packed record
    w: l_uint32; (*width in pixels*)
    h: l_uint32; (*height in pixels*)
    d: l_uint32; (*depth in bits*)
    wpl: l_uint32; (*32-bit words/line*)
    refcount: l_uint32; (*reference count (1 if no clones)*)
    xres: l_int32; (*image res (ppi) in x direction*)
    (*(use 0 if unknown)*)
    yres: l_int32; (*image res (ppi) in y direction*)
    (*(use 0 if unknown)*)
    informat: l_int32; (*input file format, IFF_**)
    text: PAnsichar; (*text string associated with pix*)
    colormap: pPixColormap; (*colormap (may be null)*)
    data: pl_uint32; (*the image data*)
  end;

implementation

end.
