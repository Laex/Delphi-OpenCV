(*
  *
  * This file is part of FFmpeg.
  *
  * FFmpeg is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Lesser General Public
  * License as published by the Free Software Foundation; either
  * version 2.1 of the License, or (at your option) any later version.
  *
  * FFmpeg is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Lesser General Public License for more details.
  *
  * You should have received a copy of the GNU Lesser General Public
  * License along with FFmpeg; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

unit frame;

{$include ffmpeg.inc}

interface

uses
  ctypes, buffer, dict, avutil, rational;

Type
  pAVColorSpace = ^TAVColorSpace;
  TAVColorSpace = ( //
    AVCOL_SPC_RGB = 0, //
    AVCOL_SPC_BT709 = 1, // < also ITU-R BT1361 / IEC 61966-2-4 xvYCC709 / SMPTE RP177 Annex B
    AVCOL_SPC_UNSPECIFIED = 2, //
    AVCOL_SPC_FCC = 4, //
    AVCOL_SPC_BT470BG = 5, // < also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM / IEC 61966-2-4 xvYCC601
    AVCOL_SPC_SMPTE170M = 6, // < also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC / functionally identical to above
    AVCOL_SPC_SMPTE240M = 7, //
    AVCOL_SPC_YCOCG = 8, // < Used by Dirac / VC-2 and H.264 FRext, see ITU-T SG16
    AVCOL_SPC_BT2020_NCL = 9, // < ITU-R BT2020 non-constant luminance system
    AVCOL_SPC_BT2020_CL = 10, // < ITU-R BT2020 constant luminance system
    AVCOL_SPC_NB // < Not part of ABI
    );

Const
  AVCOL_SPC_YCGCO = AVCOL_SPC_YCOCG;

  (*
    * The frame data may be corrupted, e.g. due to decoding errors.
  *)
  AV_FRAME_FLAG_CORRUPT = (1 shl 0);
  FF_DECODE_ERROR_INVALID_BITSTREAM = 1;
  FF_DECODE_ERROR_MISSING_REFERENCE = 2;

Type
  pAVColorRange = ^TAVColorRange;
  TAVColorRange = ( //
    AVCOL_RANGE_UNSPECIFIED = 0, //
    AVCOL_RANGE_MPEG = 1, // < the normal 219*2^(n-8) "MPEG" YUV ranges
    AVCOL_RANGE_JPEG = 2, // < the normal     2^n-1   "JPEG" YUV ranges
    AVCOL_RANGE_NB // < Not part of ABI
    );

  pAVFrameSideDataType = ^TAVFrameSideDataType;
  TAVFrameSideDataType = (
    (*
      * The data is the AVPanScan struct defined in libavcodec.
    *)
    AV_FRAME_DATA_PANSCAN //
    );

  pAVFrameSideData = ^TAVFrameSideData;
  ppAVFrameSideData = ^pAVFrameSideData;

  TAVFrameSideData = {packed} record
    _type: TAVFrameSideDataType;
    data: pByte;
    size: Integer;
    metadata: pAVDictionary;
  end;

  (*
    * This structure describes decoded (raw) audio or video data.
    *
    * AVFrame must be allocated using av_frame_alloc(). Note that this only
    * allocates the AVFrame itself, the buffers for the data must be managed
    * through other means (see below).
    * AVFrame must be freed with av_frame_free().
    *
    * AVFrame is typically allocated once and then reused multiple times to hold
    * different data (e.g. a single AVFrame to hold frames received from a
    * decoder). In such a case, av_frame_unref() will free any references held by
    * the frame and reset it to its original clean state before it
    * is reused again.
    *
    * The data described by an AVFrame is usually reference counted through the
    * AVBuffer API. The underlying buffer references are stored in AVFrame.buf /
    * AVFrame.extended_buf. An AVFrame is considered to be reference counted if at
    * least one reference is set, i.e. if AVFrame.buf[0] != NULL. In such a case,
    * every single data plane must be contained in one of the buffers in
    * AVFrame.buf or AVFrame.extended_buf.
    * There may be a single buffer for all the data, or one separate buffer for
    * each plane, or anything in between.
    *
    * sizeof(AVFrame) is not a part of the public ABI, so new fields may be added
    * to the end with a minor bump.
    * Similarly fields that are marked as to be only accessed by
    * av_opt_ptr() can be reordered. This allows 2 forks to add fields
    * without breaking compatibility with each other.
  *)
const
  AV_NUM_DATA_POINTERS = 8;

Type
  pAVNDPArray = ^TAVNDPArray;
  TAVNDPArray = array [0 .. AV_NUM_DATA_POINTERS - 1] of Integer;

  pAVFrameByteArray = ^TAVFrameByteArray;
  TAVFrameByteArray = array [0 .. AV_NUM_DATA_POINTERS - 1] of pByte;

  pAVFrameInt64Array = ^TAVFrameInt64Array;
  TAVFrameInt64Array = array [0 .. AV_NUM_DATA_POINTERS - 1] of uint64;

  pAVFrame = ^TAVFrame;
  ppAVFrame = ^pAVFrame;

  puint8 = ^uint8;
  ppuint8 = ^puint8;

  pMotion_val = ^TMotion_val;
  TMotion_val = array [0 .. 1] of int16;

  pRef_index = ^TRef_index;
  TRef_index = array [0 .. 1] of int8;

  pAVBufferRefArray = ^TAVBufferRefArray;
  TAVBufferRefArray = array [0 .. AV_NUM_DATA_POINTERS - 1] of pAVBufferRef;

  TAVFrame = {packed} record
    (*
      * pointer to the picture/channel planes.
      * This might be different from the first allocated byte
      *
      * Some decoders access areas outside 0,0 - width,height, please
      * see avcodec_align_dimensions2(). Some filters and swscale can read
      * up to 16 bytes beyond the planes, if these filters are to be used,
      * then 16 extra bytes must be allocated.
    *)
    data: TAVFrameByteArray;
    (*
      * For video, size in bytes of each picture line.
      * For audio, size in bytes of each plane.
      *
      * For audio, only linesize[0] may be set. For planar audio, each channel
      * plane must be the same size.
      *
      * For video the linesizes should be multiplies of the CPUs alignment
      * preference, this is 16 or 32 for modern desktop CPUs.
      * Some code requires such alignment other code can be slower without
      * correct alignment, for yet other it makes no difference.
      *
      * @note The linesize may be larger than the size of usable data -- there
      * may be extra padding present for performance reasons.
    *)
    linesize: array [0 .. AV_NUM_DATA_POINTERS - 1] of Integer;
    (*
      * pointers to the data planes/channels.
      *
      * For video, this should simply point to data[].
      *
      * For planar audio, each channel has a separate data pointer, and
      * linesize[0] contains the size of each channel buffer.
      * For {packed} audio, there is just one data pointer, and linesize[0]
      * contains the total size of the buffer for all channels.
      *
      * Note: Both data and extended_data should always be set in a valid frame,
      * but for planar audio with more channels that can fit in data,
      * extended_data must be used in order to access all channels.
    *)
    extended_data: ppuint8;
    (*
      * width and height of the video frame
    *)
    width, height: Integer;
    (*
      * number of audio samples (per channel) described by this frame
    *)
    nb_samples: Integer;

    (*
      * format of the frame, -1 if unknown or unset
      * Values correspond to enum AVPixelFormat for video frames,
      * enum AVSampleFormat for audio)
    *)
    format: Integer;
    (*
      * 1 -> keyframe, 0-> not
    *)
    key_frame: Integer;
    (*
      * Picture type of the frame.
    *)
    pict_type: TAVPictureType;

{$IFDEF FF_API_AVFRAME_LAVC}
    // attribute_deprecated
    base: TAVFrameByteArray;
{$ENDIF}
    (*
      * Sample aspect ratio for the video frame, 0/1 if unknown/unspecified.
    *)
    sample_aspect_ratio: TAVRational;
    (*
      * Presentation timestamp in time_base units (time when frame should be shown to user).
    *)
    pts: int64_t;
    (*
      * PTS copied from the AVPacket that was decoded to produce this frame.
    *)
    pkt_pts: int64_t;
    (*
      * DTS copied from the AVPacket that triggered returning this frame. (if frame threading isnt used)
      * This is also the Presentation time of this AVFrame calculated from
      * only AVPacket.dts values without pts values.
    *)
    pkt_dts: int64_t;
    (*
      * picture number in bitstream order
    *)
    coded_picture_number: Integer;
    (*
      * picture number in display order
    *)
    display_picture_number: Integer;
    (*
      * quality (between 1 (good) and FF_LAMBDA_MAX (bad))
    *)
    quality: Integer;
{$IFDEF FF_API_AVFRAME_LAVC}
    // attribute_deprecated
    reference: Integer;
    (*
      * QP table *)
    // attribute_deprecated
    qscale_table: pint8_t;
    (*
      * QP store stride
    *)
    // attribute_deprecated
    qstride: Integer;
    // attribute_deprecated
    qscale_type: Integer;
    (*
      * mbskip_table[mb]>=1 if MB didn't change
      * stride= mb_width = (width+15)>>4
    *)
    // attribute_deprecated
    mbskip_table: puint8_t;
    (*
      * motion vector table
      * @code
      * example:
      * int mv_sample_log2= 4 - motion_subsample_log2;
      * int mb_width= (width+15)>>4;
      * int mv_stride= (mb_width << mv_sample_log2) + 1;
      * motion_val[direction][x + y*mv_stride][0->mv_x, 1->mv_y];
      * @endcode
    *)
    // attribute_deprecated
    // int16_t (*motion_val[2])[2];
    motion_val: array [0 .. 1] of pMotion_val;
    (*
      * macroblock type table
      * mb_type_base + mb_width + 2
    *)
    // attribute_deprecated
    mb_type: puint32_t;
    (*
      * DCT coefficients
    *)
    // attribute_deprecated
    dct_coeff: pshort;
    (*
      * motion reference frame index
      * the order in which these are stored can depend on the codec.
    *)
    // attribute_deprecated
    // int8_t *ref_index[2];
    ref_index: pRef_index;
{$ENDIF}
    (*
      * for some private data of the user
    *)
    opaque: Pointer;
    (*
      * error
    *)
    error: array [0 .. AV_NUM_DATA_POINTERS - 1] of uint64;
{$IFDEF FF_API_AVFRAME_LAVC}
    // attribute_deprecated
    _type: Integer;
{$ENDIF}
    (*
      * When decoding, this signals how much the picture must be delayed.
      * extra_delay = repeat_pict / (2*fps)
    *)
    repeat_pict: Integer;
    (*
      * The content of the picture is interlaced.
    *)
    interlaced_frame: Integer;
    (*
      * If the content is interlaced, is top field displayed first.
    *)
    top_field_first: Integer;
    (*
      * Tell user application that palette has changed from previous frame.
    *)
    palette_has_changed: Integer;

{$IFDEF FF_API_AVFRAME_LAVC}
    // attribute_deprecated
    buffer_hints: Integer;
    (*
      * Pan scan.
    *)
    // attribute_deprecated
    pan_scan: pAVPanScan;
{$ENDIF}
    (*
      * reordered opaque 64bit (generally an integer or a double precision float
      * PTS but can be anything).
      * The user sets AVCodecContext.reordered_opaque to represent the input at
      * that time,
      * the decoder reorders values as needed and sets AVFrame.reordered_opaque
      * to exactly one of the values provided by the user through AVCodecContext.reordered_opaque
      * @deprecated in favor of pkt_pts
    *)
    reordered_opaque: int64_t;
{$IFDEF FF_API_AVFRAME_LAVC}
    (*
      * @deprecated this field is unused
    *)
    // attribute_deprecated
    hwaccel_picture_private: Pointer;
    // attribute_deprecated
    owner: pAVCodecContext;
    // attribute_deprecated
    thread_opaque: Pointer;
    (*
      * log2 of the size of the block which a single vector in motion_val represents:
      * (4->16x16, 3->8x8, 2-> 4x4, 1-> 2x2)
    *)
    // attribute_deprecated
    motion_subsample_log2: uint8_t;
{$ENDIF}
    (*
      * Sample rate of the audio data.
    *)
    sample_rate: Integer;
    (*
      * Channel layout of the audio data.
    *)
    channel_layout: uint64_t;
    //
    (*
      * AVBuffer references backing the data for this frame. If all elements of
      * this array are NULL, then this frame is not reference counted.
      *
      * There may be at most one AVBuffer per data plane, so for video this array
      * always contains all the references. For planar audio with more than
      * AV_NUM_DATA_POINTERS channels, there may be more buffers than can fit in
      * this array. Then the extra AVBufferRef pointers are stored in the
      * extended_buf array.
    *)
    buf: TAVBufferRefArray;
    (*
      * For planar audio which requires more than AV_NUM_DATA_POINTERS
      * AVBufferRef pointers, this array will hold all the references which
      * cannot fit into AVFrame.buf.
      *
      * Note that this is different from AVFrame.extended_data, which always
      * contains all the pointers. This array only contains the extra pointers,
      * which cannot fit into AVFrame.buf.
      *
      * This array is always allocated using av_malloc() by whoever constructs
      * the frame. It is freed in av_frame_unref().
    *)
    extended_buf: ppAVBufferRef;
    (*
      * Number of elements in extended_buf.
    *)
    nb_extended_buf: Integer;
    side_data: ppAVFrameSideData;
    nb_side_data: Integer;
    (*
      * Frame flags, a combination of AV_FRAME_FLAG_*
    *)
    flags: Integer;
    (*
      * frame timestamp estimated using various heuristics, in stream time base
      * Code outside libavcodec should access this field using:
      * av_frame_get_best_effort_timestamp(frame)
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    best_effort_timestamp: int64_t;
    (*
      * reordered pos from the last AVPacket that has been input into the decoder
      * Code outside libavcodec should access this field using:
      * av_frame_get_pkt_pos(frame)
      * - encoding: unused
      * - decoding: Read by user.
    *)
    pkt_pos: int64_t;
    (*
      * duration of the corresponding packet, expressed in
      * AVStream->time_base units, 0 if unknown.
      * Code outside libavcodec should access this field using:
      * av_frame_get_pkt_duration(frame)
      * - encoding: unused
      * - decoding: Read by user.
    *)
    pkt_duration: int64_t;
    (*
      * metadata.
      * Code outside libavcodec should access this field using:
      * av_frame_get_metadata(frame)
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    metadata: pAVDictionary;
    (*
      * decode error flags of the frame, set to a combination of
      * FF_DECODE_ERROR_xxx flags if the decoder produced a frame, but there
      * were errors during the decoding.
      * Code outside libavcodec should access this field using:
      * av_frame_get_decode_error_flags(frame)
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    decode_error_flags: Integer;
    (*
      * number of audio channels, only used for audio.
      * Code outside libavcodec should access this field using:
      * av_frame_get_channels(frame)
      * - encoding: unused
      * - decoding: Read by user.
    *)
    channels: Integer;
    (*
      * size of the corresponding packet containing the compressed
      * frame. It must be accessed using av_frame_get_pkt_size() and
      * av_frame_set_pkt_size().
      * It is set to a negative value if unknown.
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    pkt_size: Integer;
    (*
      * YUV colorspace type.
      * It must be accessed using av_frame_get_colorspace() and
      * av_frame_set_colorspace().
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    colorspace: TAVColorSpace;
    (*
      * MPEG vs JPEG YUV range.
      * It must be accessed using av_frame_get_color_range() and
      * av_frame_set_color_range().
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    color_range: TAVColorRange;
    (*
      * Not to be accessed directly from outside libavutil
    *)
    qp_table_buf: pAVBufferRef;
  end;

  (*
    // * Accessors for some AVFrame fields.
    // * The position of these field in the structure is not part of the ABI,
    // * they should not be accessed directly outside libavcodec.
  *)
  // int64_t av_frame_get_best_effort_timestamp(const AVFrame *frame);
function av_frame_get_best_effort_timestamp(const frame: pAVFrame): int64; cdecl;
// void    av_frame_set_best_effort_timestamp(AVFrame *frame, int64_t val);
// int64_t av_frame_get_pkt_duration         (const AVFrame *frame);
// void    av_frame_set_pkt_duration         (AVFrame *frame, int64_t val);
// int64_t av_frame_get_pkt_pos              (const AVFrame *frame);
// void    av_frame_set_pkt_pos              (AVFrame *frame, int64_t val);
// int64_t av_frame_get_channel_layout       (const AVFrame *frame);
// void    av_frame_set_channel_layout       (AVFrame *frame, int64_t val);
// int     av_frame_get_channels             (const AVFrame *frame);
// void    av_frame_set_channels             (AVFrame *frame, int     val);
// int     av_frame_get_sample_rate          (const AVFrame *frame);
// void    av_frame_set_sample_rate          (AVFrame *frame, int     val);
// AVDictionary *av_frame_get_metadata       (const AVFrame *frame);
// void          av_frame_set_metadata       (AVFrame *frame, AVDictionary *val);
// int     av_frame_get_decode_error_flags   (const AVFrame *frame);
// void    av_frame_set_decode_error_flags   (AVFrame *frame, int     val);
// int     av_frame_get_pkt_size(const AVFrame *frame);
// void    av_frame_set_pkt_size(AVFrame *frame, int val);
// AVDictionary **avpriv_frame_get_metadatap(AVFrame *frame);
// int8_t *av_frame_get_qp_table(AVFrame *f, int *stride, int *type);
// int av_frame_set_qp_table(AVFrame *f, AVBufferRef *buf, int stride, int type);
// enum AVColorSpace av_frame_get_colorspace(const AVFrame *frame);
// void    av_frame_set_colorspace(AVFrame *frame, enum AVColorSpace val);
// enum AVColorRange av_frame_get_color_range(const AVFrame *frame);
// void    av_frame_set_color_range(AVFrame *frame, enum AVColorRange val);
//
(*
  // * Get the name of a colorspace.
  // * @return a static string identifying the colorspace; can be NULL.
*)
// const char *av_get_colorspace_name(enum AVColorSpace val);
//
(*
  // * Allocate an AVFrame and set its fields to default values.  The resulting
  // * struct must be freed using av_frame_free().
  // *
  // * @return An AVFrame filled with default values or NULL on failure.
  // *
  // * @note this only allocates the AVFrame itself, not the data buffers. Those
  // * must be allocated through other means, e.g. with av_frame_get_buffer() or
  // * manually.
*)
// AVFrame *av_frame_alloc(void);
function av_frame_alloc(): pAVFrame; cdecl;
//
(*
  // * Free the frame and any dynamically allocated objects in it,
  // * e.g. extended_data. If the frame is reference counted, it will be
  // * unreferenced first.
  // *
  // * @param frame frame to be freed. The pointer will be set to NULL.
*)
// void av_frame_free(AVFrame **frame);
procedure av_frame_free(Var frame: pAVFrame); cdecl;
(*
  // * Setup a new reference to the data described by a given frame.
  // *
  // * Copy frame properties from src to dst and create a new reference for each
  // * AVBufferRef from src.
  // *
  // * If src is not reference counted, new buffers are allocated and the data is
  // * copied.
  // *
  // * @return 0 on success, a negative AVERROR on error
*)
// int av_frame_ref(AVFrame *dst, const AVFrame *src);
//
(*
  // * Create a new frame that references the same data as src.
  // *
  // * This is a shortcut for av_frame_alloc()+av_frame_ref().
  // *
  // * @return newly created AVFrame on success, NULL on error.
*)
// AVFrame *av_frame_clone(const AVFrame *src);
//
(*
  // * Unreference all the buffers referenced by frame and reset the frame fields.
*)
// void av_frame_unref(AVFrame *frame);
procedure av_frame_unref(frame: pAVFrame); cdecl;

(*
  // * Move everythnig contained in src to dst and reset src.
*)
// void av_frame_move_ref(AVFrame *dst, AVFrame *src);
//
(*
  // * Allocate new buffer(s) for audio or video data.
  // *
  // * The following fields must be set on frame before calling this function:
  // * - format (pixel format for video, sample format for audio)
  // * - width and height for video
  // * - nb_samples and channel_layout for audio
  // *
  // * This function will fill AVFrame.data and AVFrame.buf arrays and, if
  // * necessary, allocate and fill AVFrame.extended_data and AVFrame.extended_buf.
  // * For planar formats, one buffer will be allocated for each plane.
  // *
  // * @param frame frame in which to store the new buffers.
  // * @param align required buffer size alignment
  // *
  // * @return 0 on success, a negative AVERROR on error.
*)
// int av_frame_get_buffer(AVFrame *frame, int align);
//
(*
  // * Check if the frame data is writable.
  // *
  // * @return A positive value if the frame data is writable (which is true if and
  // * only if each of the underlying buffers has only one reference, namely the one
  // * stored in this frame). Return 0 otherwise.
  // *
  // * If 1 is returned the answer is valid until av_buffer_ref() is called on any
  // * of the underlying AVBufferRefs (e.g. through av_frame_ref() or directly).
  // *
  // * @see av_frame_make_writable(), av_buffer_is_writable()
*)
// int av_frame_is_writable(AVFrame *frame);
//
(*
  // * Ensure that the frame data is writable, avoiding data copy if possible.
  // *
  // * Do nothing if the frame is writable, allocate new buffers and copy the data
  // * if it is not.
  // *
  // * @return 0 on success, a negative AVERROR on error.
  // *
  // * @see av_frame_is_writable(), av_buffer_is_writable(),
  // * av_buffer_make_writable()
*)
// int av_frame_make_writable(AVFrame *frame);
//
(*
  // * Copy only "metadata" fields from src to dst.
  // *
  // * Metadata for the purpose of this function are those fields that do not affect
  // * the data layout in the buffers.  E.g. pts, sample rate (for audio) or sample
  // * aspect ratio (for video), but not width/height or channel layout.
  // * Side data is also copied.
*)
// int av_frame_copy_props(AVFrame *dst, const AVFrame *src);
//
(*
  // * Get the buffer reference a given data plane is stored in.
  // *
  // * @param plane index of the data plane of interest in frame->extended_data.
  // *
  // * @return the buffer reference that contains the plane or NULL if the input
  // * frame is not valid.
*)
// AVBufferRef *av_frame_get_plane_buffer(AVFrame *frame, int plane);
//
(*
  // * Add a new side data to a frame.
  // *
  // * @param frame a frame to which the side data should be added
  // * @param type type of the added side data
  // * @param size size of the side data
  // *
  // * @return newly added side data on success, NULL on error
*)
// AVFrameSideData *av_frame_new_side_data(AVFrame *frame,
// enum AVFrameSideDataType type,
// int size);
//
(*
  // * @return a pointer to the side data of a given type on success, NULL if there
  // * is no side data with such type in this frame.
*)
// AVFrameSideData *av_frame_get_side_data(const AVFrame *frame,
// enum AVFrameSideDataType type);

implementation

uses ffmpeglib;

function av_frame_alloc; external avutil_dll;
function av_frame_get_best_effort_timestamp; external avutil_dll;
procedure av_frame_unref; external avutil_dll;
procedure av_frame_free; external avutil_dll;

end.
