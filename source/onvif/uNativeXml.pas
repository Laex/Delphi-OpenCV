{ unit NativeXml

  This is a small-footprint implementation to read and write XML documents
  natively from Delpi Object Pascal code. NativeXml has very fast parsing speeds.

  You can use this code to read XML documents from files, streams or strings.
  The load routine generates events that can be used to display load progress
  on the fly.

  Note #1: this unit is a completely redesigned implementation of legacy NativeXml.
  There is a NativeXmlOld.pas in the source repos which is the legacy code.

  Note #2: any external encoding (ANSI, UTF-16, etc) is converted to an internal
  encoding that is always UTF-8. NativeXml uses Utf8String as string type internally,
  and converts from strings with external encoding in the parsing process.
  When writing, Utf8String strings are converted to the external encoding strings,
  if the encoding was set beforehand, or defaults to UTF-8 if no encoding was set.

  Note #3: the character data is always normalized inside the document (just a $0A
  instead of $0D$0A in Windows for end-of-lines). If EolStyle = esCRLF, the
  data is un-normalized before it gets consumed. If you need no un-normalisation
  (and after all it is non-optimal) you can use EolStyle = esLF (default).

  Note #4: Binary XML: Since NativeXml v4.00, you can use the binary file format of
  NativeXml, named BXM.
  BXM has these advantages:
  - No need to parse the plain text-based XML file.
  - The binary format avoids repeated instances of duplicate strings and
  thus allows a compact representation of all the XML element types.
  Furthermore, the stringtable is sorted by frequency before storing
  BXM to file, which compacts the format even more (smaller indices for
  more frequent strings, so less space in the file).
  - BXM allows options "none" (no compression), "zlib" (zlib compression,
  aka "deflate"), and other compression schemes based on event handlers.
  This allows for ~50% of the total conventional xml size for "none" and
  ~15% of the size for "zlib". So a huge size reduction.
  - In (near) future, BXM will allow other data formats besides "string",
  e.g. data formats Date, DateTime, Base64Binary, HexBinary and Decimal.
  This will reduce the binary size even more, esp for xml files that use
  Base64 for binary content.
  - My aim is to keep the BXM file format backwards compatible, so you can
  always open BXM files based on earlier versions.
  - BXM allows external encryption/compression thru event handlers. f.i. AES
  encryption is handled in functions TNativeXml.AeszEncode / AeszDecode.

  Note #5: sdStreams, sdStringTable and sdDebug are now incorporated in NativeXml.

  Author: Nils Haeck M.Sc.
  Creation Date: 01apr2003
  Major Rewrite: 10nov2010

  Contributor(s):
  Marius Z: devised and helped with the LINQ-like stackable NodeNewXYZ
  functions in TNativeXml
  Stefan Glienke: TDateTime methods use GetTimeZoneInformation
  Hans-Dieter Karl (hdk): added additional Ansi/Wide/Int64/DateTime functions, some fixes
  Alessandro Savoiardo (Ecosoft): added compatibility for posix (Mac), and a fix

  It is NOT allowed under ANY circumstances to publish, alter or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.

  Copyright (c) 2003 - 2013 Simdesign B.V. (www.simdesign.nl)
}
unit uNativeXml;

interface

{ simdesign.inc

  include file for many simdesign projects
  default path: \simlib\general

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 - 2013 Simdesign B.V.

}

// unicode avoid implicit string cast warning
{$IFDEF UNICODE}
{$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF UNICODE}
// Delphi and FPC versions

// Freepascal
{$IFDEF FPC}
{$MODE DELPHI}
{$DEFINE D7UP}
{$ENDIF FPC}
// Delphi 5
{$IFDEF VER130}
{$DEFINE D5UP}
{$ENDIF}
// Delphi 6
{$IFDEF VER140}
{$DEFINE D5UP}
{$ENDIF}
// Delphi 7
{$IFDEF VER150}
{$DEFINE D7UP}
{$ENDIF}
// Delphi 8
{$IFDEF VER160}
{$DEFINE D7UP}
{$ENDIF}
// Delphi 2005 / 9
{$IFDEF VER170}
{$DEFINE D7UP}
{$ENDIF}
// Delphi 2006 / 10
{$IFDEF VER180}
{$DEFINE D7UP}
// D10 publishes OnMouseEnter / OnMouseLeave
{$DEFINE D10UP}
{$ENDIF}
// Delphi 2007 Rad studio / 11?
{$IFDEF VER185}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$ENDIF}
// Delphi 2007 - NET / 11?
{$IFDEF VER190}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$ENDIF}
// Delphi 2009 / 12
// first UNICODE version, so then directive UNICODE is defined, no need for directive D12UP
{$IFDEF VER200}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$ENDIF}
// Delphi 2010 / 14?
{$IFDEF VER210}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$ENDIF}
// Delphi XE / 15
{$IFDEF VER220}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE2 / 16
{$IFDEF VER230}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE3 / 17
{$IFDEF VER240}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE4 / 18
{$IFDEF VER250}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE5 / 19
{$IFDEF VER260}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE6 / 20
{$IFDEF VER270}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi XE7 / 21
{$IFDEF VER280}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// Delphi 10.1 Berlin
{$IFDEF VER310}
{$DEFINE D7UP}
{$DEFINE D10UP}
{$DEFINE D15UP}
{$ENDIF}
// define if you want to include the Graphics unit, and graphics-related properties.
{ .$define USEGRAPHICS }

// define if you want to include zlib (=deflate) compression in binary xml.
{ .$define USEZLIB }

// define if you want to include AES encryption in binary xml. AES functionality is
// in 3rd-party file ElAES.pas (Eldos AES). Please observe license details.
{ .$define USEAES }

// define if you want to use tags.
// A Tag is an additional pointer field of TXmlNode that can be used by the application,
// but it is not stored in the xml text.
{ .$define USETAGS }

// define if you want an additional int64 field FSourcePos in each TXmlNode
{ .$define SOURCEPOS }

uses
{$IFDEF USEGRAPHICS}
  Graphics,
{$ENDIF USEGRAPHICS}
{$IFDEF USEZLIB}
  ZLib,
{$ENDIF USEZLIB}
{$IFDEF USEAES}
  // Eldos AES
  ElAES,
{$ENDIF USEAES}
  Classes, Contnrs,

{$IFDEF D5UP}
  // D5 does not define MSWINDOWS
{$DEFINE MSWINDOWS}
{$ENDIF D5UP}
{$IFDEF MSWINDOWS}
  // unit Windows defines MultiByteToWideChar and GetTimeZoneInformation
  Windows,
  // unit WinInet for method LoadFromURL
  WinInet,
{$ELSE MSWINDOWS}
{$IFNDEF POSIX}
  // linux: win32-compatible functions
  NativeXmlWin32Compat,
{$ENDIF POSIX}
{$ENDIF MSWINDOWS}
  SysUtils;
// units from simlib.general (sdStreams, sdStringTable, sdDebug): I have
// incorporated these files in the final NativeXml.pas

const

  // Current version of the NativeXml unit
  cNativeXmlVersion = 'v4.09';
  cNativeXmlDate = '15aug2013';

function sdClassName(AObject: TObject): Utf8String;

type
  // An event that is used to indicate load or save progress.
  TXmlProgressEvent = procedure(Sender: TObject; Position: int64) of object;

  // event used for encoding/decoding binary xml
  TXmlCoderEvent = function(StreamIn, StreamOut: TStream; CodecSize: int64): Utf8String of object;

  // TsdElementType enumerates the different kinds of elements that can be found
  // in the XML document.
  TsdElementType = (xeElement, // 0 normal element <name {attr}>[value][sub-elements]</name>
    xeAttribute, // 1 attribute ( name='value' or name="value")
    xeCharData, // 2 character data in a node
    xeComment, // 3 comment <!--{comment}-->
    xeCData, // 4 literal data <![CDATA[{data}]]>
    xeCondSection, // 5 conditional section <![ IGNORE / INCLUDE [ markup ]]>
    xeDeclaration, // 6 xml declaration <?xml{declaration}?>
    xeStylesheet, // 7 stylesheet <?xml-stylesheet{stylesheet}?>
    xeDocType, // 8 doctype dtd declaration <!DOCTYPE{spec}>
    xeDtdElement, // 9 dtd element <!ELEMENT >
    xeDtdAttList, // 10 dtd attlist <!ATTLIST >
    xeDtdEntity, // 11 dtd entity <!ENTITY >
    xeDtdNotation, // 12 dtd notation <!NOTATION >
    xeInstruction, // 13 processing instruction <?...?>
    xeWhiteSpace, // 14 chardata with only whitespace
    xeQuotedText, // 15 quoted text: "bla" or 'bla'
    xeEndTag, // 16 </...> and signal function in binary xml
    xeError // 17 some error or unknown
    );
  TsdElementTypes = set of TsdElementType;

  // Definition of different methods of string encoding.
  TsdStringEncoding = (seAnsi, // Ansi encoding, e.g. "Windows-1252" or other codepage (1 byte per character)
    seUTF8, // utf-8 (1, 2, 3 or 4 bytes per character)
    seUTF16BE, // utf-16 Big Endian (2 or 4 bytes per character)
    seUTF16LE, // utf-16 Little Endian (2 or 4  bytes per character)
    seUTF32BE, // ucs-4 Big Endian (4 bytes per character)
    seUTF32LE, // ucs-4 Little Endian (4 bytes per character)
    seUCS4_2143, // ucs-4 unusual octet order - 2143 (4 bytes per character)
    seUCS4_3412, // ucs-4 unusual octet order - 3412 (4 bytes per character)
    seEBCDIC // Extended Binary Coded Decimal Interchange Code (1 byte per character)
    );

  // Choose what kind of binary encoding will be used when calling
  // TXmlNode BufferRead and BufferWrite.
  TsdBinaryEncoding = (xbeBase64, { With this encoding, each group of 3 bytes are stored as 4
      characters, requiring 64 different characters. - DEFAULT }
    xbeBinHex { With this encoding, each byte is stored as a hexadecimal
      number, e.g. 0 = 00 and 255 = FF. }
    );

  // Node closing style:
  // ncDefault defaults to what is parsed per element
  // ncFull  looks like <node a="bla"></node> and
  // ncClose looks like <node a="bla"/>
  TsdNodeClosingStyle = (ncDefault, ncFull, ncClose);

  // End-Of-Line style
  TsdEolStyle = (esLF, // write End-Of-Line as just LF (#$0A) like normalised xml
    esCRLF, // write End-Of-Line as CR + LF (#$0D + #$0A), for the windows platform
    esCR // write End-Of-Line as CR (#$0D), for the mac platform
    );

  // Note on TNativeXml.XmlFormat:
  // - xfCompact (default) to save the xml fully compliant and at smallest size
  // - xfReadable writes additional non-significant whitespace so the customer
  // can easily read the xml file with a standard editor.
  // - xfPreserve aims to preserve whitespace data just as it is parsed
  TXmlFormatType = (xfCompact, // Save without any control chars except LF after declaration
    xfReadable, // Save in readable format with indents and end-of-lines
    xfPreserve // Preserve whitespace whenever possible
    );

  // record with info from a Byte order Mark (BOM)
  TsdBomInfo = packed record
    BOM: array [0 .. 3] of byte; // 4 bytes possibly containing the BOM
    Len: integer; // byte length of the BOM
    Encoding: TsdStringEncoding; // which string encoding does the file have?
    HasBOM: boolean; // does a file have a BOM?
  end;

  // quote char style in TsdQuotedText
  TsdQuoteCharStyle = (qsQuote, qsApos);

  TXmlCompareOption = (xcNodeName, xcNodeType, xcNodeValue, xcAttribCount, xcAttribNames, xcAttribValues, xcChildCount, xcChildNames, xcChildValues,
    xcRecursive);
  TXmlCompareOptions = set of TXmlCompareOption;

  TsdXmlBinaryMethod = (bmDefault, // no compression:         'none'
    bmZlib, // zlib compression:       'zlib'
    bmAesz // AES + zlib compression: 'aesz'
    );

  // codepage information (name and codepage record)
  TCodepageInfo = packed record
    Name: Utf8String;
    Codepage: integer;
  end;

const

  // Codepages defined in Windows
  cCodepageInfoCount = 143;
  cCodePageInfo: array [0 .. cCodepageInfoCount - 1] of TCodepageInfo = ((Name: 'IBM037'; Codepage: 37), // 1
    (Name: 'IBM437'; Codepage: 437), (Name: 'IBM500'; Codepage: 500), (Name: 'ASMO-708'; Codepage: 708), (Name: 'ASMO-449+'; Codepage: 709), // 5
    (Name: 'BCON V4'; Codepage: 709), (Name: 'Arabic'; Codepage: 710), (Name: 'DOS-720'; Codepage: 720), (Name: 'ibm737'; Codepage: 737),
    (Name: 'ibm775'; Codepage: 775), // 10
    (Name: 'ibm850'; Codepage: 850), (Name: 'ibm852'; Codepage: 852), (Name: 'IBM855'; Codepage: 855), (Name: 'ibm857'; Codepage: 857),
    (Name: 'IBM00858'; Codepage: 858), (Name: 'IBM860'; Codepage: 860), (Name: 'ibm861'; Codepage: 861), (Name: 'DOS-862'; Codepage: 862),
    (Name: 'IBM863'; Codepage: 863), (Name: 'IBM864'; Codepage: 864), // 20
    (Name: 'IBM865'; Codepage: 865), (Name: 'cp866'; Codepage: 866), (Name: 'ibm869'; Codepage: 869), (Name: 'IBM870'; Codepage: 870),
    (Name: 'windows-874'; Codepage: 874), (Name: 'cp875'; Codepage: 875), (Name: 'shift_jis'; Codepage: 932), (Name: 'gb2312'; Codepage: 936),
    (Name: 'ks_c_5601-1987'; Codepage: 949), (Name: 'big5'; Codepage: 950), // 30
    (Name: 'IBM1026'; Codepage: 1026), (Name: 'IBM01047'; Codepage: 1047), (Name: 'IBM01140'; Codepage: 1140), (Name: 'IBM01141'; Codepage: 1141),
    (Name: 'IBM01142'; Codepage: 1142), (Name: 'IBM01143'; Codepage: 1143), (Name: 'IBM01144'; Codepage: 1144), (Name: 'IBM01145'; Codepage: 1145),
    (Name: 'IBM01146'; Codepage: 1146), (Name: 'IBM01147'; Codepage: 1147), // 40
    (Name: 'IBM01148'; Codepage: 1148), (Name: 'IBM01149'; Codepage: 1149), (Name: 'utf-16'; Codepage: 1200), (Name: 'unicodeFFFE'; Codepage: 1201),
    (Name: 'windows-1250'; Codepage: 1250), (Name: 'windows-1251'; Codepage: 1251), (Name: 'windows-1252'; Codepage: 1252), (Name: 'windows-1253';
    Codepage: 1253), (Name: 'windows-1254'; Codepage: 1254), (Name: 'windows-1255'; Codepage: 1255), // 50
    (Name: 'windows-1256'; Codepage: 1256), (Name: 'windows-1257'; Codepage: 1257), (Name: 'windows-1258'; Codepage: 1258), (Name: 'Johab';
    Codepage: 1361), (Name: 'macintosh'; Codepage: 10000), (Name: 'x-mac-japanese'; Codepage: 10001), (Name: 'x-mac-chinesetrad'; Codepage: 10002),
    (Name: 'x-mac-korean'; Codepage: 10003), (Name: 'x-mac-arabic'; Codepage: 10004), (Name: 'x-mac-hebrew'; Codepage: 10005), // 60
    (Name: 'x-mac-greek'; Codepage: 10006), (Name: 'x-mac-cyrillic'; Codepage: 10007), (Name: 'x-mac-chinesesimp'; Codepage: 10008),
    (Name: 'x-mac-romanian'; Codepage: 10010), (Name: 'x-mac-ukrainian'; Codepage: 10017), (Name: 'x-mac-thai'; Codepage: 10021), (Name: 'x-mac-ce';
    Codepage: 10029), (Name: 'x-mac-icelandic'; Codepage: 10079), (Name: 'x-mac-turkish'; Codepage: 10081), (Name: 'x-mac-croatian'; Codepage: 10082),
    // 70
    (Name: 'utf-32'; Codepage: 12000), (Name: 'utf-32BE'; Codepage: 12001), (Name: 'x-Chinese_CNS'; Codepage: 20000), (Name: 'x-cp20001';
    Codepage: 20001), (Name: 'x_Chinese-Eten'; Codepage: 20002), (Name: 'x-cp20003'; Codepage: 20003), (Name: 'x-cp20004'; Codepage: 20004),
    (Name: 'x-cp20005'; Codepage: 20005), (Name: 'x-IA5'; Codepage: 20105), (Name: 'x-IA5-German'; Codepage: 20106), // 80
    (Name: 'x-IA5-Swedish'; Codepage: 20107), (Name: 'x-IA5-Norwegian'; Codepage: 20108), (Name: 'us-ascii'; Codepage: 20127), (Name: 'x-cp20261';
    Codepage: 20261), (Name: 'x-cp20269'; Codepage: 20269), (Name: 'IBM273'; Codepage: 20273), (Name: 'IBM277'; Codepage: 20277), (Name: 'IBM278';
    Codepage: 20278), (Name: 'IBM280'; Codepage: 20280), (Name: 'IBM284'; Codepage: 20284), // 90
    (Name: 'IBM285'; Codepage: 20285), (Name: 'IBM290'; Codepage: 20290), (Name: 'IBM297'; Codepage: 20297), (Name: 'IBM420'; Codepage: 20420),
    (Name: 'IBM423'; Codepage: 20423), (Name: 'IBM424'; Codepage: 20424), (Name: 'x-EBCDIC-KoreanExtended'; Codepage: 20833), (Name: 'IBM-Thai';
    Codepage: 20838), (Name: 'koi8-r'; Codepage: 20866), (Name: 'IBM871'; Codepage: 20871), // 100
    (Name: 'IBM880'; Codepage: 20880), (Name: 'IBM905'; Codepage: 20905), (Name: 'IBM00924'; Codepage: 20924), (Name: 'EUC-JP'; Codepage: 20932),
    (Name: 'x-cp20936'; Codepage: 20936), (Name: 'x-cp20949'; Codepage: 20949), (Name: 'cp1025'; Codepage: 21025), (Name: 'koi8-u'; Codepage: 21866),
    (Name: 'iso-8859-1'; Codepage: 28591), (Name: 'iso-8859-2'; Codepage: 28592), // 110
    (Name: 'iso-8859-3'; Codepage: 28593), (Name: 'iso-8859-4'; Codepage: 28594), (Name: 'iso-8859-5'; Codepage: 28595), (Name: 'iso-8859-6';
    Codepage: 28596), (Name: 'iso-8859-7'; Codepage: 28597), (Name: 'iso-8859-8'; Codepage: 28598), (Name: 'iso-8859-9'; Codepage: 28599),
    (Name: 'iso-8859-13'; Codepage: 28603), (Name: 'iso-8859-15'; Codepage: 28605), (Name: 'x-Europa'; Codepage: 29001), // 120
    (Name: 'iso-8859-8-i'; Codepage: 38598), (Name: 'iso-2022-jp'; Codepage: 50220), (Name: 'csISO2022JP'; Codepage: 50221), (Name: 'iso-2022-jp';
    Codepage: 50222), (Name: 'iso-2022-kr'; Codepage: 50225), (Name: 'x-cp50227'; Codepage: 50227), (Name: 'euc-jp'; Codepage: 51932),
    (Name: 'EUC-CN'; Codepage: 51936), (Name: 'euc-kr'; Codepage: 51949), (Name: 'hz-gb-2312'; Codepage: 52936), // 130
    (Name: 'GB18030'; Codepage: 54936), (Name: 'x-iscii-de'; Codepage: 57002), (Name: 'x-iscii-be'; Codepage: 57003), (Name: 'x-iscii-ta';
    Codepage: 57004), (Name: 'x-iscii-te'; Codepage: 57005), (Name: 'x-iscii-as'; Codepage: 57006), (Name: 'x-iscii-or'; Codepage: 57007),
    (Name: 'x-iscii-ka'; Codepage: 57008), (Name: 'x-iscii-ma'; Codepage: 57009), (Name: 'x-iscii-gu'; Codepage: 57010), // 140
    (Name: 'x-iscii-pa'; Codepage: 57011), (Name: 'utf-7'; Codepage: 65000), (Name: 'utf-8'; Codepage: 65001)); // 143

  // default charset names for TsdStringEncoding
  cStringEncodingCharsetNames: array [TsdStringEncoding] of Utf8String = ('ansi', 'utf-8', 'unicodeFFFE', 'utf-16', 'utf-32BE', 'utf-32', 'ucs4_2143',
    'ucs4_3412', 'ebcdic');

  // default codecs for TsdStringEncoding if no codepage is given
  cStringEncodingCodePages: array [TsdStringEncoding] of integer = (0 { ansi can be any codepage } , 65001 { utf-8 } , 1201 { unicodeFFFE } ,
    1200 { utf-16 } , 12001 { utf-32BE } , 12000 { utf-32 } , 0 { no codepage for UCS4_2143 } , 0 { no codepage for UCS4_3412 } ,
    0 { ebcdic can be any codepage } );

  // all xml compare options
  xcAll: TXmlCompareOptions = [xcNodeName, xcNodeType, xcNodeValue, xcAttribCount, xcAttribNames, xcAttribValues, xcChildCount, xcChildNames,
    xcChildValues, xcRecursive];

  // "signature" that defines the binary XML file/stream
  cBinaryXmlCookie: array [0 .. 3] of AnsiChar = '$BXM';

  // Delphi unicode compatibility
{$IFNDEF UNICODE}

type
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$ENDIF UNICODE}

type
  TsdWarnStyle = (wsInfo, wsHint, wsWarn, wsFail);

const
  cWarnStyleNames: array [TsdWarnStyle] of Utf8String = ('info', 'hint', 'warn', 'fail');

type

  // event with debug data
  TsdDebugEvent = procedure(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String) of object;

  // TComponent with debugging capabilities
  TDebugComponent = class(TComponent)
  protected
    FOnDebugOut: TsdDebugEvent;
  public
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String); virtual;
    // Connect to OnDebugOut to get debug information in the client application
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;

  // TPersistent with debugging capabilities
  TDebugPersistent = class(TPersistent)
  protected
    FOwner: TDebugComponent;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String); virtual;
  public
    constructor CreateDebug(AOwner: TDebugComponent); virtual;
  end;

  // XML buffered parser. It buffers the source stream into
  // a memory buffer of limited size and reads from the stream chunk-wise.
  // This way, it can do string comparisons in memory, directly on the buffer.
  TsdXmlParser = class(TDebugPersistent)
  protected
    FBomInfo: TsdBomInfo;
    FSource: TStream;
    FChunkSize: integer;
    FRawBuffer: array of byte; // raw data buffer
    FUtf8Buffer: array of AnsiChar; // utf8 data buffer
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    FRawFirstIdx: integer;
    FRawLastIdx: integer;
    FUtf8FirstIdx: integer;
    FUtf8CurrentIdx: integer;
    FUtf8LastIdx: integer;
    FUtf8BasePosition: int64;
    FBaseLineNumber: int64;
    FEndOfStream: boolean;
    FLastChar0D: boolean;
    FOnDebugOut: TsdDebugEvent;
    function LoCase(Ch: AnsiChar): AnsiChar;
    procedure IncCurrentIdxCheck(var BytesAvail: integer);
    function ReadString(AIndex, ACount: integer): Utf8String;
    function ReadNextChunk: integer;
    procedure EncodeChunk;
    function GetPosition: int64;
    function GetLineNumber: int64;
    procedure SetCodePage(const Value: integer);
  public
    constructor Create(ASource: TStream; AChunkSize: integer); virtual;
    destructor Destroy; override;
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
    property Owner: TDebugComponent read FOwner write FOwner;
    // Call flush once in a while, to check if data can be flushed out. Flushing
    // means that the part before the current pointer is removed and the bytes
    // following are moved to position 0. It is only actually done when enough
    // chunks are read, and the flushing happens chunk-wise.
    procedure Flush;
    // Is the stream from binary xml?
    function IsBinaryXml: boolean;
    // Make at least one byte available from current position
    function MakeDataAvailable: integer;
    // Get the next character from the stream
    function NextChar: AnsiChar;
    // Check if the stream at this position contains string S. If so, the stream
    // will be positioned after, if not, it will remain where it is.
    function CheckString(const S: Utf8String): boolean;
    // Move one position back in the stream
    procedure MoveBack;
    // Read a string from the stream until Terminator is found. The string returned
    // will be the part before Terminator, the stream is positioned after Terminator
    function ReadStringUntil(const Terminator: Utf8String): Utf8String;
    // Read a quoted string from the stream, return the unquoted string
    function ReadQuotedString(AQuote: AnsiChar): Utf8String;
    // Read a string from the stream until character AChar is encountered.
    // var EOS will be True if the stream reached the end.
    function ReadStringUntilChar(AChar: AnsiChar): Utf8String;
    // The encoding detected in the source stream (valid after ReadBOM or after
    // the declaration).
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    // CodePage used in text processing
    property Codepage: integer read FCodePage write SetCodePage;
    // Position in the stream in bytes from the start.
    property Position: int64 read GetPosition;
    // Line number in the stream. Lines are detected by analysing the stream
    // for occurances of #13 (CR). The line number is *calculated* when this
    // property is read, so it should not be read very regularly.
    property LineNumber: int64 read GetLineNumber;
    // Is the end of the stream detected?
    property EndOfStream: boolean read FEndOfStream;

    // Special parser procedures to parse XML content.

    // Read the next character, skip any blanks inbetween. Blanks are:
    // #$09, #$0A, #$0D, #$20
    function NextCharSkipBlanks(var Blanks: Utf8String): AnsiChar;
    // Read BOM (Byte Order Mark) from the start of the file in order to detect which
    // encoding is used.
    procedure ReadBOM;
    // Read an new tag from the stream (from the position afer "<")
    function ReadOpenTag: TsdElementType;
    // Read a string from the stream until a blank char, or a "/" or a ">" is
    // encountered.
    function ReadStringUntilBlankOrEndTag: Utf8String;
    // Info from Byte Order Mark (BOM)
    property BomInfo: TsdBomInfo read FBomInfo;
  end;

  // TsdFastMemStream deals differently with capacity compared to a normal
  // TMemoryStream; it increases the capacity with the natural growing function
  // (fibonacci) each time, and has an initial capacity of $1000. The initial
  // capacity is configurable with the create parameter.
  TsdFastMemStream = class(TStream)
  private
    FMemory: Pointer;
    FPosition: longint;
    FFib1: longint;
    FCapacity: longint;
    FSize: longint;
  protected
    procedure SetCapacity(Value: longint);
    procedure SetSize(NewSize: longint); override;
  public
    constructor Create(InitialCapacity: longint = $1000);
    destructor Destroy; override;
    procedure Clear;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: Word): longint; override;
    procedure LoadFromFile(AFilename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(AFilename: string);
    procedure SaveToStream(Stream: TStream);
    property Memory: Pointer read FMemory;
    property Size: longint read FSize write SetSize;
  end;

  // Delphi's implementation of TStringStream is severely flawed, it does a SetLength
  // on each write, which slows down everything to a crawl. This implementation over-
  // comes this issue.
  TsdStringStream = class(TsdFastMemStream)
  public
    constructor Create(const S: Utf8String);
    function DataString: Utf8String;
  end;

  // TsdBufferWriter is a buffered stream that takes another stream (ASource)
  // and writes only buffer-wise to it, and writes to the stream are first
  // done to the buffer. This stream type can only support writing.
  TsdBufferWriter = class(TsdFastMemStream)
  private
    FSource: TStream;
    FChunkSize: integer;
    FRawBuffer: array of byte;
    FRawPosition: integer;
  protected
    procedure WriteChunk(Count: integer);
  public
    // Create the buffered writer stream by passing the destination stream in ASource,
    // this destination stream must already be initialized.
    constructor Create(ASource: TStream; AChunkSize: integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
  end;

  // specialized buffered writer that obeys encoding and codepage
  TsdXmlWriter = class(TsdBufferWriter)
  private
    FOwner: TDebugComponent;
    FRawBuffer: array of byte;
    FRawBufferSize: integer;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
  public
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    constructor Create(AOwner: TDebugComponent; ASource: TStream; AChunkSize: integer);
    destructor Destroy; override;
    // overridden Write for all supported encodings (ansi, utf8, utf16le, utf16be)
    function Write(const Buffer; Count: longint): longint; override;
  end;

  {
    NativeXmlNodes:
    Parsing and writing methods for XML nodes:
    Attribute, CDATA, CharData, Comment, Declaration, DocType,
    DTD-AttList, DTD-Element, DTD-Entity, DTD-Notation, Element,
    (processing) Instruction, QuotedText, StyleSheet and ancestors:

    TXmlNode
    TsdContainerNode
    TsdElement
    TsdDtdElement
    TsdDtdAttList
    TsdDtdEntity
    TsdDtdNotation
    TsdDocType
    TsdDeclaration
    TsdStyleSheet
    TsdCharData
    TsdQuotedText
    TsdProcessingInstruction
    TsdComment
    TsdCData
    TsdConditionalSection
    TsdAttribute
  }

  // Forward declaration TsdAttribute (needed by TXmlNode)
  TsdAttribute = class;

  // Forward declaration TsdElement (needed by TXmlNode)
  TsdElement = class;

  // TXmlNode metaclass
  TsdNodeClass = class of TXmlNode;

  // Forward declaration of TNativeXml
  TNativeXml = class;

  // Forward declaration of TXmlNode
  TXmlNode = class;

  // Pass a function of this kind to TXmlNode.SortChildNodes. The function should
  // return -1 if Node1 < Node2, 0 if Node1 = Node2 and 1 if Node1 > Node2.
  TXmlNodeCompareFunction = function(Node1, Node2: TXmlNode): integer;

  // TXmlNode is the ancestor for all nodes in the xml document. See TsdElement
  // for the elements, TsdAttribute for the attributes.
  TXmlNode = class(TDebugPersistent)
  private
    // inherited from TDebugPersistent: FOwner: TDebugComponent
    function GetAttributeByName(const AName: Utf8String): TsdAttribute;
    function GetAttributeValueByName(const AName: Utf8String): Utf8String;
    function GetAttributeValueByNameWide(const AName: Utf8String): UnicodeString;
    procedure SetAttributeValueByName(const AName, Value: Utf8String);
    procedure SetAttributeValueByNameWide(const AName: Utf8String; const Value: UnicodeString);
    function GetBinaryString: RawByteString;
    procedure SetBinaryString(const Value: RawByteString);
    function GetNameUnicode: UnicodeString;
    procedure SetNameUnicode(const Value: UnicodeString);
    function GetValueUnicode: UnicodeString;
    procedure SetValueUnicode(const Value: UnicodeString);
    function GetAttributes(Index: integer): TsdAttribute;
    function GetAttributeName(Index: integer): Utf8String;
    function GetAttributeValue(Index: integer): Utf8String;
    procedure SetAttributeName(Index: integer; const Value: Utf8String);
    procedure SetAttributeValue(Index: integer; const Value: Utf8String);
    function GetAttributeValueAsInteger(Index: integer): integer;
    procedure SetAttributeValueAsInteger(Index: integer; const Value: integer);
    function GetWriteOnDefault: boolean;
    procedure SetWriteOnDefault(const Value: boolean);
    function GetName: Utf8String; virtual;
    function GetValue: Utf8String; virtual;
    procedure SetName(const Value: Utf8String); virtual;
    procedure SetValue(const Value: Utf8String); virtual;
    procedure DoProgress(Position: int64);
    function GetParentNode(ADepth: integer): TXmlNode;
    function GetEolStyle: TsdEolStyle;
    function GetPreserveWhiteSpace: boolean;
    function GetXmlFormat: TXmlFormatType;
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);
    function GetContent: Utf8String; virtual;
    function GetDirectNodeCount: integer; virtual;
    function GetContainerCount: integer; virtual;
    function GetContainers(Index: integer): TXmlNode; virtual;
    function GetElementCount: integer; virtual;
    function GetElements(Index: integer): TsdElement; virtual;
    function GetDocument: TNativeXml;
    function GetSourcePos: int64;
  protected
    FParent: TXmlNode;
{$IFDEF USETAGS}
    FTag: Pointer;
{$ENDIF USETAGS}
{$IFDEF SOURCEPOS}
    FSourcePos: int64;
{$ENDIF SOURCEPOS}
    // string table lookup methods
    function GetString(AID: integer): Utf8String;
    function AddString(const S: Utf8String): integer;
    function GetNodeCount: integer; virtual;
    function GetAttributeCount: integer; virtual;
    function GetNodes(Index: integer): TXmlNode; virtual;
    class function EscapeString(const S: Utf8String): Utf8String;
    class function ReplaceString(const S: Utf8String): Utf8String;
    function GetIndent: Utf8String; virtual;
    function GetEndOfLine: Utf8String; virtual;
    function GetSeparator: Utf8String; virtual;
    function NodeIndexByName(const AName: Utf8String): integer; virtual;
    procedure WriteValue(const AName, AValue: Utf8String); virtual;
    procedure WriteContent(S: TStream); virtual;
    function CompareNodeName(const NodeName: Utf8String): integer;
    function GetFullPath: Utf8String;
    property WriteOnDefault: boolean read GetWriteOnDefault write SetWriteOnDefault;
    function GetParentNodeName(ADepth: integer): Utf8String;
    // Create a new node object. AOwner must be the TNativeXml that is
    // going to hold this new node. Make sure to use the correct class when
    // creating, e.g. TsdElement.Create(Owner) for an element.
    constructor Create(AOwner: TComponent); virtual;
    // Create a new TXmlNode with name AName. AOwner must be the TNativeXml
    // that is going to hold this new node.
    constructor CreateName(AOwner: TNativeXml; const AName: Utf8String); virtual;
    // Create a new TXmlNode with name AName and UTF8String value AValue. AOwner
    // must be the TNativeXml that is going to hold this new node.
    constructor CreateNameValue(AOwner: TNativeXml; const AName, AValue: Utf8String); virtual;
  public
    // copy the data and subnodes from ANode; this node is cleared first
    procedure CopyFrom(ANode: TObject); virtual;
    // Create a node as a child of AParent. AOwner must be TNativeXml or descendant,
    // AParent is TXmlNode
    constructor CreateParent(AOwner: TComponent; AParent: TXmlNode); virtual;
    constructor CreateParentNear(AOwner: TComponent; AParent, ANode: TXmlNode; IsBefore: boolean);
    // for compat: assign to source XmlNode
    procedure Assign(Source: TPersistent); override;
    // Convert the Utf8String S to a UnicodeString
    class function Utf8ToWide(const S: Utf8String): UnicodeString;
    // Convert the UnicodeString W to an Utf8String
    class function WideToUtf8(const W: UnicodeString): Utf8String;
    // parse this node with parser P, result is the endnode and should be identical
    function ParseStream(P: TsdXmlParser): TXmlNode; virtual;
    // write this node to stream S
    procedure WriteStream(S: TStream); virtual;
    // The element type
    function ElementType: TsdElementType; virtual;
    // name of the element type
    function ElementTypeName: Utf8String;
    // write the node to a Utf8String
    function WriteToString: Utf8String;
    // Pointer to the owner document NativeXml
    property Document: TNativeXml read GetDocument;
{$IFDEF USETAGS}
    // Tag is a pointer value the developer can use at will. Tag does not get
    // saved to the XML. Tag is often used to point to a GUI element. Tag can
    // cast to anything other than 'pointer' but please be reminded that the size
    // of a pointer can be platform-dependent (ie 32bit vs 64bit) and writing
    // code depending on that is thus also platform-dependent.
    property Tag: Pointer read FTag write FTag;
{$ENDIF USETAGS}
    // SourcePos (int64) points to the position in the source file where the
    // nodes text begins.
    property SourcePos: int64 read GetSourcePos;
    // Parent points to the parent node of the current XML node.
    property Parent: TXmlNode read FParent;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node Name and value are empty.
    function IsClear: boolean;
    // clear the node
    procedure Clear; virtual;
    // recursively delete empty nodes
    procedure DeleteEmptyNodes; virtual;
    // Call Delete to delete this node completely from the parent node list. This
    // call only succeeds if the node has a parent. It has no effect when called for
    // the root node.
    procedure Delete; virtual;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node value is empty.
    function IsEmpty: boolean;
    // Test whether ANode is equal to another node based on compare options. If
    // MismatchNodes is provided, a list of mismatching subnodes is filled.
    function IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList = nil): boolean;
    // Use this method to add an attribute with name AName and string value AValue
    // to the node. AName and AValue must be UTF8 encoded.
    procedure AttributeAdd(const AName, AValue: Utf8String); overload;
    // Use this method to add the attribute AAttribute. AAttribute must be owned by
    // the xml document beforehand.
    procedure AttributeAdd(AAttribute: TsdAttribute); overload; virtual;
    // Add an open array of TsdAttribute objects. Attributes must be owned by
    // the xml document beforehand.
    procedure AttributesAdd(Attributes: array of TsdAttribute);
    // Clear all attributes from the current node.
    procedure AttributesClear; virtual;
    // Use this method to delete the attribute at Index in the list. Index must be
    // equal or greater than 0, and smaller than AttributeCount. Using an index
    // outside of that range has no effect.
    procedure AttributeDelete(Index: integer);
    // Clear all elements from the current node.
    procedure ElementsClear; virtual;
    // Use this method to delete the element at Index in the list. Index must be
    // equal or greater than 0, and smaller than ElementCount. Using an index
    // outside of that range has no effect.
    procedure ElementDelete(Index: integer);
    // Use this method to find the index of an attribute with name AName.
    function AttributeIndexByName(const AName: Utf8String): integer; virtual;
    // Add the node ANode to the nodelist. It will be added at the end, unless
    // it is an attribute, in that case it will be added at the end of the current
    // list of attributes. NodeAdd will set the parent of ANode to itself.
    function NodeAdd(ANode: TXmlNode): integer; virtual;
    // This function returns a pointer to the first subnode that has an attribute with
    // name AttribName and value AttribValue. If ShouldRecurse = True (default), the
    // function works recursively, using the depthfirst method.
    function NodeByAttributeValue(const NodeName, AttribName, AttribValue: Utf8String; ShouldRecurse: boolean = True): TXmlNode; overload;
{$IFDEF D7UP}
    function NodeByAttributeValue(const NodeName, AttribName: Utf8String; const AttribValue: UnicodeString; ShouldRecurse: boolean = True)
      : TXmlNode; overload;
{$ENDIF D7UP}
    // Return a reference to the first subnode in the nodelist that has name AName.
    // If no subnodes with AName are found, the function returns nil.
    function NodeByName(const AName: Utf8String): TXmlNode;
    // Use this procedure to retrieve all nodes that have name AName. Pointers to
    // these nodes are added to the list in AList. AList must be initialized
    // before calling this procedure. If you use a TsdNodeList you don't need
    // to cast the list items to TXmlNode.
    procedure NodesByName(const AName: Utf8String; const AList: TList);
    // Add an open array of TXmlNode objects. Nodes must be owned by the xml document
    // beforehand.
    procedure NodesAdd(Nodes: array of TXmlNode);
    // Delete the subnode at Index. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeDelete(Index: integer); virtual;
    // Extract the subnode at Index. The node will not be freed.
    function NodeExtract(ANode: TXmlNode): TXmlNode; virtual;
    // Remove the subnode. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeRemove(ANode: TXmlNode); virtual;
    // Remove the line of the xml file that has the subnode ANode in it.
    // The subnode and accompanying character data will be freed.
    procedure NodeRemoveEx(ANode: TXmlNode); virtual;
    // Call NodeIndexOf to get the index for ANode in the Nodes list. The first
    // node in the list has index 0, the second item has index 1, and so on. If
    // a node is not in the list, NodeIndexOf returns -1.
    function NodeIndexOf(ANode: TXmlNode): integer; virtual;
    // Insert the node ANode at location Index in the list. Make sure to honour
    // the fact that attributes are also nodes, and should always be first in
    // the list. You can find the number of attributes with AttributeCount.
    procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
    // Switch position of the nodes at Index1 and Index2.
    procedure NodeExchange(Index1, Index2: integer); virtual;
    // This function returns a pointer to the first node with AName. If this node
    // is not found, then it creates a new node with AName and returns its pointer.
    function NodeFindOrCreate(const AName: Utf8String): TXmlNode; virtual;
    // Create a new node with AName, add it to the subnode list, and return a
    // pointer to it.
    function NodeNew(const AName: Utf8String): TXmlNode; virtual;
    // Create a new node with AName, and insert it into the subnode list at location
    // Index, and return a pointer to it.
    function NodeNewAtIndex(Index: integer; const AName: Utf8String): TXmlNode; virtual;
    // Clear (and free) the complete list of subnodes.
    procedure NodesClear; virtual;
    // Find the first node which has name NodeName. Contrary to the NodeByName
    // function, this function will search the whole subnode tree, using the
    // DepthFirst method. It is possible to search for a full path too, e.g.
    // FoundNode := MyNode.FindNode('/Root/SubNode1/SubNode2/ThisNode');
    function FindNode(const NodeName: Utf8String): TXmlNode; virtual;
    // Find all nodes which have name NodeName. Contrary to the NodesByName
    // function, this function will search the whole subnode tree. If you use
    // a TsdNodeList for the AList parameter, you don't need to cast the list
    // items to TXmlNode.
    procedure FindNodes(const NodeName: Utf8String; const AList: TList); virtual;
    // Iterates the next sibling of Node
    function NextSibling(ANode: TXmlNode): TXmlNode; virtual;
    // Return the first subnode with AType, or nil if none
    function FirstNodeByType(AType: TsdElementType): TXmlNode; virtual;
    // Read TreeDepth to find out many nested levels there are for the current XML
    // node. Root has a TreeDepth of zero.
    function TreeDepth: integer;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property Name: Utf8String read GetName write SetName;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property NameUnicode: UnicodeString read GetNameUnicode write SetNameUnicode;
    // The value of the node. For elements this is the element value (based on
    // first chardata fragment), for attributes this is the attribute value. The
    // string is encoded as UTF8. Use ToWide(Node.Value) or Node.ValueUnicode
    // to get a UnicodeString compatible with "unicode" windows methods.
    property Value: Utf8String read GetValue write SetValue;
    // ValueUnicode returns the value of the node as a UnicodeString.
    property ValueUnicode: UnicodeString read GetValueUnicode write SetValueUnicode;
    // List of attributes present in this element. Use AttributeCount to iterate.
    property Attributes[Index: integer]: TsdAttribute read GetAttributes;
    // Get the number of attributes in this node
    property AttributeCount: integer read GetAttributeCount;
    // Get or set the name of the attribute at Index (as UTF8).
    property AttributeName[Index: integer]: Utf8String read GetAttributeName write SetAttributeName;
    // Get or set the value of the attribute at Index (as UTF8).
    property AttributeValue[Index: integer]: Utf8String read GetAttributeValue write SetAttributeValue;
    // Read this property to get the integer value of the attribute at index Index.
    // If the value cannot be converted, 0 will be returned. Write to it to set the
    // integer value.
    property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
    // Get a reference to an attribute node by its name. If there is no attribute
    // with that name, nil will be returned.
    property AttributeByName[const AName: Utf8String]: TsdAttribute read GetAttributeByName;
    // Get the value of an attribute with name AName. If no attribute is present,
    // an empty string is returned. When setting this value, an attribute is
    // created if it does not yet exist.
    property AttributeValueByName[const AName: Utf8String]: Utf8String read GetAttributeValueByName write SetAttributeValueByName;
    property AttributeValueByNameWide[const AName: Utf8String]: UnicodeString read GetAttributeValueByNameWide write SetAttributeValueByNameWide;
    // Use HasAttribute to determine if the node has an attribute with name AName.
    function HasAttribute(const AName: Utf8String): boolean; virtual;
    // List of subnodes, by index. Iterate through the list using NodeCount
    // and this property. The attributes are listed first, then followed by
    // all other node types, in the order as found in the XML document.
    property Nodes[Index: integer]: TXmlNode read GetNodes; default;
    // Get number of sub-elements present in this node.
    property ElementCount: integer read GetElementCount;
    // List of sub-elements, by index.
    property Elements[Index: integer]: TsdElement read GetElements;
    // Get number of subnodes present in this node (this includes attributes,
    // cdata, char-data, sub-elements, etcetera).
    property NodeCount: integer read GetNodeCount;
    // content of the node (raw source without the pre- and post matter)
    property Content: Utf8String read GetContent;
    // Fullpath will return the complete path of the node from the root, e.g.
    // /Root/SubNode1/SubNode2/ThisNode
    property FullPath: Utf8String read GetFullPath;
    // direct node count (aka the attributes and optional whitespace inbetween)
    property DirectNodeCount: integer read GetDirectNodeCount;
    // (child) container count
    property Containers[Index: integer]: TXmlNode read GetContainers;
    property ContainerCount: integer read GetContainerCount;

    // Get/Set ValueAsXYZ functions

    // Convert the node's value to boolean and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsBoolDef(ADefault: boolean): boolean; virtual;
    // Convert the node's value to a double and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsFloatDef(ADefault: double): double; virtual;
    // Convert the node's value to a TDateTime and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsDateTimeDef(ADefault: TDateTime): TDateTime; virtual;
    // Convert the node's value to integer and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsIntegerDef(ADefault: integer): integer; virtual;
    // Convert the node's value to int64 and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsInt64Def(ADefault: int64): int64; virtual;
    // Convert the node's value to boolean and return the result.
    function GetValueAsBool: boolean; virtual;
    // Convert the node's value to a double and return the result.
    function GetValueAsFloat: double; virtual;
    // Convert the node's value to a TDateTime and return the result.
    function GetValueAsDateTime: TDateTime; virtual;
    // Convert the node's value to integer and return the result.
    function GetValueAsInteger: integer; virtual;
    // Convert the node's value to int64 and return the result.
    function GetValueAsInt64: int64; virtual;
    // Store AValue as boolean
    procedure SetValueAsBool(const AValue: boolean); virtual;
    // Store AValue as float
    procedure SetValueAsFloat(const AValue: double); virtual;
    // Store AValue as Date
    procedure SetValueAsDate(const AValue: TDateTime); virtual;
    // Store AValue as Time
    procedure SetValueAsTime(const AValue: TDateTime); virtual;
    // Store AValue as DateTime
    procedure SetValueAsDateTime(const AValue: TDateTime); virtual;
    // Store AValue as Integer
    procedure SetValueAsInteger(const AValue: integer); virtual;
    // Store AValue as Int64
    procedure SetValueAsInt64(const AValue: int64); virtual;

    // ValueAsXYZ properties

    // Read and store existent value as boolean
    property ValueAsBool: boolean read GetValueAsBool write SetValueAsBool;
    // Read and store existent value as float
    property ValueAsFloat: double read GetValueAsFloat write SetValueAsFloat;
    // Store existent value as Date
    property ValueAsDate: TDateTime write SetValueAsDate;
    // Store existent value as Time
    property ValueAsTime: TDateTime write SetValueAsTime;
    // Read and store existent value as DateTime
    property ValueAsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
    // Read and store existent value as Integer
    property ValueAsInteger: integer read GetValueAsInteger write SetValueAsInteger;
    // Read and store existent value as Int64
    property ValueAsInt64: int64 read GetValueAsInt64 write SetValueAsInt64;

    // ReadXYZ functions

    // Find the attribute with AName, and convert its value to a boolean. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeBool(const AName: Utf8String; ADefault: boolean = False): boolean; virtual;
    // Find the attribute with AName, and convert its value to an integer. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeInteger(const AName: Utf8String; ADefault: integer = 0): integer; virtual;
    function ReadAttributeInt64(const AName: Utf8String; ADefault: int64 = 0): int64; virtual; // added by hdk
    // Find the attribute with AName, and convert its value to a float. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeFloat(const AName: Utf8String; ADefault: double = 0): double; virtual;
    // Find the attribute with AName. If the attribute is not found, ADefault will
    // be returned.
    function ReadAttributeString(const AName: Utf8String; ADefault: Utf8String = ''): Utf8String; virtual;
    function ReadAttributeUnicodeString(const AName: Utf8String; ADefault: UnicodeString = ''): UnicodeString; virtual; // added by hdk
    function ReadAttributeAnsiString(const AName: Utf8String; ADefault: AnsiString = ''): AnsiString; virtual; // added by hdk
    // Read the subnode with AName and convert it to a boolean value. If the
    // subnode is not found, or cannot be converted, the boolean ADefault will
    // be returned.
    function ReadAttributeDateTime(const AName: Utf8String; ADefault: TDateTime = 0): TDateTime; virtual; // added by hdk

    function ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean; virtual;
{$IFDEF USEGRAPHICS}
    // Read the properties Color, Mode, Style and Width for the TPen object APen
    // from the subnode with AName.
    procedure ReadPen(const AName: Utf8String; APen: TPen); virtual;
    // Read the properties Color and Style for the TBrush object ABrush from the
    // subnode with AName.
    procedure ReadBrush(const AName: Utf8String; ABrush: TBrush); virtual;
    // Read the subnode with AName and convert its value to TColor. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadColor(const AName: Utf8String; ADefault: TColor = clBlack): TColor; virtual;
{$ENDIF USEGRAPHICS}
    // Read the subnode with AName and convert its value to TDateTime. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadDateTime(const AName: Utf8String; ADefault: TDateTime = 0): TDateTime; virtual;
    // Read the subnode with AName and convert its value to a double. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadFloat(const AName: Utf8String; ADefault: double = 0.0): double; virtual;
    // Read the subnode with AName and convert its value to an integer. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadInteger(const AName: Utf8String; ADefault: integer = 0): integer; virtual;
    function ReadInt64(const AName: Utf8String; ADefault: int64 = 0): int64; virtual; // added by hdk
    // Read the subnode with AName and return its UTF8String value. If the subnode is
    // not found, ADefault will be returned.
    function ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String; virtual;
    // Read the subnode with AName and return its UnicodeString value. If the subnode is
    // not found, ADefault will be returned.
    function ReadUnicodeString(const AName: Utf8String; const ADefault: UnicodeString = ''): UnicodeString; virtual;
    function ReadAnsiString(const AName: Utf8String; const ADefault: AnsiString = ''): AnsiString; virtual; // added by hdk

    // WriteXYZ functions

    // If the attribute with name AName exists, then set its value to the integer
    // AValue. If it does not exist, then create a new attribute AName with the
    // integer value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeInteger(const AName: Utf8String; AValue: integer; ADefault: integer = 0); virtual;
    procedure WriteAttributeInt64(const AName: Utf8String; AValue: int64; ADefault: int64 = 0); virtual; // added by hdk
    // If the attribute with name AName exists, then set its value to the float
    // AValue. If it does not exist, then create a new attribute AName with the
    // float value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeFloat(const AName: Utf8String; AValue: double; ADefault: double = 0); virtual;
    // If the attribute with name AName exists, then set its value to the string
    // AValue. If it does not exist, then create a new attribute AName with the
    // string value with quotes. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeString(const AName: Utf8String; AValue: Utf8String; ADefault: Utf8String = ''); virtual;
    procedure WriteAttributeUnicodeString(const AName: Utf8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
    procedure WriteAttributeAnsiString(const AName: Utf8String; const AValue: AnsiString; const ADefault: AnsiString = ''); virtual; // added by hdk
    // If the attribute with name AName exists, then set its value to the TDateTime
    // AValue. If it does not exist, then create a new attribute AName with the
    // TDateTime value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeDateTime(const AName: Utf8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual; // changed by hdk
    // If the attribute with name AName exists, then set its value to the boolean
    // AValue. If it does not exist, then create a new attribute AName with the
    // boolean value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeBool(const AName: Utf8String; AValue: boolean; ADefault: boolean = False); virtual;
    // Add or replace the subnode with AName and set its value to represent the boolean
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteBool(const AName: Utf8String; AValue: boolean; ADefault: boolean = False); virtual;
{$IFDEF USEGRAPHICS}
    // Write properties Color, Mode, Style and Width of the TPen object APen to
    // the subnode with AName. If AName does not exist, it will be created.
    procedure WritePen(const AName: Utf8String; APen: TPen); virtual;
    // Write properties Color and Style of the TBrush object ABrush to the subnode
    // with AName. If AName does not exist, it will be created.
    procedure WriteBrush(const AName: Utf8String; ABrush: TBrush); virtual;
    // Add or replace the subnode with AName and set its value to represent the TColor
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteColor(const AName: Utf8String; AValue: TColor; ADefault: TColor = clBlack); virtual;
{$ENDIF USEGRAPHICS}
    // Add or replace the subnode with AName and set its value to represent the TDateTime
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    // The XML format used is compliant with W3C's specification of date and time.
    procedure WriteDateTime(const AName: Utf8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the double
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteFloat(const AName: Utf8String; AValue: double; ADefault: double = 0.0); virtual;
    // Add or replace the subnode with AName and set its value to represent the hexadecimal representation of
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteHex(const AName: Utf8String; AValue, Digits: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the integer
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInteger(const AName: Utf8String; AValue: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the int64
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInt64(const AName: Utf8String; AValue: int64; ADefault: int64 = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the UTF8String
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteString(const AName, AValue: Utf8String; const ADefault: Utf8String = ''); virtual;
    // Add or replace the subnode with AName and set its value to represent the UnicodeString
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteUnicodeString(const AName: Utf8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
    procedure WriteAnsiString(const AName: Utf8String; const AValue: AnsiString; const ADefault: AnsiString = ''); virtual; // added by hdk

    // Returns the length of the data in the buffer, once it would be decoded by
    // the Base64 method. The length of the unencoded data is determined from the
    // length of the encoded data. Base64 must use the padding characters.
    function BufferLength: integer; virtual;
    // Use this method to read binary data from the node into Buffer with a length of Count.
    procedure BufferRead(var Buffer; Count: integer; BinaryEncoding: TsdBinaryEncoding = xbeBase64); virtual;
    // Use this method to write binary data in Buffer with a length of Count to the
    // current node. The data will appear as text using Base64 method in the final XML document.
    procedure BufferWrite(const Buffer; Count: integer); virtual;
    // Use BinaryString to add/extract binary data in an easy way to/from the node. Internally the
    // data gets stored as Base64-encoded data. Do not use this method for normal textual
    // information, it is better to use ValueAsString in that case (adds less overhead).
    property BinaryString: RawByteString read GetBinaryString write SetBinaryString;
    // return the index of the node in its parent
    function IndexInParent: integer;
    // sort the child nodes based on a compare function. If Compare = nil, just
    // alphabetical compare is used.
    procedure SortChildNodes(Compare: TXmlNodeCompareFunction);
  end;

  // List of nodes
  TsdNodeList = class(TObjectList)
  private
    function GetItems(Index: integer): TXmlNode;
    function GetNextSiblingOf(ANode: TXmlNode): TXmlNode;
    function GetLastSiblingOf(ANode: TXmlNode): TXmlNode;
  public
    // TsdNodeList has a different default than TObjectList
    // since 'AOwnsObjects' should usually be false in client code
    constructor Create(AOwnsObjects: boolean = False); virtual;
    // ByType returns the first item in the list that has element type AType.
    // If no item is found, the function returns nil.
    function ByType(AType: TsdElementType): TXmlNode;
    function FindFirst: TXmlNode;
    function FindNext(ANode: TXmlNode): TXmlNode;
    property Items[Index: integer]: TXmlNode read GetItems; default;
  end;

  TsdXmlNodeEvent = procedure(Sender: TObject; ANode: TXmlNode) of object;

  // Node representing a xml char-data fragment
  TsdCharData = class(TXmlNode)
  private
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure SetValue(const Value: Utf8String); override;
  protected
    FValueID: integer; // core value ID
    // the core value is the escaped, eol-normalized value
    function GetCoreValue: Utf8String; virtual;
    // PlatformValue is unnormalized CoreValue
    function GetPlatformValue: Utf8String; virtual;
    procedure SetCoreValue(const Value: Utf8String); virtual;
  public
    procedure CopyFrom(ANode: TObject); override;
    destructor Destroy; override;
    function IsWhiteSpace: boolean; virtual;
    function GetValueUsingReferences(Nodes: array of TXmlNode): Utf8String;
    function ElementType: TsdElementType; override;
    function HasNonStandardReferences: boolean;
    procedure WriteStream(S: TStream); override;
  end;

  // Node representing whitespace chardata
  TsdWhiteSpace = class(TsdCharData)
  public
    function ElementType: TsdElementType; override;
  end;

  // Node representing quoted text ('bla' or "bla")
  TsdQuotedText = class(TsdCharData)
  private
    FQuoteStyle: TsdQuoteCharStyle;
    function GetName: Utf8String; override;
  protected
  public
    procedure CopyFrom(ANode: TObject); override;
    constructor Create(AOwner: TComponent); override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml attribute.
  TsdAttribute = class(TXmlNode)
  private
    FCoreValue: TsdQuotedText;
  protected
    FNameID: integer;
    function GetName: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
    constructor Create(AOwner: TComponent); override;
  public
    procedure CopyFrom(ANode: TObject); override;
    destructor Destroy; override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // TsdContainerNode is the base class for all element types that can have
  // sub-nodes.
  TsdContainerNode = class(TXmlNode)
  private
    FDirectNodeCount: integer;
    FValueIndex: integer;
    FNodeClosingStyle: TsdNodeClosingStyle;
  protected
    // list of subnodes: direct nodes first, then subelements
    FNodes: TsdNodeList;
    function ParseAttributeList(P: TsdXmlParser): AnsiChar; virtual;
    function ParseFixStructuralErrors(const AEndTagName: Utf8String): TXmlNode;
    // parse the element list; the result (endtag) should be this element
    function ParseElementList(P: TsdXmlParser; const SupportedTags: TsdElementTypes): TXmlNode; virtual;
    // parses the value in descendants TsdElement and TsdDocType
    procedure ParseIntermediateData(P: TsdXmlParser); virtual;
    function ParseQuotedTextList(P: TsdXmlParser): AnsiChar; virtual;
    procedure WriteAttributeList(S: TStream; Count: integer); virtual;
    function GetNodeCount: integer; override;
    function GetNodes(Index: integer): TXmlNode; override;
    function HasSubContainers: boolean; virtual;
    property NodeList: TsdNodeList read FNodes;
    // count of the attributes
    function GetDirectNodeCount: integer; override;
    function GetContainers(Index: integer): TXmlNode; override;
    function GetContainerCount: integer; override;
    function GetElements(Index: integer): TsdElement; override;
    function GetElementCount: integer; override;
    function GetNodeClosingStyle: TsdNodeClosingStyle; virtual;
    constructor Create(AOwner: TComponent); override;
  public
    procedure CopyFrom(ANode: TObject); override;
    destructor Destroy; override;
    procedure Clear; override;
    // Use this method to add the attribute AAttribute. AAttribute must be owned by
    // the xml document beforehand.
    procedure AttributeAdd(AAttribute: TsdAttribute); override;
    // default node addition in a container node
    function NodeAdd(ANode: TXmlNode): integer; override;
    procedure NodeDelete(Index: integer); override;
    function NodeExtract(ANode: TXmlNode): TXmlNode; override;
    function NodeIndexOf(ANode: TXmlNode): integer; override;
    procedure NodeInsert(Index: integer; ANode: TXmlNode); override;
    procedure NodeInsertNear(ANode, AOther: TXmlNode; IsBefore: boolean);
    procedure NodeExchange(Index1, Index2: integer); override;
    procedure NodesClear; override;
    function FirstNodeByType(AType: TsdElementType): TXmlNode; override;
    function NextSibling(ANode: TXmlNode): TXmlNode; override;
    property NodeClosingStyle: TsdNodeClosingStyle read GetNodeClosingStyle write FNodeClosingStyle;
  end;

  // Node representing an xml element.
  TsdElement = class(TsdContainerNode)
  private
    FNameID: integer;
  protected
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure SetValue(const Value: Utf8String); override;
    procedure ParseIntermediateData(P: TsdXmlParser); override;
  public
    procedure CopyFrom(ANode: TObject); override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml declaration, e.g. <?xml version="1.0"?>
  TsdDeclaration = class(TsdContainerNode)
  private
    function GetEncoding: Utf8String;
    function GetVersion: Utf8String;
    procedure SetEncoding(const Value: Utf8String);
    procedure SetVersion(const Value: Utf8String);
  protected
    function GetName: Utf8String; override;
  public
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
    property Version: Utf8String read GetVersion write SetVersion;
    // encoding aka charset
    property Encoding: Utf8String read GetEncoding write SetEncoding;
  end;

  // Node representing an xml comment. Get/set Value for the comment.
  TsdComment = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
  public
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing a CData element. Get/Set value for the data in CDATA.
  TsdCData = class(TsdComment)
  protected
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
  public
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Conditional Section (todo)
  TsdConditionalSection = class(TsdComment)
  end;

  // DocType declaration element. It can have sub-nodes with dtd elements,
  // entities, notations, etc.
  TsdDocType = class(TsdContainerNode)
  private
    FNameID: integer;
    FExternalID: TsdCharData;
    FSystemLiteral: TsdQuotedText;
    FPubIDLiteral: TsdQuotedText;
  protected
    constructor Create(AOwner: TComponent); override;
    function GetName: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure ParseIntermediateData(P: TsdXmlParser); override;
  public
    procedure CopyFrom(ANode: TObject); override;
    destructor Destroy; override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
    // External ID: either SYSTEM or PUBLIC
    property ExternalID: TsdCharData read FExternalID;
    // The system literal without quotes
    property SystemLiteral: TsdQuotedText read FSystemLiteral;
    // The PubID literal without quotes
    property PubIDLiteral: TsdQuotedText read FPubIDLiteral;
  end;

  // DTD Element declaration
  TsdDtdElement = class(TsdElement)
  protected
    function GetValue: Utf8String; override;
    procedure ParseContent(P: TsdXmlParser); virtual;
    procedure WriteContent(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
  end;

  // DTD AttList declaration
  TsdDtdAttList = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Entity declaration
  TsdDtdEntity = class(TsdDtdElement)
  protected
    procedure ParseContent(P: TsdXmlParser); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Notation declaration
  TsdDtdNotation = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // (processing) instruction
  TsdInstruction = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
  public
    function ElementType: TsdElementType; override;
    function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
  end;

  // TsdStyleSheet
  TsdStyleSheet = class(TsdDeclaration)
  protected
    function GetName: Utf8String; override;
  public
    // function ParseStream(P: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Forward declaration of TsdBinaryXml
  TsdBinaryXml = class;

  // A symbol table, holding a collection of unique strings, sorted in 2 ways
  // for fast access. Strings can be added with AddString or AddStringRec.
  // When a string is added or updated, an ID is returned which the application
  // can use to retrieve the string, using GetString.
  TsdSymbolTable = class(TDebugComponent)
  private
    FByID: TObjectList;
    FBySymbol: TObjectList;
    FPluralSymbolCount: integer;
    function GetSymbolCount: integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Clear the string table
    procedure Clear;

    // Add a potentially new string S to the table, the function
    // returns its string ID.
    function AddString(const S: Utf8String): integer;

    // retrieve the string based on its string ID. The string ID is only unique
    // within this string table, so do not use IDs from other tables.
    function GetString(ID: integer): Utf8String;

    // total number of symbols in the table
    property SymbolCount: integer read GetSymbolCount;

    // plural symbols in the table. plural symbols are symbols that have
    // a frequency > 1. ie the symbol is found more than once in the app.
    // PluralCount is only valid after method SortByFrequency.
    property PluralSymbolCount: integer read FPluralSymbolCount;

    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(S: TStream);
    function LoadSymbol(S: TStream): Cardinal;
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStream(S: TStream; ACount: integer);
    procedure SaveSymbol(S: TStream; ASymbolID: Cardinal);

    procedure ClearFrequency;
    procedure IncrementFrequency(ID: integer);
    procedure SortByFrequency(var ANewIDs: array of Cardinal);
  end;

  // TNativeXml is a fast XML parser (parsing on typical hardware storage
  // 15 Mb per second), because it loads external data in chunks and buffers it in
  // memory. Use Create to create a new instance, use LoadFromFile/LoadFromStream to
  // load the XML document from a file or stream, and use SaveToFile and SaveToStream to
  // save the XML document.
  TNativeXml = class(TDebugComponent)
  private
    // inherited from TDebugComponent: FOnDebugOut: TsdDebugEvent;
    function GetOrCreateDeclarationNode: TXmlNode; virtual;
    function GetCharset: Utf8String;
    function GetPreserveWhiteSpace: boolean;
    procedure SetCharset(const Value: Utf8String);
    procedure SetBinaryDocument(ABinaryXml: TsdBinaryXml);
    procedure SetBinaryMethod(const Value: TsdXmlBinaryMethod);
    procedure SetPreserveWhiteSpace(const Value: boolean);
    procedure SetExternalEncoding(const Value: TsdStringEncoding);
    procedure SetExternalCodepage(const Value: integer);
    procedure SetXmlFormat(const Value: TXmlFormatType);
{$IFDEF USEZLIB}
    function ZlibEncode(SIn, SOut: TStream; CodecSize: int64): Utf8String;
    function ZlibDecode(SIn, SOut: TStream; PlainSize: int64): Utf8String;
{$IFDEF USEAES}
    function AeszEncode(SIn, SOut: TStream; CodecSize: int64): Utf8String;
    function AeszDecode(SIn, SOut: TStream; PlainSize: int64): Utf8String;
{$ENDIF USEAES}
{$ENDIF USEZLIB}
  protected
    FRootNodes: TsdNodeList;
    FSymbolTable: TsdSymbolTable;

    // options
    FAbortParsing: boolean;
    FDirectCloseTag: Utf8String;
    FDropCommentsOnParse: boolean;
    FEolStyle: TsdEolStyle;
    FFloatAllowScientific: boolean;
    FFloatSignificantDigits: integer;
    FExternalBomInfo: TsdBomInfo;
    FExternalCodePage: integer;
    FExternalEncoding: TsdStringEncoding;
    FFixStructuralErrors: boolean;
    FHasDeclaration: boolean;
    FHasDocType: boolean;
    FHasRoot: boolean;
    FIndentString: Utf8String;
    FNodeClosingStyle: TsdNodeClosingStyle;
    FParserWarnings: boolean;
    FRootName: Utf8String;
    FSplitSecondDigits: integer;
    FXmlFormat: TXmlFormatType;
    FUseLocalBias: boolean;
    FWriteOnDefault: boolean;
    FBinaryMethod: TsdXmlBinaryMethod;
    FAesKeyHex: Utf8String; // optional encryption key string in hex
    FSingleTagNames: TStringList; // used in FixStructuralErrors

    // events
    FOnNodeNew: TsdXmlNodeEvent;
    FOnNodeLoaded: TsdXmlNodeEvent;
    FOnProgress: TXmlProgressEvent;
    procedure ClearData(AHasDeclaration, AHasDocType, AHasRoot: boolean);
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);

    // GetParserPosition gives the parser's current position in the stream when
    // loading.
    function GetParserPosition(P: TsdXmlParser): int64;
    function GetCommentString: Utf8String;
    procedure SetCommentString(const Value: Utf8String);
    function GetStyleSheet: TsdStyleSheet;
    function GetDeclaration: TsdDeclaration;
    function GetDocType: TsdDocType;
    function GetRoot: TsdElement;
    function GetRootNodeCount: integer;
    function GetRootNodeClass: TsdNodeClass; virtual;
    function GetRootContainers(Index: integer): TsdContainerNode; virtual;
    function GetRootContainerCount: integer; virtual;
    function GetVersionString: Utf8String;
    procedure SetVersionString(const Value: Utf8String);

    // GetParserLineNumber gives the parser's current line number in the stream
    // when loading.
    function GetParserLineNumber(P: TsdXmlParser): int64;
    procedure MoveSubNodes(AList: TsdNodeList; FromNode, ToNode: TXmlNode);
    procedure DoProgress(Position: int64);
    function LineFeed: Utf8String;

    // ParseStream is called from any of the XmlNode descendants
    // and is the core method to get the xml data from external data to
    // the document object model.
    procedure ParseStream(Parser: TsdXmlParser);

    // reset the defaults
    procedure ResetDefaults;

    // WriteStream is called from any of the XmlNode descendants
    // and is the core method to write the xml data to the stream
    procedure WriteStream(S: TStream);
  public

    // constructors

    // Create an xml document with options for declaration, root element and root name.
    constructor CreateEx(AOwner: TComponent; HasDeclaration, HasDocType, HasRoot: boolean; ARootName: Utf8String);
    // Use CreateName to Create a new Xml document that will automatically
    // contain a root element with name ARootName. This constructor also adds
    // the default declaration. With default AOwner = nil, there is backward
    // compatibility with legacy NativeXml
    constructor CreateName(const ARootName: Utf8String; AOwner: TComponent = nil);
    // constructor with just the root element with an empty name
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create; overload;
    // Destroys a TNativeXml instance
    destructor Destroy; override;

    // general methods

    // canonicalize XML (C14N process): after canonicalization of the document,
    // it will be.. encoded in utf-8 only, xml declaration removed, entities
    // expanded to their character equivalent, CDATA sections replaced by character
    // equivalent, special &lt; &gt; and &quot; entities encoded, attributes
    // normalized as if by validating parser, empty elements opened with start
    // and end tags, namespace declarations and attributes sorted.
    // The function returns the number of entities expanded.
    function Canonicalize: integer;
    // Clear all the nodes in the xml document
    procedure Clear; virtual;
    // class method: Decode base64-encoded data (Utf8String) to binary data (RawByteString)
    class function DecodeBase64(const Source: Utf8String; OnDebug: TsdDebugEvent): RawByteString;
    // class method: encode binary data (RawByteString) to Utf8String, adding a
    // control character (default #$0A) each 76 characters
    class function EncodeBase64(const Source: RawByteString; const ControlChars: Utf8String = #$0A): Utf8String;
    // Find first TXmlNode instance in the document, or nil if none found (aka document is empty)
    function FindFirst: TXmlNode;
    // Find next TXmlNode instance in the document, based on previous TXmlNode instance ANode
    function FindNext(ANode: TXmlNode): TXmlNode;
    // fire AEvent for each node in the document
    procedure ForEach(Sender: TObject; AEvent: TsdXmlNodeEvent);
    // IndentString is the string used for indentations. By default, it is a
    // tab (#$09). Set IndentString to something else if you need to have
    // specific indentation, or set it to an empty string to avoid indentation.
    property IndentString: Utf8String read FIndentString write FIndentString;
    // Function IsEmpty returns true if the root is clear, or in other words, the
    // root contains no value, no name, no subnodes and no attributes.
    function IsEmpty: boolean;
    // load from binary xml file (bxm). The advisory file extension is *.BXM
    procedure LoadFromBinaryFile(const AFilename: string); virtual;
    // load from binary xml stream (bxm)
    procedure LoadFromBinaryStream(AStream: TStream); virtual;
    // load the xml from a URL, and return the loaded size in bytes
    function LoadFromURL(const URL: Utf8String): int64; virtual;
    // Call procedure LoadFromFile to load an XML document from the filename
    // specified. See Create for an example. The LoadFromFile procedure will raise
    // an exception when it encounters non-wellformed XML.
    procedure LoadFromFile(const AFilename: string); virtual;
    // Load an XML document from the stream AStream. The LoadFromStream
    // procedure will raise an exception when it encounters non-wellformed XML.
    // This method can be used with any TStream descendant. The stream is read
    // chunk-wise (using 64K chunks). See also LoadFromFile and ReadFromString.
    procedure LoadFromStream(AStream: TStream); virtual;
    // Use New to make a new xml document
    procedure New; virtual;
    // parse substitute content from ANode (usually a TsdCharData). ANode will be
    // removed and the substitute content gets parsed and becomes part of the object model.
    function ParseSubstituteContentFromNode(ANode: TXmlNode; const ASubstitute: Utf8String): TXmlNode;
    // Call procedure ReadFromString to load an XML document from the UTF8String AValue.
    // The ReadFromString procedure will raise an exception of type EFilerError
    // when it encounters non-wellformed XML.
    procedure ReadFromString(const AValue: Utf8String); virtual;
    // Remove whitespace chardata in nodes in order to compact the document. This
    // method is auto called when changing the XmlFormat: from xfPreserve to
    // xfCompact or xfReadable
    procedure RemoveWhitespace;
    // Call SaveToFile to save the XML document to a file with FileName. If the
    // filename exists, it will be overwritten without warning. If the file cannot
    // be created, a standard I/O exception will be generated. Set XmlFormat to
    // xfReadable if you want the file to contain indentations to make the XML
    // more human-readable. This is not the default and also not compliant with
    // the XML specification.
    procedure SaveToFile(const AFilename: string); virtual;
    // Call SaveToStream to save the XML document to the Stream. Stream
    // can be any TStream descendant. Set XmlFormat to xfReadable if you want
    // the stream to contain indentations to make the XML more human-readable. This
    // is not the default and also not compliant with the XML specification. See
    // SaveToFile for information on how to save in special encoding.
    procedure SaveToStream(Stream: TStream); virtual;
    // Call SaveToBinaryFile to save XML to file in binary format. The advisory
    // file extension is *.BXM
    procedure SaveToBinaryFile(const AFilename: string); virtual;
    // Call SaveToBinaryStream to save XML to stream in binary format (*.bxm)
    procedure SaveToBinaryStream(Stream: TStream); virtual;
    // Call WriteToString to write the entire XML document stream including
    // optional BOM to a generic string.
    function WriteToString: string; virtual;
    // Call WriteToLocalString to write the XML document to a Utf8String.
    function WriteToLocalString: Utf8String; virtual;
    // Call WriteToLocalUnicodeString to write the XML document to a UnicodeString.
    function WriteToLocalUnicodeString: UnicodeString; virtual;

    // properties

    // optional encryption key string in hex, must be hexadecimal notation of
    // 16 bytes (ie 32 characters). Default is '00000000000000000000000000000000'.
    // Workflow:
    // - first make sure {$define USEAES} is defined!
    // - set AesKeyHex := <your 32-char hexadecimal key>
    // - Set BinaryMethod := bmAesz
    // - to save and encrypt, use SaveToBinaryStream
    // - to load and decrypt, use LoadFromBinaryStream
    property AesKeyHex: Utf8String read FAesKeyHex write FAesKeyHex;
    // Binary XML method: bmDefault (no compression), bmZlib (zlib compression)
    // or bmAesz (AES encryption plus zlib compression)
    property BinaryMethod: TsdXmlBinaryMethod read FBinaryMethod write SetBinaryMethod;
    // Declaration is the xml declaration node. If present, it is the topmost node
    property Declaration: TsdDeclaration read GetDeclaration;
    // DeoctType is the Doctype Definition node (DTD). If present, it comes
    // after the declaration
    property DocType: TsdDocType read GetDocType;
    // Root is the topmost element in the XML document. Access Root to read any
    // child elements. When creating a new XML document, you can automatically
    // include a Root element, by creating using CreateEx or CreateName.
    property Root: TsdElement read GetRoot;
    // RootNodes can be used to directly access the nodes in the root of the
    // XML document. Usually this list consists of one declaration node followed
    // by an element node which is the Root. You can use this property to add or
    // delete comments, stylesheets, dtd's etc.
    property RootNodes: TsdNodeList read FRootNodes;
    // Payload rootnode class (TsdElement by default, but apps may create
    // a class that descends from TsdElement)
    property RootNodeClass: TsdNodeClass read GetRootNodeClass;
    // item count of the RootNodeList, ie usually max 3: the declaration, the DTD,
    // the Root (TsdElement or RootNodeClass descendant).
    property RootNodeCount: integer read GetRootNodeCount;
    // root containers
    property RootContainers[Index: integer]: TsdContainerNode read GetRootContainers;
    // number of root containers (as opposed to all root nodes)
    property RootContainerCount: integer read GetRootContainerCount;
    // A comment string above the root element <!--{comment}--> can be accessed with
    // this property. Assign a comment to this property to add it to the XML document.
    // Use property RootNodes to add/insert/extract multiple comments.
    property CommentString: Utf8String read GetCommentString write SetCommentString;
    // Set DropCommentsOnParse if you're not interested in any comment nodes in your object
    // model data. All comments encountered during parsing will simply be skipped and
    // not added as a node with ElementType = xeComment (which is default). Note that
    // when you set this option, you cannot later reconstruct an XML file with the comments
    // back in place.
    property DropCommentsOnParse: boolean read FDropCommentsOnParse write FDropCommentsOnParse;
    // After reading, this property contains the XML version (usually '1.0').
    property VersionString: Utf8String read GetVersionString write SetVersionString;
    // Charset (e.g. 'utf-8', 'utf-16' or any other multibyte/ansi codepage description.
    // This charset description is stored in the declaration node.
    // Example: In order to get this header:
    // <?xml version="1.0" encoding="utf-16"?>
    // enter this code:
    // <CODE>MyXmlDocument.Charset := 'utf-16';</CODE>
    // When reading a file, Charset will contain the encoding used.
    property Charset: Utf8String read GetCharset write SetCharset;
    // SymbolTable holds all the content (strings, base64binary, hexbinary,
    // date, datetime) in the xml tree
    property SymbolTable: TsdSymbolTable read FSymbolTable;
    // Get the stylesheet used for this XML document. If the node does not
    // exist yet, it will be created. TsdStyleSheet exists for backwards compatibility;
    // StyleSheet is deprecated in the xml spec.
    property StyleSheet: TsdStyleSheet read GetStyleSheet;
    // External encoding is valid after loading, and indicates the encoding
    // detected in the external xml document. Internally, all string values are always
    // encoded in UTF8, so if the external stream is Ansi with codepage or UTF16, a conversion
    // is done. When writing to a file/stream, a BOM is generated for the two-byte
    // character encodings (UTF16LE and UUTF16BE). UTF8 uses *no BOM* according to
    // the XML specification.
    // Any conversion is done from UTF8 to external encodings if necessary. You can
    // *set* ExternalEncoding too but only for welldefined encodings (seUTF8, seUTF16LE,
    // seUTF16BE). If you want to use an ansi encoding, then set ExternalCodepage.
    property ExternalEncoding: TsdStringEncoding read FExternalEncoding write SetExternalEncoding;
    // the codepage used in the external xml document
    property ExternalCodepage: integer read FExternalCodePage write SetExternalCodepage;
    // if ncUnknown (default), parsed setting will be preserved per element.
    // if ncFull, single tags will be left full (eg '<bla x="1"></bla>').
    // if ncClose , single tags will be closed (eg '<bla x="1"/>').
    property NodeClosingStyle: TsdNodeClosingStyle read FNodeClosingStyle write FNodeClosingStyle;
    // XmlFormat by default is set to xfCompact. This setting is compliant to the spec,
    // and NativeXml will not generate any more characters than necessary.
    // By setting XmlFormat to xfReadable, you can generate readable XML
    // files that contain indentation and end-of-lines after each element.
    // By setting XmlFormat to xfPreserve, NativeXml preserves all the markup, no
    // matter what, as the user/app typed/intended it. Note that setting xfPreserve
    // may cause slightly more additional nodes being added in the xml when the
    // document is parsed. If you switch to xfCompact or xfReadable after parsing,
    // (or switch to xfPreserve too late, after parsing) all the markup is lost
    // and cannot be retrieved.
    property XmlFormat: TXmlFormatType read FXmlFormat write SetXmlFormat;
    // EolStyle by default is set to esCRLF for Windows and esLF for non-Windows.
    // - esLF (formerly esLinux) writes just a LF (#$0A) as end-of-line
    // - esCRLF (formerly esWindows) writes a CRLF (#$0D#$0A) as end-of-line
    // - esCR (used for Mac) writes a CR (#$0D) as end-of-line
    // Please note that the esCRLF is the default in Windows. However,
    // since it is not the default in normalized xml and the un-normalization
    // adds an overhead in e.g. base64 processing of large binary chunks of data,
    // it is best to avoid the esCRLF option, if not absolutely
    // necessary. Many text processors work flawlessly with just LF as end-of-line,
    // except Windows Notepad. CR-LF is a relic of the past, where mechanic
    // typewriters would slam the carriage to the base position, then feed a line.
    property EolStyle: TsdEolStyle read FEolStyle write FEolStyle;
    // OnProgress event
    property OnProgress: TXmlProgressEvent read FOnProgress write FOnProgress;
    // Set PreserveWhiteSpace to True to preserve all whitespace present in the
    // file when parsing. The blocks of whitespace are stored as CharData nodes.
    // This sets XmlFormat := xfPreserve under the hood.
    property PreserveWhiteSpace: boolean read GetPreserveWhiteSpace write SetPreserveWhiteSpace;
    // Set AbortParsing to True if you use the OnNodeNew and OnNodeLoaded events in
    // a SAX-like manner, and you want to abort the parsing process halfway.
    property AbortParsing: boolean read FAbortParsing write FAbortParsing;
    // when true, NativeXmlEx will try to fix certain structural errors in non-valid
    // xml that usually come from single tags in html (default = False)
    property FixStructuralErrors: boolean read FFixStructuralErrors write FFixStructuralErrors;
    // Set WriteOnDefault to False if you do not want to write default values to
    // the XML document. This option can avoid creating huge documents with
    // redundant info, and will speed up writing.
    property WriteOnDefault: boolean read FWriteOnDefault write FWriteOnDefault;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will allow to output scientific notation in some cases, if the
    // result is significantly shorter than normal output, but only if the value
    // of FloatAllowScientific is True (default).
    property FloatAllowScientific: boolean read FFloatAllowScientific write FFloatAllowScientific;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will use this number of significant digits. The default is
    // cDefaultFloatSignificantDigits, and set to 6.
    property FloatSignificantDigits: integer read FFloatSignificantDigits write FFloatSignificantDigits;
    // When converting date/time values to strings, NativeXml will use this
    // number of digits after the seconds. The default is cDefaultSplitSecondDigits,
    // and set to 0. With this default, no tens/hundreds/thousands after the second are used
    property SplitSecondDigits: integer read FSplitSecondDigits write FSplitSecondDigits;
    // When converting date/time values to strings, NativeXml will use a local bias
    // towards UTC if this option is True. Default is False.
    property UseLocalBias: boolean read FUseLocalBias write FUseLocalBias;
    // Connect to OnNodeNew to get informed of new nodes being added while loading.
    property OnNodeNew: TsdXmlNodeEvent read FOnNodeNew write FOnNodeNew;
    // Connect to OnNodeLoaded to get informed of nodes being finished loading.
    property OnNodeLoaded: TsdXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
    // Connect to OnDebugOut to get debug information in the client application
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;

    // some more added  methods in a LINQ-like way:
    // attributes
    function AttrText(AName, AValue: Utf8String): TsdAttribute;
    function AttrInt(AName: Utf8String; AValue: integer): TsdAttribute;
    function AttrInt64(AName: Utf8String; AValue: int64): TsdAttribute;
    function AttrHex(AName: Utf8String; AValue, ADigits: integer): TsdAttribute; overload;
    function AttrHex(AName: Utf8String; AValue: int64; ADigits: integer): TsdAttribute; overload;
    function AttrFloat(AName: Utf8String; AValue: double): TsdAttribute; overload;
    function AttrFloat(AName: Utf8String; AValue: double; ASignificantDigits: integer; AAllowScientific: boolean): TsdAttribute; overload;
    function AttrDateTime(AName: Utf8String; AValue: TDateTime): TsdAttribute;
    function AttrBool(AName: Utf8String; AValue: boolean): TsdAttribute;

    // container nodes
    function NodeNew(AName: Utf8String): TXmlNode; overload; virtual;
    function NodeNew(AName: Utf8String; SubNodes: array of TXmlNode): TXmlNode; overload; virtual;
    function NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    // string nodes
    function NodeNewText(AName, AValue: Utf8String): TXmlNode; overload;
    function NodeNewTextEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewText(AName, AValue: Utf8String; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewType(AName: Utf8String; AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewTypeEx(AName: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewType(AName: Utf8String; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTypeEx(AName: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewAttr(AName: Utf8String; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewAttrEx(AName: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewAttr(AName: Utf8String; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewAttrEx(AName: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode)
      : TXmlNode; overload;

    function NodeNewTextType(AName, AValue: Utf8String; AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewTextTypeEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewTextType(AName, AValue: Utf8String; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode)
      : TXmlNode; overload;

    function NodeNewTextAttr(AName, AValue: Utf8String; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextAttrEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextAttr(AName, AValue: Utf8String; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextAttrEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode)
      : TXmlNode; overload;

    function NodeNewTextTypeAttr(AName, AValue: Utf8String; AElementType: TsdElementType; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewTextTypeAttr(AName, AValue: Utf8String; AElementType: TsdElementType; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute)
      : TXmlNode; overload;
    function NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;

    // integer nodes
    function NodeNewInt(AName: Utf8String; AValue: integer): TXmlNode; overload;
    function NodeNewIntEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewInt(AName: Utf8String; AValue: integer; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntType(AName: Utf8String; AValue: integer; AElementType: TsdElementType): TXmlNode; overload;
    function NodeNewIntTypeEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode; overload;
    function NodeNewIntType(AName: Utf8String; AValue: integer; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode)
      : TXmlNode; overload;

    function NodeNewIntAttr(AName: Utf8String; AValue: integer; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntAttrEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntAttr(AName: Utf8String; AValue: integer; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntAttrEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;

    function NodeNewIntTypeAttr(AName: Utf8String; AValue: integer; AElementType: TsdElementType; Attributes: array of TsdAttribute)
      : TXmlNode; overload;
    function NodeNewIntTypeAttrEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute): TXmlNode; overload;
    function NodeNewIntTypeAttr(AName: Utf8String; AValue: integer; AElementType: TsdElementType; Attributes: array of TsdAttribute;
      SubNodes: array of TXmlNode): TXmlNode; overload;
    function NodeNewIntTypeAttrEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode;
      Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode; overload;
  end;

  { Canonicalize an xml document

    The acronym for canonicalization is "C14N"

    An xml document after C14N must be:
    - encoded in utf-8 only
    - xml declaration removed
    - entities expanded to their character equivalent
    - CDATA sections replaced by character equivalent
    - special &lt; &gt; and &quot; entities encoded
    - attributes normalized as if by validating parser
    - empty elements opened with start and end tags
    - namespace declarations and attributes sorted

    Experimental!
  }

  TsdXmlCanonicalizer = class(TDebugComponent)
  public
    function Canonicalize(AXml: TNativeXml): integer;
  end;

  { binary xml

    The idea here is that binary xml is a compact representation of the same
    xml file, without the need of parsing or writing out the actual textual
    representation.

    A binary xml file just loads/saves the unique string table and the node structure.
    Since there is the opportunity to compress and encrypt the binary xml with
    additional methods, application code can use binary xml to work with efficient
    binary files without the hassle of parsing/writing and with optional compression
    or optional encryption. See also TNativeXml.LoadFromBinaryStream / SaveToBinaryStream.

  }
  TsdBinaryXml = class(TDebugComponent)
  private
    FDocument: TNativeXml;
    FOnEncode: TXmlCoderEvent;
    FOnDecode: TXmlCoderEvent;
    FNewIDs: array of Cardinal;
    FElementTypeCount: array [TsdElementType] of Cardinal;
    function UpdateID(AID: Cardinal): Cardinal;
    function IncrementFrequency(AID: Cardinal): Cardinal;
  protected
    function ReadCardinal(S: TStream): Cardinal;
    procedure ReadDocument(S: TStream);
    function ReadNode(S: TStream; AParent: TXmlNode; var SubCount: integer): TXmlNode;
    procedure SortByFrequency;
    procedure WriteCardinal(S: TStream; ACardinal: Cardinal);
    procedure WriteDocument(S: TStream);
    procedure WriteNode(S: TStream; ANode: TXmlNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStream(S: TStream); virtual;
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(S: TStream); virtual;
    property Document: TNativeXml read FDocument write FDocument;
    property OnEncode: TXmlCoderEvent read FOnEncode write FOnEncode;
    property OnDecode: TXmlCoderEvent read FOnDecode write FOnDecode;
  end;

  {
    constants and utility functions of NativeXml
  }
const

  cNodeClass: array [TsdElementType] of TsdNodeClass = (TsdElement, TsdAttribute, TsdCharData, TsdComment, TsdCData, TsdConditionalSection,
    TsdDeclaration, TsdStyleSheet, TsdDocType, TsdDtdElement, TsdDtdAttList, TsdDtdEntity, TsdDtdNotation, TsdInstruction, TsdWhiteSpace,
    TsdQuotedText, nil, nil);

  // chunk sizes: external stream is loaded/saved in these chunks of memory data
  // - valid values are $4 - unbounded till memory size
  // - sane values are $20 - $1000
  cParserChunkSize = $100;
  cWriterChunkSize = $100;

  // Count of different escape phrases
  cEscapePhraseCount = 5;

  // These are phrases that must be escaped. Note that "&" is first since
  // when another would be replaced first (eg ">" by "&lt;") this could
  // cause the new "&" in "&lt;" to be replaced by "&amp;";
  cXmlEscapePhrases: array [0 .. cEscapePhraseCount - 1] of Utf8String = ('&', '<', '>', '''', '"');

  // These are the phrases that replace the escape phrases - in the same order
  // As a result, these phrases are visible in the core xml source
  cXmlReplacePhrases: array [0 .. cEscapePhraseCount - 1] of Utf8String = ('&amp;', '&lt;', '&gt;', '&apos;', '&quot;');

  // special characters used for whitespace / blanks
  cXmlBlankChars: set of AnsiChar = [#$09, #$0A, #$0D, #$20];

  cXmlBlankCharsOrEndTag: set of AnsiChar = [#$09, #$0A, #$0D, #$20, '[', '/', '>'];

  cQuoteChars: set of AnsiChar = ['"', ''''];

  cQuoteCharStyleNames: array [TsdQuoteCharStyle] of Utf8String = ('"', '''');

  // codepage IBM852, used for GUI implem
  CP_852: integer = 852;

  // Windows-1250 codepage, used for GUI implem
  CP_1250: integer = 1250;

  // Windows-1252 codepage, used for GUI implem
  CP_1252: integer = 1252;

  // UTF8 codepage (outcommented to avoid clash in BCB - it is already defined
  // in windows)
  // CP_UTF8: integer = 65001;

  // UTF16 codepage
  CP_UTF16: integer = 1200;

  // ISO 8859-1 codepage, used for GUI implem
  CP_ISO8859_1: integer = 28591;

  // These characters are used when generating BASE64 AnsiChars from buffer data
  cBase64Char: array [0 .. 63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  cBase64PadChar: AnsiChar = '=';

  cBomInfoListCount = 15;
  // array with Byte Order Mark (BOM) info
  cBomInfoList: array [0 .. cBomInfoListCount - 1] of TsdBomInfo = ((BOM: ($3C, $3F, $78, $6D); Len: 4; Encoding: seAnsi; HasBOM: False), // 0
    (BOM: ($EF, $BB, $BF, $00); Len: 3; Encoding: seUTF8; HasBOM: True), (BOM: ($00, $00, $FE, $FF); Len: 4; Encoding: seUTF32BE; HasBOM: True),
    (BOM: ($FF, $FE, $00, $00); Len: 4; Encoding: seUTF32LE; HasBOM: True), (BOM: ($00, $00, $FF, $FE); Len: 4; Encoding: seUCS4_2143; HasBOM: True),
    (BOM: ($FE, $FF, $00, $00); Len: 4; Encoding: seUCS4_3412; HasBOM: True), (BOM: ($FE, $FF, $00, $00); Len: 2; Encoding: seUTF16BE; HasBOM: True),
    // 6
    (BOM: ($FF, $FE, $00, $00); Len: 2; Encoding: seUTF16LE; HasBOM: True), // 7
    (BOM: ($00, $00, $00, $3C); Len: 4; Encoding: seUTF32BE; HasBOM: False), (BOM: ($3C, $00, $00, $00); Len: 4; Encoding: seUTF32LE; HasBOM: False),
    (BOM: ($00, $00, $3C, $00); Len: 4; Encoding: seUCS4_2143; HasBOM: False), (BOM: ($00, $3C, $00, $00); Len: 4; Encoding: seUCS4_3412;
    HasBOM: False), (BOM: ($00, $3C, $00, $3F); Len: 4; Encoding: seUTF16BE; HasBOM: False), (BOM: ($3C, $00, $3F, $00); Len: 4; Encoding: seUTF16LE;
    HasBOM: False), (BOM: ($4C, $6F, $A7, $94); Len: 4; Encoding: seEBCDIC; HasBOM: False));
  cBomInfoIdxUTF16BE = 6;
  cBomInfoIdxUTF16LE = 7;

  cElementTypeNames: array [TsdElementType] of Utf8String = ('Element', 'Attribute', 'CharData', 'Comment', 'CData', 'ConditionalSection',
    'Declaration', 'Stylesheet', 'DocType', 'DtdElement', 'DtdAttList', 'DtdEntity', 'DtdNotation', 'Instruction', 'WhiteSpace', 'QuotedText',
    'EndTag', 'Error');

  // binary xml version
  // v1: stylesheet based on chardata
  // v2: stylesheet based on containernode
  cBinaryXmlVersion: Cardinal = 2;

resourcestring

  sPrematureEnd = 'stream terminated prematurely at pos %d';
  sInvalidStream = 'invalid stream';
  sUnknownEncoding = 'unknown encoding';
  sUnsupportedEncoding = 'unsupported encoding (%s)';
  sDefaultCharUsed = 'default char used for codepage substitution';
  sNotSupported = 'feature is not supported yet';
  sIllegalTag = 'illegal tag ("%s") at pos %d';
  sUnsupportedTag = 'unsupported tag ("%s") at pos %d';
  sIllegalEndTag = 'illegal end tag ("%s") at line %d (streampos %d)';
  sQuoteCharExpected = 'quote char expected at pos %d';
  sCannotAddNode = 'cannot add node to this type of element';
  sCannotAddAttribute = 'cannot add attribute';
  sCannotSetName = 'cannot set name on this type of element';
  sCannotSetValue = 'cannot set value on this type of element';
  sCannotManipulate = 'cannot manipulate nodes in this type of element';
  sBeginEndMismatch = 'begin and end tag mismatch: "%s" and "%s" at line %d (pos %d)';
  sLevelMismatch = 'level mismatch between subnode "%s" and endnode "%s" at line %d (pos %d)';
  sRootElementNotDefined = 'XML root element not defined.';
  sNonDefaultChardata = 'non-default chardata at line %d (pos %d)';
  sSignificantDigitsOutOfRange = 'significant digits out of range';
  sMissingDataInBinaryStream = 'missing data in binary stream';
  sErrorCalcStreamLength = 'error while calculating streamlength';
  sXmlNodeNotAssigned = 'XML node is not assigned';
  sXmlOwnerNotAssigned = 'XML owner is not assigned';
  sXmlParentNotAssigned = 'XML parent is not assigned';
  sUnknownBinaryEncodingBinhex = 'unknown encoding: xbeBinHex (deprecated)';
  sCopyFromOnlyWithXmlNode = 'CopyFrom can only be used with TXmlNode descendants';

var

  // NativeXml defaults
  cDefaultAesKeyHex: Utf8String = '00000000000000000000000000000000';
  cDefaultBinaryMethod: TsdXmlBinaryMethod = bmDefault;
  cDefaultDirectCloseTag: Utf8String = '/>';
  cDefaultDropCommentsOnParse: boolean = False;
  cDefaultFloatAllowScientific: boolean = True;
  cDefaultFloatSignificantDigits: integer = 6;
  cDefaultEncodingString: Utf8String = 'utf-8';
{$IFDEF MSWINDOWS}
  cDefaultEolStyle: TsdEolStyle = esCRLF;
{$ELSE MSWINDOWS}
  cDefaultEolStyle: TsdEolStyle = esLF;
{$ENDIF MSWINDOWS}
  cDefaultExternalEncoding: TsdStringEncoding = seUTF8;
  cDefaultFixStructuralErrors: boolean = False;
  cDefaultIndentString: Utf8String = #$09; // tab
  cDefaultNodeClosingStyle: TsdNodeClosingStyle = ncClose;
  cDefaultSortAttributes: boolean = False;
  cDefaultSplitSecondDigits: integer = 0;
  cDefaultVersionString: Utf8String = '1.0';
  cDefaultXmlFormat: TXmlFormatType = xfPreserve;
  cDefaultUseLocalBias: boolean = False;
  cDefaultWriteOnDefault: boolean = True;

  // helpful XML addtions
  cReadableDirectCloseTag: Utf8String = ' />';
  // see GetXmlFormatSettings in initialization section
  cXmlFormatSettings: TFormatSettings;

  { Utility functions }

  // Convert UnicodeString to Utf8String
function sdWideToUtf8(const W: UnicodeString): Utf8String;

// Convert UTF8 string to UnicodeString
function sdUtf8ToWide(const U: Utf8String): UnicodeString;

// Convert Ansi to Utf8 string
function sdAnsiToUtf8(const A: AnsiString; ACodePage: integer): Utf8String;

// Convert Utf8 to Ansi string
function sdUtf8ToAnsi(const U: Utf8String; ACodePage: integer): AnsiString;

function sdTrim(const S: Utf8String): Utf8String; overload;
function sdTrim(const S: Utf8String; var IsTrimmed: boolean): Utf8String; overload;
function sdTrim(const S: Utf8String; var PreString, PostString: Utf8String): Utf8String; overload;

// compress any eol (ie CR-LF) to normalised eol (LF)
function sdNormaliseEol(const S: Utf8String): Utf8String;
// expand any normalised eol (LF) to un-normalised eol (ie CR-LF) based on TsdEolStyle
function sdUnNormaliseEol(const S: Utf8String; const EolStyle: TsdEolStyle): Utf8String;

function sdEscapeString(const AValue: Utf8String): Utf8String;

// replace escaped phrases and references written in the core xml source
// with replacement characters
function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean; References: array of TXmlNode): Utf8String; overload;
function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean): Utf8String; overload;
function sdReplaceString(const AValue: Utf8String): Utf8String; overload;

function sdCommaToDot(const AValue: Utf8String): Utf8String;

function sdReadFromStream(S: TStream; CharCount: integer): Utf8String;
procedure sdWriteToStream(S: TStream; const Value: Utf8String);
function sdReadCardinal(S: TStream): Cardinal;
procedure sdWriteCardinal(S: TStream; ACardinal: Cardinal);

// Based on the charset, find the codepage. If no charset is
// matched, the function returns ADefaultCodepage (default utf-8, 65001)
function sdCharsetToCodePage(ACharset: Utf8String; ADefaultCodepage: integer = 65001): integer;

// Based on the charset, find the TsdStringEncoding. If no charset is
// matched, the function returns a encoding of seUTF8
function sdCharsetToStringEncoding(ACharset: Utf8String): TsdStringEncoding;

// find the charset corresponding to windows codepage
function sdCodepageToCharset(ACodePage: integer): Utf8String;

function Utf8CompareText(const S1, S2: Utf8String): integer;

{ debug functions }

function sdDebugMessageToString(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String): Utf8String;

{ utility functions }

// compare two bytes
function sdCompareByte(Byte1, Byte2: byte): integer;

// compare two integers
function sdCompareInteger(Int1, Int2: integer): integer;

// type conversions

// get the timezone bias
function GetTimeZoneBias: integer;

// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
function sdDateTimeToString(ADate: TDateTime; UseDate: boolean = True; UseTime: boolean = True; SplitSecondDigits: integer = 0;
  UseLocalBias: boolean = False): Utf8String;

function sdBoolToString(Value: boolean): Utf8String;
function sdBoolFromString(Value: Utf8String): boolean;

// Convert a number to a Utf8String, using SignificantDigits to indicate the number of
// significant digits, and AllowScientific to allow for scientific notation if that
// results in much shorter notation.
function sdFloatToString(Value: double; SignificantDigits: integer; AllowScientific: boolean): Utf8String; overload;
function sdFloatToString(Value: double): Utf8String; overload;
function sdFloatFromString(Value: Utf8String): double;

function sdIntToString(Value: integer): Utf8String;
function sdIntFromString(Value: Utf8String): integer;

function sdInt64ToString(Value: int64): Utf8String;
function sdInt64FromString(Value: Utf8String): int64;

function sdStreamReadCardinal(S: TStream): Cardinal;
procedure sdStreamWriteCardinal(S: TStream; ACardinal: Cardinal);
procedure sdStreamWriteString(S: TStream; const AString: Utf8String);

// Convert the Utf8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, an exception will be raised.
function sdStringToDateTime(const ADate: Utf8String; UseLocalBias: boolean = False): TDateTime;

// Convert the UTF8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
function sdStringToDateTimeDef(const ADate: Utf8String; ADefault: TDateTime; UseLocalBias: boolean = False): TDateTime;

// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as UTF8String, without any linebreaks.
function EncodeBase64(const Source: RawByteString): Utf8String;
function EncodeBase64Buf(const Buffer; Count: integer): Utf8String;

// Decode BASE64 data in Source into binary data. The function returns the binary
// data as Utf8String. The Source Utf8String may contain linebreaks and control characters,
// these will be stripped.
function DecodeBase64(const Source: Utf8String): RawByteString;
procedure DecodeBase64Buf(var Source: Utf8String; var Buffer; Count: integer);

// Decode BINHEX data in Source into RawByteStrng with binary data (for compatibility with old NativeXml)
function DecodeBinHex(const Source: Utf8String): RawByteString;
procedure DecodeBinhexBuf(var Source: Utf8String; var Buffer; Count: integer);

// This function removes control characters from Utf8String AValue (Tab, CR, LF and Space)
function sdRemoveControlChars(const AValue: Utf8String): Utf8String;

// This function adds control characters Chars repeatedly after each Interval
// of characters to UTF8String Value. Default interval is 76 (seems to be used in many
// applications)
function sdAddControlChars(const AValue: Utf8String; const ControlChars: Utf8String; Interval: integer = 76): Utf8String;

// Convert Ansi to Utf8 using buffers
// please note: Utf8Buf can use 3x more size than AnsiBuf in extreme cases.
// Result is the Utf8Buf bytecount
function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer; var LastChar0D: boolean): integer;

// convert raw buffer to normalized buffer (suitable for utf8 or ansi)
function sdNormaliseBuffer(const RawBuf; var NormBuf; Count: integer; var LastChar0D: boolean): integer;

// Convert Utf8 to Ansi using buffers
function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer; var DefaultCharUsed: boolean): integer;

// determine the character length of the first Utf8 character in the buffer
function sdUtf8CharacterLength(const Buffer): integer;

// Convert a "WideString" (UTF16 LE) buffer to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the Utf8Buf must be at least 1.5 the size of the WideBuf.
// The function returns the number of *bytes* written.
function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer; var LastChar0D: boolean): integer;

// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;

implementation

type

  // A symbol item used in symbol lists (do not use directly)
  TsdSymbol = class
  private
    FID: integer;
    FFreq: Cardinal;
    FSymbolStyle: Cardinal;
    FFirst: Pbyte;
    FCharCount: integer;
  public
    destructor Destroy; override;
    function AsString: Utf8String;
    property SymbolStyle: Cardinal read FSymbolStyle;
    property CharCount: integer read FCharCount;
  end;

  // A list of symbols (do not use directly)
  TsdSymbolList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdSymbol;
  protected
    // Assumes list is sorted by refstring
    function Find(ASymbol: TsdSymbol; var Index: integer): boolean;
  public
    property Items[Index: integer]: TsdSymbol read GetItems; default;
  end;

  { debug functions }

function sdDebugMessageToString(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String): Utf8String;
var
  SenderString: Utf8String;
begin
  if assigned(Sender) then
    SenderString := Utf8String(Sender.ClassName)
  else
    SenderString := '';
  Result := '[' + cWarnStyleNames[WarnStyle] + '] ' + SenderString + ': ' + AMessage;
end;

function sdClassName(AObject: TObject): Utf8String;
begin
  Result := 'nil';
  if assigned(AObject) then
    Result := Utf8String(AObject.ClassName);
end;

{ utility functions }

function sdCompareByte(Byte1, Byte2: byte): integer;
begin
  if Byte1 < Byte2 then
    Result := -1
  else if Byte1 > Byte2 then
    Result := 1
  else
    Result := 0;
end;

function sdCompareInteger(Int1, Int2: integer): integer;
begin
  if Int1 < Int2 then
    Result := -1
  else if Int1 > Int2 then
    Result := 1
  else
    Result := 0;
end;

// compare two symbols. This is NOT an alphabetic compare. symbols are first
// compared by length, then by first byte, then last byte then second, then
// N-1, until all bytes are compared.
function sdCompareSymbol(Symbol1, Symbol2: TsdSymbol): integer;
var
  CharCount: integer;
  First1, First2, Last1, Last2: Pbyte;
  IsEqual: boolean;
begin
  // Compare string length first
  Result := sdCompareInteger(Symbol1.CharCount, Symbol2.CharCount);
  if Result <> 0 then
    exit;

  // Compare FFirst
  Result := sdCompareByte(Symbol1.FFirst^, Symbol2.FFirst^);
  if Result <> 0 then
    exit;

  // CharCount of RS1 (and RS2, since they are equal)
  CharCount := Symbol1.CharCount;

  // Setup First & Last pointers
  First1 := Symbol1.FFirst;
  First2 := Symbol2.FFirst;

  // compare memory (boolean op). CompareMem might have optimized code depending
  // on memory manager (ASM, MMX, SSE etc) to binary compare the block.
  // Since sdCompareRefString may be used to compare relatively large blocks of
  // text, which are often exact copies, using CompareMem before special comparison
  // is warrented.
  IsEqual := CompareMem(First1, First2, CharCount);
  if IsEqual then
  begin
    Result := 0;
    exit;
  end;

  // finally the special conparison: Compare each time last ptrs then first ptrs,
  // until they meet in the middle
  Last1 := First1;
  inc(Last1, CharCount);
  Last2 := First2;
  inc(Last2, CharCount);

  repeat

    dec(Last1);
    dec(Last2);
    if First1 = Last1 then
      exit;

    Result := sdCompareByte(Last1^, Last2^);
    if Result <> 0 then
      exit;

    inc(First1);
    inc(First2);
    if First1 = Last1 then
      exit;

    Result := sdCompareByte(First1^, First2^);
    if Result <> 0 then
      exit;

  until False;
end;

{ TsdSymbol }

function TsdSymbol.AsString: Utf8String;
begin
  SetString(Result, PAnsiChar(FFirst), FCharCount);
end;

destructor TsdSymbol.Destroy;
begin
  FreeMem(FFirst);
  inherited;
end;

{ TsdSymbolList }

function TsdSymbolList.GetItems(Index: integer): TsdSymbol;
begin
  Result := TsdSymbol(Get(Index));
end;

function TsdSymbolList.Find(ASymbol: TsdSymbol; var Index: integer): boolean;
var
  AMin, AMax: integer;
begin
  Result := False;

  // Find position - binary method
  AMin := 0;
  AMax := Count;
  while AMin < AMax do
  begin
    Index := (AMin + AMax) div 2;
    case sdCompareSymbol(Items[Index], ASymbol) of
      - 1:
        AMin := Index + 1;
      0:
        begin
          Result := True;
          exit;
        end;
      1:
        AMax := Index;
    end;
  end;
  Index := AMin;
end;

{$IFDEF POSIX}

// Ecosoft 06/06/2012 - Compatibility XE2 OSX
function MultiByteToWideChar(Codepage, Flags: Cardinal; LocaleStr: PAnsiChar; LocaleStrLen: integer; UnicodeStr: PWideChar;
  UnicodeStrLen: integer): integer;
begin
  Result := UnicodeFromLocaleChars(Codepage, Flags, LocaleStr, LocaleStrLen, UnicodeStr, UnicodeStrLen);
end;

function WideCharToMultiByte(Codepage, Flags: Cardinal; UnicodeStr: PWideChar; UnicodeStrLen: integer; LocaleStr: PAnsiChar; LocaleStrLen: integer;
  DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): integer;
begin
  Result := LocaleCharsFromUnicode(Codepage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr, LocaleStrLen, DefaultChar, UsedDefaultChar);
end;
{$ENDIF POSIX}

type
  TAnsiCharArray = array [0 .. 32767] of AnsiChar;

  { TXmlNode }

procedure TXmlNode.AttributeAdd(const AName, AValue: Utf8String);
var
  A: TsdAttribute;
begin
  A := TsdAttribute.Create(TNativeXml(FOwner));
  A.Name := AName;
  A.Value := AValue;
  AttributeAdd(A);
end;

procedure TXmlNode.AttributeAdd(AAttribute: TsdAttribute);
begin
  raise Exception.Create(sCannotAddAttribute);
end;

procedure TXmlNode.AttributesAdd(Attributes: array of TsdAttribute);
var
  x: integer;
begin
  for x := Low(Attributes) to High(Attributes) do
    AttributeAdd(Attributes[x]);
end;

function TXmlNode.GetAttributeCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to GetNodeCount - 1 do
    if GetNodes(i) is TsdAttribute then
      inc(Result);
end;

constructor TXmlNode.Create(AOwner: TComponent);
begin
  inherited Create;
  if not(AOwner is TNativeXml) then
    raise Exception.Create(sXmlOwnerNotAssigned);
  FOwner := TNativeXml(AOwner);
end;

constructor TXmlNode.CreateParent(AOwner: TComponent; AParent: TXmlNode);
begin
  Create(AOwner);
  if not(AParent is TXmlNode) then
    raise Exception.Create(sXmlParentNotAssigned);
  AParent.NodeAdd(Self);
end;

constructor TXmlNode.CreateParentNear(AOwner: TComponent; AParent, ANode: TXmlNode; IsBefore: boolean);
begin
  Create(AOwner);
  if not(AParent is TsdContainerNode) then
    raise Exception.Create(sXmlParentNotAssigned);
  TsdContainerNode(AParent).NodeInsertNear(Self, ANode, IsBefore);
end;

constructor TXmlNode.CreateName(AOwner: TNativeXml; const AName: Utf8String);
begin
  Create(AOwner);
  Name := AName;
end;

constructor TXmlNode.CreateNameValue(AOwner: TNativeXml; const AName, AValue: Utf8String);
begin
  Create(AOwner);
  Name := AName;
  Value := AValue;
end;

function TXmlNode.ElementType: TsdElementType;
begin
  // overridden in descendants
  Result := xeError;
end;

function TXmlNode.ElementTypeName: Utf8String;
begin
  Result := cElementTypeNames[ElementType];
end;

class function TXmlNode.EscapeString(const S: Utf8String): Utf8String;
begin
  Result := sdEscapeString(S);
end;

function TXmlNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := nil;
end;

class function TXmlNode.WideToUtf8(const W: UnicodeString): Utf8String;
begin
  Result := sdWideToUtf8(W);
end;

function TXmlNode.GetAttributeByName(const AName: Utf8String): TsdAttribute;
var
  i: integer;
  A: TsdAttribute;
begin
  for i := 0 to GetAttributeCount - 1 do
  begin
    A := GetAttributes(i);
    if Utf8CompareText(A.Name, AName) = 0 then
    begin
      Result := A;
      exit;
    end;
  end;
  Result := nil;
end;

function TXmlNode.GetAttributeName(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if assigned(A) then
    Result := A.Name
  else
    Result := '';
end;

function TXmlNode.GetAttributes(Index: integer): TsdAttribute;
var
  i, Idx: integer;
begin
  Idx := 0;
  Result := nil;
  for i := 0 to GetNodeCount - 1 do
  begin
    if GetNodes(i) is TsdAttribute then
    begin
      if Idx = Index then
      begin
        Result := TsdAttribute(GetNodes(i));
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TXmlNode.GetAttributeValue(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
  Result := StrToIntDef(GetAttributeValue(Index), 0);
end;

function TXmlNode.GetAttributeValueByName(const AName: Utf8String): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetIndent: Utf8String;
var
  i: integer;
begin
  Result := '';
  if assigned(FOwner) then
  begin
    case GetXmlFormat of
      xfCompact, xfPreserve:
        Result := '';
      xfReadable:
        for i := 0 to TreeDepth - 1 do
          Result := Result + TNativeXml(FOwner).IndentString;
    end; // case
  end;
end;

function TXmlNode.GetEndOfLine: Utf8String;
begin
  Result := '';
  if GetXmlFormat = xfReadable then
    Result := GetSeparator;
end;

function TXmlNode.GetSeparator: Utf8String;
begin
  case GetEolStyle of
    esLF:
      Result := #$0A; // linux
    esCRLF:
      Result := #$0D#$0A; // windows
    esCR:
      Result := #$0D; // mac
  else
    // default in case other separator styles emerge
    Result := #$0A;
  end;
end;

function TXmlNode.GetName: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := nil;
end;

function TXmlNode.GetParentNode(ADepth: integer): TXmlNode;
var
  i: integer;
begin
  Result := Self;
  for i := 0 to ADepth do
  begin
    Result := Result.FParent;
    if not assigned(Result) then
      exit;
  end;
end;

function TXmlNode.GetParentNodeName(ADepth: integer): Utf8String;
var
  Node: TXmlNode;
begin
  // parent node name
  Node := GetParentNode(ADepth);
  if assigned(Node) then
    Result := Node.GetName
  else
    Result := '';
end;

function TXmlNode.GetNameUnicode: UnicodeString;
begin
  Result := sdUtf8ToWide(GetName);
end;

function TXmlNode.GetValue: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetValueUnicode: UnicodeString;
begin
  Result := sdUtf8ToWide(GetValue);
end;

function TXmlNode.IsClear: boolean;
begin
  Result := IsEmpty and (length(Name) = 0);
end;

function TXmlNode.IsEmpty: boolean;
begin
  Result := (GetNodeCount = 0) and (length(Value) = 0)
end;

function TXmlNode.IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList): boolean;
var
  ThisSubNode, ThatSubNode: TXmlNode;
  NodeResult, ChildResult: boolean;
  // local
  procedure AddMismatchNode(ANode: TXmlNode);
  begin
    if assigned(MismatchNodes) then
      MismatchNodes.Add(ANode);
  end;
// local
  function NodeCompareOptions: boolean;
  begin
    // We assume there are differences
    Result := False;

    // node name
    if xcNodeName in Options then
      if Utf8CompareText(Name, ANode.Name) <> 0 then
        exit;

    // node type
    if xcNodeType in Options then
      if ElementType <> ANode.ElementType then
        exit;

    // node value
    if xcNodeValue in Options then
      if Utf8CompareText(Value, ANode.Value) <> 0 then
        exit;

    // attribute count
    if xcAttribCount in Options then
      if AttributeCount <> ANode.AttributeCount then
        exit;

    // child container count
    if xcChildCount in Options then
      if ContainerCount <> ANode.ContainerCount then
        exit;

    // If we arrive here, it means no differences were found, return True
    Result := True;
  end;
// local
  function ChildCompareOptions: boolean;
  var
    i: integer;
  begin
    Result := True;

    // child and attribute node names and values
    if Options * [xcChildNames, xcChildValues, xcAttribNames, xcAttribValues] <> [] then
    begin
      // iterate nodes
      for i := 0 to NodeCount - 1 do
      begin
        ThisSubNode := Nodes[i];
        if (ThisSubNode is TsdAttribute) or (ThisSubNode is TsdElement) then
        begin
          ThatSubNode := ANode.NodeByName(ThisSubNode.Name);
          if not assigned(ThatSubNode) then
          begin
            // No we dont have it
            if (xcChildNames in Options) or (xcAttribNames in Options) then
            begin
              AddMismatchNode(ThisSubNode);
              Result := False;
            end;
          end
          else
          begin
            // Do child and attribute value check
            if (xcChildValues in Options) or (xcAttribValues in Options) then
            begin
              if Utf8CompareText(ThisSubNode.Value, ThatSubNode.Value) <> 0 then
              begin
                AddMismatchNode(ThisSubNode);
                Result := False;
              end;
            end;
            // Do recursive check
            if xcRecursive in Options then
              if not ThisSubNode.IsEqualTo(ThatSubNode, Options, MismatchNodes) then
                Result := False;
          end;
        end;
      end;
    end;
  end;

// main
begin
  Result := False;
  if not assigned(ANode) then
    exit;

  // node compare options
  NodeResult := NodeCompareOptions;
  if NodeResult = False then
    AddMismatchNode(Self);

  // child compare options
  ChildResult := ChildCompareOptions;

  // final result
  Result := NodeResult and ChildResult;
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeByName(const AName: Utf8String): TXmlNode;

  function RemoveFirst(const DName: Utf8String; var RName: Utf8String): boolean;
  Var
    i: integer;
  begin
    i := Pos(':', DName);
    Result := i <> 0;
    if Result then
    begin
      RName := DName;
      System.Delete(RName, 1, i);
    end;
  end;

var
  i: integer;
  RName: Utf8String;
begin
  for i := 0 to GetNodeCount - 1 do
    if (Utf8CompareText(GetNodes(i).Name, AName) = 0) or ((RemoveFirst(GetNodes(i).Name, RName)) and (Utf8CompareText(RName, AName) = 0)) then
    begin
      Result := GetNodes(i);
      exit;
    end;
  Result := nil;
end;

function TXmlNode.GetNodeCount: integer;
begin
  // functionality is in descendant TsdContainerNode
  Result := 0;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodesClear;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeRemove(ANode: TXmlNode);
var
  Idx: integer;
begin
  Idx := NodeIndexOf(ANode);
  if Idx >= 0 then
    NodeDelete(Idx);
end;

procedure TXmlNode.NodeRemoveEx(ANode: TXmlNode);
// Remove the line of the xml file that has the subnode ANode in it.
// The subnode and accompanying character data will be freed.
var
  Idx, Idx1, Cnt: integer;
begin
  Idx := NodeIndexOf(ANode);
  Idx1 := Idx - 1;

  while (Idx1 >= 0) and (Nodes[Idx1] is TsdWhiteSpace) do
    dec(Idx1);

  inc(Idx1);

  Cnt := Idx - Idx1 + 1;
  while Cnt > 0 do
  begin
    NodeDelete(Idx1);
    dec(Cnt);
  end;
end;

function TXmlNode.NodeExtract(ANode: TXmlNode): TXmlNode;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  // functionality is in descendant TsdContainerNode
  Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeNew(const AName: Utf8String): TXmlNode;
// Add a new child node and return its pointer
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[ElementType];
  if not assigned(NodeClass) then
  begin
    Result := nil;
    exit;
  end;

  // Create new node
  Result := NodeClass.Create(TNativeXml(FOwner));
  if assigned(Result) then
  begin
    Result.Name := AName;
    NodeAdd(Result);
  end;
end;

function TXmlNode.NodeNewAtIndex(Index: integer; const AName: Utf8String): TXmlNode;
// Create a new node with AName, and insert it into the subnode list at location
// Index, and return a pointer to it.
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[ElementType];
  if not assigned(NodeClass) then
  begin
    Result := nil;
    exit;
  end;

  // Create new node
  Result := NodeClass.Create(TNativeXml(FOwner));
  if assigned(Result) then
  begin
    Result.Name := AName;
    NodeInsert(Index, Result);
  end;
end;

function TXmlNode.ParseStream(P: TsdXmlParser): TXmlNode;
begin
  // XmlNode parsing is abstract, but in descending nodes the parsing begins here.
  // The parsing starts right after Parser.ReadOpenTag and should stop after the
  // matching endtag of the node. The result should be the matching endnode.
  //
  // Example: TsdElement.ParseStream
  //
  // functionality in descendants
  Result := Self;
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if not assigned(A) then
    exit;
  A.Name := Value;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if not assigned(A) then
    exit;
  A.Value := Value;
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
  SetAttributeValue(Index, IntToStr(Value));
end;

procedure TXmlNode.SetAttributeValueByName(const AName, Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if not assigned(A) then
  begin
    A := TsdAttribute.Create(TNativeXml(FOwner));
    A.Name := AName;
    NodeAdd(A);
  end;
  A.Value := Value;
end;

procedure TXmlNode.SetName(const Value: Utf8String);
begin
  // functionality in descendants
  raise Exception.Create(sCannotSetName);
end;

procedure TXmlNode.SetNameUnicode(const Value: UnicodeString);
begin
  SetName(sdWideToUtf8(Value));
end;

procedure TXmlNode.SetValue(const Value: Utf8String);
begin
  // functionality in descendants
  raise Exception.Create(sCannotSetValue);
end;

procedure TXmlNode.SetValueUnicode(const Value: UnicodeString);
begin
  SetValue(sdWideToUtf8(Value));
end;

function TXmlNode.GetString(AID: integer): Utf8String;
var
  Table: TsdSymbolTable;
begin
  Result := '';
  if assigned(FOwner) then
  begin
    Table := TNativeXml(FOwner).FSymbolTable;
    if assigned(Table) then
      Result := Table.GetString(AID);
  end;
end;

function TXmlNode.AddString(const S: Utf8String): integer;
var
  Table: TsdSymbolTable;
begin
  Result := 0;
  if assigned(FOwner) then
  begin
    Table := TNativeXml(FOwner).FSymbolTable;
    if assigned(Table) then
      Result := Table.AddString(S)
  end;
end;

class function TXmlNode.Utf8ToWide(const S: Utf8String): UnicodeString;
begin
  Result := sdUtf8ToWide(S);
end;

function TXmlNode.TreeDepth: integer;
begin
  if assigned(FParent) then
    Result := FParent.TreeDepth + 1
  else
    Result := 0;
end;

class function TXmlNode.ReplaceString(const S: Utf8String): Utf8String;
begin
  Result := sdReplaceString(S);
end;

procedure TXmlNode.WriteStream(S: TStream);
begin
  // functionality is in descendants
end;

function TXmlNode.ReadAttributeBool(const AName: Utf8String; ADefault: boolean = False): boolean;
begin
  Result := StrToBoolDef(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeInteger(const AName: Utf8String; ADefault: integer = 0): integer;
begin
  Result := StrToIntDef(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeInt64(const AName: Utf8String; ADefault: int64): int64; // added by hdk
begin
  Result := StrToInt64Def(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeFloat(const AName: Utf8String; ADefault: double = 0): double;
begin
  Result := StrToFloatDef(AttributeValueByName[AName], ADefault, cXmlFormatSettings); // changed by hdk
end;

function TXmlNode.ReadAttributeString(const AName: Utf8String; ADefault: Utf8String = ''): Utf8String;
begin
  Result := AttributeValueByName[AName];
  if length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeUnicodeString(const AName: Utf8String; ADefault: UnicodeString): UnicodeString; // added by hdk
begin
  Result := sdUtf8ToWide(AttributeValueByName[AName]);
  if length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeAnsiString(const AName: Utf8String; ADefault: AnsiString): AnsiString; // added by hdk
begin
  Result := sdUtf8ToAnsi(AttributeValueByName[AName], CP_ACP);
  if length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeDateTime(const AName: Utf8String; ADefault: TDateTime): TDateTime; // added by hdk
begin
  Result := sdStringToDateTimeDef(AttributeValueByName[AName], ADefault, TNativeXml(FOwner).FUseLocalBias);
end;

function TXmlNode.ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsBoolDef(ADefault);
end;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.ReadPen(const AName: Utf8String; APen: TPen);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then
    with Child do
    begin
      // Read values
      APen.Color := ReadColor('Color', clBlack);
      APen.Mode := TPenMode(ReadInteger('Mode', integer(pmCopy)));
      APen.Style := TPenStyle(ReadInteger('Style', integer(psSolid)));
      APen.Width := ReadInteger('Width', 1);
    end
  else
  begin
    // Defaults
    APen.Color := clBlack;
    APen.Mode := pmCopy;
    APen.Style := psSolid;
    APen.Width := 1;
  end;
end;

procedure TXmlNode.ReadBrush(const AName: Utf8String; ABrush: TBrush);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then
    with Child do
    begin
      // Read values
      ABrush.Color := ReadColor('Color', clWhite);
      ABrush.Style := TBrushStyle(ReadInteger('Style', integer(bsSolid)));
    end
  else
  begin
    // Defaults
    ABrush.Bitmap := nil;
    ABrush.Color := clWhite;
    ABrush.Style := bsSolid;
  end;
end;

function TXmlNode.ReadColor(const AName: Utf8String; ADefault: TColor = 0): TColor;
begin
  Result := ReadInteger(AName, integer(ADefault));
end;
{$ENDIF USEGRAPHICS}

function TXmlNode.ReadDateTime(const AName: Utf8String; ADefault: TDateTime): TDateTime;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsDateTimeDef(ADefault);
end;

function TXmlNode.ReadFloat(const AName: Utf8String; ADefault: double): double;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsFloatDef(ADefault);
end;

function TXmlNode.ReadInteger(const AName: Utf8String; ADefault: integer): integer;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsIntegerDef(ADefault);
end;

function TXmlNode.ReadInt64(const AName: Utf8String; ADefault: int64): int64; // added by hdk
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsInt64Def(ADefault);
end;

function TXmlNode.ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.Value;
end;

function TXmlNode.ReadUnicodeString(const AName: Utf8String; const ADefault: UnicodeString): UnicodeString;
begin
  Result := sdUtf8ToWide(ReadString(AName, sdWideToUtf8(ADefault)));
end;

function TXmlNode.ReadAnsiString(const AName: Utf8String; const ADefault: AnsiString): AnsiString; // added by hdk
begin
  Result := sdUtf8ToAnsi(ReadString(AName, sdAnsiToUtf8(ADefault, CP_ACP)), CP_ACP);
end;

function TXmlNode.GetValueAsBoolDef(ADefault: boolean): boolean;
begin
  Result := StrToBoolDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsDateTimeDef(ADefault: TDateTime): TDateTime;
begin
  Result := sdStringToDateTimeDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsFloatDef(ADefault: double): double;
var
  V: Utf8String;
begin
  // backwards compat: old version used to allow commas in floats
  V := sdCommaToDot(GetValue);
  Result := StrToFloatDef(V, ADefault, cXmlFormatSettings); // changed by hdk
end;

function TXmlNode.GetValueAsIntegerDef(ADefault: integer): integer;
begin
  Result := StrToIntDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsInt64Def(ADefault: int64): int64;
begin
  Result := StrToInt64Def(GetValue, ADefault);
end;

function TXmlNode.GetValueAsBool: boolean;
begin
  Result := StrToBool(GetValue);
end;

function TXmlNode.GetValueAsDateTime: TDateTime;
begin
  Result := sdStringToDateTime(GetValue);
end;

function TXmlNode.GetValueAsFloat: double;
begin
{$IFDEF D7UP}
  Result := StrToFloat(GetValue, cXmlFormatSettings); // changed by hdk
{$ELSE D7UP}
  // D5 version
  Result := StrToFloat(GetValue);
{$ENDIF D7UP}
end;

function TXmlNode.GetValueAsInteger: integer;
begin
  Result := StrToInt(GetValue);
end;

function TXmlNode.GetValueAsInt64: int64;
begin
  Result := StrToInt64(GetValue);
end;

procedure TXmlNode.SetValueAsBool(const AValue: boolean);
begin
  SetValue(sdBoolToString(AValue));
end;

procedure TXmlNode.SetValueAsDate(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, True, False, 0, False));
end;

procedure TXmlNode.SetValueAsTime(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, False, True, TNativeXml(FOwner).SplitSecondDigits, TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.SetValueAsDateTime(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, True, True, TNativeXml(FOwner).SplitSecondDigits, TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.SetValueAsFloat(const AValue: double);
begin
  SetValue(sdFloatToString(AValue, TNativeXml(FOwner).FFloatSignificantDigits, TNativeXml(FOwner).FFloatAllowScientific));
end;

procedure TXmlNode.SetValueAsInteger(const AValue: integer);
begin
  SetValue(sdIntToString(AValue));
end;

procedure TXmlNode.SetValueAsInt64(const AValue: int64);
begin
  SetValue(sdInt64ToString(AValue));
end;

procedure TXmlNode.NodesByName(const AName: Utf8String; const AList: TList);
// Fill AList with nodes that have name AName
var
  i: integer;
begin
  if not assigned(AList) or not assigned(Self) then
    exit;
  AList.Clear;
  for i := 0 to GetNodeCount - 1 do
    if Utf8CompareText(Nodes[i].Name, AName) = 0 then
      AList.Add(Nodes[i]);
end;

procedure TXmlNode.WriteBool(const AName: Utf8String; AValue, ADefault: boolean);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdBoolToString(AValue));
end;

procedure TXmlNode.WriteDateTime(const AName: Utf8String; AValue, ADefault: TDateTime);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdDateTimeToString(AValue, True, True, TNativeXml(FOwner).FSplitSecondDigits, TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.WriteFloat(const AName: Utf8String; AValue, ADefault: double);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdFloatToString(AValue, TNativeXml(FOwner).FFloatSignificantDigits, TNativeXml(FOwner).FFloatAllowScientific));
end;

procedure TXmlNode.WriteHex(const AName: Utf8String; AValue, Digits: integer; ADefault: integer);
var
  HexString: Utf8String;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    HexString := '$' + Utf8String(IntToHex(AValue, Digits));
    WriteValue(AName, HexString);
  end;
end;

procedure TXmlNode.WriteInteger(const AName: Utf8String; AValue, ADefault: integer);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdIntToString(AValue));
end;

procedure TXmlNode.WriteInt64(const AName: Utf8String; AValue, ADefault: int64);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdInt64ToString(AValue));
end;

procedure TXmlNode.WriteString(const AName, AValue, ADefault: Utf8String);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, AValue);
end;

procedure TXmlNode.WriteUnicodeString(const AName: Utf8String; const AValue, ADefault: UnicodeString);
begin
  WriteString(AName, sdWideToUtf8(AValue), sdWideToUtf8(ADefault));
end;

procedure TXmlNode.WriteAnsiString(const AName: Utf8String; const AValue, ADefault: AnsiString); // added by hdk
begin
  WriteString(AName, sdAnsiToUtf8(AValue, CP_ACP), sdAnsiToUtf8(ADefault, CP_ACP));
end;

procedure TXmlNode.NodesAdd(Nodes: array of TXmlNode);
var
  x: integer;
begin
  for x := Low(Nodes) to High(Nodes) do
    NodeAdd(Nodes[x]);
end;

function TXmlNode.GetWriteOnDefault: boolean;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).WriteOnDefault
  else
    Result := False;
end;

procedure TXmlNode.SetWriteOnDefault(const Value: boolean);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).WriteOnDefault := Value;
end;

function TXmlNode.NodeFindOrCreate(const AName: Utf8String): TXmlNode;
// Find the node with AName, and if not found, add new one
begin
  Result := NodeByName(AName);
  if not assigned(Result) then
    Result := NodeNew(AName);
end;

function TXmlNode.NodeIndexByName(const AName: Utf8String): integer;
begin
  Result := 0;
  while Result < NodeCount do
  begin
    if Utf8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = NodeCount then
    Result := -1;
end;

function TXmlNode.AttributeIndexByName(const AName: Utf8String): integer;
begin
  Result := 0;
  // attributes are nodes from 0 to DirectNodeCount - 1
  while Result < DirectNodeCount do
  begin
    if Utf8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = DirectNodeCount then
    Result := -1;
end;

procedure TXmlNode.WriteValue(const AName, AValue: Utf8String);
var
  Child: TXmlNode;
begin
  Child := NodeFindOrCreate(AName);
  if assigned(Child) then
    Child.Value := AValue;
end;

procedure TXmlNode.DoProgress(Position: int64);
begin
  // Call the onprogress
  if assigned(FOwner) then
    TNativeXml(FOwner).DoProgress(Position);
end;

function TXmlNode.BufferLength: integer;
var
  BufData: Utf8String;
  BufPos: integer;
begin
  BufData := sdRemoveControlChars(GetValue);
  Result := length(BufData) div 4;
  if Result * 4 <> length(BufData) then
    raise EFilerError.Create(sErrorCalcStreamLength);
  Result := Result * 3;
  // Check padding chars
  BufPos := length(BufData);
  if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
  begin
    dec(BufPos);
    dec(Result);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      dec(Result);
  end;
end;

procedure TXmlNode.BufferRead(var Buffer; Count: integer; BinaryEncoding: TsdBinaryEncoding);
// Read data from XML base64/Binhex to the buffer (default is xbeBase64)
var
  BufData: Utf8String;
begin
  BufData := sdRemoveControlChars(GetValue);
  case BinaryEncoding of
    xbeBase64:
      // this is the default method
      DecodeBase64Buf(BufData, Buffer, Count);
    xbeBinHex:
      // for compat with older versions
      DecodeBinhexBuf(BufData, Buffer, Count);
  end;
end;

procedure TXmlNode.BufferWrite(const Buffer; Count: integer);
// Write data from the buffer to XML in base64 format
var
  BufData: Utf8String;
begin
  if Count > 0 then
    BufData := EncodeBase64Buf(Buffer, Count);

  // For comformity with Base64, we must add linebreaks
  SetValue(sdAddControlChars(BufData, GetEndOfLine + GetIndent));
end;

procedure TXmlNode.WriteAttributeInteger(const AName: Utf8String; AValue, ADefault: integer);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdIntToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeInt64(const AName: Utf8String; AValue, ADefault: int64); // added by hdk
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdInt64ToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeFloat(const AName: Utf8String; AValue, ADefault: double);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdFloatToString(AValue, TNativeXml(FOwner).FFloatSignificantDigits, TNativeXml(FOwner).FFloatAllowScientific);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeString(const AName: Utf8String; AValue, ADefault: Utf8String);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := AValue;
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeUnicodeString(const AName: Utf8String; const AValue, ADefault: UnicodeString);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdWideToUtf8(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeAnsiString(const AName: Utf8String; const AValue, ADefault: AnsiString); // added by hdk
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdAnsiToUtf8(AValue, CP_ACP);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeDateTime(const AName: Utf8String; AValue, ADefault: TDateTime);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdDateTimeToString(AValue, True, True, TNativeXml(FOwner).FSplitSecondDigits, TNativeXml(FOwner).FUseLocalBias);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeBool(const AName: Utf8String; AValue, ADefault: boolean);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdBoolToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

{$IFDEF USEGRAPHICS}

procedure TXmlNode.WritePen(const AName: Utf8String; APen: TPen);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', APen.Color, clBlack);
    WriteInteger('Mode', integer(APen.Mode), 0);
    WriteInteger('Style', integer(APen.Style), 0);
    WriteInteger('Width', APen.Width, 0);
  end;
end;

procedure TXmlNode.WriteBrush(const AName: Utf8String; ABrush: TBrush);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', ABrush.Color, clBlack);
    WriteInteger('Style', integer(ABrush.Style), 0);
  end;
end;

procedure TXmlNode.WriteColor(const AName: Utf8String; AValue, ADefault: TColor);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteHex(AName, ColorToRGB(AValue), 8, 0);
end;
{$ENDIF USEGRAPHICS}

function TXmlNode.GetBinaryString: RawByteString;
begin
  SetLength(Result, BufferLength);
  if length(Result) > 0 then
    BufferRead(Result[1], length(Result));
end;

procedure TXmlNode.SetBinaryString(const Value: RawByteString);
begin
  if length(Value) = 0 then
  begin
    SetValue('');
    exit;
  end;
  // fill the buffer
  BufferWrite(Value[1], length(Value));
end;

function TXmlNode.GetEolStyle: TsdEolStyle;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FEolStyle
  else
    Result := cDefaultEolStyle;
end;

function TXmlNode.GetPreserveWhiteSpace: boolean;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).GetPreserveWhiteSpace
  else
    Result := True;
end;

function TXmlNode.GetXmlFormat: TXmlFormatType;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FXmlFormat
  else
    Result := cDefaultXmlFormat;
end;

procedure TXmlNode.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).DoNodeLoaded(ANode);
end;

procedure TXmlNode.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).DoNodeNew(ANode);
end;

function TXmlNode.GetContent: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteContent(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TXmlNode.WriteContent(S: TStream);
begin
  // functionality in descendants
end;

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName, AttribValue: Utf8String; ShouldRecurse: boolean): TXmlNode;
// This function returns a pointer to the first subnode that has an attribute with
// name AttribName and value AttribValue.
var
  i: integer;
  Node: TXmlNode;
begin
  Result := nil;
  // Find all nodes that are potential results
  for i := 0 to NodeCount - 1 do
  begin
    Node := Nodes[i];
    if (Utf8CompareText(Node.Name, NodeName) = 0) and Node.HasAttribute(AttribName) and
      (Utf8CompareText(Node.AttributeValueByName[AttribName], AttribValue) = 0) then
    begin
      Result := Node;
      exit;
    end;
    // Recursive call
    if ShouldRecurse then
      Result := Node.NodeByAttributeValue(NodeName, AttribName, AttribValue, True);
    if assigned(Result) then
      exit;
  end;
end;

function TXmlNode.HasAttribute(const AName: Utf8String): boolean;
var
  i: integer;
begin
  for i := 0 to AttributeCount - 1 do
    if AttributeName[i] = AName then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TXmlNode.Clear;
begin
  // functionality in descendants
end;

procedure TXmlNode.DeleteEmptyNodes;
var
  i: integer;
  Node: TXmlNode;
begin
  for i := NodeCount - 1 downto 0 do
  begin
    Node := Nodes[i];
    // Recursive call
    Node.DeleteEmptyNodes;
    // Check if we should delete child node
    if Node.IsEmpty then
      NodeDelete(i);
  end;
end;

procedure TXmlNode.Assign(Source: TPersistent);
begin
  if Source is TXmlNode then
  begin
    CopyFrom(TXmlNode(Source));
  end
  else
    inherited;
end;

function TXmlNode.WriteToString: Utf8String;
var
  SS: TsdStringStream;
begin
  SS := TsdStringStream.Create('');
  try
    WriteStream(SS);
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure TXmlNode.CopyFrom(ANode: TObject);
begin
  if not(ANode is TXmlNode) then
    raise Exception.Create(sCopyFromOnlyWithXmlNode);

  Clear;
  // other functionality is in descendants
end;

function TXmlNode.FindNode(const NodeName: Utf8String): TXmlNode;
// Find the first node which has name NodeName. Contrary to the NodeByName
// function, this function will search the whole subnode tree, using the
// DepthFirst method.
var
  i: integer;
begin
  Result := nil;
  // Loop through all subnodes
  for i := 0 to NodeCount - 1 do
  begin
    Result := Nodes[i];
    // If the subnode has name NodeName then we have a result, exit
    if Result.CompareNodeName(NodeName) = 0 then
      exit;
    // If not, we will search the subtree of this node
    Result := Result.FindNode(NodeName);
    if assigned(Result) then
      exit;
  end;
end;

procedure TXmlNode.FindNodes(const NodeName: Utf8String; const AList: TList);
// local
  procedure FindNodesRecursive(ANode: TXmlNode; AList: TList);
  var
    i: integer;
    SubNode: TXmlNode;
  begin
    for i := 0 to ANode.NodeCount - 1 do
    begin
      SubNode := ANode.Nodes[i];
      if SubNode.CompareNodeName(NodeName) = 0 then
        AList.Add(SubNode);
      FindNodesRecursive(SubNode, AList);
    end;
  end;

// main
begin
  AList.Clear;
  FindNodesRecursive(Self, AList);
end;

function TXmlNode.CompareNodeName(const NodeName: Utf8String): integer;
begin
  // Compare with FullPath or local name based on NodeName's first character
  if length(NodeName) > 0 then
  begin
    if NodeName[1] = '/' then
    begin
      // FullPath
      Result := Utf8CompareText(FullPath, NodeName);
      exit;
    end;
  end;
  // local name
  Result := Utf8CompareText(Name, NodeName);
end;

function TXmlNode.GetFullPath: Utf8String;
// GetFullpath will return the complete path of the node from the root, e.g.
// /Root/SubNode1/SubNode2/ThisNode
begin
  Result := '/' + Name;
  if TreeDepth > 0 then
    // Recursive call
    Result := Parent.GetFullPath + Result;
end;

procedure TXmlNode.Delete;
begin
  if assigned(Parent) then
    Parent.NodeRemove(Self);
end;

function TXmlNode.GetDirectNodeCount: integer;
begin
  // functionality in descendants
  Result := 0;
end;

function TXmlNode.GetContainerCount: integer;
begin
  // functionality in descendants
  Result := 0;
end;

function TXmlNode.GetContainers(Index: integer): TXmlNode;
begin
  // functionality in descendants
  Result := nil;
end;

function TXmlNode.GetElementCount: integer;
begin
  // functionality in descendants
  Result := 0;
end;

function TXmlNode.GetElements(Index: integer): TsdElement;
begin
  // functionality in descendants
  Result := nil;
end;

function TXmlNode.GetDocument: TNativeXml;
begin
  if FOwner is TNativeXml then
    Result := TNativeXml(FOwner)
  else
    Result := nil;
end;

procedure TXmlNode.SetAttributeValueByNameWide(const AName: Utf8String; const Value: UnicodeString);
begin
  AttributeValueByName[AName] := sdWideToUtf8(Value);
end;

function TXmlNode.GetAttributeValueByNameWide(const AName: Utf8String): UnicodeString;
begin
  Result := sdUtf8ToWide(AttributeValueByName[AName]);
end;

function TXmlNode.IndexInParent: integer;
// Retrieve our index in the parent's nodelist
begin
  Result := -1;
  if assigned(Parent) then
    Result := Parent.NodeIndexOf(Self);
end;

{$IFDEF D7UP}

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName: Utf8String; const AttribValue: UnicodeString; ShouldRecurse: boolean): TXmlNode;
begin
  Result := NodeByAttributeValue(NodeName, AttribName, sdWideToUtf8(AttribValue), ShouldRecurse);
end;
{$ENDIF D7UP}

procedure TXmlNode.SortChildNodes(Compare: TXmlNodeCompareFunction);
// Sort the child nodes using the quicksort algorithm
// local
  function DoNodeCompare(Node1, Node2: TXmlNode): integer;
  begin
    if assigned(Compare) then
      Result := Compare(Node1, Node2)
    else
      Result := Utf8CompareText(Node1.Name, Node2.Name);
  end;
// local
  procedure QuickSort(iLo, iHi: integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while DoNodeCompare(Nodes[Lo], Nodes[Mid]) < 0 do
        inc(Lo);
      while DoNodeCompare(Nodes[Hi], Nodes[Mid]) > 0 do
        dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        NodeExchange(Lo, Hi);
        if Mid = Lo then
          Mid := Hi
        else if Mid = Hi then
          Mid := Lo;
        inc(Lo);
        dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then
      QuickSort(iLo, Hi);
    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;

// main
begin
  if NodeCount > 1 then
    QuickSort(0, NodeCount - 1);
end;

procedure TXmlNode.AttributesClear;
begin
  while AttributeCount > 0 do
  begin
    AttributeDelete(0);
  end;
end;

procedure TXmlNode.AttributeDelete(Index: integer);
var
  Attribute: TsdAttribute;
begin
  Attribute := Attributes[Index];
  if assigned(Attribute) then
    NodeRemove(Attribute);
end;

procedure TXmlNode.ElementsClear;
begin
  while ElementCount > 0 do
  begin
    ElementDelete(0);
  end;
end;

procedure TXmlNode.ElementDelete(Index: integer);
var
  Element: TsdElement;
begin
  Element := Elements[Index];
  if assigned(Element) then
    NodeRemove(Element);
end;

function TXmlNode.NextSibling(ANode: TXmlNode): TXmlNode;
begin
  // default is nil, iterating only starts from TsdContainerNode
  Result := nil;
end;

function TXmlNode.GetSourcePos: int64;
begin
{$IFDEF SOURCEPOS}
  Result := FSourcePos;
{$ELSE SOURCEPOS}
  Result := 0;
{$ENDIF SOURCEPOS}
end;

{ TsdCharData }

destructor TsdCharData.Destroy;
begin
  FValueID := 0;
  inherited;
end;

function TsdCharData.ElementType: TsdElementType;
begin
  Result := xeCharData;
end;

function TsdCharData.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdCharData.GetCoreValue: Utf8String;
begin
  Result := GetString(FValueID);
end;

function TsdCharData.GetPlatformValue: Utf8String;
begin
  Result := sdUnNormaliseEol(GetCoreValue, GetEolStyle);
end;

function TsdCharData.GetValue: Utf8String;
begin
  // value is the replaced, eol-unnormalized corevalue
  Result := sdReplaceString(sdUnNormaliseEol(GetCoreValue, GetEolStyle));
end;

function TsdCharData.GetValueUsingReferences(Nodes: array of TXmlNode): Utf8String;
var
  HasNonStandardReferences: boolean;
begin
  Result := sdReplaceString(sdUnNormaliseEol(GetCoreValue, GetEolStyle), HasNonStandardReferences, Nodes);
end;

procedure TsdCharData.SetCoreValue(const Value: Utf8String);
begin
  FValueID := AddString(Value);
end;

procedure TsdCharData.SetValue(const Value: Utf8String);
begin
  // core value is the escaped, eol-normalized value
  SetCoreValue(sdEscapeString(sdNormaliseEol(Value)))
end;

procedure TsdCharData.WriteStream(S: TStream);
begin
  // write the chardata: the platform value is the
  sdWriteToStream(S, GetPlatformValue);
  // sdWriteToStream(S, GetCoreValue);
end;

procedure TsdCharData.SetName(const Value: Utf8String);
begin
  // since the API is general with LINQ style, we allow a setter but in the
  // XML the chardata name will not be present
  if length(Value) > 0 then
    DoDebugOut(Self, wsHint, sCannotSetName);
end;

procedure TsdCharData.CopyFrom(ANode: TObject);
begin
  inherited;
  SetCoreValue(TsdCharData(ANode).GetCoreValue);
end;

function TsdCharData.HasNonStandardReferences: boolean;
var
  Res: boolean;
begin
  sdReplaceString(GetCoreValue, Res);
  Result := Res;
end;

function TsdCharData.IsWhiteSpace: boolean;
var
  S: Utf8String;
begin
  S := TNativeXml(FOwner).FSymbolTable.GetString(FValueID);
  S := sdTrim(S, Result);
end;

{ TsdWhitespace }

function TsdWhiteSpace.ElementType: TsdElementType;
begin
  Result := xeWhiteSpace;
end;

{ TsdAttribute }

procedure TsdAttribute.CopyFrom(ANode: TObject);
begin
  inherited;
  // copy local data
  FCoreValue.CopyFrom(TsdAttribute(ANode).FCoreValue);
  SetName(TsdAttribute(ANode).GetName);
end;

constructor TsdAttribute.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCoreValue := TsdQuotedText.Create(AOwner);
  FCoreValue.FParent := Self;
end;

destructor TsdAttribute.Destroy;
begin
  FNameID := 0;
  FreeAndNil(FCoreValue);
  inherited;
end;

function TsdAttribute.ElementType: TsdElementType;
begin
  Result := xeAttribute;
end;

function TsdAttribute.GetName: Utf8String;
begin
  Result := GetString(FNameID);
end;

function TsdAttribute.GetValue: Utf8String;
begin
  if assigned(FCoreValue) then
    Result := sdReplaceString(FCoreValue.GetCoreValue)
  else
    Result := '';
end;

function TsdAttribute.ParseStream(P: TsdXmlParser): TXmlNode;
var
  IsTrimmed: boolean;
begin
  Result := Self;
{$IFDEF SOURCEPOS}
  FSourcePos := P.Position;
{$ENDIF SOURCEPOS}
  // Get the attribute name
  FNameID := AddString(sdTrim(P.ReadStringUntilChar('='), IsTrimmed));
  if assigned(FCoreValue) then
    // value
    FCoreValue.ParseStream(P);
end;

procedure TsdAttribute.SetName(const Value: Utf8String);
begin
  FNameID := AddString(Value);
end;

procedure TsdAttribute.SetValue(const Value: Utf8String);
begin
  // FCoreValue is directly created in TsdAttribute.Create, so safe
  FCoreValue.SetCoreValue(sdEscapeString(Value));
end;

procedure TsdAttribute.WriteStream(S: TStream);
begin
  sdWriteToStream(S, GetName + '=');
  // now add the quoted value
  if assigned(FCoreValue) then
    FCoreValue.WriteStream(S);
end;

{ TsdQuotedText }

procedure TsdQuotedText.CopyFrom(ANode: TObject);
begin
  inherited;
  FQuoteStyle := TsdQuotedText(ANode).FQuoteStyle;
end;

constructor TsdQuotedText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuoteStyle := qsQuote;
end;

function TsdQuotedText.ElementType: TsdElementType;
begin
  Result := xeQuotedText;
end;

function TsdQuotedText.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdQuotedText.ParseStream(P: TsdXmlParser): TXmlNode;
var
  Blanks: Utf8String;
  QuoteChar: AnsiChar;
begin
  Result := Self;
  // Get the quoted value
  QuoteChar := P.NextCharSkipBlanks(Blanks);
  if QuoteChar = cQuoteCharStyleNames[qsQuote] then
    FQuoteStyle := qsQuote
  else if QuoteChar = cQuoteCharStyleNames[qsApos] then
    FQuoteStyle := qsApos
  else
  begin
    DoDebugOut(Self, wsWarn, Format(sQuoteCharExpected, [P.Position]));
    if Document.FixStructuralErrors then
    begin
      // an unquoted value.. we try to read till space
      QuoteChar := ' ';
      P.MoveBack;
    end
    else
      exit;
  end;

  FValueID := AddString(P.ReadQuotedString(QuoteChar));
end;

procedure TsdQuotedText.WriteStream(S: TStream);
var
  QC: Utf8String;
begin
  QC := cQuoteCharStyleNames[FQuoteStyle];
  sdWriteToStream(S, QC + GetPlatformValue + QC);
  DoProgress(S.Position);
end;

{ TsdContainerNode }

constructor TsdContainerNode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TsdNodeList.Create(True);
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

destructor TsdContainerNode.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

procedure TsdContainerNode.AttributeAdd(AAttribute: TsdAttribute);
begin
  if (AAttribute = nil) or (AAttribute.FOwner <> FOwner) then
  begin
    DoDebugOut(Self, wsFail, sXmlOwnerNotAssigned);
    exit;
  end;
  NodeInsert(FDirectNodeCount, AAttribute);
  inc(FDirectNodeCount);
end;

function TsdContainerNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := FNodes.ByType(AType);
end;

function TsdContainerNode.GetNodes(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < FNodes.Count) then
    Result := FNodes[Index]
  else
    Result := nil;
end;

function TsdContainerNode.HasSubContainers: boolean;
var
  i: integer;
begin
  // determine if there is at least one subcontainer
  Result := False;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TsdContainerNode.NodeAdd(ANode: TXmlNode): integer;
// EcoSoft Fix revert From 4.01
begin
  Result := -1;
  if not assigned(ANode) then
    exit;

  // attributes and whitespace are handled separately because NodeAdd may be called with
  // attributes after elements in client apps (even tho this is not best practice)
  if (ANode is TsdAttribute) or (ANode is TsdWhiteSpace) then
  begin
    // attributes inserted at FDirectNodeCount (and this value incremented)
    FNodes.Insert(FDirectNodeCount, ANode);
    Result := FDirectNodeCount;
    inc(FDirectNodeCount);
  end
  else
  begin
    // other subnodes like elements and CharData: add at the end of the list
    Result := FNodes.Add(ANode);
  end;
  ANode.FParent := Self;
end;

function TsdContainerNode.GetNodeCount: integer;
begin
  Result := FNodes.Count
end;

procedure TsdContainerNode.NodeDelete(Index: integer);
begin
  FNodes.Delete(Index);

  // BUGFIX: node index below FDirectNodeCount: decrease FDirectNodeCount
  // ("below" instead of "below or on")
  if Index < FDirectNodeCount then
    dec(FDirectNodeCount);

  if FValueIndex >= 0 then
  begin
    // node index = FValueIndex: set FValueIndex to -1
    if Index = FValueIndex then
    begin
      FValueIndex := -1;
      exit;
    end;
    // node index below FValueIndex: decrease FValueIndex
    if Index < FValueIndex then
      dec(FValueIndex);
  end;
end;

procedure TsdContainerNode.NodeExchange(Index1, Index2: integer);
begin
  FNodes.Exchange(Index1, Index2);
end;

function TsdContainerNode.NodeExtract(ANode: TXmlNode): TXmlNode;
begin
  Result := TXmlNode(FNodes.Extract(ANode));
end;

function TsdContainerNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := FNodes.IndexOf(ANode);
end;

procedure TsdContainerNode.NodesClear;
var
  i: integer;
begin
  for i := NodeCount - 1 downto 0 do
  begin
    NodeDelete(i);
  end;
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

procedure TsdContainerNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  FNodes.Insert(Index, ANode);
  ANode.FParent := Self;
end;

procedure TsdContainerNode.NodeInsertNear(ANode, AOther: TXmlNode; IsBefore: boolean);
var
  Idx: integer;
begin
  Idx := NodeIndexOf(AOther);
  if IsBefore then
    NodeInsert(Idx, ANode)
  else
    NodeInsert(Idx + 1, ANode);
end;

function TsdContainerNode.ParseAttributeList(P: TsdXmlParser): AnsiChar;
var
  Blanks: Utf8String;
  AttributeNode: TsdAttribute;
  WhiteSpaceNode: TsdWhiteSpace;
begin
  repeat
    Result := P.NextCharSkipBlanks(Blanks);
    if length(Blanks) > 0 then
    begin
      if Blanks <> ' ' then
      begin
        DoDebugOut(Self, wsHint, Format(sNonDefaultChardata, [P.LineNumber, P.Position]));
        // add non-default blank chardata in attribute list
        if GetPreserveWhiteSpace then
        begin
          WhiteSpaceNode := TsdWhiteSpace.Create(TNativeXml(FOwner));
          NodeAdd(WhiteSpaceNode);
          inc(FDirectNodeCount);
          WhiteSpaceNode.SetCoreValue(Blanks);
        end;
      end
    end;

    // Are any of the characters determining the end?
    if Result in ['!', '/', '>', '?'] then
      exit;

    // regular user error, signal this
    if Result in cQuoteChars then
    begin

      DoDebugOut(Self, wsWarn, Format('illegal quote at pos %d', [P.Position]));

    end
    else
    begin

      P.MoveBack;
      AttributeNode := TsdAttribute.Create(TNativeXml(FOwner));
      AttributeAdd(AttributeNode);
      DoNodeNew(AttributeNode);
      AttributeNode.ParseStream(P);
      DoNodeLoaded(AttributeNode);

    end;
  until P.EndOfStream;
end;

function TsdContainerNode.ParseFixStructuralErrors(const AEndTagName: Utf8String): TXmlNode;
var
  Depth: integer;
  DeeperNodeName: Utf8String;
  Idx: integer;
begin
  // check if there is a parent node with this name that is already parsed
  Depth := 0;
  repeat
    if assigned(FParent) then
      DeeperNodeName := FParent.Name
    else
      DeeperNodeName := '';

    if DeeperNodeName = AEndTagName then
    begin
      // this is the parent's node name, so we must defer execution to the parent
      DoDebugOut(Self, wsHint, Format('parent%d = "%s", this endtag = "%s": maybe "%s" should be closed', [Depth, DeeperNodeName, AEndTagName,
        GetName]));

      // add to the single tag names list
      if not TNativeXml(FOwner).FSingleTagNames.Find(GetName, Idx) then
        TNativeXml(FOwner).FSingleTagNames.Add(GetName);

      // we now break
      break;
    end;

    // move the node to a lower hierarchy
    if assigned(FParent) and assigned(FParent.Parent) then
    begin
      DoDebugOut(Self, wsInfo, Format('moving node "%s" from parent "%s" to grandparent "%s"', [GetName, FParent.Name, FParent.Parent.Name]));

      // add to the single tag names list
      if not TNativeXml(FOwner).FSingleTagNames.Find(Parent.Name, Idx) then
        TNativeXml(FOwner).FSingleTagNames.Add(Parent.Parent.Name);

      FParent.NodeExtract(Self);
      FParent.Parent.NodeAdd(Self);
    end;

    inc(Depth);
  until length(DeeperNodeName) = 0;

  // signal that this parser hierarchy is no longer valid
  Result := FParent;
end;

function TsdContainerNode.ParseElementList(P: TsdXmlParser; const SupportedTags: TsdElementTypes): TXmlNode;
// parse the element list, the result (endnode) should be this element
var
  B: AnsiChar;
  BeginTagName, EndTagName: Utf8String;
  Tag: TsdElementType;
  NodeClass: TsdNodeClass;
  SubNode, EndNode: TXmlNode;
  EndNodeName: Utf8String;
  IsTrimmed: boolean;
begin
  Result := nil;

  repeat
    // Process char data
    ParseIntermediateData(P);

    // Process subtags and end tag
    if P.EndOfStream then
    begin
      DoDebugOut(Self, wsFail, Format(sPrematureEnd, [P.Position]));
      exit;
    end;
    P.MoveBack;

    B := P.NextChar;
    if B = '<' then
    begin

      // Determine tag type
      Tag := P.ReadOpenTag;
      if not(Tag in SupportedTags) then
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [cElementTypeNames[Tag], P.Position]));
        exit;
      end;

      // End tag?
      if Tag = xeEndTag then
      begin
        // up front this is the end tag so the result is this node
        Result := Self;

        // Read end tag
        EndTagName := sdTrim(P.ReadStringUntilChar('>'), IsTrimmed);
        FNodeClosingStyle := ncFull;

        // Check if begin and end tags match
        if GetName <> EndTagName then
        begin
          BeginTagName := GetName;

          // usually a user error with omitted direct end tag
          DoDebugOut(Self, wsWarn, Format(sBeginEndMismatch, [GetName, EndTagName, P.LineNumber, P.Position]));

          // try to fix structural errors?
          if TNativeXml(FOwner).FFixStructuralErrors then
            Result := ParseFixStructuralErrors(EndTagName);

        end;

        // We're done reading this element, so we will set the capacity of the
        // nodelist to just the amount of items to avoid having overhead.
        FNodes.SetCapacity(FNodes.Count);
        exit;
      end;

      // Determine node class
      NodeClass := cNodeClass[Tag];
      if not assigned(NodeClass) then
      begin
        DoDebugOut(Self, wsFail, Format(sUnsupportedTag, [P.Position]));
        exit;
      end;

      // Create new node and add
      SubNode := NodeClass.Create(TNativeXml(FOwner));
      NodeAdd(SubNode);
      if Tag <> xeElement then
        DoNodeNew(SubNode);

      // The node will parse itself
      EndNode := SubNode.ParseStream(P);
      if EndNode <> SubNode then
      begin
        if assigned(EndNode) then
          EndNodeName := EndNode.GetName
        else
          EndNodeName := 'nil';
        DoDebugOut(Self, wsWarn, Format(sLevelMismatch, [SubNode.GetName, EndNodeName, P.LineNumber, P.Position]));
        Result := EndNode;
        exit;
      end;

      // CDATA subnodes could provide the value of the element
      if SubNode is TsdCData then
      begin
        if FValueIndex < 0 then
          FValueIndex := FNodes.Count - 1;
      end;

      DoNodeLoaded(SubNode);

    end
    else
    begin
      // Since this virtual proc is also used for doctype parsing.. check
      // end char here
      if (B = ']') and (ElementType = xeDocType) then
        break;
    end;
  until TNativeXml(FOwner).FAbortParsing or P.EndOfStream;
end;

procedure TsdContainerNode.ParseIntermediateData(P: TsdXmlParser);
begin
  // default does nothing
end;

function TsdContainerNode.ParseQuotedTextList(P: TsdXmlParser): AnsiChar;
var
  Blanks: Utf8String;
  QuotedTextNode: TsdQuotedText;
begin
  repeat
    Result := P.NextCharSkipBlanks(Blanks);
    if (length(Blanks) > 0) and (Blanks <> ' ') then
    begin
      DoDebugOut(Self, wsHint, Format(sNonDefaultChardata, [P.Position]));
    end;

    // Are any of the characters determining the end?
    // if Result in ['!', '/', '>' ,'?'] then
    // avoid question mark "?" in quoted text (this proc only called by TsdDtdElement)
    if Result in ['!', '/', '>'] then
      exit;

    P.MoveBack;
    QuotedTextNode := TsdQuotedText.Create(TNativeXml(FOwner));
    NodeAdd(QuotedTextNode);
    DoNodeNew(QuotedTextNode);
    QuotedTextNode.ParseStream(P);
    DoNodeLoaded(QuotedTextNode);
  until P.EndOfStream;
end;

procedure TsdContainerNode.WriteAttributeList(S: TStream; Count: integer);
var
  i: integer;
  PrevSubNode, ThisSubNode: TXmlNode;
begin
  PrevSubNode := nil;
  for i := 0 to Count - 1 do
  begin
    ThisSubNode := FNodes[i];
    // write attributes and intermingled chardata
    if ThisSubNode is TsdAttribute then
    begin
      if not(PrevSubNode is TsdCharData) then
        // write blank if there is no previous chardata
        sdWriteToStream(S, ' ');
      // write attribute
      ThisSubNode.WriteStream(S);
    end;
    if ThisSubNode is TsdCharData then
    begin
      // write chardata
      ThisSubNode.WriteStream(S);
    end;

    // next iteration
    PrevSubNode := ThisSubNode;
  end;
end;

procedure TsdContainerNode.Clear;
begin
  inherited;
  FNodes.Clear;
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

procedure TsdContainerNode.CopyFrom(ANode: TObject);
var
  i: integer;
  ThisSubNode, ThatSubNode: TXmlNode;
  NodeClass: TsdNodeClass;
begin
  inherited;

  // copy nodes
  for i := 0 to TsdContainerNode(ANode).FNodes.Count - 1 do
  begin
    ThatSubNode := TsdContainerNode(ANode).FNodes[i];
    NodeClass := TsdNodeClass(ThatSubNode.ClassType);
    ThisSubNode := NodeClass.Create(TNativeXml(FOwner));
    FNodes.Add(ThisSubNode);
    ThisSubNode.FParent := Self;
    ThisSubNode.CopyFrom(ThatSubNode);
  end;

  // copy local data
  FDirectNodeCount := TsdContainerNode(ANode).FDirectNodeCount;
  FValueIndex := TsdContainerNode(ANode).FValueIndex;
end;

function TsdContainerNode.GetContainers(Index: integer): TXmlNode;
var
  i, Idx: integer;
begin
  Result := nil;
  Idx := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
    begin
      if Idx = Index then
      begin
        Result := TsdContainerNode(FNodes[i]);
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TsdContainerNode.GetContainerCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
      inc(Result);
  end;
end;

function TsdContainerNode.GetElements(Index: integer): TsdElement;
var
  i, Idx: integer;
begin
  Result := nil;
  Idx := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdElement then
    begin
      if Idx = Index then
      begin
        Result := TsdElement(FNodes[i]);
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TsdContainerNode.GetElementCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdElement then
      inc(Result);
  end;
end;

function TsdContainerNode.GetDirectNodeCount: integer;
begin
  Result := FDirectNodeCount;
end;

function TsdContainerNode.NextSibling(ANode: TXmlNode): TXmlNode;
begin
  Result := FNodes.GetNextSiblingOf(ANode);
end;

function TsdContainerNode.GetNodeClosingStyle: TsdNodeClosingStyle;
begin
  Result := TNativeXml(FOwner).NodeClosingStyle;
  if Result = ncDefault then
    Result := FNodeClosingStyle;
end;

{ TsdElement }

procedure TsdElement.CopyFrom(ANode: TObject);
begin
  inherited;
  // copy other data
  SetName(TsdElement(ANode).GetName);
  FNodeClosingStyle := TsdElement(ANode).FNodeClosingStyle;
end;

function TsdElement.ElementType: TsdElementType;
begin
  Result := xeElement;
end;

function TsdElement.GetName: Utf8String;
begin
  Result := GetString(FNameID);
end;

function TsdElement.GetValue: Utf8String;
begin
  // Return the value of the CharData subnode designated by the parser
  if (FValueIndex >= 0) and (FValueIndex < FNodes.Count) then
  begin
    // chardata value at FValueIndex
    // This calls TsdCharData.GetValue(),
    // then TsdCharData.GetCoreValue().
    Result := FNodes[FValueIndex].Value;

    // BUGFIX: no longer allowed, value is already unnormalized in chardata
    { // do un-normalisation if mac/windows
      if GetEolStyle <> esLF then
      Result := sdUnNormaliseEol(Result, GetEolStyle); }

  end
  else
    // default value
    Result := '';
end;

procedure TsdElement.ParseIntermediateData(P: TsdXmlParser);
var
  S, CharDataString: Utf8String;
  CharDataNode: TsdCharData;
  WhiteSpaceNode: TsdCharData;
{$IFDEF SOURCEPOS}
  SourcePos: int64;
{$ENDIF SOURCEPOS}
  PreString, PostString: Utf8String;
begin
{$IFDEF SOURCEPOS}
  SourcePos := P.Position;
{$ENDIF SOURCEPOS}
  S := P.ReadStringUntilChar('<');
  CharDataString := sdTrim(S, PreString, PostString);

  if GetPreserveWhiteSpace and (length(PreString) > 0) then
  begin
    WhiteSpaceNode := TsdWhiteSpace.Create(TNativeXml(FOwner));
    WhiteSpaceNode.FValueID := AddString(PreString);
    NodeAdd(WhiteSpaceNode);
  end;

  if length(CharDataString) > 0 then
  begin
    // Insert CharData node
    CharDataNode := TsdCharData.Create(TNativeXml(FOwner));
{$IFDEF SOURCEPOS}
    CharDataNode.FSourcePos := SourcePos;
{$ENDIF SOURCEPOS}
    CharDataNode.FValueID := AddString(CharDataString);
    NodeAdd(CharDataNode);

    // ParseIntermediateData can be called multiple times from ParseElementList.
    // if there was no chardata node yet before, this is the value index
    if FValueIndex = -1 then
    begin
      FValueIndex := FNodes.Count - 1;
    end;

    DoNodeNew(CharDataNode);
    DoNodeLoaded(CharDataNode);
  end;

  if GetPreserveWhiteSpace and (length(PostString) > 0) then
  begin
    WhiteSpaceNode := TsdWhiteSpace.Create(TNativeXml(FOwner));
    WhiteSpaceNode.FValueID := AddString(PostString);
    NodeAdd(WhiteSpaceNode);
  end;
end;

function TsdElement.ParseStream(P: TsdXmlParser): TXmlNode;
var
  Ch: AnsiChar;
  AName: Utf8String;
  IsTrimmed: boolean;
begin
  Result := Self;

  // Flush the reader.
  P.Flush;

  // the index of the chardata subnode that will hold the value, initially -1
  FValueIndex := -1;

{$IFDEF SOURCEPOS}
  FSourcePos := P.Position;
{$ENDIF SOURCEPOS}
  // Parse name
  AName := sdTrim(P.ReadStringUntilBlankOrEndTag, IsTrimmed);
  SetName(AName);

  DoNodeNew(Self);

  // Parse attribute list
  Ch := ParseAttributeList(P);

  // up till now attributes and optional chardata are direct nodes
  FDirectNodeCount := FNodes.Count;

  if Ch = '/' then
  begin
    // Direct tag
    Ch := P.NextChar;
    if Ch <> '>' then
    begin
      DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [Ch, P.LineNumber, P.Position]));
      exit;
    end;
    NodeClosingStyle := ncClose;
  end
  else
  begin
    if Ch <> '>' then
    begin
      DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [Ch, P.LineNumber, P.Position]));
      exit;
    end;

    // parse subelements
    Result := ParseElementList(P, [xeElement .. xeCData, xeInstruction .. xeEndTag]);
  end;

  // progress for elements
  DoProgress(P.Position);
end;

procedure TsdElement.SetName(const Value: Utf8String);
begin
  FNameID := AddString(Value);
end;

procedure TsdElement.SetValue(const Value: Utf8String);
var
  Res: Utf8String;
  Node: TXmlNode;
begin
  if length(Value) > 0 then
  begin
    // value that will be set.
    Res := Value;

    // add or update a value
    if FValueIndex < 0 then
    begin

      // we do not have a value node, so we will add it after FDirectNodeCount
      Node := TsdCharData.Create(TNativeXml(FOwner));
      Node.Value := Res;
      NodeInsert(FDirectNodeCount, Node);
      FValueIndex := FDirectNodeCount;

    end
    else
    begin

      // just update the value
      FNodes[FValueIndex].Value := Res;

    end;
  end
  else
  begin
    // remove the value
    if FValueIndex >= 0 then
      NodeDelete(FValueIndex); // this sets it to -1 as well
  end;
end;

procedure TsdElement.WriteStream(S: TStream);
var
  i: integer;
  SubNode: TXmlNode;
  HasSubElements: boolean;
begin
  // determine if there is at least one subelement
  HasSubElements := HasSubContainers;

  // write element
  sdStreamWriteString(S, GetIndent + '<' + GetName);

  // write attributes
  WriteAttributeList(S, FDirectNodeCount);

  if (FNodes.Count = FDirectNodeCount) and (NodeClosingStyle = ncClose) then
  begin

    // directly write close tag
    sdStreamWriteString(S, TNativeXml(FOwner).FDirectCloseTag);
    sdStreamWriteString(S, GetEndOfLine);

  end
  else
  begin
    // indirect node
    sdStreamWriteString(S, '>');

    // write sub-nodes
    for i := FDirectNodeCount to FNodes.Count - 1 do
    begin
      SubNode := FNodes[i];

      // due to optional chardatas after the parent we use these "if"s
      if (i = FDirectNodeCount) and not(SubNode is TsdCharData) then
      begin
        sdStreamWriteString(S, GetEndOfLine);
      end;
      if (i > FDirectNodeCount) and (SubNode is TsdCharData) and HasSubElements then
      begin
        sdStreamWriteString(S, SubNode.GetIndent);
      end;

      if (SubNode is TsdElement) or (SubNode is TsdCharData) then
        SubNode.WriteStream(S);

      if HasSubElements and (SubNode is TsdCharData) then
        sdStreamWriteString(S, GetEndOfLine);
    end;

    // endtag
    if HasSubElements then
      sdStreamWriteString(S, GetIndent);

    sdStreamWriteString(S, '</' + GetName + '>' + GetEndOfLine);
  end;

  DoProgress(S.Position);
end;

{ TsdDeclaration }

function TsdDeclaration.ElementType: TsdElementType;
begin
  Result := xeDeclaration;
end;

function TsdDeclaration.GetEncoding: Utf8String;
begin
  Result := AttributeValueByName['encoding'];
end;

function TsdDeclaration.GetName: Utf8String;
begin
  Result := 'xml';
end;

function TsdDeclaration.GetVersion: Utf8String;
begin
  Result := AttributeValueByName['version'];
end;

function TsdDeclaration.ParseStream(P: TsdXmlParser): TXmlNode;
var
  B: AnsiChar;
begin
  Result := Self;

  // Directly parse the attribute list
  B := ParseAttributeList(P);
  if B <> '?' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, P.LineNumber, P.Position]));
    exit;
  end;
  B := P.NextChar;
  if B <> '>' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, P.LineNumber, P.Position]));
    exit;
  end;

  // declaration is special, we check $0D and $0A and allow them at the end
  B := P.NextChar;
  if B <> #$0D then
    P.MoveBack;

  B := P.NextChar;
  if B <> #$0A then
    P.MoveBack;
end;

procedure TsdDeclaration.SetEncoding(const Value: Utf8String);
begin
  AttributeValueByName['encoding'] := Value;
end;

procedure TsdDeclaration.SetVersion(const Value: Utf8String);
begin
  AttributeValueByName['version'] := Value;
end;

procedure TsdDeclaration.WriteStream(S: TStream);
begin
  // XML declaration <?xml{declaration}?>
  sdWriteToStream(S, GetIndent + '<?xml');
  WriteAttributeList(S, FNodes.Count);

  sdWriteToStream(S, '?>');

  sdWriteToStream(S, GetSeparator);
  DoProgress(S.Position);
end;

{ TsdComment }

function TsdComment.ElementType: TsdElementType;
begin
  Result := xeComment;
end;

function TsdComment.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdComment.ParseStream(P: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  FValueID := AddString(P.ReadStringUntil('-->'));
end;

procedure TsdComment.WriteStream(S: TStream);
begin
  // Comment <!--{comment}-->
  sdWriteToStream(S, '<!--' + GetCoreValue + '-->');
  DoProgress(S.Position);
end;

{ TsdCData }

function TsdCData.ElementType: TsdElementType;
begin
  Result := xeCData;
end;

function TsdCData.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdCData.GetValue: Utf8String;
begin
  Result := GetString(FValueID);
end;

function TsdCData.ParseStream(P: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  // assumes that the "<![CDATA[" is aleady parsed
  FValueID := AddString(P.ReadStringUntil(']]>'));
end;

procedure TsdCData.SetValue(const Value: Utf8String);
begin
  FValueID := AddString(Value);
end;

procedure TsdCData.WriteStream(S: TStream);
begin
  // literal data <![CDATA[{data}]]>
  sdWriteToStream(S, '<![CDATA[' + GetCoreValue + ']]>');
  DoProgress(S.Position);
end;

{ TsdDocType }

procedure TsdDocType.CopyFrom(ANode: TObject);
begin
  inherited;
  // copy depending data
  FExternalID.CopyFrom(TsdDocType(ANode).FExternalID);
  FSystemLiteral.CopyFrom(TsdDocType(ANode).FSystemLiteral);
  FPubIDLiteral.CopyFrom(TsdDocType(ANode).FPubIDLiteral);
end;

constructor TsdDocType.Create(AOwner: TComponent);
begin
  inherited;
  FExternalID := TsdCharData.Create(AOwner);
  FSystemLiteral := TsdQuotedText.Create(AOwner);
  FPubIDLiteral := TsdQuotedText.Create(AOwner);
end;

destructor TsdDocType.Destroy;
begin
  FreeAndNil(FExternalID);
  FreeAndNil(FSystemLiteral);
  FreeAndNil(FPubIDLiteral);
  inherited;
end;

function TsdDocType.ElementType: TsdElementType;
begin
  Result := xeDocType;
end;

function TsdDocType.GetName: Utf8String;
begin
  Result := GetString(FNameID);
end;

procedure TsdDocType.ParseIntermediateData(P: TsdXmlParser);
// DocType has no value, just add whitespace node if there are non-default blanks
var
  Blanks: Utf8String;
  B: AnsiChar;
  WhiteSpaceNode: TsdWhiteSpace;
begin
  repeat
    B := P.NextCharSkipBlanks(Blanks);

    if GetPreserveWhiteSpace and (length(Blanks) > 0) and (Blanks <> ' ') then
    begin
      WhiteSpaceNode := TsdWhiteSpace.Create(TNativeXml(FOwner));
      WhiteSpaceNode.FValueID := AddString(Blanks);
      NodeAdd(WhiteSpaceNode);
    end;

    if not(B in [']', '<']) then
    begin
      DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, P.Position]));
      P.ReadStringUntilBlankOrEndTag
    end
    else
      break;
  until False;
end;

function TsdDocType.ParseStream(P: TsdXmlParser): TXmlNode;
var
  Blanks1, Blanks2, Blanks3, Blanks4: Utf8String;
  B: AnsiChar;
  IsTrimmed: boolean;
begin
  Result := Self;
  // sequence <!DOCTYPE is already parsed here
  // Parse name
  P.NextCharSkipBlanks(Blanks1);
  P.MoveBack;
  SetName(sdTrim(P.ReadStringUntilBlankOrEndTag, IsTrimmed));
  P.NextCharSkipBlanks(Blanks2);
  P.MoveBack;
  B := P.NextChar;
  if not(B in ['[', '>']) then
  begin
    P.MoveBack;
    // Parse external ID
    if P.CheckString('SYSTEM') then
    begin
      FExternalID.Value := 'SYSTEM';
      FSystemLiteral.ParseStream(P);
    end
    else
    begin
      if P.CheckString('PUBLIC') then
      begin
        FExternalID.Value := 'PUBLIC';
        FPubIDLiteral.ParseStream(P);
        FSystemLiteral.ParseStream(P);
      end
      else
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, P.Position]));
        exit;
      end;
    end;
    B := P.NextCharSkipBlanks(Blanks3);
  end;
  if B = '[' then
  begin
    Result := ParseElementList(P,
      // we allow these elements in the DTD
      [xeComment, xeDtdElement, xeDtdAttList, xeDtdEntity, xeDtdNotation, xeInstruction, xeCharData]);
    B := P.NextCharSkipBlanks(Blanks4);
  end;
  if B <> '>' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, P.Position]));
  end;
end;

procedure TsdDocType.SetName(const Value: Utf8String);
begin
  FNameID := AddString(Value);
end;

procedure TsdDocType.WriteStream(S: TStream);
var
  i: integer;
  Line: Utf8String;
begin
  Line := GetIndent + '<!DOCTYPE ' + GetName;
  sdWriteToStream(S, Line);

  // in case of specific external id
  if FExternalID.Value = 'SYSTEM' then
  begin
    Line := ' SYSTEM ';
    sdWriteToStream(S, Line);
    FSystemLiteral.WriteStream(S);
  end;
  if FExternalID.Value = 'PUBLIC' then
  begin
    Line := ' PUBLIC ';
    sdWriteToStream(S, Line);
    FPubIDLiteral.WriteStream(S);
    sdWriteToStream(S, ' ');
    FSystemLiteral.WriteStream(S);
  end;

  sdWriteToStream(S, ' ');

  if GetNodeCount > 0 then
  begin
    sdWriteToStream(S, '[' + GetEndOfLine);
    for i := 0 to GetNodeCount - 1 do
    begin
      Nodes[i].WriteStream(S);
    end;
    sdWriteToStream(S, ']');
  end;
  sdWriteToStream(S, '>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdDtdElement }

function TsdDtdElement.ElementType: TsdElementType;
begin
  Result := xeDtdElement;
end;

function TsdDtdElement.GetValue: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteContent(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TsdDtdElement.ParseContent(P: TsdXmlParser);
var
  CharDataNode: TsdCharData;
begin
  CharDataNode := TsdCharData.Create(TNativeXml(FOwner));
  NodeAdd(CharDataNode);
  DoNodeNew(CharDataNode);
  CharDataNode.FValueID := AddString(P.ReadStringUntil('>'));
  DoNodeLoaded(CharDataNode);
end;

function TsdDtdElement.ParseStream(P: TsdXmlParser): TXmlNode;
var
  Blanks1, Blanks2: Utf8String;
  IsTrimmed: boolean;
begin
  Result := Self;
  P.NextCharSkipBlanks(Blanks1);
  P.MoveBack;
  Name := sdTrim(P.ReadStringUntilBlankOrEndTag, IsTrimmed);
  P.NextCharSkipBlanks(Blanks2);
  P.MoveBack;
  // parse the content of element, attlist, entity, etc.
  // default ParseContent is in TsdDtdElement, and TsdDtdEntity has overridden
  // method.
  ParseContent(P);
end;

procedure TsdDtdElement.WriteContent(S: TStream);
var
  i: integer;
begin
  if GetNodeCount > 0 then
  begin
    for i := 0 to GetNodeCount - 1 do
    begin
      Nodes[i].WriteStream(S);
    end;
  end;
end;

procedure TsdDtdElement.WriteStream(S: TStream);
var
  ElementTypeString: Utf8String;
begin
  case ElementType of
    xeDtdElement:
      ElementTypeString := 'ELEMENT';
    xeDtdAttList:
      ElementTypeString := 'ATTLIST';
    xeDtdEntity:
      ElementTypeString := 'ENTITY';
    xeDtdNotation:
      ElementTypeString := 'NOTATION';
  else
    raise EFilerError.Create(sUnsupportedTag);
  end; // case

  // write front matter
  sdStreamWriteString(S, '<!' + ElementTypeString + ' ' + GetName + ' ');

  // write content
  WriteContent(S);

  // write end matter
  sdStreamWriteString(S, '>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdDtdAttList }

function TsdDtdAttList.ElementType: TsdElementType;
begin
  Result := xeDtdAttList;
end;

{ TsdDtdEntity }

function TsdDtdEntity.ElementType: TsdElementType;
begin
  Result := xeDtdEntity;
end;

procedure TsdDtdEntity.ParseContent(P: TsdXmlParser);
var
  Ch: AnsiChar;
begin
  // list of quotedtext
  Ch := ParseQuotedTextList(P);
  if Ch <> '>' then
    DoDebugOut(Self, wsFail, Format(sIllegalEndTag, [Ch, P.LineNumber, P.Position]));
end;

{ TsdDtdNotation }

function TsdDtdNotation.ElementType: TsdElementType;
begin
  Result := xeDtdNotation;
end;

{ TsdInstruction }

function TsdInstruction.ElementType: TsdElementType;
begin
  Result := xeInstruction;
end;

function TsdInstruction.GetName: Utf8String;
begin
  Result := 'PI';
end;

function TsdInstruction.ParseStream(P: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  FValueID := AddString(P.ReadStringUntil('?>'));
end;

procedure TsdInstruction.WriteStream(S: TStream);
var
  Line: Utf8String;
begin
  // processing instruction <?{value}?>
  Line := GetIndent + '<?' + GetValue + '?>' + GetEndOfLine;
  sdWriteToStream(S, Line);
  DoProgress(S.Position);
end;

{ TsdStyleSheet }

function TsdStyleSheet.ElementType: TsdElementType;
begin
  Result := xeStylesheet;
end;

function TsdStyleSheet.GetName: Utf8String;
begin
  Result := 'xml-stylesheet';
end;

{ function TsdStyleSheet.ParseStream(P: TsdXmlParser): TXmlNode;
  var
  B: AnsiChar;
  begin
  Result := Self;
  // Directly parse the attribute list
  B := ParseAttributeList(P);
  if B <> '?' then
  begin
  DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, P.LineNumber, P.Position]));
  exit;
  end;
  B := P.NextChar;
  if B <> '>' then
  begin
  DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, P.LineNumber, P.Position]));
  exit;
  end;
  end; }

procedure TsdStyleSheet.WriteStream(S: TStream);
begin
  // Stylesheet <?xml-stylesheet{stylesheet}?>
  sdWriteToStream(S, GetIndent + '<?xml-stylesheet');
  WriteAttributeList(S, FNodes.Count);
  sdWriteToStream(S, '?>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdNodeList }

function TsdNodeList.ByType(AType: TsdElementType): TXmlNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ElementType = AType then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

constructor TsdNodeList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
end;

function TsdNodeList.GetItems(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

function TsdNodeList.FindFirst: TXmlNode;
begin
  if Count = 0 then
    Result := nil
  else
    Result := Items[0];
end;

function TsdNodeList.FindNext(ANode: TXmlNode): TXmlNode;
var
  Last: TXmlNode;
begin
  Result := nil;
  if not assigned(ANode) then
    exit;

  if ANode.NodeCount > 0 then
  begin
    Result := ANode.Nodes[0];
    exit;
  end;

  while assigned(ANode) do
  begin
    Last := GetLastSiblingOf(ANode);
    if ANode = Last then
    begin
      ANode := ANode.Parent;
    end
    else
    begin
      Result := GetNextSiblingOf(ANode);
      exit;
    end;
  end;

end;

function TsdNodeList.GetNextSiblingOf(ANode: TXmlNode): TXmlNode;
var
  Parent: TXmlNode;
  Idx: integer;
begin
  Parent := ANode.Parent;
  if Parent = nil then
  begin
    Idx := IndexOf(ANode);
    if Idx < 0 then
      raise Exception.Create('index must be >= 0');
    Result := Items[Idx + 1];
  end
  else
  begin
    Idx := Parent.NodeIndexOf(ANode);
    Result := Parent.Nodes[Idx + 1];
  end;
end;

function TsdNodeList.GetLastSiblingOf(ANode: TXmlNode): TXmlNode;
var
  Parent: TXmlNode;
  LastIdx: integer;
begin
  Result := nil;
  if ANode = nil then
    exit;

  Parent := ANode.Parent;
  if Parent = nil then
  begin
    LastIdx := Count - 1;
    if LastIdx >= 0 then
      Result := Items[LastIdx];
  end
  else
  begin
    LastIdx := Parent.NodeCount - 1;
    if LastIdx >= 0 then
      Result := Parent.Nodes[LastIdx];
  end;
end;

{ TDebugComponent }

procedure TDebugComponent.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
var
  AOwner: TComponent;
begin
  AOwner := Self;
  while AOwner is TDebugComponent do
  begin
    if assigned(TDebugComponent(AOwner).FOnDebugOut) then
    begin
      TDebugComponent(AOwner).FOnDebugOut(Sender, WarnStyle, AMessage);
      exit;
    end;
    AOwner := AOwner.Owner;
  end;
end;

{ TDebugPersistent }

constructor TDebugPersistent.CreateDebug(AOwner: TDebugComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDebugPersistent.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if FOwner is TDebugComponent then
    TDebugComponent(FOwner).DoDebugOut(Sender, WarnStyle, AMessage);
end;

{ TsdFastMemStream }

procedure TsdFastMemStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

constructor TsdFastMemStream.Create(InitialCapacity: integer);
begin
  inherited Create;
  FFib1 := InitialCapacity div 2;
  FCapacity := InitialCapacity;
  if FFib1 < 4 then
    FFib1 := 4;
  if FCapacity < 4 then
    FCapacity := 4;
  ReallocMem(FMemory, FCapacity);
end;

destructor TsdFastMemStream.Destroy;
begin
  ReallocMem(FMemory, 0);
  inherited;
end;

procedure TsdFastMemStream.LoadFromFile(AFilename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TsdFastMemStream.LoadFromStream(Stream: TStream);
var
  Count: longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then
    Stream.ReadBuffer(FMemory^, Count);
end;

function TsdFastMemStream.Read(var Buffer; Count: integer): longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      Move(Pointer(longint(FMemory) + FPosition)^, Buffer, Result);
      inc(FPosition, Result);
      exit;
    end;
  end;
  Result := 0;
end;

procedure TsdFastMemStream.SaveToFile(AFilename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TsdFastMemStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then
    Stream.WriteBuffer(FMemory^, FSize);
end;

function TsdFastMemStream.Seek(Offset: integer; Origin: Word): longint;
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      inc(FPosition, Offset);
    soFromEnd:
      FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TsdFastMemStream.SetCapacity(Value: longint);
// Fibonacci 0,1,1,2,3,5,8,...  FCapacity is Fib2.
// Fibonacci is a natural growing function where
// 0 + 1 = 1; 1 + 1 = 2; 1 + 2 = 3; 2 + 3 = 5; etc
var
  Fib3: longint;
begin
  while FCapacity < Value do
  begin
    Fib3 := FFib1 + FCapacity;
    FFib1 := FCapacity;
    FCapacity := Fib3;
  end;
  ReallocMem(FMemory, FCapacity);
end;

procedure TsdFastMemStream.SetSize(NewSize: longint);
var
  OldPosition: longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soFromEnd);
end;

function TsdFastMemStream.Write(const Buffer; Count: integer): longint;
var
  NewPos: longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    NewPos := FPosition + Count;
    if NewPos > 0 then
    begin
      if NewPos > FSize then
      begin
        if NewPos > FCapacity then
          SetCapacity(NewPos);
        FSize := NewPos;
      end;
      System.Move(Buffer, Pointer(longint(FMemory) + FPosition)^, Count);
      FPosition := NewPos;
      Result := Count;
      exit;
    end;
  end;
  Result := 0;
end;

{ TsdStringStream }

constructor TsdStringStream.Create(const S: Utf8String);
begin
  inherited Create;
  SetSize(length(S));
  if Size > 0 then
  begin
    Write(S[1], Size);
    Position := 0;
  end;
end;

function TsdStringStream.DataString: Utf8String;
begin
  SetLength(Result, Size);
  if Size > 0 then
  begin
    Position := 0;
    Read(Result[1], length(Result));
  end;
end;

{ TsdBufferWriter }

constructor TsdBufferWriter.Create(ASource: TStream; AChunkSize: integer);
begin
  inherited Create;
  FSource := ASource;
  FChunkSize := AChunkSize;
  SetLength(FRawBuffer, FChunkSize);
end;

destructor TsdBufferWriter.Destroy;
begin
  // write the last chunk, if any
  WriteChunk(FRawPosition);
  // free the rawbuffer
  SetLength(FRawBuffer, 0);
  inherited;
end;

function TsdBufferWriter.Read(var Buffer; Count: integer): longint;
begin
  // not implemented
  raise Exception.Create('not implemented');
end;

function TsdBufferWriter.Write(const Buffer; Count: integer): longint;
var
  Idx, Siz: integer;
begin
  // index in the source buffer
  Idx := 0;
  // remaining size
  Siz := Count;

  // surplus
  while FRawPosition + Siz >= FChunkSize do
  begin
    Move(TByteArray(Buffer)[Idx], FRawBuffer[FRawPosition], FChunkSize - FRawPosition);
    WriteChunk(FChunkSize);
    dec(Siz, FChunkSize - FRawPosition);
    inc(Idx, FChunkSize - FRawPosition);
    FRawPosition := 0;
  end;

  // copy the raw buffer
  Move(TByteArray(Buffer)[Idx], FRawBuffer[FRawPosition], Siz);
  inc(FRawPosition, Siz);

  Result := Count;
end;

procedure TsdBufferWriter.WriteChunk(Count: integer);
begin
  if Count > 0 then
  begin
    FSource.WriteBuffer(FRawBuffer[0], Count);
  end;
end;

{ TsdSymbolTable }

function TsdSymbolTable.AddString(const S: Utf8String): integer;
var
  Found: boolean;
  L, BySymbolIndex: integer;
  ASymbol, Item: TsdSymbol;
begin
  Result := 0;
  L := length(S);

  // zero-length string
  if L = 0 then
    exit;

  ASymbol := TsdSymbol.Create;
  try
    ASymbol.FFirst := Pbyte(@S[1]);
    ASymbol.FCharCount := L;

    // Try to find the new string
    Found := TsdSymbolList(FBySymbol).Find(ASymbol, BySymbolIndex);
    if Found then
    begin
      // yes it is found
      Item := TsdSymbol(FBySymbol[BySymbolIndex]);
      Result := Item.FID;
      exit;
    end;

    // Not found.. must make new item
    Item := TsdSymbol.Create;
    Item.FCharCount := ASymbol.FCharCount;

    // reallocate memory and copy the string data
    ReallocMem(Item.FFirst, Item.FCharCount);
    Move(S[1], Item.FFirst^, Item.FCharCount);

    // add to the ByID objectlist
    FByID.Add(Item);
    Item.FID := FByID.Count;
    Result := Item.FID;

    // insert into the ByRS list
    FBySymbol.Insert(BySymbolIndex, Item);

  finally
    // this ensures we do not deallocate the memory that may be in use elsewhere
    ASymbol.FFirst := nil;
    ASymbol.Free;
  end;

end;

procedure TsdSymbolTable.Clear;
begin
  FByID.Clear;
  FBySymbol.Clear;
end;

procedure TsdSymbolTable.ClearFrequency;
var
  i: integer;
begin
  for i := 0 to FByID.Count - 1 do
    TsdSymbol(FByID[i]).FFreq := 0;
end;

constructor TsdSymbolTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FByID := TObjectList.Create(True);
  FBySymbol := TsdSymbolList.Create(False);
end;

destructor TsdSymbolTable.Destroy;
begin
  FreeAndNil(FBySymbol);
  FreeAndNil(FByID);
  inherited;
end;

function TsdSymbolTable.GetSymbolCount: integer;
begin
  Result := FByID.Count;
end;

function TsdSymbolTable.GetString(ID: integer): Utf8String;
begin
  // Find the ID

  // zero string
  if ID <= 0 then
  begin
    Result := '';
    exit;
  end;

  // out of bounds?
  if ID > FByID.Count then
  begin
    // output warning
    DoDebugOut(Self, wsWarn, 'string ID not found');
    Result := '';
  end;

  Result := TsdSymbol(FByID[ID - 1]).AsString;
end;

procedure TsdSymbolTable.IncrementFrequency(ID: integer);
var
  RS: TsdSymbol;
begin
  RS := TsdSymbol(FByID[ID - 1]);
  inc(RS.FFreq);
end;

procedure TsdSymbolTable.LoadFromFile(const AFilename: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(AFilename);
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TsdSymbolTable.LoadFromStream(S: TStream);
var
  i: integer;
  TableCount: Cardinal;
begin
  Clear;

  // DoDebugOut(Self, wsInfo, format('stream position: %d', [S.Position]));

  // table count
  TableCount := sdStreamReadCardinal(S);
  if TableCount = 0 then
    exit;

  for i := 0 to TableCount - 1 do
  begin
    LoadSymbol(S);
  end;
end;

function TsdSymbolTable.LoadSymbol(S: TStream): Cardinal;
var
  Symbol: TsdSymbol;
  BySymbolIndex: integer;
  Found: boolean;
begin
  Symbol := TsdSymbol.Create;

  // For now, we just use ssString uniquely as symbol style,.
  // In updates, different symbol styles can be added.
  Symbol.FSymbolStyle := sdStreamReadCardinal(S);

  Symbol.FCharCount := sdStreamReadCardinal(S);

  if Symbol.FCharCount > 0 then
  begin
    // reallocate memory and copy the string data
    ReallocMem(Symbol.FFirst, Symbol.FCharCount);
    S.Read(Symbol.FFirst^, Symbol.FCharCount);
  end;

  // add to the ByID objectlist
  FByID.Add(Symbol);
  Symbol.FID := FByID.Count;
  Result := Symbol.FID;

  // find the symbol
  Found := TsdSymbolList(FBySymbol).Find(Symbol, BySymbolIndex);
  if Found then
  begin
    DoDebugOut(Self, wsFail, 'duplicate symbol!');
    exit;
  end;

  // insert into the ByRS list
  FBySymbol.Insert(BySymbolIndex, Symbol);
end;

procedure TsdSymbolTable.SaveToFile(const AFilename: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S, SymbolCount);
    S.SaveToFile(AFilename);
  finally
    S.Free;
  end;
end;

procedure TsdSymbolTable.SaveToStream(S: TStream; ACount: integer);
var
  i: integer;
begin
  // write (part of the) symbol table
  sdStreamWriteCardinal(S, ACount);
  for i := 0 to ACount - 1 do
  begin
    SaveSymbol(S, i + 1);
  end;
end;

procedure TsdSymbolTable.SaveSymbol(S: TStream; ASymbolID: Cardinal);
var
  RS: TsdSymbol;
  StringVal: Utf8String;
  CharCount: Cardinal;
begin
  if ASymbolID <= 0 then
    DoDebugOut(Self, wsFail, 'symbol ID <= 0');
  RS := TsdSymbol(FByID[ASymbolID - 1]);

  // For now, we just use ssString uniquely as symbol style.
  // In updates, different symbol styles can be added.
  sdStreamWriteCardinal(S, RS.SymbolStyle);

  StringVal := RS.AsString;
  CharCount := length(StringVal);
  sdStreamWriteCardinal(S, CharCount);
  sdStreamWriteString(S, StringVal);
end;

procedure TsdSymbolTable.SortByFrequency(var ANewIDs: array of Cardinal);
// local
  function CompareFreq(Pos1, Pos2: integer): integer;
  var
    RS1, RS2: TsdSymbol;
  begin
    RS1 := TsdSymbol(FByID[Pos1]);
    RS2 := TsdSymbol(FByID[Pos2]);
    if RS1.FFreq > RS2.FFreq then
      Result := -1
    else if RS1.FFreq < RS2.FFreq then
      Result := 1
    else
      Result := 0;
  end;
// local
  procedure QuickSort(iLo, iHi: integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while CompareFreq(Lo, Mid) < 0 do
        inc(Lo);
      while CompareFreq(Hi, Mid) > 0 do
        dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        FByID.Exchange(Lo, Hi);
        if Mid = Lo then
          Mid := Hi
        else if Mid = Hi then
          Mid := Lo;
        inc(Lo);
        dec(Hi);
      end;
    until Lo > Hi;

    if Hi > iLo then
      QuickSort(iLo, Hi);

    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;

// main
var
  i: integer;
begin
  // sort by frequency
  QuickSort(0, FByID.Count - 1);

  // plural count
  FPluralSymbolCount := 0;
  i := 0;
  while i < FByID.Count do
  begin
    if TsdSymbol(FByID[i]).FFreq >= 2 then
      inc(FPluralSymbolCount)
    else
      break;
    inc(i);
  end;

  // tell app about new ID
  for i := 0 to FByID.Count - 1 do
  begin
    ANewIDs[TsdSymbol(FByID[i]).FID] := i + 1;
  end;

  // then rename IDs
  for i := 0 to FByID.Count - 1 do
  begin
    TsdSymbol(FByID[i]).FID := i + 1;
  end;
end;

{ TNativeXml }

function TNativeXml.Canonicalize: integer;
var
  C14N: TsdXmlCanonicalizer;
begin
  C14N := TsdXmlCanonicalizer.Create(Self);
  try
    Result := C14N.Canonicalize(Self);
  finally
    C14N.Free;
  end;
end;

procedure TNativeXml.Clear;
begin
  ClearData(FHasDeclaration, FHasDocType, FHasRoot);
end;

procedure TNativeXml.ClearData(AHasDeclaration, AHasDocType, AHasRoot: boolean);
var
  Declaration: TsdDeclaration;
  DocType: TsdDocType;
  Root: TXmlNode;
begin
  // clear symboltable and rootnodes
  FSymbolTable.Clear;
  FRootNodes.Clear;

  // build default items in rootnodes

  // add declaration
  if AHasDeclaration then
  begin
    Declaration := TsdDeclaration.Create(Self);
    Declaration.Version := cDefaultVersionString;
    Declaration.Encoding := cDefaultEncodingString;
    // when NativeXml is cleared instead of parsed,
    // the declaration needs to be initialised
    // using the two attributes
    Declaration.FDirectNodeCount := 2;
    FRootNodes.Add(Declaration);
  end;

  // add doctype
  if AHasDocType then
  begin
    DocType := TsdDocType.Create(Self);
    DocType.Name := FRootName;
    DocType.ExternalID.Value := 'SYSTEM';
    FRootNodes.Add(DocType);
  end;

  // add the root element
  if AHasRoot then
  begin
    Root := GetRootNodeClass.Create(Self);
    Root.Name := FRootName;
    FRootNodes.Add(Root);
  end;
end;

constructor TNativeXml.Create;
begin
  Create(Nil);
end;

constructor TNativeXml.CreateEx(AOwner: TComponent; HasDeclaration, HasDocType, HasRoot: boolean; ARootName: Utf8String);
begin
  inherited Create(AOwner);

  // the symboltable holds all the string snippets (UTF8) in this component
  FSymbolTable := TsdSymbolTable.Create(Self);

  // FRootNodes is an owned list
  FRootNodes := TsdNodeList.Create(True);

  // CreateEx options
  FHasDeclaration := HasDeclaration;
  FHasDocType := HasDocType;
  FHasRoot := HasRoot;
  FRootName := ARootName;

  // this resets defaults
  ResetDefaults;

  // now clear the rootnodes and create optional declaration, doctype and root
  ClearData(FHasDeclaration, FHasDocType, FHasRoot);
end;

constructor TNativeXml.CreateName(const ARootName: Utf8String; AOwner: TComponent);
begin
  // we create a standard declaration and root element with rootname
  CreateEx(AOwner, True, False, True, ARootName);
end;

constructor TNativeXml.Create(AOwner: TComponent);
begin
  // simple constructor without declaration, but with a standard root element
  CreateEx(AOwner, False, False, True, '');
end;

destructor TNativeXml.Destroy;
begin
  FreeAndNil(FRootNodes);
  FreeAndNil(FSymbolTable);
  inherited;
end;

procedure TNativeXml.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOnNodeLoaded) then
    FOnNodeLoaded(Self, ANode);
end;

procedure TNativeXml.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOnNodeNew) then
    FOnNodeNew(Self, ANode);
end;

procedure TNativeXml.DoProgress(Position: int64);
begin
  if assigned(FOnProgress) then
    FOnProgress(Self, Position);
end;

function TNativeXml.GetCommentString: Utf8String;
// Get the first comment node, and return its value
var
  Node: TXmlNode;
begin
  Result := '';
  Node := FRootNodes.ByType(xeComment);
  if assigned(Node) then
    Result := Node.Value;
end;

function TNativeXml.GetOrCreateDeclarationNode: TXmlNode;
begin
  // write declaration node (if not there)
  Result := FRootNodes[0];
  if not(Result is TsdDeclaration) then
  begin
    Result := TsdDeclaration.Create(Self);
    FRootNodes.Insert(0, Result);
  end;
end;

function TNativeXml.GetCharset: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Encoding;
end;

function TNativeXml.GetPreserveWhiteSpace: boolean;
begin
  Result := (FXmlFormat = xfPreserve);
end;

function TNativeXml.GetParserLineNumber(P: TsdXmlParser): int64;
begin
  if assigned(P) then
    Result := P.LineNumber
  else
    Result := 0;
end;

function TNativeXml.GetParserPosition(P: TsdXmlParser): int64;
begin
  if assigned(P) then
    Result := P.Position
  else
    Result := 0;
end;

function TNativeXml.GetDeclaration: TsdDeclaration;
begin
  // the first xeDeclaration node in the root nodes
  Result := TsdDeclaration(FRootNodes.ByType(xeDeclaration));
end;

function TNativeXml.GetDocType: TsdDocType;
begin
  // the first xeDocType node in the root nodes
  Result := TsdDocType(FRootNodes.ByType(xeDocType));
end;

function TNativeXml.GetRoot: TsdElement;
begin
  // the first xeElement node in the root nodes
  Result := TsdElement(FRootNodes.ByType(xeElement));
end;

function TNativeXml.GetRootNodeClass: TsdNodeClass;
begin
  // default node class is TsdElement
  Result := TsdElement;
end;

function TNativeXml.GetRootNodeCount: integer;
begin
  Result := FRootNodes.Count;
end;

function TNativeXml.GetRootContainers(Index: integer): TsdContainerNode;
var
  i, Idx: integer;
begin
  Result := nil;
  Idx := 0;
  for i := 0 to FRootNodes.Count - 1 do
  begin
    if FRootNodes[i] is TsdContainerNode then
    begin
      if Idx = Index then
      begin
        Result := TsdContainerNode(FRootNodes[i]);
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TNativeXml.GetRootContainerCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FRootNodes.Count - 1 do
  begin
    if FRootNodes[i] is TsdContainerNode then
      inc(Result);
  end;
end;

function TNativeXml.GetStyleSheet: TsdStyleSheet;
begin
  Result := TsdStyleSheet(FRootNodes.ByType(xeStylesheet));
  if not assigned(Result) then
  begin
    // Add a stylesheet node as second one if none present
    Result := TsdStyleSheet.Create(Self);
    FRootNodes.Insert(1, Result);
  end;
end;

function TNativeXml.GetVersionString: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Version;
end;

function TNativeXml.IsEmpty: boolean;
var
  R: TXmlNode;
begin
  R := GetRoot;
  Result := not assigned(R) or R.IsClear;
end;

function TNativeXml.LineFeed: Utf8String;
begin
  case FXmlFormat of
    xfReadable:
      Result := #13#10;
    xfCompact:
      Result := #10;
  else
    Result := #10;
  end; // case
end;

procedure TNativeXml.LoadFromBinaryFile(const AFilename: string);
var
  Bxm: TsdBinaryXml;
begin
  Bxm := TsdBinaryXml.Create(Self);
  try
    SetBinaryDocument(Bxm);
    Bxm.LoadFromFile(AFilename);
  finally
    Bxm.Free;
  end;
end;

procedure TNativeXml.LoadFromBinaryStream(AStream: TStream);
var
  Bxm: TsdBinaryXml;
  DeclarationNode: TsdDeclaration;
  DeclarationEncodingString: Utf8String;
begin
  Bxm := TsdBinaryXml.Create(Self);
  try
    SetBinaryDocument(Bxm);
    Bxm.LoadFromStream(AStream);

    // after loading the bxm, we must set external encoding and external codepage
    // in case we save the original xml later
    DeclarationNode := GetDeclaration;
    if assigned(DeclarationNode) then
    begin
      DeclarationEncodingString := DeclarationNode.Encoding;
      FExternalEncoding := sdCharsetToStringEncoding(DeclarationEncodingString);
      FExternalCodePage := sdCharsetToCodePage(DeclarationEncodingString);
    end;
  finally
    Bxm.Free;
  end;
end;

{$IFDEF MSWINDOWS}

function TNativeXml.LoadFromURL(const URL: Utf8String): int64;
var
  M: TMemoryStream;
  NetHandle, UrlHandle: HINTERNET;
  Buffer: array [0 .. $400 - 1] of AnsiChar;
  BytesRead: Cardinal;
begin
  Result := 0;

  NetHandle := InternetOpenA('nativexml', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if not assigned(NetHandle) then
  begin
    // NetHandle is not valid.
    DoDebugOut(Self, wsFail, 'Unable to initialize WinInet');
    exit;
  end;

  try
    UrlHandle := InternetOpenUrlA(NetHandle, PAnsiChar(URL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not assigned(UrlHandle) then
    begin
      // UrlHandle is not valid.
      DoDebugOut(Self, wsFail, Format('Cannot open URL %s', [URL]));
      exit;
    end;

    M := TMemoryStream.Create;
    try
      // UrlHandle valid? Proceed with download
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
        if BytesRead > 0 then
          M.Write(Buffer, BytesRead);
      until BytesRead = 0;

      InternetCloseHandle(UrlHandle);

      // now load the stream
      M.Position := 0;
      LoadFromStream(M);
      // final size in bytes of the url stream
      Result := M.Size;

    finally
      M.Free;
    end;

  finally
    InternetCloseHandle(NetHandle);
  end;
end;
{$ELSE MSWINDOWS}

function TNativeXml.LoadFromURL(const URL: Utf8String): int64;
begin
  DoDebugOut(Self, wsFail, 'not implemented (needs WININET)');
  Result := 0;
end;
{$ENDIF MSWINDOWS}

procedure TNativeXml.LoadFromFile(const AFilename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TNativeXml.LoadFromStream(AStream: TStream);
var
  Parser: TsdXmlParser;
  S: TMemoryStream;
begin
  FSymbolTable.Clear;
  FRootNodes.Clear;

  Parser := TsdXmlParser.Create(AStream, cParserChunkSize);
  try
    Parser.Owner := Self;

    try
      // we also allow binary xml since v4.02
      if Parser.IsBinaryXml then
      begin
        AStream.Position := 0;
        // do this encapsulation because AStream could be
        // a filestream and this buffering will be faster
        S := TMemoryStream.Create;
        try
          S.LoadFromStream(AStream);
          LoadFromBinaryStream(S);
        finally
          S.Free;
        end;
        exit;
      end;

      // parse the stream
      ParseStream(Parser);

      // copy encoding data from the parser
      FExternalEncoding := Parser.Encoding;
      FExternalCodePage := Parser.Codepage;
      FExternalBomInfo := Parser.BomInfo;

    finally
      // final onprogress
      DoProgress(AStream.Size);
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TNativeXml.MoveSubNodes(AList: TsdNodeList; FromNode, ToNode: TXmlNode);
var
  i: integer;
  Node: TXmlNode;
begin
  if (AList = nil) or (FromNode = nil) or (ToNode = nil) then
    exit;
  if AList.Count = 0 then
    exit;

  // move subnodes
  for i := 0 to AList.Count - 1 do
  begin
    Node := AList[i];
    if Node.Parent = FromNode then
    begin
      FromNode.NodeExtract(Node);
      ToNode.NodeAdd(Node);
    end;
  end;
end;

procedure TNativeXml.New;
begin
  // backward-compatible procedure New: Ensure FHasDeclaration option and then
  // clear the document
  FHasDeclaration := True;
  Clear;
end;

procedure TNativeXml.ParseStream(Parser: TsdXmlParser);
var
  B: AnsiChar;
  ElementType: TsdElementType;
  NodeClass: TsdNodeClass;
  Node: TXmlNode;
  StringData: Utf8String;
  CD: TsdCharData;
{$IFDEF SOURCEPOS}
  SP: int64;
{$ENDIF SOURCEPOS}
  IsTrimmed: boolean;
  DeclarationEncodingString: Utf8String;
  i, Idx: integer;
  Parent, Content: TXmlNode;
  Container: TsdContainerNode;
  HasClosed: boolean;
begin
  FAbortParsing := False;

  // prepare for "FixStructuralErrors"
  if FFixStructuralErrors then
  begin
    FSingleTagNames := TStringList.Create;
    FSingleTagNames.Sorted := True;
  end
  else
    FSingleTagNames := nil;

  // read BOM
  Parser.ReadBOM;

  // store external bominfo for use later when writing
  FExternalBomInfo := Parser.BomInfo;

  // Read next tag
  repeat
{$IFDEF SOURCEPOS}
    SP := Parser.Position;
{$ENDIF SOURCEPOS}
    StringData := Parser.ReadStringUntilChar('<');

    // if we do not preserve whitespace, then trim the data in the parsing process
    if not GetPreserveWhiteSpace then
      StringData := sdTrim(StringData, IsTrimmed);

    if length(StringData) > 0 then
    begin
      // inbetween nodes, add chardata node (usually whitespace)
      CD := TsdCharData.Create(Self);
{$IFDEF SOURCEPOS}
      CD.FSourcePos := SP;
{$ENDIF SOURCEPOS}
      CD.SetCoreValue(StringData);
      FRootNodes.Add(CD);
      DoNodeNew(CD);
      DoNodeLoaded(CD);
    end;

    // At the end of the stream? Then stop
    if Parser.EndOfStream then
      break;
    Parser.MoveBack;

    B := Parser.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ElementType := Parser.ReadOpenTag;
      if ElementType = xeError then
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
        exit;
      end;

      // Determine node class
      NodeClass := cNodeClass[ElementType];
      if not assigned(NodeClass) then
      begin
        DoDebugOut(Self, wsWarn, Format(sUnsupportedTag, [cElementTypeNames[ElementType], Parser.Position]));
        exit;
      end;

      // Create new node and add
      Node := NodeClass.Create(Self);
      FRootNodes.Add(Node);
      if ElementType <> xeElement then
        DoNodeNew(Node);

      // The node will parse itself
      Node.ParseStream(Parser);
      DoNodeLoaded(Node);

      // After adding nodes:
      // see if we added the declaration node
      if Node.ElementType = xeDeclaration then
      begin
        // give the parser the codepage from encoding in the declaration.
        // The .SetCodePage setter cares for the re-encoding of the chunk.
        DeclarationEncodingString := TsdDeclaration(Node).Encoding;
        Parser.Encoding := sdCharsetToStringEncoding(DeclarationEncodingString);
        Parser.Codepage := sdCharsetToCodePage(DeclarationEncodingString);

        DoDebugOut(Self, wsInfo, Format('declaration with encoding "%s" and codepage %d', [TsdDeclaration(Node).Encoding, Parser.Codepage]));
      end;

      // drop comments when parsing?
      if (Node.ElementType = xeComment) and FDropCommentsOnParse then
      begin
        // drop comment on parse
        DoDebugOut(Self, wsInfo, 'option DropCommentsOnParse is true, deleting comment');
        FRootNodes.Remove(Node);
      end;

    end;

    // Check if application has aborted parsing
  until FAbortParsing or Parser.EndOfStream;

  // fix structural errors option..
  if FFixStructuralErrors then
  begin
    // list the single tag names
    { for Idx := 0 to FSingleTagNames.Count - 1 do
      DoDebugOut(Self, wsInfo, format('single tag name: %s', [FSingleTagNames[Idx]])); }

    Node := FRootNodes.FindFirst;
    while assigned(Node) do
    begin
      // notorious single tag?
      if FSingleTagNames.Find(Node.Name, Idx) then
      begin
        // here's the scoop: the single tag inadvertently might have subnodes which
        // it should not have, i.e. any nodes with index >= FDirectNodeCount.
        // We extract these, and we add them to the parent nodelist after the
        // node's index.
        Parent := Node.Parent;
        Container := TsdContainerNode(Node);
        Idx := Parent.NodeIndexOf(Container);
        HasClosed := False;
        // extract in reverse order, then insert at parent Idx + 1 in reverse
        // order too, so in all the order is correct
        for i := Container.FNodes.Count - 1 downto Container.FDirectNodeCount do
        begin
          Content := Container.NodeList[i];
          Container.NodeExtract(Content);
          Parent.NodeInsert(Idx + 1, Content);
          HasClosed := True;
        end;
        if HasClosed then
          DoDebugOut(Self, wsInfo, Format('correctly closed node "%s" at pos %d', [Container.Name, Container.SourcePos]));
      end;
      // iterate. The nice thing about FindFirst/FindNext is that finding the
      // next node is evaluated on the spot. No dirty list history problems
      Node := FRootNodes.FindNext(Node);
    end;
    // finally, free the single tag names list
    FreeAndNil(FSingleTagNames);
  end;
end;

function TNativeXml.ParseSubstituteContentFromNode(ANode: TXmlNode; const ASubstitute: Utf8String): TXmlNode;
// this is a simple version of TNativeXml.ParseStream, in order to re-parse
// substituted chardata (e.g. from entities, see also NativeXmlC14n.pas)
var
  S: TsdStringStream;
  Parser: TsdXmlParser;
  Parent: TXmlNode;

  // local
  function ParseSubstituteStream(Parser: TsdXmlParser): TXmlNode;
  var
    B: AnsiChar;
    ElementType: TsdElementType;
    NodeClass: TsdNodeClass;
    Node: TXmlNode;
    StringData: Utf8String;
    CD: TsdCharData;
    IsTrimmed: boolean;
  begin
    FAbortParsing := False;

    // result will have the first re-parsed node
    Result := nil;

    Parser.EncodeChunk;

    // Read next tag
    repeat
      StringData := sdTrim(Parser.ReadStringUntilChar('<'), IsTrimmed);

      if length(StringData) > 0 then
      begin
        // Add chardata node
        CD := TsdCharData.Create(Self);
        CD.Value := StringData;
        Parent.NodeAdd(CD);
        if not assigned(Result) then
          Result := CD;
      end;

      // At the end of the stream? Then stop
      if Parser.EndOfStream then
        break;
      Parser.MoveBack;

      B := Parser.NextChar;
      if B = '<' then
      begin
        // Determine tag type
        ElementType := Parser.ReadOpenTag;
        if ElementType = xeError then
        begin
          DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
          exit;
        end;

        // Determine node class
        NodeClass := cNodeClass[ElementType];
        if not assigned(NodeClass) then
        begin
          DoDebugOut(Self, wsWarn, Format(sUnsupportedTag, [cElementTypeNames[ElementType], Parser.Position]));
          exit;
        end;

        // Create new node and add
        Node := NodeClass.Create(Self);
        Parent.NodeAdd(Node);
        if not assigned(Result) then
          Result := Node;

        // The node will parse itself
        Node.ParseStream(Parser);
      end;

      // Check if application has aborted parsing
    until FAbortParsing or Parser.EndOfStream;
  end;

// main
begin
  Result := nil;
  Parent := ANode.Parent;
  if not assigned(Parent) then
    exit;

  // remove the node that gets substituted
  Parent.NodeRemove(ANode);

  S := TsdStringStream.Create(ASubstitute);
  try
    S.Position := 0;
    Parser := TsdXmlParser.Create(S, cParserChunkSize);
    try
      Parser.Owner := Self;
      Result := ParseSubstituteStream(Parser);
    finally
      FreeAndNil(Parser);
    end;
  finally
    S.Free;
  end;
end;

procedure TNativeXml.ReadFromString(const AValue: Utf8String);
var
  S: TStream;
begin
  S := TsdStringStream.Create(AValue);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TNativeXml.RemoveWhitespace;
var
  Node, Sub: TXmlNode;
  CN: TsdContainerNode;
  CharDataString: Utf8String;
  i: integer;
  IsTrimmed: boolean;
begin
  Node := FindFirst;
  while assigned(Node) do
  begin
    // check container node
    if Node is TsdContainerNode then
    begin
      CN := TsdContainerNode(Node);

      // search for whitespace nodes and whitespace in chardata
      i := 0;
      while i < CN.NodeCount do
      begin
        Sub := CN.Nodes[i];

        // trim chardata for values
        if (Sub.ClassType = TsdCharData) then
        begin
          CharDataString := FSymbolTable.GetString(TsdCharData(Sub).FValueID);
          CharDataString := sdTrim(CharDataString, IsTrimmed);
          if IsTrimmed then
          begin
            if length(CharDataString) = 0 then
            begin
              // no longer used, so remove chardata
              CN.NodeDelete(i); // this also sets FValueIndex to -1
              // next node
              continue;
            end
            else
            begin
              // update trimmed chardata in table
              TsdCharData(Sub).FValueID := FSymbolTable.AddString(CharDataString);
            end;
          end;
        end;

        // remove whitespace nodes
        if Sub is TsdWhiteSpace then
        begin
          CN.NodeDelete(i);
          // next node
          continue;
        end;

        inc(i);
      end;
    end;

    // iterate
    Node := FindNext(Node);
  end;

  // remove chardata nodes in the rootnodelist
  i := 0;
  while i < FRootNodes.Count do
  begin
    if (FRootNodes[i] is TsdCharData) then
    begin
      FRootNodes.Delete(i);
      continue;
    end;
    inc(i);
  end;
end;

procedure TNativeXml.ResetDefaults;
begin
  // reset the options to these defaults:
  FDirectCloseTag := cDefaultDirectCloseTag;
  FDropCommentsOnParse := cDefaultDropCommentsOnParse;
  FEolStyle := cDefaultEolStyle;
  FExternalEncoding := cDefaultExternalEncoding;
  FFixStructuralErrors := cDefaultFixStructuralErrors;
  FFloatAllowScientific := cDefaultFloatAllowScientific;
  FFloatSignificantDigits := cDefaultFloatSignificantDigits;
  FIndentString := cDefaultIndentString;
  FNodeClosingStyle := cDefaultNodeClosingStyle;
  FSplitSecondDigits := cDefaultSplitSecondDigits;
  FUseLocalBias := cDefaultUseLocalBias;
  FWriteOnDefault := cDefaultWriteOnDefault;
  FXmlFormat := cDefaultXmlFormat;
  FAesKeyHex := cDefaultAesKeyHex;
  FBinaryMethod := cDefaultBinaryMethod;
end;

procedure TNativeXml.SaveToFile(const AFilename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TNativeXml.SaveToStream(Stream: TStream);
var
  Writer: TsdXmlWriter;
  BomInfo: TsdBomInfo;
begin
  // Create xml writer, which enabes correct BOM, encoding and codepage.
  Writer := TsdXmlWriter.Create(Self, Stream, cWriterChunkSize);
  try

    // based on externalencoding, we create the external BOM
    case FExternalEncoding of
      seAnsi:
        begin
          BomInfo.Len := 0;
          BomInfo.Encoding := seAnsi;
          BomInfo.HasBOM := False;
        end;
      seUTF8:
        begin
          BomInfo.Len := 0;
          BomInfo.Encoding := seUTF8;
          BomInfo.HasBOM := False;
        end;
      seUTF16BE:
        begin
          // Len = 2 and HasBom = True
          BomInfo := cBomInfoList[cBomInfoIdxUTF16BE];
        end;
      seUTF16LE:
        begin
          // Len = 2 and HasBom = True
          BomInfo := cBomInfoList[cBomInfoIdxUTF16LE];
        end;
    else
      DoDebugOut(Self, wsFail, sUnsupportedEncoding);
      exit;
    end;
    FExternalBomInfo := BomInfo;

    // external byte order mark
    if FExternalBomInfo.HasBOM then
    begin
      Writer.FEncoding := seUTF8; // only to write BOM without hassle
      Writer.Write(FExternalBomInfo.BOM[0], FExternalBomInfo.Len);
    end;

    // set external encoding
    Writer.FEncoding := FExternalEncoding;
    Writer.FCodePage := FExternalCodePage;

    // write the stream
    WriteStream(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TNativeXml.SaveToBinaryFile(const AFilename: string);
var
  Bxm: TsdBinaryXml;
begin
  Bxm := TsdBinaryXml.Create(Self);
  try
    SetBinaryDocument(Bxm);
    Bxm.SaveToFile(AFilename);
  finally
    Bxm.Free;
  end;
end;

procedure TNativeXml.SaveToBinaryStream(Stream: TStream);
var
  Bxm: TsdBinaryXml;
begin
  Bxm := TsdBinaryXml.Create(Self);
  try
    SetBinaryDocument(Bxm);
    Bxm.SaveToStream(Stream);
  finally
    Bxm.Free;
  end;
end;

{$IFDEF USEZLIB}

function TNativeXml.ZlibEncode(SIn, SOut: TStream; CodecSize: int64): Utf8String;
var
  CS: TCompressionStream;
begin
  // tell application this method uses zlib
  Result := 'zlib';

  // param codecsize not used

  // SOut is the destination stream, clMax is the maximum compression level
  CS := TCompressionStream.Create(clMax, SOut);
  try
    CS.CopyFrom(SIn, SIn.Size);
  finally
    CS.Free;
  end;
end;

function TNativeXml.ZlibDecode(SIn, SOut: TStream; PlainSize: int64): Utf8String;
var
  DS: TDecompressionStream;
begin
  Result := 'zlib';
  SIn.Position := 0;
  SOut.Position := 0;

  DS := TDecompressionStream.Create(SIn);
  try
    // codec size is plain size
    SOut.CopyFrom(DS, PlainSize);
    SOut.Position := 0;
  finally
    DS.Free;
  end;
end;

{$IFDEF USEAES}

function TNativeXml.AeszEncode(SIn, SOut: TStream; CodecSize: int64): Utf8String;
var
  CS: TCompressionStream;
  Count: integer;
  SDec: TMemoryStream;
  AESKey128: TAESKey128;
  RawKey: RawByteString;
  ExpandedKey: TAESExpandedKey128;
begin
  // tell application this method uses aesz (AES + ZLib)
  Result := 'aesz';

  // param codecsize not used

  SDec := TMemoryStream.Create;
  try
    // SDec is the itermediary stream, clMax is the maximum compression level
    CS := TCompressionStream.Create(clMax, SDec);
    try
      CS.CopyFrom(SIn, SIn.Size);
    finally
      CS.Free;
    end;

    Count := SDec.Size;
    RawKey := DecodeBinHex(FAesKeyHex);
    Move(RawKey[1], AESKey128, 16);

    // not necessary for just one stream but principle is easy
    ExpandAESKeyForEncryption(AESKey128, ExpandedKey);

    SDec.Position := 0;
    EncryptAESStreamECB(SDec, Count, ExpandedKey, SOut);

  finally
    SDec.Free;
  end;
end;

function TNativeXml.AeszDecode(SIn, SOut: TStream; PlainSize: int64): Utf8String;
var
  DS: TDecompressionStream;
  SDec: TMemoryStream;
  Count: integer;
  AESKey128: TAESKey128;
  RawKey: RawByteString;
  ExpandedKey: TAESExpandedKey128;
begin
  Result := 'aesz';
  SIn.Position := 0;
  SOut.Position := 0;

  SDec := TMemoryStream.Create;
  try
    Count := SIn.Size;
    RawKey := DecodeBinHex(FAesKeyHex);
    Move(RawKey[1], AESKey128, 16);

    // not necessary for just one stream but principle is easy
    ExpandAESKeyForDecryption(AESKey128, ExpandedKey);

    SIn.Position := 0;
    DecryptAESStreamECB(SIn, Count, ExpandedKey, SDec);
    SDec.Position := 0;

    DS := TDecompressionStream.Create(SDec);
    try
      // codec size is plain size
      SOut.CopyFrom(DS, PlainSize);
      SOut.Position := 0;
    finally
      DS.Free;
    end;

  finally
    SDec.Free;
  end;
end;
{$ENDIF USEAES}
{$ENDIF USEZLIB}

procedure TNativeXml.SetCommentString(const Value: Utf8String);
// Find first comment node and set it's value, otherwise add new comment node
// right below the xml declaration
var
  Node: TXmlNode;
begin
  Node := FRootNodes.ByType(xeComment);
  if not assigned(Node) and (length(Value) > 0) then
  begin
    Node := TsdComment.Create(Self);
    FRootNodes.Insert(1, Node);
  end;
  if assigned(Node) then
    Node.Value := Value;
end;

procedure TNativeXml.SetCharset(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if (Value = GetCharset) or (length(Value) = 0) then
    exit;

  // write declaration (if not there)
  Node := GetOrCreateDeclarationNode;

  // write charset
  if Node is TsdDeclaration then
    TsdDeclaration(Node).Encoding := Value;

  // write the external codepage
  FExternalCodePage := sdCharsetToCodePage(Value);
  // write external encoding
  FExternalEncoding := sdCharsetToStringEncoding(Value);
end;

procedure TNativeXml.SetPreserveWhiteSpace(const Value: boolean);
begin
  if GetPreserveWhiteSpace <> Value then
  begin
    if Value = True then
      SetXmlFormat(xfPreserve);
  end;
end;

procedure TNativeXml.SetXmlFormat(const Value: TXmlFormatType);
begin
  if FXmlFormat = Value then
    exit;

  // allow removing whitespace if current FXmlFormat is
  // xfPreserve and new value is not
  if FXmlFormat = xfPreserve then
    RemoveWhitespace;

  FXmlFormat := Value;
end;

procedure TNativeXml.SetVersionString(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if Value = GetVersionString then
    exit;
  Node := FRootNodes[0];
  if not(Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Version := Value;
end;

procedure TNativeXml.WriteStream(S: TStream);
var
  i: integer;
  Node: TXmlNode;
begin
  if not assigned(Root) and FParserWarnings then
    raise EFilerError.Create(sRootElementNotDefined);

  DoProgress(0);

  // write the root nodes
  for i := 0 to FRootNodes.Count - 1 do
  begin
    // external codepage info
    if i = 0 then
    begin
      Node := FRootNodes[i];
      if Node.ElementType = xeDeclaration then
        DoDebugOut(Self, wsInfo, Format('writing declaration with encoding "%s" and codepage %d', [TsdDeclaration(Node).Encoding,
          FExternalCodePage]));
    end;

    FRootNodes[i].WriteStream(S);
  end;

  DoProgress(S.Size);
end;

function TNativeXml.WriteToString: string;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    SaveToStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TNativeXml.WriteToLocalString: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TNativeXml.WriteToLocalUnicodeString: UnicodeString;
begin
  Result := sdUtf8ToWide(WriteToLocalString);
end;

function TNativeXml.FindFirst: TXmlNode;
begin
  if not assigned(FRootNodes) then
    Result := nil
  else
    Result := FRootNodes.FindFirst;
end;

function TNativeXml.FindNext(ANode: TXmlNode): TXmlNode;
begin
  Result := FRootNodes.FindNext(ANode);
end;

procedure TNativeXml.ForEach(Sender: TObject; AEvent: TsdXmlNodeEvent);
var
  Node: TXmlNode;
begin
  if not assigned(AEvent) or not assigned(Sender) then
    exit;
  Node := FindFirst;
  while assigned(Node) do
  begin
    AEvent(Sender, Node);
    Node := FindNext(Node);
  end;
end;

function TNativeXml.AttrText(AName, AValue: Utf8String): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;
end;

function TNativeXml.AttrInt(AName: Utf8String; AValue: integer): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdIntToString(AValue);
end;

function TNativeXml.AttrInt64(AName: Utf8String; AValue: int64): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdInt64ToString(AValue);
end;

function TNativeXml.AttrHex(AName: Utf8String; AValue, ADigits: integer): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := '$' + IntToHex(AValue, ADigits);
end;

function TNativeXml.AttrHex(AName: Utf8String; AValue: int64; ADigits: integer): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := '$' + IntToHex(AValue, ADigits);
end;

function TNativeXml.AttrFloat(AName: Utf8String; AValue: double): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdFloatToString(AValue, cDefaultFloatSignificantDigits, cDefaultFloatAllowScientific);
end;

function TNativeXml.AttrFloat(AName: Utf8String; AValue: double; ASignificantDigits: integer; AAllowScientific: boolean): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdFloatToString(AValue, ASignificantDigits, AAllowScientific);
end;

function TNativeXml.AttrDateTime(AName: Utf8String; AValue: TDateTime): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdDateTimeToString(AValue, True, True, FSplitSecondDigits);
end;

function TNativeXml.AttrBool(AName: Utf8String; AValue: boolean): TsdAttribute;
begin
  Result := TsdAttribute.Create(Self);
  Result.Name := AName;
  Result.Value := sdBoolToString(AValue);
end;

function TNativeXml.NodeNew(AName: Utf8String): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], []);
end;

function TNativeXml.NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], []);
end;

function TNativeXml.NodeNew(AName: Utf8String; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, [], SubNodes);
end;

function TNativeXml.NodeNewEx(AName: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewType(AName: Utf8String; AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', AElementType, [], []);
end;

function TNativeXml.NodeNewTypeEx(AName: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', AElementType, AXmlNode, [], []);
end;

function TNativeXml.NodeNewType(AName: Utf8String; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', AElementType, [], SubNodes);
end;

function TNativeXml.NodeNewTypeEx(AName: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', AElementType, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewAttr(AName: Utf8String; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, Attributes, []);
end;

function TNativeXml.NodeNewAttrEx(AName: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, Attributes, []);
end;

function TNativeXml.NodeNewAttr(AName: Utf8String; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, '', xeElement, Attributes, SubNodes);
end;

function TNativeXml.NodeNewAttrEx(AName: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode)
  : TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, '', xeElement, AXmlNode, Attributes, SubNodes);
end;

function TNativeXml.NodeNewText(AName, AValue: Utf8String): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, [], []);
end;

function TNativeXml.NodeNewTextEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, [], []);
end;

function TNativeXml.NodeNewText(AName, AValue: Utf8String; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, [], SubNodes);
end;

function TNativeXml.NodeNewTextEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewTextType(AName, AValue: Utf8String; AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, [], []);
end;

function TNativeXml.NodeNewTextTypeEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode, [], []);
end;

function TNativeXml.NodeNewTextType(AName, AValue: Utf8String; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, [], SubNodes);
end;

function TNativeXml.NodeNewTextTypeEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode)
  : TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewTextAttr(AName, AValue: Utf8String; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, Attributes, []);
end;

function TNativeXml.NodeNewTextAttrEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, Attributes, []);
end;

function TNativeXml.NodeNewTextAttr(AName, AValue: Utf8String; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, xeElement, Attributes, SubNodes);
end;

function TNativeXml.NodeNewTextAttrEx(AName, AValue: Utf8String; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, xeElement, AXmlNode, Attributes, SubNodes);
end;

function TNativeXml.NodeNewTextTypeAttr(AName, AValue: Utf8String; AElementType: TsdElementType; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes, []);
end;

function TNativeXml.NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, AValue, AElementType, AXmlNode, Attributes, []);
end;

function TNativeXml.NodeNewTextTypeAttr(AName, AValue: Utf8String; AElementType: TsdElementType; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[AElementType];
  Result := NodeClass.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;

  Result.AttributesAdd(Attributes);
  Result.NodesAdd(SubNodes);
end;

function TNativeXml.NodeNewTextTypeAttrEx(AName, AValue: Utf8String; AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  AXmlNode := NodeNewTextTypeAttr(AName, AValue, AElementType, Attributes, SubNodes);
  Result := AXmlNode;
end;

function TNativeXml.NodeNewInt(AName: Utf8String; AValue: integer): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, [], []);
end;

function TNativeXml.NodeNewIntEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode, [], []);
end;

function TNativeXml.NodeNewInt(AName: Utf8String; AValue: integer; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, [], SubNodes);
end;

function TNativeXml.NodeNewIntEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewIntAttr(AName: Utf8String; AValue: integer; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, Attributes, []);
end;

function TNativeXml.NodeNewIntAttrEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode, Attributes, []);
end;

function TNativeXml.NodeNewIntAttr(AName: Utf8String; AValue: integer; Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), xeElement, Attributes, SubNodes);
end;

function TNativeXml.NodeNewIntAttrEx(AName: Utf8String; AValue: integer; out AXmlNode: TXmlNode; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), xeElement, AXmlNode, Attributes, SubNodes);
end;

function TNativeXml.NodeNewIntTypeAttr(AName: Utf8String; AValue: integer; AElementType: TsdElementType; Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, Attributes, []);
end;

function TNativeXml.NodeNewIntTypeAttrEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode, Attributes, []);
end;

function TNativeXml.NodeNewIntType(AName: Utf8String; AValue: integer; AElementType: TsdElementType): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, [], []);
end;

function TNativeXml.NodeNewIntTypeEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode, [], []);
end;

function TNativeXml.NodeNewIntType(AName: Utf8String; AValue: integer; AElementType: TsdElementType; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, [], SubNodes);
end;

function TNativeXml.NodeNewIntTypeEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode, [], SubNodes);
end;

function TNativeXml.NodeNewIntTypeAttr(AName: Utf8String; AValue: integer; AElementType: TsdElementType; Attributes: array of TsdAttribute;
  SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttr(AName, sdIntToString(AValue), AElementType, Attributes, SubNodes);
end;

function TNativeXml.NodeNewIntTypeAttrEx(AName: Utf8String; AValue: integer; AElementType: TsdElementType; out AXmlNode: TXmlNode;
  Attributes: array of TsdAttribute; SubNodes: array of TXmlNode): TXmlNode;
begin
  Result := NodeNewTextTypeAttrEx(AName, sdIntToString(AValue), AElementType, AXmlNode, Attributes, SubNodes);
end;

class function TNativeXml.DecodeBase64(const Source: Utf8String; OnDebug: TsdDebugEvent): RawByteString;
begin
  try
    Result := uNativeXml.DecodeBase64(Source);
  except
    on EFilerError do
      OnDebug(nil, wsFail, sErrorCalcStreamLength);
  end;
end;

class function TNativeXml.EncodeBase64(const Source: RawByteString; const ControlChars: Utf8String): Utf8String;
begin
  Result := sdAddControlChars(uNativeXml.EncodeBase64(Source), ControlChars);
end;

procedure TNativeXml.SetExternalEncoding(const Value: TsdStringEncoding);
var
  Codepage: integer;
  Node: TXmlNode;
begin
  Codepage := cStringEncodingCodePages[Value];
  if Codepage = 0 then
  begin
    DoDebugOut(Self, wsFail, Format('external encoding "%s" is not allowed (use ExternalCodepage)', [cStringEncodingCharsetNames[Value]]));
    exit;
  end;

  FExternalEncoding := Value;
  FExternalCodePage := Codepage;

  Node := GetOrCreateDeclarationNode;
  TsdDeclaration(Node).Encoding := cStringEncodingCharsetNames[FExternalEncoding];
end;

procedure TNativeXml.SetExternalCodepage(const Value: integer);
var
  Node: TXmlNode;
  i, Idx: integer;
  se: TsdStringEncoding;
begin
  if FExternalCodePage = Value then
    exit;

  // search for codepage info
  Idx := -1;
  for i := 0 to cCodepageInfoCount - 1 do
  begin
    if cCodePageInfo[i].Codepage = Value then
    begin
      Idx := i;
      break;
    end;
  end;

  if Idx = -1 then
  begin
    DoDebugOut(Self, wsFail, Format('external codepage "%d" not found', [Value]));
    exit;
  end;

  // codepage found
  FExternalCodePage := Value;

  // start by default for the bulk of codepages
  FExternalEncoding := seAnsi;

  // search string encodings for special codepages
  for se := low(TsdStringEncoding) to high(TsdStringEncoding) do
  begin
    if cStringEncodingCodePages[se] = FExternalCodePage then
    begin
      FExternalEncoding := se;
    end;
  end;

  Node := GetOrCreateDeclarationNode;
  TsdDeclaration(Node).Encoding := cCodePageInfo[Idx].Name;
end;

procedure TNativeXml.SetBinaryDocument(ABinaryXml: TsdBinaryXml);
begin
  ABinaryXml.Document := Self;
  // additional options
  case FBinaryMethod of
    bmDefault:
      begin
        ABinaryXml.OnEncode := nil;
        ABinaryXml.OnDecode := nil;
      end;
{$IFDEF USEZLIB}
    bmZlib:
      begin
        ABinaryXml.OnEncode := ZlibEncode;
        ABinaryXml.OnDecode := ZlibDecode;
      end;
{$IFDEF USEAES}
    bmAesz:
      begin
        ABinaryXml.OnEncode := AeszEncode;
        ABinaryXml.OnDecode := AeszDecode;
      end;
{$ENDIF USEAES}
{$ENDIF USEZLIB}
  else
    DoDebugOut(Self, wsWarn, 'binary method not available (check compiler defines)');
  end;
end;

procedure TNativeXml.SetBinaryMethod(const Value: TsdXmlBinaryMethod);
begin
  FBinaryMethod := Value;
end;

{ TsdXmlCanonicalizer - experimental! }

function TsdXmlCanonicalizer.Canonicalize(AXml: TNativeXml): integer;
var
  Decl: TXmlNode;
  DTD: TsdDocType;
  DtdEntityNodes: array of TXmlNode;
  i, j, TotalNodeCount, CharDataCount, NewReferencesCount: integer;
  Node: TXmlNode;
  CharData: TsdCharData;
  SubstituteText: Utf8String;
{$IFDEF D5UP}
  Src: PChar;
{$ENDIF D5UP}
begin
  CharDataCount := 0;
  Result := 0; // ReferencesCount

  // encode in utf-8 only - this is already achieved by the parser

  // xml compacted
  // AXml.XmlFormat := xfCompact;

  // remove xml declaration
  Decl := AXml.RootNodes[0];
  if Decl is TsdDeclaration then
  begin
    AXml.RootNodes.Delete(0);
  end;

  // recursively expand entities to their character equivalent:

  // find dtdentity nodes in the dtd
  DTD := TsdDocType(AXml.RootNodes.ByType(xeDocType));
  if assigned(DTD) then
  begin
    j := 0;
    SetLength(DtdEntityNodes, j);
    for i := 0 to DTD.NodeCount - 1 do
      if DTD.Nodes[i] is TsdDtdEntity then
      begin
        inc(j);
        SetLength(DtdEntityNodes, j);
        DtdEntityNodes[j - 1] := TsdDtdEntity(DTD.Nodes[i]);
      end;
  end;

  // find references
  repeat
    NewReferencesCount := 0;
    TotalNodeCount := 0;
    Node := AXml.FindFirst;

    while assigned(Node) do
    begin
      inc(TotalNodeCount);

      // check for entity references
      if Node is TsdCharData then
      begin
        inc(CharDataCount);

        // non-standard references usually come from entity references in the dtd
        CharData := TsdCharData(Node);
        if CharData.HasNonStandardReferences then
        begin
          inc(NewReferencesCount);

          // substitute chardata value using the references
{$IFDEF D7UP}
          SubstituteText := AnsiDequotedStr(CharData.GetValueUsingReferences(DtdEntityNodes), '"');
{$ELSE D7UP}
          // D5 version
          Src := PChar(CharData.GetValueUsingReferences(DtdEntityNodes));
          SubstituteText := AnsiExtractQuotedStr(Src, '"');
{$ENDIF D7UP}
          Node := AXml.ParseSubstituteContentFromNode(CharData, SubstituteText);
        end;
      end;

      Node := AXml.FindNext(Node);
    end;
    inc(Result, NewReferencesCount);

  until NewReferencesCount = 0;

  // replace CDATA sections by character equivalent
  // todo

  // encode special &lt; &gt; and &quot; entities
  // todo

  // normalize attributes as if by validating parser
  // todo

  // open empty elements with start and end tags
  // todo

  // sort namespace declarations and attributes
  // todo

  DoDebugOut(Self, wsInfo, Format('total node count: %d, chardata count: %d, references count: %d', [TotalNodeCount, CharDataCount, Result]));
  DoDebugOut(Self, wsInfo, 'C14N created');
end;

{ TsdXmlParser }

function TsdXmlParser.CheckString(const S: Utf8String): boolean;
// case-insensitive string check
var
  i, Count, StartIdx: integer;
begin
  Count := MakeDataAvailable;
  StartIdx := FUtf8CurrentIdx;
  Result := True;
  for i := 1 to length(S) do
  begin
    if FEndOfStream then
    begin
      Result := False;
      exit;
    end;
    // case-insensitive, so we use LoCase in both sides (LoCase is
    // faster than function LowerCase, since it deals directly with chars).
    if LoCase(S[i]) <> LoCase(FUtf8Buffer[FUtf8CurrentIdx]) then
    begin
      Result := False;
      // revert
      FUtf8CurrentIdx := StartIdx;
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;
end;

constructor TsdXmlParser.Create(ASource: TStream; AChunkSize: integer);
begin
  inherited Create;
  FSource := ASource;
  FChunkSize := AChunkSize;
  SetLength(FRawBuffer, FChunkSize);

  // Read from the stream directly to the raw buffer
  FRawFirstIdx := 0;
  FRawLastIdx := FSource.Read(FRawBuffer[0], FChunkSize);
  FUtf8FirstIdx := 0;
  FUtf8CurrentIdx := 0;
  FLastChar0D := False;
end;

destructor TsdXmlParser.Destroy;
begin
  SetLength(FRawBuffer, 0);
  SetLength(FUtf8Buffer, 0);
  inherited;
end;

procedure TsdXmlParser.EncodeChunk;

// local
  procedure EncodeAnsiChunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    // Utf8 buffer might be 3x ansi size at max
    SetLength(FUtf8Buffer, FUtf8FirstIdx + 3 * RawLen);
    Utf8Len := sdAnsiToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], FCodePage, RawLen, FLastChar0D);
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;

// local
  procedure EncodeUtf8Chunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    // buffers
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FUtf8FirstIdx + RawLen);
    Utf8Len := sdNormaliseBuffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], RawLen, FLastChar0D);
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;

// local
  procedure EncodeUtf16Chunk;
  type
    TWordArray = array of Word;
  var
    RawLen, Utf8Len: integer;
    i: integer;
    W: Word;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;

    // If UTF16 BE (Big Endian), we must swap byte order
    if FEncoding = seUTF16BE then
    begin
      for i := FRawFirstIdx div 2 to FRawLastIdx div 2 - 1 do
      begin
        W := TWordArray(FRawBuffer)[i];
        TWordArray(FRawBuffer)[i] := Swap(W);
      end;
    end;

    // Utf8 buffer might be 2x utf16 size at max
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FUtf8FirstIdx + (2 * RawLen));

    // Now convert from UTF16 to UTF8
    Utf8Len := sdWideToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], RawLen div 2, FLastChar0D);
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;

// main
begin
  // EOL normalisation is now integrated so no need for a separate NormaliseEOL function.

  // IMPORTANT OBSERVATION:
  // Normalization could occur between two chunks so boolean FLastChar0D must be
  // transmitted (however, it is usually false). In case the final char of first
  // chunk is $0D, FLastChar0D signals this and if first char of next chunk is $0A,
  // this char is skipped.

  // call EncodeChunk methods based on encoding
  case FEncoding of
    seAnsi:
      begin
        if (FCodePage = 0) or (FCodePage = 65001) then // UTF8
        begin
          EncodeUtf8Chunk;
        end
        else
        begin
          EncodeAnsiChunk;
        end;
      end;

    seUTF8:
      begin
        EncodeUtf8Chunk;
      end;

    seUTF16BE, seUTF16LE:
      begin
        EncodeUtf16Chunk;
      end;
  end;

end;

procedure TsdXmlParser.Flush;
var
  i: integer;
  RawLen, Utf8Len: integer;
begin
  // Number of bytes to move
  RawLen := FRawLastIdx - FRawFirstIdx;
  Utf8Len := FUtf8LastIdx - FUtf8FirstIdx;

  if FUtf8CurrentIdx - FUtf8FirstIdx > 0 then
  begin
    // Calcuate base line number and base position
    for i := 0 to FUtf8FirstIdx - 1 do
    begin
      // linefeed
      if FUtf8Buffer[i] = #$0A then
        inc(FBaseLineNumber);
    end;
    inc(FUtf8BasePosition, FUtf8FirstIdx);

    // moves
    Move(FRawBuffer[FRawFirstIdx], FRawBuffer[0], RawLen);
    Move(FUtf8Buffer[FUtf8FirstIdx], FUtf8Buffer[0], Utf8Len);

    // update current idx
    dec(FUtf8CurrentIdx, FUtf8FirstIdx);

    // update first/last indices
    FRawFirstIdx := 0;
    FRawLastIdx := RawLen;
    FUtf8FirstIdx := 0;
    FUtf8LastIdx := Utf8Len;
  end;
end;

function TsdXmlParser.GetLineNumber: int64;
var
  i: integer;
begin
  Result := FBaseLineNumber;
  for i := 0 to FUtf8CurrentIdx - 1 do
  begin
    // linefeed
    if FUtf8Buffer[i] = #$0A then
      inc(Result);
  end;
end;

function TsdXmlParser.GetPosition: int64;
begin
  Result := FUtf8BasePosition + FUtf8CurrentIdx;
end;

procedure TsdXmlParser.IncCurrentIdxCheck(var BytesAvail: integer);
// increment FCurrentIdx and check bytes available
begin
  inc(FUtf8CurrentIdx);
  dec(BytesAvail);
  if BytesAvail <= 0 then
    BytesAvail := MakeDataAvailable
end;

function TsdXmlParser.IsBinaryXml: boolean;
var
  i: integer;
  Cookie: array [0 .. 3] of AnsiChar;
begin
  Result := False;
  if FRawLastIdx <= length(cBinaryXmlCookie) then
    exit;

  // read binary cookie
  Move(FRawBuffer[0], Cookie, 4);

  for i := 0 to length(cBinaryXmlCookie) - 1 do
    if Cookie[i] <> cBinaryXmlCookie[i] then
      exit;

  // cookie for binary xml matches
  Result := True;
end;

function TsdXmlParser.LoCase(Ch: AnsiChar): AnsiChar;
const
  cInterval: integer = Ord('a') - Ord('A');
begin
  Result := Ch;
  case Result of
    'A' .. 'Z':
      inc(Result, cInterval);
  end;
end;

function TsdXmlParser.MakeDataAvailable: integer;
var
  BytesRead: integer;
begin
  Result := FUtf8LastIdx - FUtf8CurrentIdx;
  while Result < 1 do
  begin
    // We must make data available
    BytesRead := ReadNextChunk;
    Result := FUtf8LastIdx - FUtf8CurrentIdx;

    // Still no data available?
    if BytesRead = 0 then
    begin
      FEndOfStream := True;
      exit;
    end;
  end;
end;

procedure TsdXmlParser.MoveBack;
begin
  assert(FUtf8CurrentIdx > 0);
  dec(FUtf8CurrentIdx);
end;

function TsdXmlParser.NextChar: AnsiChar;
begin
  MakeDataAvailable;
  if FEndOfStream then
  begin
    Result := #0;
    exit;
  end;
  Result := FUtf8Buffer[FUtf8CurrentIdx];
  inc(FUtf8CurrentIdx);
end;

function TsdXmlParser.ReadNextChunk: integer;
begin
  SetLength(FRawBuffer, FRawLastIdx + FChunkSize);

  // Read from the stream directly to our chunk
  // Result is the bytes read
  Result := FSource.Read(FRawBuffer[FRawLastIdx], FChunkSize);
  if Result > 0 then
  begin
    FRawFirstIdx := FRawLastIdx;
    FUtf8FirstIdx := FUtf8LastIdx;
    inc(FRawLastIdx, Result);
    EncodeChunk;
  end;
end;

function TsdXmlParser.ReadQuotedString(AQuote: AnsiChar): Utf8String;
begin
  // It seems that the xml spec simply does not allow double quotes as in
  // Delphi, so we do not need a complicated algo to do this. We can simply
  // search for the quote again as terminator.
  Result := ReadStringUntilChar(AQuote);
end;

function TsdXmlParser.ReadString(AIndex, ACount: integer): Utf8String;
begin
  SetLength(Result, ACount);
  if ACount > 0 then
    Move(FUtf8Buffer[AIndex], Result[1], ACount);
end;

function TsdXmlParser.ReadStringUntil(const Terminator: Utf8String): Utf8String;
var
  Count, MatchLen: integer;
  FirstChar: AnsiChar;
  StartIdx: integer;
begin
  FirstChar := Terminator[1];
  MatchLen := length(Terminator);
  StartIdx := FUtf8CurrentIdx;
  Count := MakeDataAvailable;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] = FirstChar then
    begin

      if CheckString(Terminator) then
      begin
        // We found the terminating string
        Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx - MatchLen);
        exit;
      end;

    end;
    IncCurrentIdxCheck(Count);
  end;
  // when left here stream ended prematurely
  DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [GetPosition]));
end;

function TsdXmlParser.ReadStringUntilChar(AChar: AnsiChar): Utf8String;
var
  Count: integer;
  StartIdx: integer;
begin
  Count := MakeDataAvailable;

  StartIdx := FUtf8CurrentIdx;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] = AChar then
    begin
      // We found AChar
      Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
      // Adjust FUtf8CurrentIdx
      inc(FUtf8CurrentIdx);
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;

  // Arriving here: end of stream and AChar not reached
  Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
end;

procedure TsdXmlParser.SetCodePage(const Value: integer);
begin
  FCodePage := Value;
  // re-encode the chunk (e.g. from default utf-8 codepage to other ansi codepage)
  EncodeChunk;
end;

// TsdXmlParser

function TsdXmlParser.NextCharSkipBlanks(var Blanks: Utf8String): AnsiChar;
var
  Count: integer;
begin
  Blanks := '';
  Count := MakeDataAvailable;
  while not FEndOfStream do
  begin
    Result := FUtf8Buffer[FUtf8CurrentIdx];
    IncCurrentIdxCheck(Count);
    if not(Result in cXmlBlankChars) then
      exit;
    Blanks := Blanks + Result;
  end;
  Result := #0;
end;

procedure TsdXmlParser.ReadBOM;
var
  i, j: integer;
  BOM: array [0 .. 3] of byte;
  BomInfoFound: boolean;
begin
  if FRawLastIdx <= 4 then
  begin
    DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [FRawLastIdx]));
    exit;
  end;

  // read the BOM if it is there
  Move(FRawBuffer[0], BOM, 4);

  i := 0;
  BomInfoFound := False;
  while i < cBomInfoListCount do
  begin
    BomInfoFound := True;
    for j := 0 to cBomInfoList[i].Len - 1 do
    begin
      if BOM[j] <> cBomInfoList[i].BOM[j] then
      begin
        BomInfoFound := False;
        break;
      end;
    end;
    if BomInfoFound then
    begin
      FBomInfo := cBomInfoList[i];
      FEncoding := FBomInfo.Encoding;
      break;
    end;
    inc(i);
  end;

  // BOM info found?
  if BomInfoFound then
  begin
    // check for non-supported encodings
    if not(FEncoding in [seAnsi, seUTF8, seUTF16BE, seUTF16LE]) then
    begin
      DoDebugOut(Self, wsFail, Format(sUnsupportedEncoding, [cStringEncodingCharsetNames[FEncoding]]));
      // avoid trying to read exotic encodings such as EBDIC
      exit;
    end;

    // Rewind based on BOM
    if FBomInfo.HasBOM then
    begin
      FRawLastIdx := FChunkSize - FBomInfo.Len;
      Move(FRawBuffer[FBomInfo.Len], FRawBuffer[0], FRawLastIdx);
      SetLength(FRawBuffer, FRawLastIdx);
      DoDebugOut(Self, wsInfo, Format('BOM with encoding %s', [cStringEncodingCharsetNames[FEncoding]]));
    end;

  end
  else
  begin
    // No BOM, and unknown encoding, e.g. html instead of xml
    // we use UTF8 as default
    DoDebugOut(Self, wsWarn, sUnknownEncoding);
    FEncoding := seUTF8;
  end;

  // encode the first chunk
  EncodeChunk;
end;

function TsdXmlParser.ReadOpenTag: TsdElementType;
var
  AnsiCh: AnsiChar;
begin
  Result := xeError;
  AnsiCh := NextChar;
  if FEndOfStream then
    exit;

  case AnsiCh of
    '!':
      begin
        AnsiCh := LoCase(NextChar);
        case AnsiCh of
          '[':
            if CheckString('cdata[') then
              Result := xeCData;
          'd':
            if CheckString('octype') then
              Result := xeDocType;
          'e':
            begin
              if CheckString('lement') then
                Result := xeDtdElement;
              if CheckString('ntity') then
                Result := xeDtdEntity;
            end;
          'a':
            if CheckString('ttlist') then
              Result := xeDtdAttList;
          'n':
            if CheckString('otation') then
              Result := xeDtdNotation;
          '-':
            if CheckString('-') then
              Result := xeComment;
        else
          begin
            DoDebugOut(Self, wsFail, Format(sIllegalTag, [AnsiCh, GetPosition]));
            exit;
          end;
        end;
      end;
    '?':
      begin
        if CheckString('xml') then
        begin
          if CheckString('-stylesheet') then
            Result := xeStylesheet
          else
            Result := xeDeclaration;
        end
        else
          Result := xeInstruction;
      end;
    '/':
      Result := xeEndTag;
  else
    Result := xeElement;
    MoveBack;
  end;
end;

function TsdXmlParser.ReadStringUntilBlankOrEndTag: Utf8String;
var
  Count: integer;
  StartIdx: integer;
begin
  Count := MakeDataAvailable;
  StartIdx := FUtf8CurrentIdx;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] in cXmlBlankCharsOrEndTag then
    begin
      // We found the termination
      Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;
  // when left here, stream ended prematurely
  DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [GetPosition]));
end;

{ TsdXmlWriter }

constructor TsdXmlWriter.Create(AOwner: TDebugComponent; ASource: TStream; AChunkSize: integer);
begin
  inherited Create(ASource, AChunkSize);
  FOwner := AOwner;
end;

destructor TsdXmlWriter.Destroy;
begin
  SetLength(FRawBuffer, 0);
  inherited;
end;

procedure TsdXmlWriter.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if FOwner is TDebugComponent then
    TDebugComponent(FOwner).DoDebugOut(Sender, WarnStyle, AMessage);
end;

function TsdXmlWriter.Write(const Buffer; Count: integer): longint;
type
  PWord = ^Word;
var
  i, AnsiCount, WideCount: integer;
  DefaultCharUsed: boolean;
  W: PWord; // pointer to a word
  // local
  procedure AllocRawBuffer(ASize: integer);
  begin
    if FRawBufferSize < ASize then
    begin
      FRawBufferSize := ASize;
      SetLength(FRawBuffer, FRawBufferSize);
    end;
  end;

// main
begin
  case FEncoding of

    seAnsi:
      begin
        AllocRawBuffer(Count);
        AnsiCount := sdUtf8ToAnsiBuffer(Buffer, FRawBuffer[0], FCodePage, Count, DefaultCharUsed);
        Result := inherited Write(FRawBuffer[0], AnsiCount);
        if DefaultCharUsed then
        begin
          DoDebugOut(Self, wsWarn, sDefaultCharUsed);
        end;
      end;

    seUTF8:
      begin
        Result := inherited Write(Buffer, Count)
      end;

    seUTF16LE:
      begin
        AllocRawBuffer(2 * Count);
        WideCount := sdUtf8ToWideBuffer(Buffer, FRawBuffer[0], Count);
        Result := inherited Write(FRawBuffer[0], 2 * WideCount);
      end;

    seUTF16BE:
      begin
        AllocRawBuffer(2 * Count);
        WideCount := sdUtf8ToWideBuffer(Buffer, FRawBuffer[0], Count);

        // swap the byte order from little endian to big endian
        W := PWord(@FRawBuffer[0]);
        for i := 0 to WideCount - 1 do
        begin
          W^ := Swap(W^);
          inc(W);
        end;

        Result := inherited Write(FRawBuffer[0], 2 * WideCount);
      end;
  else
    // unsupported encoding
    DoDebugOut(Self, wsFail, sUnsupportedEncoding);
    Result := 0;
  end;
end;

{ Utility Functions }

{$IFDEF MSWINDOWS}

function sdWideToUtf8(const W: UnicodeString): Utf8String;
var
  WideCount, Utf8Count: integer;
  LastChar0D: boolean;
begin
  WideCount := length(W);
  LastChar0D := False;
  SetLength(Result, WideCount * 3); // just to be sure
  if WideCount = 0 then
    exit;

  Utf8Count := sdWideToUtf8Buffer(W[1], Result[1], WideCount, LastChar0D);
  SetLength(Result, Utf8Count);
end;

function sdUtf8ToWide(const U: Utf8String): UnicodeString;
var
  Utf8Count, WideCount: integer;
begin
  Utf8Count := length(U);
  SetLength(Result, Utf8Count);
  if Utf8Count = 0 then
    exit;

  WideCount := sdUtf8ToWideBuffer(U[1], Result[1], Utf8Count);
  SetLength(Result, WideCount);
end;
{$ELSE}

// FPC functions
function sdWideToUtf8(const W: UnicodeString): Utf8String;
begin
  Result := W;
end;

function sdUtf8ToWide(const U: Utf8String): UnicodeString;
begin
  Result := U;
end;
{$ENDIF}

function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer; var LastChar0D: boolean): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.

// update: EOL normalisation is integrated in this version
var
  W: Word;
  ByteCh: byte;
  WideIdx, Utf8Idx: integer;
  AddChar: boolean;
begin
  WideIdx := 0;
  Utf8Idx := 0;
  while WideIdx < WideCount do
  begin
    W := TWordArray(WideBuf)[WideIdx];
    if W <= $7F then
    begin

      AddChar := True;
      ByteCh := byte(W);

      if ByteCh = $0D then
      begin
        ByteCh := $0A;
        LastChar0D := True;
      end
      else
      begin
        if (ByteCh = $0A) and LastChar0D then
          AddChar := False;
        LastChar0D := False;
      end;

      if AddChar then
      begin
        TByteArray(Utf8Buf)[Utf8Idx] := ByteCh;
        inc(Utf8Idx);
      end;

    end
    else
    begin
      if W > $7FF then
      begin
        TByteArray(Utf8Buf)[Utf8Idx] := byte($E0 or (W shr 12));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or ((W shr 6) and $3F));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end
      else
      begin // $7F < W <= $7FF
        TByteArray(Utf8Buf)[Utf8Idx] := byte($C0 or (W shr 6));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end;
    end;
    inc(WideIdx);
  end;
  Result := Utf8Idx;
end;

function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;
// Convert an UTF8 buffer to Unicode (UTF16 LE) buffer. This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at WideBuf must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Utf8Buf block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: Word;
  C: byte;
  WideIdx, Utf8Idx: integer;
begin
  Utf8Idx := 0;
  WideIdx := 0;
  while Utf8Idx < ByteCount do
  begin
    // 1st byte
    W := TByteArray(Utf8Buf)[Utf8Idx];
    inc(Utf8Idx);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := TByteArray(Utf8Buf)[Utf8Idx];
        inc(Utf8Idx);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          continue;
        W := Word((W shl 6) or (C and $3F));
      end;
      // 2nd or 3rd byte
      C := TByteArray(Utf8Buf)[Utf8Idx];
      inc(Utf8Idx);
      if C and $C0 <> $80 then
        // malformed trail byte
        continue;
      TWordArray(WideBuf)[WideIdx] := Word((W shl 6) or (C and $3F));
      inc(WideIdx);
    end
    else
    begin
      TWordArray(WideBuf)[WideIdx] := W;
      inc(WideIdx);
    end;
  end;
  Result := WideIdx;
end;

function sdAnsiToUtf8(const A: AnsiString; ACodePage: integer): Utf8String;
var
  AnsiCount, Utf8Count: integer;
  LastChar0D: boolean;
begin
  AnsiCount := length(A);
  SetLength(Result, AnsiCount * 3); // just to be sure
  if AnsiCount = 0 then
    exit;

  LastChar0D := False;
  Utf8Count := sdAnsiToUtf8Buffer(A[1], Result[1], ACodePage, AnsiCount, LastChar0D);
  SetLength(Result, Utf8Count);
end;

function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer; var LastChar0D: boolean): integer;
// ansi to utf8 with EOL normalisation
var
  AnsiIdx, Utf8Idx: integer;
  AnsiCh: AnsiChar;
  WideCh: WideChar;
  Len: integer;
  AddChar: boolean;
begin
  AnsiIdx := 0;
  Utf8Idx := 0;

  while AnsiIdx < AnsiCount do
  begin
    AddChar := True;
    AnsiCh := TAnsiCharArray(AnsiBuf)[AnsiIdx];

    if Ord(AnsiCh) < $80 then
    begin
      // characters smaller than $80

      if AnsiCh = #$0D then
      begin
        AnsiCh := #$0A;
        LastChar0D := True;
      end
      else
      begin
        if (AnsiCh = #$0A) and LastChar0D then
          AddChar := False;
        LastChar0D := False;
      end;

      if AddChar then
      begin
        TAnsiCharArray(Utf8Buf)[Utf8Idx] := AnsiCh;
        inc(Utf8Idx);
      end;

    end
    else
    begin
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // MultiByteToWideChar is in the Windows unit of Borland Delphi7 and on
      MultiByteToWideChar(ACodePage, 0, @AnsiCh, 1, @WideCh, 1);
      Len := sdWideToUtf8Buffer(WideCh, TAnsiCharArray(Utf8Buf)[Utf8Idx], 1, LastChar0D);
      inc(Utf8Idx, Len);
    end;
    inc(AnsiIdx);
  end;
  Result := Utf8Idx;
end;

function sdNormaliseBuffer(const RawBuf; var NormBuf; Count: integer; var LastChar0D: boolean): integer;
// raw to normalised buffer (suitable for UTF8 or ansi)
var
  RawIdx, NormIdx: integer;
  ByteCh: byte;
  AddChar: boolean;
begin
  RawIdx := 0;
  NormIdx := 0;

  while RawIdx < Count do
  begin
    AddChar := True;
    ByteCh := TByteArray(RawBuf)[RawIdx];

    if ByteCh = $0D then
    begin
      ByteCh := $0A;
      LastChar0D := True;
    end
    else
    begin
      if (ByteCh = $0A) and LastChar0D then
        AddChar := False;
      LastChar0D := False;
    end;

    if AddChar then
    begin
      TByteArray(NormBuf)[NormIdx] := ByteCh;
      inc(NormIdx);
    end;

    inc(RawIdx);
  end;
  Result := NormIdx;
end;

function sdUtf8ToAnsi(const U: Utf8String; ACodePage: integer): AnsiString;
// Convert UTF8 to Ansi string
var
  Utf8Count, AnsiCount: integer;
  DefaultCharUsed: boolean;
begin
  Utf8Count := length(U);
  SetLength(Result, Utf8Count);
  if Utf8Count = 0 then
    exit;

  AnsiCount := sdUtf8ToAnsiBuffer(U[1], Result[1], ACodePage, Utf8Count, DefaultCharUsed);
  SetLength(Result, AnsiCount);
end;

function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer; var DefaultCharUsed: boolean): integer;
var
  AnsiIdx, Utf8Idx: integer;
  Utf8Ch: AnsiChar;
  WideCh: WideChar;
  Len: integer;
  DU: Pointer;
const
  cDefaultChar: AnsiChar = '?';
begin
  AnsiIdx := 0;
  Utf8Idx := 0;
  while Utf8Idx < Utf8Count do
  begin
    Utf8Ch := TAnsiCharArray(Utf8Buf)[Utf8Idx];
    if Ord(Utf8Ch) < $80 then
    begin
      // characters < $80: just copy the single characters
      DefaultCharUsed := False;
      Len := 1;
      TAnsiCharArray(AnsiBuf)[AnsiIdx] := Utf8Ch;
      inc(AnsiIdx);
    end
    else
    begin
      Len := sdUtf8CharacterLength(TAnsiCharArray(Utf8Buf)[Utf8Idx]);
      sdUtf8ToWideBuffer(TAnsiCharArray(Utf8Buf)[Utf8Idx], WideCh, 1);
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // WideCharToMultiByte is in the Windows unit of Borland Delphi 7
      DefaultCharUsed := False;
      DU := @DefaultCharUsed;
      WideCharToMultiByte(ACodePage, 0, @WideCh, 1, @TAnsiCharArray(AnsiBuf)[AnsiIdx], 1, @cDefaultChar, @DU);
      DefaultCharUsed := DU <> nil;
      inc(AnsiIdx);
    end;
    inc(Utf8Idx, Len);
  end;
  Result := AnsiIdx;
end;

function sdEscapeString(const AValue: Utf8String): Utf8String;
// contributor: Michael Cessna
var
  i, Len: integer;
  P: PAnsiChar;
  HasEscapes: boolean;
  ScratchMem: TsdFastMemStream;
begin
  Result := '';
  Len := length(AValue);
  if Len = 0 then
    exit;

  HasEscapes := False;
  P := PAnsiChar(AValue);
  for i := 0 to Len - 1 do
  begin
    case P^ of
      '"':
        begin
          HasEscapes := True;
          break;
        end;
      '''':
        begin
          HasEscapes := True;
          break;
        end;
      '&':
        begin
          HasEscapes := True;
          break;
        end;
      '<':
        begin
          HasEscapes := True;
          break;
        end;
      '>':
        begin
          HasEscapes := True;
          break;
        end;
    end;
    inc(P);
  end;
  if not HasEscapes then
  begin
    Result := AValue;
    exit;
  end;

  // ScratchMem is a TsdFastMemStream
  ScratchMem := TsdFastMemStream.Create(Len * 2);
  try
    P := PAnsiChar(AValue);
    for i := 0 to Len - 1 do
    begin
      case P^ of
        '"':
          ScratchMem.Write(AnsiString('&quot;'), 6);
        '''':
          ScratchMem.Write(AnsiString('&apos;'), 6);
        '&':
          ScratchMem.Write(AnsiString('&amp;'), 5);
        '<':
          ScratchMem.Write(AnsiString('&lt;'), 4);
        '>':
          ScratchMem.Write(AnsiString('&gt;'), 4);
      else
        ScratchMem.Write(P^, 1);
      end;
      inc(P);
    end;
    SetString(Result, PAnsiChar(ScratchMem.Memory), ScratchMem.Position);
  finally
    ScratchMem.Free;
  end;
end;

function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean; References: array of TXmlNode): Utf8String; overload;
var
  i, j, k, V, Code, Len: integer;
  W: Word;
  P, Q: PAnsiChar;
  HasReferences, FoundReference: boolean;
  Reference, Replacement: Utf8String;
  ScratchMem: TsdFastMemStream;

  // local
  function FindNonStandardReferenceReplacement(AReference: Utf8String): Utf8String;
  var
    i: integer;
    Entity: TsdDtdEntity;
    ReferenceName, ReferenceValue: Utf8String;
  begin
    Result := '';
    if length(References) = 0 then
      exit;
    for i := 0 to length(References) - 1 do
    begin
      if References[i] is TsdDtdEntity then
      begin
        Entity := TsdDtdEntity(References[i]);
        ReferenceName := '&' + Entity.Name + ';';
        ReferenceValue := Entity.Value;
        if AReference = ReferenceName then
        begin
          Result := ReferenceValue;
          break;
        end;
      end;
    end;
  end;

// main
begin
  Result := '';
  Len := length(AValue);
  if Len = 0 then
    exit;

  HasReferences := False;
  HasNonStandardReferences := False;
  P := PAnsiChar(AValue);
  for i := 0 to Len - 1 do
  begin
    if P^ = '&' then
      HasReferences := True;
    inc(P);
  end;
  if not HasReferences then
  begin
    Result := AValue;
    exit;
  end;

  // ScratchMem is a TsdFastMemStream
  ScratchMem := TsdFastMemStream.Create(Len);
  try
    P := PAnsiChar(AValue);
    i := 0;
    while i < Len do
    begin
      FoundReference := False;
      if P^ = '&' then
      begin
        Q := P;
        inc(Q);
        for j := i + 1 to Len - 1 do
        begin
          if Q^ = '&' then
          begin
            // erronous duplicate quote! just let it be
            FoundReference := False;
            break;
          end;

          if Q^ = ';' then
          begin
            // find reference
            Reference := Copy(AValue, i + 1, j - i + 1);
            inc(P, length(Reference) - 1);
            inc(i, length(Reference) - 1);

            // Look up standard reference escapes
            for k := 0 to cEscapePhraseCount - 1 do
            begin
              if Reference = cXmlReplacePhrases[k] then
              begin
                // replacement
                Replacement := cXmlEscapePhrases[k];
                ScratchMem.Write(Replacement[1], length(Replacement));
                FoundReference := True;
                break;
              end;
            end;

            // Look up hex character reference
            if not FoundReference then
            begin
              if Copy(Reference, 1, 2) = '&#' then
              begin
                Reference := Copy(Reference, 3, length(Reference) - 3);
                if length(Reference) > 0 then
                begin
                  if (Reference[1] = 'x') or (Reference[1] = 'X') then
                    // Hex notation
                    Reference[1] := '$';
                  Val(string(Reference), V, Code);
                  if (V >= 0) and (V <= $FFFF) and (Code = 0) then
                  begin
                    W := V;
                    Replacement := sdWideToUtf8(WideChar(W));
                    ScratchMem.Write(Replacement[1], length(Replacement));
                    FoundReference := True;
                    break;
                  end;
                end;
              end;
            end;

            if not FoundReference then
            begin

              // there might be a non-standard reference, try to replace
              Replacement := FindNonStandardReferenceReplacement(Reference);
              if length(Replacement) = 0 then
              begin
                // replacement not found, so just write the reference
                ScratchMem.Write(Reference[1], length(Reference));
              end
              else
              begin
                // write the replacement that was found :)
                ScratchMem.Write(Replacement[1], length(Replacement));
              end;
              FoundReference := True;
              HasNonStandardReferences := True;

            end;
            break;
          end;
          inc(Q);
        end;
      end;
      if not FoundReference then
        ScratchMem.Write(P^, 1);
      inc(P);
      inc(i);
    end;
    SetString(Result, PAnsiChar(ScratchMem.Memory), ScratchMem.Position);
  finally
    ScratchMem.Free;
  end;
end;

function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean): Utf8String;
var
  References: array of TXmlNode;
begin
  SetLength(References, 0);
  sdReplaceString(AValue, HasNonStandardReferences, References);
end;

function sdReplaceString(const AValue: Utf8String): Utf8String; overload;
var
  HasNonStandardReferences: boolean;
  References: array of TXmlNode;
begin
  HasNonStandardReferences := False;
  SetLength(References, 0);
  Result := sdReplaceString(AValue, HasNonStandardReferences, References);
end;

function sdCommaToDot(const AValue: Utf8String): Utf8String;
var
  i: integer;
begin
  Result := AValue;
  for i := 1 to length(AValue) do
    if AValue[i] = ',' then
      Result[i] := '.';
end;

function sdTrim(const S: Utf8String): Utf8String;
var
  i, L: integer;
begin
  L := length(S);
  i := 1;
  while (i <= L) and (S[i] <= ' ') do
    inc(i);

  if i > L then
    Result := ''

  else
  begin
    while S[L] <= ' ' do
      dec(L);

    Result := Copy(S, i, L - i + 1);
  end;
end;

function sdTrim(const S: Utf8String; var IsTrimmed: boolean): Utf8String;
begin
  Result := sdTrim(S);
  IsTrimmed := length(Result) < length(S);
end;

function sdTrim(const S: Utf8String; var PreString, PostString: Utf8String): Utf8String;
var
  i, L: integer;
begin
  L := length(S);
  i := 1;
  while (i <= L) and (S[i] <= ' ') do
    inc(i);

  if i > L then
  begin
    PreString := S;
    Result := '';
    PostString := '';
  end
  else
  begin
    while S[L] <= ' ' do
      dec(L);

    PreString := Copy(S, 1, i - 1);;
    Result := Copy(S, i, L - i + 1);
    PostString := Copy(S, L + 1, length(S) - L);
  end;
end;

function sdNormaliseEol(const S: Utf8String): Utf8String;
// compress all eol (CR-LF, LF or CR) to normalised eol (only LF aka #$0A)
var
  i, j, L, IntervalCount: integer;
  HasCR, HasLF: boolean;
begin
  L := length(S);
  HasCR := False;
  HasLF := False;
  IntervalCount := 0;
  i := 1;
  while i <= L do
  begin
    if (S[i] = #$0D) then
      HasCR := True;
    if (S[i] = #$0A) then
    begin
      HasLF := True;
      inc(IntervalCount);
    end;
    inc(i);
  end;

  // both CR and LF found?
  if HasCR and HasLF then
  begin

    // we now know interval count, set the correct length
    SetLength(Result, L - IntervalCount);
    i := 1;
    j := 1;
    while i <= L do
    begin
      case S[i] of
        #$0D:
          Result[j] := #$0A;
        #$0A:
          dec(j);
      else
        Result[j] := S[i];
      end;
      inc(i);
      inc(j);
    end;

  end
  else
  begin

    // either CR or LF found, but not both.
    // We can assume Result := S with only minor changes
    Result := S;

    if HasCR then
    begin

      // HasCR found.. we will replace CR with LF
      i := 1;
      while i <= L do
      begin
        if S[i] = #$0D then
          Result[i] := #$0A;
        inc(i);
      end;

    end;
  end;
end;

function sdUnNormaliseEol(const S: Utf8String; const EolStyle: TsdEolStyle): Utf8String;
// expand all normalised eol (LF) to un-normalised eol (CR-LF for Win, CR for Mac)
var
  i, j, L, IntervalCount: integer;
begin
  // determine interval count
  L := length(S);
  IntervalCount := 0;
  i := 1;
  while i <= L do
  begin
    if S[i] = #$0A then
      inc(IntervalCount);
    inc(i);
  end;

  // no intervals?
  if IntervalCount = 0 then
  begin
    Result := S;
    exit;
  end;

  // Mac style?
  if EolStyle = esCR then
  begin
    Result := S;
    i := 1;
    while i <= L do
    begin
      if S[i] = #$0A then
        Result[i] := #$0D;
      inc(i);
    end;
    exit;
  end;

  // Windows style?
  if EolStyle = esCRLF then
  begin
    // we now know interval count, set the correct length
    SetLength(Result, L + IntervalCount);
    i := 1;
    j := 1;
    while i <= L do
    begin
      case S[i] of
        #$0A:
          begin
            Result[j] := #$0D;
            inc(j);
            Result[j] := #$0A;
          end
      else
        Result[j] := S[i];
      end;
      inc(i);
      inc(j);
    end;
  end;
end;

function sdReadFromStream(S: TStream; CharCount: integer): Utf8String;
begin
  SetLength(Result, CharCount);
  if CharCount > 0 then
  begin
    S.Read(Result[1], CharCount);
  end;
end;

procedure sdWriteToStream(S: TStream; const Value: Utf8String);
begin
  if length(Value) > 0 then
  begin
    S.Write(Value[1], length(Value));
  end;
end;

function sdReadCardinal(S: TStream): Cardinal;
var
  C: byte;
  Bits: integer;
begin
  Result := 0;
  Bits := 0;
  repeat
    S.Read(C, 1);
    if C > 0 then
    begin
      inc(Result, (C and $7F) shl Bits);
      inc(Bits, 7);
      if Bits > 32 then
        raise Exception.Create('invalid cardinal reader');
    end;
  until (C and $80) = 0;
end;

procedure sdWriteCardinal(S: TStream; ACardinal: Cardinal);
var
  C: byte;
begin
  repeat
    if ACardinal <= $7F then
    begin
      C := ACardinal;
      S.Write(C, 1);
      exit;
    end
    else
      C := (ACardinal and $7F) or $80;

    S.Write(C, 1);
    ACardinal := ACardinal shr 7;
  until ACardinal = 0;
end;

procedure sdStreamWriteCardinal(S: TStream; ACardinal: Cardinal);
var
  C: byte;
begin
  repeat
    if ACardinal <= $7F then
    begin
      C := ACardinal;
      S.Write(C, 1);
      exit;
    end
    else
      C := (ACardinal and $7F) or $80;
    S.Write(C, 1);
    ACardinal := ACardinal shr 7;
  until ACardinal = 0;
end;

function sdCharsetToCodePage(ACharset: Utf8String; ADefaultCodepage: integer = 65001): integer;
var
  i: integer;
begin
  for i := 0 to cCodepageInfoCount - 1 do
  begin
    if AnsiCompareText(ACharset, cCodePageInfo[i].Name) = 0 then
    begin
      Result := cCodePageInfo[i].Codepage;
      exit;
    end;
  end;
  // Default
  Result := ADefaultCodepage;
end;

function sdCharsetToStringEncoding(ACharset: Utf8String): TsdStringEncoding;
var
  Codepage: integer;
begin
  Codepage := sdCharsetToCodePage(ACharset);
  case Codepage of
    1200:
      Result := seUTF16LE;
    1201:
      Result := seUTF16BE;
    65001:
      Result := seUTF8;
  else
    Result := seAnsi;
  end;
end;

function sdCodepageToCharset(ACodePage: integer): Utf8String;
// find the charset corresponding to windows codepage
var
  i: integer;
begin
  for i := 0 to cCodepageInfoCount - 1 do
  begin
    if cCodePageInfo[i].Codepage = ACodePage then
    begin
      Result := cCodePageInfo[i].Name;
      exit;
    end;
  end;
  // default to 'utf-8'
  Result := 'utf-8';
end;

function Utf8CompareText(const S1, S2: Utf8String): integer;
begin
  // AnsiCompareText is case-insensitive
  Result := AnsiCompareText(AnsiString(S1), AnsiString(S2));
end;

function GetTimeZoneBias: integer;
{$IFDEF MSWINDOWS}
// uses windows unit, func GetTimeZoneInformation
// contributor: Stefan Glienke
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_UNKNOWN:
      Result := TimeZoneInfo.Bias;
    TIME_ZONE_ID_STANDARD:
      Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT:
      Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
  else
    Result := 0;
  end;
end;
{$ELSE MSWINDOWS}

begin
  // NH: I dont know the linux equivalent..
  Result := 0;
end;
{$ENDIF MSWINDOWS}
{ XYZ to string functions }

function sdDateTimeToString(ADate: TDateTime; UseDate: boolean = True; UseTime: boolean = True; SplitSecondDigits: integer = 0;
  UseLocalBias: boolean = False): Utf8String;
// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Word;
  ABias: integer;
  DatePortion, TimePortion, SplitSecondPortion, LocalBiasPortion: Utf8String;
const
  Neg: array [boolean] of string = ('+', '-');
begin
  DatePortion := '';
  TimePortion := '';

  if UseDate then
  begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    DatePortion := Utf8String(Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]));
    // if we also use time, add the 'T' in advance
    if UseTime then
      DatePortion := DatePortion + 'T';
  end;
  if UseTime then
  begin
    DecodeTime(ADate, AHour, AMin, ASec, AMSec);
    if SplitSecondDigits > 0 then
    begin
      SplitSecondPortion := Utf8String(Format('%.3d', [AMSec]));
      if SplitSecondDigits < 3 then
      begin
        SplitSecondPortion := Copy(SplitSecondPortion, 1, SplitSecondDigits);
      end;
      SplitSecondPortion := '.' + SplitSecondPortion;
    end
    else
    begin
      SplitSecondPortion := '';
    end;
    if UseLocalBias then
    begin
      ABias := GetTimeZoneBias;
      LocalBiasPortion := Utf8String(Format('%s%.2d:%.2d', [Neg[ABias > 0], Abs(ABias) div MinsPerHour, Abs(ABias) mod MinsPerHour]))
    end
    else
    begin
      LocalBiasPortion := 'Z';
    end;
    // final time portion
    TimePortion := Utf8String(Format('%.2d:%.2d:%.2d', [AHour, AMin, ASec])) + SplitSecondPortion + LocalBiasPortion;
  end;
  // final result
  Result := DatePortion + TimePortion;
end;

function sdBoolToString(Value: boolean): Utf8String;
const
  // do NOT localize! This is part of the W3 XML spec
  cBoolValues: array [boolean] of Utf8String = ('false', 'true');
begin
  Result := cBoolValues[Value];
end;

function sdBoolFromString(Value: Utf8String): boolean;
begin
  Result := StrToBool(Value);
end;

function sdFloatToString(Value: double; SignificantDigits: integer; AllowScientific: boolean): Utf8String; overload;
const
  Limits: array [1 .. 9] of integer = (10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  Limit, Limitd, PointPos, IntVal, ScPower: integer;
  Body: Utf8String;
begin
  if (SignificantDigits < 1) or (SignificantDigits > 9) then
    raise Exception.Create(sSignificantDigitsOutOfRange);

  // Zero
  if Value = 0 then
  begin
    Result := '0';
    exit;
  end;

  // Sign
  if Value < 0 then
  begin
    Result := '-';
    Value := -Value;
  end
  else
    Result := '';

  // Determine point position
  Limit := Limits[SignificantDigits];
  Limitd := Limit div 10;
  PointPos := SignificantDigits;
  while Value < Limitd do
  begin
    Value := Value * 10;
    dec(PointPos);
  end;
  while Value >= Limit do
  begin
    Value := Value * 0.1;
    inc(PointPos);
  end;

  // Round
  IntVal := round(Value);

  // Exceptional case which happens when the value rounds up to the limit
  if IntVal = Limit then
  begin
    IntVal := IntVal div 10;
    inc(PointPos);
  end;

  // Strip off any zeros, these reduce significance count
  while (IntVal mod 10 = 0) and (PointPos < SignificantDigits) do
  begin
    dec(SignificantDigits);
    IntVal := IntVal div 10;
  end;

  // Check for scientific notation
  ScPower := 0;
  if AllowScientific and ((PointPos < -1) or (PointPos > SignificantDigits + 2)) then
  begin
    ScPower := PointPos - 1;
    dec(PointPos, ScPower);
  end;

  // Body
  Body := IntToStr(IntVal);
  while PointPos > SignificantDigits do
  begin
    Body := Body + '0';
    inc(SignificantDigits);
  end;
  while PointPos < 0 do
  begin
    Body := '0' + Body;
    inc(PointPos);
  end;
  if PointPos = 0 then
    Body := '.' + Body
  else if PointPos < SignificantDigits then
    Body := Copy(Body, 1, PointPos) + '.' + Copy(Body, PointPos + 1, SignificantDigits);

  // Final result
  if ScPower = 0 then
    Result := Result + Body
  else
    Result := Result + Body + 'E' + IntToStr(ScPower);
end;

function sdFloatToString(Value: double): Utf8String; overload;
begin
  Result := sdFloatToString(Value, cDefaultFloatSignificantDigits, cDefaultFloatAllowScientific);
end;

function sdFloatFromString(Value: Utf8String): double;
begin
{$IFDEF D7UP}
  Result := StrToFloat(Value, cXmlFormatSettings);
{$ELSE D7UP}
  // D5 version
  Result := StrToFloat(Value);
{$ENDIF D7UP}
end;

function sdIntToString(Value: integer): Utf8String;
begin
  Result := Utf8String(IntToStr(Value));
end;

function sdIntFromString(Value: Utf8String): integer;
begin
  Result := StrToInt(Value);
end;

function sdInt64ToString(Value: int64): Utf8String;
begin
  // int64 can be used with IntToStr
  Result := Utf8String(IntToStr(Value));
end;

function sdInt64FromString(Value: Utf8String): int64;
begin
  Result := StrToInt64(Value);
end;

{ end XYZ to string functions }

// stream methods
function sdStreamReadCardinal(S: TStream): Cardinal;
var
  C: byte;
  Bits: integer;
begin
  Result := 0;
  Bits := 0;
  repeat
    S.Read(C, 1);
    if C > 0 then
    begin
      inc(Result, (C and $7F) shl Bits);
      inc(Bits, 7)
    end;
  until (C and $80) = 0;
end;

// function sdStreamReadString(S: TStream; ACharCount: Cardinal): Utf8String;
// procedure sdStreamWriteCardinal(S: TStream; ACardinal: Cardinal);
procedure sdStreamWriteString(S: TStream; const AString: Utf8String);
var
  L: integer;
begin
  L := length(AString);
  if L > 0 then
  begin
    S.Write(AString[1], L);
  end;
end;

{ string to XYZ functions }

function sdStringToDateTime(const ADate: Utf8String; UseLocalBias: boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// contributor: Stefan Glienke
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Word;
  ALocalBias, ABias: integer;
begin
  AYear := StrToInt(Copy(ADate, 1, 4));
  AMonth := StrToInt(Copy(ADate, 6, 2));
  ADay := StrToInt(Copy(ADate, 9, 2));
  if length(ADate) > 16 then
  begin
    AHour := StrToInt(Copy(ADate, 12, 2));
    AMin := StrToInt(Copy(ADate, 15, 2));
    ASec := StrToIntDef(Copy(ADate, 18, 2), 0); // They might be omitted, so default to 0
    AMSec := StrToIntDef(Copy(ADate, 21, 3), 0); // They might be omitted, so default to 0
  end
  else
  begin
    AHour := 0;
    AMin := 0;
    ASec := 0;
    AMSec := 0;
  end;
  Result := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec, AMSec);
  if UseLocalBias then
  begin
    ALocalBias := GetTimeZoneBias;
    if (length(ADate) > 24) then
    begin
      ABias := StrToInt(Copy(ADate, 25, 2)) * MinsPerHour + StrToInt(Copy(ADate, 28, 2));
      if ADate[24] = '+' then
        ABias := ABias * -1;
      Result := Result + ABias / MinsPerDay;
    end;
    Result := Result - ALocalBias / MinsPerDay;
  end;
end;

function sdStringToDateTimeDef(const ADate: Utf8String; ADefault: TDateTime; UseLocalBias: boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
begin
  try
    Result := sdStringToDateTime(ADate, UseLocalBias);
  except
    Result := ADefault;
  end;
end;

function EncodeBase64(const Source: RawByteString): Utf8String;
// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as string, without any linebreaks.
begin
  if length(Source) > 0 then
    Result := EncodeBase64Buf(Source[1], length(Source))
  else
    Result := '';
end;

function EncodeBase64Buf(const Buffer; Count: integer): Utf8String;
var
  i, j: integer;
  Core: integer;
  FourChar: Cardinal;
  S: Pbyte;
begin
  // Make sure "Core" is always a multiple of 3, and this multiple
  // gets saved as 4 characters
  Core := (Count + 2) div 3;

  // Set the length of the string that stores encoded characters
  SetLength(Result, Core * 4);
  S := @Buffer;

  // Do the loop "Core" times
  for i := 0 to Core - 1 do
  begin
    FourChar := 0;
    for j := 0 to 2 do
    begin
      FourChar := FourChar shl 8 + S^;
      inc(S);
    end;
    for j := 0 to 3 do
    begin
      Result[i * 4 + 4 - j] := cBase64Char[FourChar and $3F];
      FourChar := FourChar shr 6;
    end;
  end;

  // For comformity to Base64, we must pad the data instead of zero out
  // when the size is not an exact multiple of 3
  case Core * 3 - Count of
    0:
      ; // nothing to do
    1: // pad one char
      Result[Core * 4] := cBase64PadChar;
    2: // pad two chars
      begin
        Result[Core * 4 - 1] := cBase64PadChar;
        Result[Core * 4] := cBase64PadChar;
      end;
  end; // case
end;

function DecodeBase64(const Source: Utf8String): RawByteString;
// Decode BASE64 data in Source into binary data. The function returns the binary
// data as Utf8String.
var
  BufData: Utf8String;
  BufSize, BufPos: integer;
begin
  BufData := sdRemoveControlChars(Source);

  // Determine length of data
  BufSize := length(BufData) div 4;
  if BufSize * 4 <> length(BufData) then
    raise EFilerError.Create(sErrorCalcStreamLength);
  BufSize := BufSize * 3;

  // Check padding chars
  BufPos := length(BufData);
  if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
  begin
    dec(BufPos);
    dec(BufSize);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      dec(BufSize);
  end;
  SetLength(Result, BufSize);

  // Decode
  if BufSize > 0 then
    DecodeBase64Buf(BufData, Result[1], BufSize);
end;

procedure DecodeBase64Buf(var Source: Utf8String; var Buffer; Count: integer);
var
  i, j: integer;
  BufPos, Core: integer;
  FourChar: Cardinal;
  D: Pbyte;
  Map: array [AnsiChar] of byte;
begin
  // Core * 4 is the number of chars to read - check length
  Core := length(Source) div 4;
  if Count > Core * 3 then
    raise EFilerError.Create(sMissingDataInBinaryStream);

  // Prepare map
  for i := 0 to 63 do
    Map[cBase64Char[i]] := i;
  D := @Buffer;

  // Check for final padding, and replace with "zeros". There can be
  // at max two pad chars ('=')
  BufPos := length(Source);
  if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
  begin
    Source[BufPos] := cBase64Char[0];
    dec(BufPos);
    if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
      Source[BufPos] := cBase64Char[0];
  end;

  // Do this "Core" times
  for i := 0 to Core - 1 do
  begin
    FourChar := 0;

    // Unroll the characters
    for j := 0 to 3 do
      FourChar := FourChar shl 6 + Map[Source[i * 4 + j + 1]];

    // and unroll the bytes
    for j := 2 downto 0 do
    begin
      // Check overshoot
      if integer(D) - integer(@Buffer) >= Count then
        exit;
      D^ := FourChar shr (j * 8) and $FF;
      inc(D);
    end;
  end;
end;

function EncodeBinHex(const Source: RawByteString): Utf8String;
// Encode binary data in Source as BINHEX. The function returns the BINHEX encoded
// data as UTF8String, without any linebreaks.
var
  Text: Utf8String;
begin
  SetLength(Text, length(Source) * 2);
  BinToHex(PAnsiChar(Source), PAnsiChar(Text), length(Source));
  Result := Text;
end;

function DecodeBinHex(const Source: Utf8String): RawByteString;
// Decode BINHEX data in Source into binary data. The function returns the binary
// data as RawByteString. Use a TStringStream to convert this data to a stream.
var
  Data: Utf8String;
  Size: integer;
  Buffer: RawByteString;
begin
  Data := sdRemoveControlChars(Source);

  // Determine length of data
  Size := length(Data) div 2;
  if Size * 2 <> length(Data) then
    raise EFilerError.Create(sErrorCalcStreamLength);

  SetLength(Buffer, Size);
  HexToBin(PAnsiChar(Data), PAnsiChar(Buffer), Size);
  Result := Buffer;
end;

procedure DecodeBinhexBuf(var Source: Utf8String; var Buffer; Count: integer);
var
  Size: integer;
begin
  // Determine length of data
  Size := Count div 2;
  if Size * 2 <> Count then
    raise EFilerError.Create(sErrorCalcStreamLength);

  HexToBin(PAnsiChar(Source), PAnsiChar(Buffer), Count);
end;

function sdRemoveControlChars(const AValue: Utf8String): Utf8String;
// Remove control characters from Utf8String AValue
var
  i, j: integer;
begin
  SetLength(Result, length(AValue));
  i := 1;
  j := 1;
  while i <= length(AValue) do
    if AValue[i] in cXmlBlankChars then
      inc(i)
    else
    begin
      Result[j] := AValue[i];
      inc(i);
      inc(j);
    end;
  // Adjust length
  if i <> j then
    SetLength(Result, j - 1);
end;

function sdAddControlChars(const AValue: Utf8String; const ControlChars: Utf8String; Interval: integer): Utf8String;
// Insert Chars in AValue at each Interval
var
  i, j, L: integer;
  // local
  procedure InsertControlChars;
  var
    k: integer;
  begin
    for k := 1 to length(ControlChars) do
    begin
      Result[j] := ControlChars[k];
      inc(j);
    end;
  end;

// main
begin
  if (length(ControlChars) = 0) or (Interval <= 0) then
  begin
    Result := AValue;
    exit;
  end;

  // Calculate length based on original length and total extra length for control chars
  L := length(AValue) + ((length(AValue) - 1) div Interval + 3) * length(ControlChars);
  SetLength(Result, L);

  // Copy and insert
  j := 1;
  for i := 1 to length(AValue) do
  begin
    if (i mod Interval) = 1 then
      // Insert control chars
      InsertControlChars;
    Result[j] := AValue[i];
    inc(j);
  end;
  InsertControlChars;

  // Adjust length
  dec(j);
  if L > j then
    SetLength(Result, j);
end;

{ former unit sdStringEncodig }

function sdUtf8CharacterLength(const Buffer): integer;
// determine the character length (1..4 bytes) of the Utf8 character
// in the buffer
type
  TByteArray = array [0 .. 3] of byte;
var
  P0, P1, P2, P3: byte;
begin
  P0 := TByteArray(Buffer)[0];
  Result := 1;
  if P0 < $C0 then // %11000000
  begin
    // regular single byte character
    exit;
  end;
  P1 := TByteArray(Buffer)[1];
  if (P0 and $E0) = $C0 then
  begin
    // could be 2 byte character
    if (P1 and $C0) = $80 then
    begin
      Result := 2;
    end;
    exit;
  end;
  P2 := TByteArray(Buffer)[2];
  if (P0 and $F0) = $E0 then
  begin
    // could be 3 byte character
    if ((P1 and $C0) = $80) and ((P2 and $C0) = $80) then
    begin
      Result := 3;
    end;
    exit;
  end;
  P3 := TByteArray(Buffer)[3];
  if (P0 and $F8) = $F0 then
  begin
    // could be 4 byte character
    // NB 4 byte chars are incompatible with Widechar since
    // they are outside the basic lingual plane
    if ((P1 and $C0) = $80) and ((P2 and $C0) = $80) and ((P3 and $C0) = $80) then
    begin
      Result := 4;
    end;
  end;
end;

{ TsdBinaryXml }

constructor TsdBinaryXml.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TsdBinaryXml.Destroy;
begin
  SetLength(FNewIDs, 0);
  inherited;
end;

function TsdBinaryXml.IncrementFrequency(AID: Cardinal): Cardinal;
begin
  Result := AID;
  if AID > 0 then
    FDocument.FSymbolTable.IncrementFrequency(AID);
end;

procedure TsdBinaryXml.LoadFromFile(const AFilename: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(AFilename);
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TsdBinaryXml.LoadFromStream(S: TStream);
var
  Cookie: Utf8String;
  Version: Cardinal;
  CoderName: Utf8String;
  SIn, SOut: TStream;
  PayloadPos: int64;
  PlainSize: int64;
  LineFeed: byte;
begin
  Cookie := sdReadFromStream(S, length(cBinaryXmlCookie));
  if Cookie <> cBinaryXmlCookie then
  begin
    DoDebugOut(Self, wsFail, 'invalid binary xml stream');
    exit;
  end;

  // read binary xml version number
  Version := ReadCardinal(S);
  if cBinaryXmlVersion < Version then
  begin
    // future version: failure
    DoDebugOut(Self, wsFail, 'incompatible binary version');
    exit;
  end
  else
  begin
    if cBinaryXmlVersion > Version then
    begin
      // former version: issue a warning
      DoDebugOut(Self, wsWarn, Format('loading binary version=%d, current version=%d', [Version, cBinaryXmlVersion]));
    end;
  end;

  // external compression/encryption options
  CoderName := sdReadFromStream(S, 4); // 4-character coder name

  // to allow visibility of  codec at topmost line
  S.Read(LineFeed, 1);

  PayloadPos := S.Position;

  if CoderName <> 'none' then
  begin
    SIn := TMemoryStream.Create;
    SOut := TMemoryStream.Create;
    try
      if assigned(OnDecode) then
      begin

        // plain size was stored beforehand
        PlainSize := sdReadCardinal(S);

        SIn.CopyFrom(S, S.Size - S.Position);
        SIn.Position := 0;
        CoderName := OnDecode(SIn, SOut, PlainSize);

        DoDebugOut(Self, wsInfo, Format('coder name: %s, compressed size: %d, plain size: %d', [CoderName, SIn.Size, SOut.Size]));

        SOut.Position := 0;
        S.Position := PayloadPos;
        S.CopyFrom(SOut, SOut.Size);
        S.Position := PayloadPos;
      end
      else
      begin
        DoDebugOut(Self, wsFail, 'OnDecode event unassigned');
        exit;
      end;

    finally
      SIn.Free;
      SOut.Free;
    end;
  end;

  // entirely clear xml document
  FDocument.ClearData(False, False, False);

  // read (partial) symbol table
  FDocument.SymbolTable.LoadFromStream(S);
  FDocument.DoProgress(S.Position);

  // read document
  ReadDocument(S);

end;

function TsdBinaryXml.ReadCardinal(S: TStream): Cardinal;
var
  C: byte;
  Bits: integer;
begin
  Result := 0;
  Bits := 0;
  repeat
    S.Read(C, 1);
    if C > 0 then
    begin
      inc(Result, (C and $7F) shl Bits);
      inc(Bits, 7);
      if Bits > 32 then
      begin
        DoDebugOut(Self, wsFail, 'invalid cardinal reader');
        Result := 0;
        exit;
      end;
    end;
  until (C and $80) = 0;
end;

procedure TsdBinaryXml.ReadDocument(S: TStream);
var
  SubCounts: array of integer;
  Parents: array of TXmlNode;
  Node, Parent: TXmlNode;
  NodeCount, SubCount, Level: integer;
begin

  Level := 0;
  NodeCount := 0;
  Parent := nil;

  repeat

    // Read a new node from the stream, the subcount
    // is initialized/updated in the ReadNode function
    Node := ReadNode(S, Parent, SubCount);

    if assigned(Node) then
    begin
      // DoDebugOut(Self, wsInfo, format('level=%d nodename=%s nodetype=%s',
      // [Level, Node.Name, cElementTypeNames[Node.ElementType]]));

      // increment count of all nodes
      inc(NodeCount);

      // if level is 0 then add the node to NativeXml root node list
      if (Level = 0) then
      begin
        Document.FRootNodes.Add(Node);
      end;
    end;

    // more subnodes?
    if SubCount > 0 then
    begin
      Parent := Node;
      inc(Level);
      // check dimensioning of subcounts and parents arrays
      if Level >= length(SubCounts) then
      begin
        SetLength(SubCounts, Level + 1);
        SetLength(Parents, Level + 1);
      end;
      Parents[Level] := Node;
      SubCounts[Level] := SubCount;
    end
    else
    begin
      // subcount = 0, no sub nodes.. determine the level and parent for next node

      if Level > 0 then
      begin
        dec(SubCounts[Level]);
        while (SubCounts[Level] = 0) and (Level > 0) do
        begin
          dec(Level);
          if Level > 0 then
            dec(SubCounts[Level]);
        end;
        if Level = 0 then
          Parent := nil
        else
          Parent := Parents[Level];
      end;

    end;

    // progress each 100 nodes
    if NodeCount mod 100 = 0 then
    begin
      Document.DoProgress(S.Position);
    end;

  until not assigned(Node);

  // debug
  DoDebugOut(Self, wsInfo, Format('total node count: %d', [NodeCount]));
end;

function TsdBinaryXml.ReadNode(S: TStream; AParent: TXmlNode; var SubCount: integer): TXmlNode;
var
  NodeType: TsdElementType;
  Table: TsdSymbolTable;
  C: Cardinal;

  // local
  function ReadSymbol(S: TStream): Cardinal;
  var
    C: Cardinal;
  begin
    C := ReadCardinal(S);
    if C > 0 then
      Result := C - 1 // SymbolID = C - 1
    else
      Result := Table.LoadSymbol(S);
  end;

// local
  function NodeCreate(ANodeClass: TsdNodeClass): TXmlNode;
  var
    NodeList: TsdNodeList;
  begin
    Result := ANodeClass.Create(FDocument);
    Result.FParent := AParent;
    if AParent is TsdContainerNode then
    begin
      NodeList := TsdContainerNode(AParent).NodeList;
      NodeList.Add(Result);
    end;
  end;

// local
  procedure ReadContainerNode(ANode: TXmlNode);
  begin
    // read persistent properties of containernode
    SubCount := ReadCardinal(S);
    TsdContainerNode(ANode).FDirectNodeCount := ReadCardinal(S);
    TsdContainerNode(ANode).FValueIndex := integer(ReadCardinal(S)) - 1;
  end;

// local
  procedure ReadElement(ANode: TXmlNode);
  begin
    // read persistent properties of containernode
    ReadContainerNode(ANode);
    // read persistent properties of element
    TsdElement(ANode).FNameID := ReadSymbol(S);
    TsdElement(ANode).FNodeClosingStyle := TsdNodeClosingStyle(ReadCardinal(S));
  end;

// local
  procedure ReadCharData(ANode: TXmlNode);
  begin
    TsdCharData(ANode).FValueID := ReadSymbol(S);
  end;

// local
  procedure ReadQuotedText(ANode: TXmlNode);
  begin
    TsdCharData(ANode).FValueID := ReadSymbol(S);
    TsdQuotedText(ANode).FQuoteStyle := TsdQuoteCharStyle(ReadCardinal(S));
  end;

// main
begin
  Result := nil;
  SubCount := 0;
  Table := FDocument.SymbolTable;

  C := ReadCardinal(S);
  // more than highest element type?
  if integer(C) > Ord(xeEndTag) then
  begin
    DoDebugOut(Self, wsFail, Format('unknown element type %d', [C]));
    exit;
  end;

  NodeType := TsdElementType(C);

  case NodeType of

    xeElement, xeDtdElement, xeDtdEntity, xeDtdNotation, xeDtdAttList:
      begin
        if NodeType = xeElement then
          Result := NodeCreate(TsdElement);
        if NodeType = xeDtdElement then
          Result := NodeCreate(TsdDtdElement);
        if NodeType = xeDtdEntity then
          Result := NodeCreate(TsdDtdEntity);
        if NodeType = xeDtdNotation then
          Result := NodeCreate(TsdDtdNotation);
        if NodeType = xeDtdAttList then
          Result := NodeCreate(TsdDtdAttList);
        ReadElement(Result);
      end;

    xeAttribute:
      begin
        Result := NodeCreate(TsdAttribute);
        TsdAttribute(Result).FNameID := ReadSymbol(S);
        ReadQuotedText(TsdAttribute(Result).FCoreValue);
      end;

    xeDeclaration, xeStylesheet:
      begin
        if NodeType = xeDeclaration then
          Result := NodeCreate(TsdDeclaration);
        if NodeType = xeStylesheet then
          Result := NodeCreate(TsdStyleSheet);
        ReadContainerNode(Result);
      end;

    xeDocType:
      begin
        Result := NodeCreate(TsdDocType);
        ReadContainerNode(Result);
        TsdDocType(Result).FNameID := ReadSymbol(S);
        ReadCharData(TsdDocType(Result).FExternalID);
        ReadQuotedText(TsdDocType(Result).FSystemLiteral);
        ReadQuotedText(TsdDocType(Result).FPubIDLiteral);
      end;

    xeCharData, xeComment, xeCData, xeWhiteSpace, xeCondSection, xeInstruction:
      begin
        if NodeType = xeCharData then
          Result := NodeCreate(TsdCharData);
        if NodeType = xeComment then
          Result := NodeCreate(TsdComment);
        if NodeType = xeCData then
          Result := NodeCreate(TsdCData);
        if NodeType = xeWhiteSpace then
          Result := NodeCreate(TsdWhiteSpace);
        if NodeType = xeCondSection then
          Result := NodeCreate(TsdConditionalSection);
        if NodeType = xeInstruction then
          Result := NodeCreate(TsdInstruction);
        ReadCharData(Result);
      end;

    xeQuotedText:
      begin
        Result := NodeCreate(TsdQuotedText);
        ReadQuotedText(Result);
      end;

    xeEndTag:
      begin
        // this signals end of document
        Result := nil;
      end;
  else
    begin
      DoDebugOut(Self, wsFail, Format('unknown %s', [cElementTypeNames[NodeType]]));
    end;

  end;

end;

procedure TsdBinaryXml.SaveToFile(const AFilename: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S);
    S.SaveToFile(AFilename);
  finally
    S.Free;
  end;
end;

procedure TsdBinaryXml.SaveToStream(S: TStream);
var
  Table: TsdSymbolTable;
  CoderName: Utf8String;
  CodecPos, StartPos: int64;
  SIn, SOut: TStream;
const
  NewLine: byte = $0A;
begin
  // cookie
  sdWriteToStream(S, cBinaryXmlCookie);

  // write binary xml version
  WriteCardinal(S, cBinaryXmlVersion);

  // codec position
  CodecPos := S.Position;

  // write default option "none", can be rewritten in OnDecode
  sdWriteToStream(S, 'none');

  // to allow visibility of  codec at topmost line
  S.Write(NewLine, 1);

  // start position of payload
  StartPos := S.Position;

  // sort string table by frequency
  SortByFrequency;

  // write symbol table
  Table := FDocument.SymbolTable;
  Table.SaveToStream(S, Table.PluralSymbolCount);

  // write xml structure and single symbols
  WriteDocument(S);

  if assigned(FOnEncode) then
  begin
    S.Position := StartPos;
    SIn := TMemoryStream.Create;
    SOut := TMemoryStream.Create;
    try
      SIn.CopyFrom(S, S.Size - StartPos);
      SIn.Position := 0;
      CoderName := FOnEncode(SIn, SOut, SIn.Size);
      if length(CoderName) <> 4 then
      begin
        DoDebugOut(Self, wsFail, 'coder name length <> 4');
        exit;
      end;
      S.Position := CodecPos;
      sdWriteToStream(S, CoderName);
      S.Position := StartPos;
      SOut.Position := 0;

      // first store plain size
      sdWriteCardinal(S, SIn.Size);

      // then copy output codec stream to S
      S.CopyFrom(SOut, SOut.Size);
      S.Size := S.Position;

    finally
      SIn.Free;
      SOut.Free;
    end;
  end;

end;

procedure TsdBinaryXml.SortByFrequency;
type
  TForEachFunc = function(C: Cardinal): Cardinal of object;
var
  Node: TXmlNode;
  // local
  procedure ForEachNode(ANode: TXmlNode; ForEachFunc: TForEachFunc);
  var
    NodeType: TsdElementType;
  begin
    NodeType := ANode.ElementType;
    case NodeType of
      xeElement, xeDtdElement, xeDtdEntity, xeDtdNotation, xeDtdAttList:
        TsdElement(ANode).FNameID := ForEachFunc(TsdElement(ANode).FNameID);

      xeCharData, xeComment, xeCData, xeWhiteSpace, xeCondSection, xeInstruction, xeQuotedText:
        TsdCharData(ANode).FValueID := ForEachFunc(TsdCharData(ANode).FValueID);

      xeAttribute:
        begin
          TsdAttribute(ANode).FNameID := ForEachFunc(TsdAttribute(ANode).FNameID);
          TsdAttribute(ANode).FCoreValue.FValueID := ForEachFunc(TsdAttribute(ANode).FCoreValue.FValueID);
        end;

      xeDocType:
        begin
          TsdDocType(ANode).FNameID := ForEachFunc(TsdElement(ANode).FNameID);
          TsdDocType(ANode).FExternalID.FValueID := ForEachFunc(TsdDocType(ANode).FExternalID.FValueID);
          TsdDocType(ANode).FSystemLiteral.FValueID := ForEachFunc(TsdDocType(ANode).FSystemLiteral.FValueID);
          TsdDocType(ANode).FPubIDLiteral.FValueID := ForEachFunc(TsdDocType(ANode).FPubIDLiteral.FValueID);
        end;
    end;
  end;

// main
begin
  // create NewIDs index
  SetLength(FNewIDs, FDocument.SymbolTable.SymbolCount + 1);

  // clear frequency first
  FDocument.SymbolTable.ClearFrequency;

  // increment frequency for each NameID/ValueID in Node
  Node := FDocument.FRootNodes.FindFirst;
  while assigned(Node) do
  begin
    ForEachNode(Node, IncrementFrequency);
    // DoDebugOut(Self, wsInfo, Node.ElementTypeName + ' ' + Node.Name);
    Node := FDocument.FRootNodes.FindNext(Node);
  end;

  // sort symbol table by frequency
  FDocument.SymbolTable.SortByFrequency(FNewIDs);

  // update IDs in document
  Node := FDocument.FRootNodes.FindFirst;
  while assigned(Node) do
  begin
    ForEachNode(Node, UpdateID);
    Node := FDocument.FRootNodes.FindNext(Node);
  end;
end;

function TsdBinaryXml.UpdateID(AID: Cardinal): Cardinal;
begin
  Result := FNewIDs[AID];
end;

procedure TsdBinaryXml.WriteCardinal(S: TStream; ACardinal: Cardinal);
var
  C: byte;
begin
  repeat
    if ACardinal <= $7F then
    begin
      C := ACardinal;
      S.Write(C, 1);
      exit;
    end
    else
      C := (ACardinal and $7F) or $80;

    S.Write(C, 1);
    ACardinal := ACardinal shr 7;
  until ACardinal = 0;
end;

procedure TsdBinaryXml.WriteDocument(S: TStream);
var
  Count: Cardinal;
  Node: TXmlNode;
  // i: TsdElementType;
begin
  Count := 0;

  // use FindFirst/FindNext to iterate thru all the nodes
  Node := FDocument.FRootNodes.FindFirst;
  while assigned(Node) do
  begin
    // this node
    WriteNode(S, Node);
    inc(Count);

    // next node
    Node := FDocument.FRootNodes.FindNext(Node);
  end;

  { for i := low(TsdElementType) to high(TsdElementType) do
    begin
    DoDebugOut(Self, wsInfo, format('%s: %d', [cElementTypeNames[i], FElementTypeCount[i]]));
    end; }

  // write the end tag signal
  WriteCardinal(S, Ord(xeEndTag));

  // debug
  DoDebugOut(Self, wsInfo, Format('total node count: %d', [Count]));
end;

procedure TsdBinaryXml.WriteNode(S: TStream; ANode: TXmlNode);
var
  NodeType: TsdElementType;
  Table: TsdSymbolTable;

  // local
  procedure WriteSymbol(ASymbolID: Cardinal);
  begin
    if ASymbolID <= Cardinal(Table.PluralSymbolCount) then
      WriteCardinal(S, ASymbolID + 1) // "ASymbolID + 1" from 1..N
    else
    begin
      WriteCardinal(S, 0); // this signals we do not use table
      Table.SaveSymbol(S, ASymbolID);
    end;
  end;
// local
  procedure WriteContainerNode(ANode: TXmlNode);
  begin
    // check
    if not(ANode is TsdContainerNode) then
      DoDebugOut(Self, wsFail, 'wrong element type');
    // write persistent properties of containernode
    WriteCardinal(S, TsdContainerNode(ANode).FNodes.Count);
    WriteCardinal(S, TsdContainerNode(ANode).FDirectNodeCount);
    WriteCardinal(S, TsdContainerNode(ANode).FValueIndex + 1);
  end;
// local
  procedure WriteElementNode(ANode: TXmlNode);
  begin
    // check
    if not(ANode is TsdElement) then
      DoDebugOut(Self, wsFail, 'wrong element type');
    // write persistent properties of containernode
    WriteContainerNode(ANode);
    // write persistent properties of element
    WriteSymbol(TsdElement(ANode).FNameID);
    WriteCardinal(S, Ord(TsdElement(ANode).FNodeClosingStyle));
  end;
// local
  procedure WriteCharData(ANode: TXmlNode);
  begin
    // check
    if not(ANode is TsdCharData) then
      DoDebugOut(Self, wsFail, 'wrong element type');
    WriteSymbol(TsdCharData(ANode).FValueID);
  end;
// local
  procedure WriteQuotedText(ANode: TXmlNode);
  begin
    // check
    if not(ANode is TsdQuotedText) then
      DoDebugOut(Self, wsFail, 'wrong element type');
    WriteSymbol(TsdCharData(ANode).FValueID);
    WriteCardinal(S, Ord(TsdQuotedText(ANode).FQuoteStyle));
  end;

// main
begin
  Table := FDocument.SymbolTable;

  NodeType := ANode.ElementType;
  inc(FElementTypeCount[NodeType]); // for stats

  WriteCardinal(S, Ord(NodeType));

  // DoDebugOut(Self, wsInfo, IntToStr(ANode.TreeDepth) + ' ' + cElementTypeNames[NodeType] + ' ' + ANode.Name);

  case NodeType of

    xeElement, xeDtdElement, xeDtdEntity, xeDtdNotation, xeDtdAttList:
      begin
        WriteElementNode(ANode);
      end;

    xeAttribute:
      begin
        WriteSymbol(TsdAttribute(ANode).FNameID);
        WriteQuotedText(TsdAttribute(ANode).FCoreValue);
      end;

    xeDeclaration, xeStylesheet:
      begin
        WriteContainerNode(ANode);
      end;

    xeDocType:
      begin
        WriteContainerNode(ANode);
        WriteSymbol(TsdDocType(ANode).FNameID);
        WriteCharData(TsdDocType(ANode).FExternalID);
        WriteQuotedText(TsdDocType(ANode).FSystemLiteral);
        WriteQuotedText(TsdDocType(ANode).FPubIDLiteral);
      end;

    xeCharData, xeComment, xeCData, xeWhiteSpace, xeCondSection, xeInstruction:
      begin
        WriteCharData(ANode);
      end;

    xeQuotedText:
      begin
        WriteQuotedText(ANode);
      end;

    xeEndTag:
      begin
        // this signals end of document
      end;

  else
    begin
      DoDebugOut(Self, wsWarn, Format('unknown %s "%s"', [cElementTypeNames[NodeType], ANode.Name]));
    end;

  end;
end;

{$IFDEF D7UP}

procedure GetXmlFormatSettings;
var
  TimePrefix, TimePostfix, HourFormat: string;
begin
  cXmlFormatSettings.CurrencyString := '';
  cXmlFormatSettings.CurrencyFormat := 0;
  cXmlFormatSettings.NegCurrFormat := 0;
  cXmlFormatSettings.ThousandSeparator := ',';
  cXmlFormatSettings.DecimalSeparator := '.';
  cXmlFormatSettings.CurrencyDecimals := 0;
  cXmlFormatSettings.DateSeparator := '/';
  cXmlFormatSettings.ShortDateFormat := 'm/d/yy';
  cXmlFormatSettings.LongDateFormat := 'mmmm d, yyyy';
  cXmlFormatSettings.TimeSeparator := ':';
  cXmlFormatSettings.TimeAMString := 'am';
  cXmlFormatSettings.TimePMString := 'pm';
  TimePrefix := '';
  HourFormat := 'h';
  TimePostfix := ' AMPM';
  cXmlFormatSettings.ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  cXmlFormatSettings.LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  cXmlFormatSettings.ListSeparator := ',';
end;
{$ELSE D7UP}

// D5 stub
procedure GetXmlFormatSettings;
begin
end;
{$ENDIF D7UP}

initialization

// NativeXml's xml format settings (with decimal separator = '.')
GetXmlFormatSettings;

end.
