unit SDL2_Net;
{*******************************************************************************

  SDL2_Net.pas    v1.0  29/07/2013 first version for DelphiXE
                  v1.1  27/08/2013 add MACOS compability
                  v1.2  31/05/2014 delete sdl2.inc

  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion SDL from the JEDI-Team written by Domenique Louis and others.

  convert SDL/SDL2 to SDL2 for DelphiXE by Kotai 2013/2014  www.remakesonline.com

  The initial developer of this Pascal code was :
  Dominqiue Louis <Dominique@SavageSoftware.com.au>


*******************************************************************************}


interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  SDL2;

const

  {$IFDEF MSWINDOWS}
    SDL_NetLibName = 'SDL2_net.dll';
  {$ENDIF}

  {$IFDEF ANDROID}
    SDL_NetLibName = 'libSDL2_net.so';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
      SDL_NetLibName = 'libSDL2_net.a';
    {$ELSE}
      SDL_NetLibName = 'SDL2_net';
//      SDL_NetLibName = '../Frameworks/SDL2_net.framework/Versions/A/SDL2_net';
    {$ENDIF}
  {$ENDIF}

  {* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL *}
  SDL_NET_MAJOR_VERSION = 1;
{$EXTERNALSYM SDL_NET_MAJOR_VERSION}
  SDL_NET_MINOR_VERSION = 2;
{$EXTERNALSYM SDL_NET_MINOR_VERSION}
  SDL_NET_PATCHLEVEL = 7;
{$EXTERNALSYM SDL_NET_PATCHLEVEL}

  // SDL_Net.h constants
  {* Resolve a host name and port to an IP address in network form.
   If the function succeeds, it will return 0.
   If the host couldn't be resolved, the host portion of the returned
   address will be INADDR_NONE, and the function will return -1.
   If 'host' is NULL, the resolved host will be set to INADDR_ANY.
 *}
  INADDR_ANY = $00000000;
{$EXTERNALSYM INADDR_ANY}

  INADDR_NONE = $FFFFFFFF;
{$EXTERNALSYM INADDR_NONE}

{***********************************************************************}
{* UDP network API                                                     *}
{***********************************************************************}
{* The maximum channels on a a UDP socket *}
  SDLNET_MAX_UDPCHANNELS = 32;
{$EXTERNALSYM SDLNET_MAX_UDPCHANNELS}
{* The maximum addresses bound to a single UDP socket channel *}
  SDLNET_MAX_UDPADDRESSES = 4;
{$EXTERNALSYM SDLNET_MAX_UDPADDRESSES}

type
  // SDL_net.h types
  {***********************************************************************}
  {* IPv4 hostname resolution API                                        *}
  {***********************************************************************}
  PIPAddress = ^TIPAddress;
  TIPAddress = record
    host : Uint32; // 32-bit IPv4 host address */
    port : Uint16; // 16-bit protocol port */
  end;

  {***********************************************************************}
  {* TCP network API                                                     *}
  {***********************************************************************}
  PTCPSocket = ^TTCPSocket;
  TTCPSocket = record
    ready : integer;
//{$IFDEF MSWINDOWS}
    channel : integer;
//{$ENDIF}
    remoteAddress : TIPaddress;
    localAddress : TIPaddress;
    sflag : integer;
  end;

  {***********************************************************************}
  {* UDP network API                                                     *}
  {***********************************************************************}
  PUDP_Channel = ^TUDP_Channel;
  TUDP_Channel = record
    numbound : integer;
    address : array[ 0..SDLNET_MAX_UDPADDRESSES - 1 ] of TIPAddress;
  end;

  PUDPSocket = ^TUDPSocket;
  TUDPSocket = record
    ready : integer;
//{$IFDEF MSWINDOWS}
    channel : integer;
//{$ENDIF}
    address : TIPAddress;
    binding : array[ 0..SDLNET_MAX_UDPCHANNELS - 1 ] of TUDP_Channel;
  end;

  PUDPpacket = ^TUDPpacket;
  PPUDPpacket = ^PUDPpacket;
  TUDPpacket = record
    channel : integer; {* The src/dst channel of the packet *}
    data : PUint8; {* The packet data *}
    len : integer; {* The length of the packet data *}
    maxlen : integer; {* The size of the data buffer *}
    status : integer; {* packet status after sending *}
    address : TIPAddress; {* The source/dest address of an incoming/outgoing packet *}
  end;

  {***********************************************************************}
  {* Hooks for checking sockets for available data                       *}
  {***********************************************************************}
  PSDLNet_Socket = ^TSDLNet_Socket;
  TSDLNet_Socket = record
    ready : integer;
//{$IFDEF MSWINDOWS}
    channel : integer;
//{$ENDIF}
  end;

  PSDLNet_SocketSet = ^TSDLNet_SocketSet;
  TSDLNet_SocketSet = record
    numsockets : integer;
    maxsockets : integer;
    sockets : PSDLNet_Socket;
  end;

  {* Any network socket can be safely cast to this socket type *}
  PSDLNet_GenericSocket = ^TSDLNet_GenericSocket;
  TSDLNet_GenericSocket = record
    ready : integer;
  end;

{ This macro can be used to fill a version structure with the compile-time
  version of the SDL_net library. }
procedure SDL_NET_VERSION( var X : TSDL_version );

{* Initialize/Cleanup the network API
   SDL must be initialized before calls to functions in this library,
   because this library uses utility functions from the SDL library.
*}
function SDLNet_Init() : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Init' {$ENDIF} {$ENDIF};

procedure SDLNet_Quit();
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Quit' {$ENDIF} {$ENDIF};

 {* Resolve a host name and port to an IP address in network form.
   If the function succeeds, it will return 0.
   If the host couldn't be resolved, the host portion of the returned
   address will be INADDR_NONE, and the function will return -1.
   If 'host' is NULL, the resolved host will be set to INADDR_ANY.
 *}
function SDLNet_ResolveHost( var address : TIPaddress; host : PChar; port : Uint16 ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_ResolveHost' {$ENDIF} {$ENDIF};

{* Resolve an ip address to a host name in canonical form.
   If the ip couldn't be resolved, this function returns NULL,
   otherwise a pointer to a static buffer containing the hostname
   is returned.  Note that this function is not thread-safe.
*}
function SDLNet_ResolveIP( var ip : TIPaddress ) : PChar;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_ResolveIP' {$ENDIF} {$ENDIF};

  {***********************************************************************}
  {* TCP network API                                                     *}
  {***********************************************************************}

{* Open a TCP network socket
   If ip.host is INADDR_NONE, this creates a local server socket on the
   given port, otherwise a TCP connection to the remote host and port is
   attempted.  The address passed in should already be swapped to network
   byte order (addresses returned from SDLNet_ResolveHost() are already
   in the correct form).
   The newly created socket is returned, or NULL if there was an error.
*}
function SDLNet_TCP_Open( var ip : TIPaddress ) : PTCPSocket;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_Open' {$ENDIF} {$ENDIF};

{* Accept an incoming connection on the given server socket.
   The newly created socket is returned, or NULL if there was an error.
*}
function SDLNet_TCP_Accept( server : PTCPsocket ) : PTCPSocket;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_Accept' {$ENDIF} {$ENDIF};

{* Get the IP address of the remote system associated with the socket.
   If the socket is a server socket, this function returns NULL.
*}
function SDLNet_TCP_GetPeerAddress( sock : PTCPsocket ) : PIPAddress;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_GetPeerAddress' {$ENDIF} {$ENDIF};

{* Send 'len' bytes of 'data' over the non-server socket 'sock'
   This function returns the actual amount of data sent.  If the return value
   is less than the amount of data sent, then either the remote connection was
   closed, or an unknown socket error occurred.
*}
function SDLNet_TCP_Send( sock : PTCPsocket; data : Pointer; len : integer ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_Send' {$ENDIF} {$ENDIF};

{* Receive up to 'maxlen' bytes of data over the non-server socket 'sock',
   and store them in the buffer pointed to by 'data'.
   This function returns the actual amount of data received.  If the return
   value is less than or equal to zero, then either the remote connection was
   closed, or an unknown socket error occurred.
*}
function SDLNet_TCP_Recv( sock : PTCPsocket; data : Pointer; maxlen : integer ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_Recv' {$ENDIF} {$ENDIF};

{* Close a TCP network socket *}
procedure SDLNet_TCP_Close( sock : PTCPsocket );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_TCP_Close' {$ENDIF} {$ENDIF};


  {***********************************************************************}
  {* UDP network API                                                     *}
  {***********************************************************************}

  {* Allocate/resize/free a single UDP packet 'size' bytes long.
   The new packet is returned, or NULL if the function ran out of memory.
 *}
function SDLNet_AllocPacket( size : integer ) : PUDPpacket;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_AllocPacket' {$ENDIF} {$ENDIF};

function SDLNet_ResizePacket( packet : PUDPpacket; newsize : integer ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_ResizePacket' {$ENDIF} {$ENDIF};

procedure SDLNet_FreePacket( packet : PUDPpacket );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_FreePacket' {$ENDIF} {$ENDIF};

{* Allocate/Free a UDP packet vector (array of packets) of 'howmany' packets,
   each 'size' bytes long.
   A pointer to the first packet in the array is returned, or NULL if the
   function ran out of memory.
 *}
function SDLNet_AllocPacketV( howmany : integer; size : integer ) : PUDPpacket;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_AllocPacketV' {$ENDIF} {$ENDIF};

procedure SDLNet_FreePacketV( packetV : PUDPpacket );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_FreePacketV' {$ENDIF} {$ENDIF};

{* Open a UDP network socket
   If 'port' is non-zero, the UDP socket is bound to a local port.
   This allows other systems to send to this socket via a known port.
*}
function SDLNet_UDP_Open( port : Uint16 ) : PUDPsocket;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Open' {$ENDIF} {$ENDIF};

{* Bind the address 'address' to the requested channel on the UDP socket.
   If the channel is -1, then the first unbound channel will be bound with
   the given address as it's primary address.
   If the channel is already bound, this new address will be added to the
   list of valid source addresses for packets arriving on the channel.
   If the channel is not already bound, then the address becomes the primary
   address, to which all outbound packets on the channel are sent.
   This function returns the channel which was bound, or -1 on error.
*}
function SDLNet_UDP_Bind( sock : PUDPsocket; channel : integer; var address : TIPaddress ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Bind' {$ENDIF} {$ENDIF};

{* Unbind all addresses from the given channel *}
procedure SDLNet_UDP_Unbind( sock : PUDPsocket; channel : integer );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Unbind' {$ENDIF} {$ENDIF};

{* Get the primary IP address of the remote system associated with the
   socket and channel.  If the channel is -1, then the primary IP port
   of the UDP socket is returned -- this is only meaningful for sockets
   opened with a specific port.
   If the channel is not bound and not -1, this function returns NULL.
 *}
function SDLNet_UDP_GetPeerAddress( sock : PUDPsocket; channel : integer ) : PIPAddress;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_GetPeerAddress' {$ENDIF} {$ENDIF};

{* Send a vector of packets to the the channels specified within the packet.
   If the channel specified in the packet is -1, the packet will be sent to
   the address in the 'src' member of the packet.
   Each packet will be updated with the status of the packet after it has
   been sent, -1 if the packet send failed.
   This function returns the number of packets sent.
*}
function SDLNet_UDP_SendV( sock : PUDPsocket; packets : PPUDPpacket; npackets : integer ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_SendV' {$ENDIF} {$ENDIF};

{* Send a single packet to the specified channel.
   If the channel specified in the packet is -1, the packet will be sent to
   the address in the 'src' member of the packet.
   The packet will be updated with the status of the packet after it has
   been sent.
   This function returns 1 if the packet was sent, or 0 on error.
*}
function SDLNet_UDP_Send( sock : PUDPsocket; channel : integer; packet : PUDPpacket ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Send' {$ENDIF} {$ENDIF};

{* Receive a vector of pending packets from the UDP socket.
   The returned packets contain the source address and the channel they arrived
   on.  If they did not arrive on a bound channel, the the channel will be set
   to -1.
   The channels are checked in highest to lowest order, so if an address is
   bound to multiple channels, the highest channel with the source address
   bound will be returned.
   This function returns the number of packets read from the network, or -1
   on error.  This function does not block, so can return 0 packets pending.
*}
function SDLNet_UDP_RecvV( sock : PUDPsocket; packets : PPUDPpacket ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_RecvV' {$ENDIF} {$ENDIF};

{* Receive a single packet from the UDP socket.
   The returned packet contains the source address and the channel it arrived
   on.  If it did not arrive on a bound channel, the the channel will be set
   to -1.
   The channels are checked in highest to lowest order, so if an address is
   bound to multiple channels, the highest channel with the source address
   bound will be returned.
   This function returns the number of packets read from the network, or -1
   on error.  This function does not block, so can return 0 packets pending.
*}
function SDLNet_UDP_Recv( sock : PUDPsocket; packet : PUDPpacket ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Recv' {$ENDIF} {$ENDIF};

{* Close a UDP network socket *}
procedure SDLNet_UDP_Close( sock : PUDPsocket );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_UDP_Close' {$ENDIF} {$ENDIF};

  {***********************************************************************}
  {* Hooks for checking sockets for available data                       *}
  {***********************************************************************}

{* Allocate a socket set for use with SDLNet_CheckSockets()
   This returns a socket set for up to 'maxsockets' sockets, or NULL if
   the function ran out of memory.
 *}
function SDLNet_AllocSocketSet( maxsockets : integer ) : PSDLNet_SocketSet;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_AllocSocketSet' {$ENDIF} {$ENDIF};

{* Add a socket to a set of sockets to be checked for available data *}
function SDLNet_AddSocket( set_ : PSDLNet_SocketSet; sock : PSDLNet_GenericSocket ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_AddSocket' {$ENDIF} {$ENDIF};

function SDLNet_TCP_AddSocket( set_ : PSDLNet_SocketSet; sock : PTCPSocket ) : integer;

function SDLNet_UDP_AddSocket( set_ : PSDLNet_SocketSet; sock : PUDPSocket ) : integer;


{* Remove a socket from a set of sockets to be checked for available data *}
function SDLNet_DelSocket( set_ : PSDLNet_SocketSet; sock : PSDLNet_GenericSocket ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_DelSocket' {$ENDIF} {$ENDIF};

function SDLNet_TCP_DelSocket( set_ : PSDLNet_SocketSet; sock : PTCPSocket ) : integer;

function SDLNet_UDP_DelSocket( set_ : PSDLNet_SocketSet; sock : PUDPSocket ) : integer;

{* This function checks to see if data is available for reading on the
   given set of sockets.  If 'timeout' is 0, it performs a quick poll,
   otherwise the function returns when either data is available for
   reading, or the timeout in milliseconds has elapsed, which ever occurs
   first.  This function returns the number of sockets ready for reading,
   or -1 if there was an error with the select() system call.
*}
function SDLNet_CheckSockets( set_ : PSDLNet_SocketSet; timeout : Sint32 ) : integer;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_CheckSockets' {$ENDIF} {$ENDIF};

{* After calling SDLNet_CheckSockets(), you can use this function on a
   socket that was in the socket set, to find out if data is available
   for reading.
*}
function SDLNet_SocketReady( sock : PSDLNet_GenericSocket ) : boolean;

{* Free a set of sockets allocated by SDL_NetAllocSocketSet() *}
procedure SDLNet_FreeSocketSet( set_ : PSDLNet_SocketSet );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_FreeSocketSet' {$ENDIF} {$ENDIF};

{***********************************************************************}
{* Platform-independent data conversion functions                      *}
{***********************************************************************}

{* Write a 16/32 bit value to network packet buffer *}
procedure SDLNet_Write16( value : Uint16; area : Pointer );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Write16' {$ENDIF} {$ENDIF};

procedure SDLNet_Write32( value : Uint32; area : Pointer );
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Write32' {$ENDIF} {$ENDIF};

{* Read a 16/32 bit value from network packet buffer *}
function SDLNet_Read16( area : Pointer ) : Uint16;
//{$IFNDEF CPUARM}
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Read16' {$ENDIF} {$ENDIF};
//{$ENDIF}

function SDLNet_Read32( area : Pointer ) : Uint32;
//{$IFNDEF CPUARM}
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_Read32' {$ENDIF} {$ENDIF};
//{$ENDIF}

{***********************************************************************}
{* Error reporting functions                                           *}
{***********************************************************************}

{* We'll use SDL's functions for error reporting *}
procedure SDLNet_SetError( fmt : PChar );
function SDLNet_GetError() : PChar;

(* I'm eventually going to try to disentangle SDL_net from SDL, thus making
   SDL_net an independent X-platform networking toolkit.  Not today though....

proceudre SDLNet_SetError(const char *fmt, ...);
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_SetError' {$ENDIF} {$ENDIF};

function SDLNet_GetError() : PChar;
cdecl; external SDL_NetLibName {$IFDEF MACOS} {$IFNDEF IOS} name '_SDLNet_GetError' {$ENDIF} {$ENDIF};
*)





//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************
//******************************************************************************





implementation

//******************************************************************************

procedure SDL_NET_VERSION( var X : TSDL_version );
begin
     X.major := SDL_NET_MAJOR_VERSION;
     X.minor := SDL_NET_MINOR_VERSION;
     X.patch := SDL_NET_PATCHLEVEL;
end;

//******************************************************************************

function SDLNet_TCP_AddSocket( set_ : PSDLNet_SocketSet; sock : PTCPSocket ) : integer;
begin
     result := SDLNet_AddSocket( set_, PSDLNet_GenericSocket( sock ) );
end;

//******************************************************************************
function SDLNet_UDP_AddSocket( set_ : PSDLNet_SocketSet; sock : PUDPSocket ) : integer;
begin
     result := SDLNet_AddSocket( set_, PSDLNet_GenericSocket( sock ) );
end;

//******************************************************************************
function SDLNet_TCP_DelSocket( set_ : PSDLNet_SocketSet; sock : PTCPSocket ) : integer;
begin
     result := SDLNet_DelSocket( set_, PSDLNet_GenericSocket( sock ) );
end;

//******************************************************************************
function SDLNet_UDP_DelSocket( set_ : PSDLNet_SocketSet; sock : PUDPSocket ) : integer;
begin
     result := SDLNet_DelSocket( set_, PSDLNet_GenericSocket( sock ) );
end;

//******************************************************************************

{* After calling SDLNet_CheckSockets(), you can use this function on a
   socket that was in the socket set, to find out if data is available
   for reading.
*}
function SDLNet_SocketReady( sock : PSDLNet_GenericSocket ) : boolean;
begin
     result := ( ( sock <> nil ) and ( sock^.ready = 1 ) );
end;

//******************************************************************************

procedure SDLNet_SetError( fmt : PChar );
begin
     SDL_SetError( fmt );
end;

//******************************************************************************

function SDLNet_GetError() : PChar;
begin
     result := SDL_GetError;
end;

//******************************************************************************

//{$IFDEF CPUARM}
//function SDLNet_Read16( area : Pointer ) : Uint16;
//begin
//  result := ((PUint8(area)^ shl  8) or (PUint8(area+1)^ shl 0));
//end;

//******************************************************************************

//function SDLNet_Read32( area : Pointer ) : Uint32;
//begin
//  result := ((PUint8(area)^ shl  24) or (PUint8(area+1)^ shl 16) or (PUint8(area+2)^ shl 8) or (PUint8(area+3)^ shl 0) );
//end;
//{$ENDIF}

end.

