unit errno;

interface

Const

  EPERM = 1; // Operation not permitted
  ENOENT = 2; // No such file or directory
  ESRCH = 3; // No such process
  EINTR = 4; // Interrupted system call
  EIO = 5; // Input/output error
  ENXIO = 6; // Device not configured
  E2BIG = 7; // Argument list too long
  ENOEXEC = 8; // Exec format error
  EBADF = 9; // Bad file number
  ECHILD = 10; // No spawned processes
  EAGAIN = 11; // Resource temporarily unavailable
  ENOMEM = 12; // Cannot allocate memory
  EACCES = 13; // Access denied
  EFAULT = 14; // Bad address
  ENOTBLK = 15; // Not block device
  EBUSY = 16; // Device busy
  EEXIST = 17; // File exist
  EXDEV = 18; // Cross-device link
  ENODEV = 19; // Operation not supported by device
  ENOTDIR = 20; // Not a directory
  EISDIR = 21; // Is a directory
  EINVAL = 22; // Invalid argument
  ENFILE = 23; // Too many open files in system
  EMFILE = 24; // Too many files open
  ENOTTY = 25; // Inappropriate ioctl for device
  ETXTBSY = 26; // Unknown error
  EFBIG = 27; // File too large
  ENOSPC = 28; // No space left on device
  ESPIPE = 29; // Illegal seek
  EROFS = 30; // Read-only file system
  EMLINK = 31; // Too many links
  EPIPE = 32; // Broken pipe
  EDOM = 33; // Numerical arg out of domain
  ERANGE = 34; // Result too large
  EUCLEAN = 35; // Structure needs cleaning
  EDEADLK = 36; // Resource deadlock avoided
  EUNKNOWN = 37; // Unknown error
  ENAMETOOLONG = 38; // File name too long
  ENOLCK = 39; // No locks available
  ENOSYS = 40; // Function not implemented
  ENOTEMPTY = 41; // Directory not empty
  EILSEQ = 42; // Invalid multibyte sequence

  //
  // Sockets errors
  //

  EWOULDBLOCK = 45; // Operation would block
  EINPROGRESS = 46; // Operation now in progress
  EALREADY = 47; // Operation already in progress
  ENOTSOCK = 48; // Socket operation on nonsocket
  EDESTADDRREQ = 49; // Destination address required
  EMSGSIZE = 50; // Message too long
  EPROTOTYPE = 51; // Protocol wrong type for socket
  ENOPROTOOPT = 52; // Bad protocol option
  EPROTONOSUPPORT = 53; // Protocol not supported
  ESOCKTNOSUPPORT = 54; // Socket type not supported
  EOPNOTSUPP = 55; // Operation not supported
  EPFNOSUPPORT = 56; // Protocol family not supported
  EAFNOSUPPORT = 57; // Address family not supported
  EADDRINUSE = 58; // Address already in use
  EADDRNOTAVAIL = 59; // Cannot assign requested address
  ENETDOWN = 60; // Network is down
  ENETUNREACH = 61; // Network is unreachable
  ENETRESET = 62; // Network dropped connection on reset
  ECONNABORTED = 63; // Connection aborted
  ECONNRESET = 64; // Connection reset by peer
  ENOBUFS = 65; // No buffer space available
  EISCONN = 66; // Socket is already connected
  ENOTCONN = 67; // Socket is not connected
  ESHUTDOWN = 68; // Cannot send after socket shutdown
  ETOOMANYREFS = 69; // Too many references
  ETIMEDOUT = 70; // Operation timed out
  ECONNREFUSED = 71; // Connection refused
  ELOOP = 72; // Cannot translate name
  EWSNAMETOOLONG = 73; // Name component or name was too long
  EHOSTDOWN = 74; // Host is down
  EHOSTUNREACH = 75; // No route to host
  EWSNOTEMPTY = 76; // Cannot remove a directory that is not empty
  EPROCLIM = 77; // Too many processes
  EUSERS = 78; // Ran out of quota
  EDQUOT = 79; // Ran out of disk quota
  ESTALE = 80; // File handle reference is no longer available
  EREMOTE = 81; // Item is not available locally

  //
  // Resolver errors
  //

  EHOSTNOTFOUND = 82; // Host not found
  ETRYAGAIN = 83; // Nonauthoritative host not found
  ENORECOVERY = 84; // A nonrecoverable error occured
  ENODATA = 85; // Valid name, no data record of requested type

  //
  // Misc. error codes
  //

  EPROTO = 86; // Protocol error
  ECHKSUM = 87; // Checksum error
  EBADSLT = 88; // Invalid slot
  EREMOTEIO = 89; // Remote I/O error

  //
  // Error code aliases
  //

  ETIMEOUT = ETIMEDOUT;
  EBUF = ENOBUFS;
  EROUTE = ENETUNREACH;
  ECONN = ENOTCONN;
  ERST = ECONNRESET;
  EABORT = ECONNABORTED;

implementation

end.
