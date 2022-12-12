/*  ===  Operating system interface wrappers  ======================  */

/*  $Id: os.c,v 1.132 2022/11/17 14:16:07 setlorg Exp $  */

/*  Free software (c) dB - see file COPYING for license (GPL).  */

/*
 *  Here find wrapped versions of routines from the standard C library,
 *  from POSIX, and from the common BSD socket/select extensions (which
 *  are themselves now defined by POSIX).
 *
 *  Most of the wrapping is to cover what happens when signals are
 *  caught during the execution of certain system routines, or to
 *  abend on errors in a uniform way, but some of it is just isolation
 *  of possible system dependencies.
 *
 *  Ideally, this should be the only file you have to make extensive
 *  changes in if you try to port to a non-POSIX operating system.
 */

#include "setlrun.h"

#if HAVE_SYS_NEUTRINO_H  /* QNX 6.x */
#include <sys/neutrino.h>
#endif

#if HAVE_TERMIOS_H
#include <termios.h>
#elif HAVE_TERMIO_H
#include <termio.h>
#elif HAVE_BSD_SGTTY_H
#include <bsd/sgtty.h>
#endif

#if HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#if HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#if HAVE_STROPTS_H
#include <stropts.h>
#endif

#ifndef HAVE_SOCKLEN_T
typedef unsigned int  socklen_t;  /* e.g., for Apple */
#endif

#ifndef O_NOCTTY
/* Presumably an ancient Unix; assume flag not supported.  */
#define O_NOCTTY  0
#endif

#ifdef TIOCSCTTY
/* Controlling terminal can be requested explicitly.  */
#define O_NOCTTY_COND  O_NOCTTY
#else
/* Must fall back to acquisition of first tty by session leader.  */
#define O_NOCTTY_COND  0
#endif


/*
 *  This time base is set to os_mono() (which measures elapsed time
 *  monotonically since some arbitrary system init in the past) at
 *  the beginning of execution of a SETL program (the "outer" one,
 *  if there ever comes a time when execute() recurses) and at the
 *  beginning of each new child process created by os_fork(), using
 *  os_set_time_base().
 */
static struct timespec time_base;  /* for real-time (wall) clock */

static int max_open_files = 0;  /* 1 + max possible file descriptor */

/*
 *  On the Alpha with OSF 4.0, the definition of PENDIN only seems to
 *  be included if _OSF_SOURCE is defined; but defining _OSF_SOURCE
 *  raises worse problems, so I take the easy way out here...
 */
#ifndef PENDIN
#define PENDIN 0x20000000
#endif


void os_error(const char *s) {
  panic("%s: %s", s, os_strerror(errno));
}


void *os_malloc(size_t nbytes) {
  int saved_errno = errno;
  void *r = malloc(nbytes);
  if (r == NULL) panic("malloc: failed to obtain %zu bytes", nbytes);
  errno = saved_errno;
  return r;
}

void *os_realloc(void *p, size_t nbytes) {
  int saved_errno = errno;
  void *r = realloc(p, nbytes);
  if (r == NULL) panic("realloc: failed to obtain %zu bytes", nbytes);
  errno = saved_errno;
  return r;
}

void os_free(void *p) {
  if (p) {
    int saved_errno = errno;
    free(p);
    errno = saved_errno;
  }
}


char *os_getenv(const char *name) {
  int saved_errno = errno;
  char *r = getenv(name);
  errno = saved_errno;
  return r;
}

void os_setenv(const char *name, const char *value) {
  int saved_errno = errno;
#if HAVE_SETENV
  /* Note that setenv() internally allocates system space as needed.
   * It may (and probably does) release space for an existing value
   * when a new value is being associated with an existing name.  */
  if (setenv(name, value, 1) == -1) os_error("setenv");
#else
  /* This fallback should no longer be needed, since POSIX mandated
   * that setenv() be required, but we won't be hard-line about that.
   * Note that the space allocated for the "name=value" string is
   * never released. */
  char *c = (char *) os_malloc(strlen(name) + 1 + strlen(value) + 1);
  strcpy(c,name);
  strcat(c,"=");
  strcat(c,value);
  os_putenv(c);
#endif
  errno = saved_errno;
}

void os_unsetenv(const char *name) {
  int saved_errno = errno;
#if HAVE_UNSETENV
  /* unsetenv() may (and probably does) release the space for the name
   * and associated value when the name is found in the environment.  */
  if (unsetenv(name) == -1) os_error("unsetenv");
#else
  /* This undocumented way of unsetting an environment variable when
   * the (non-Berkeley, non-POSIX) system doesn't define unsetenv()
   * _empirically_ works:  with no "=..." part, it deletes the entry.
   * As in the corresponding fallback case for os_setenv() above,
   * note the permanence of the system space allocation. */
  os_putenv (strdup_malloc(name));
#endif
  errno = saved_errno;
}

/* Caller beware - many implementations of putenv() do not make a copy
 * of the string you pass, only of the pointer.  */
void os_putenv(char *s) {
  int saved_errno = errno;
  if (putenv(s) == -1) os_error("putenv");
  errno = saved_errno;
}

mode_t os_get_umask(void) {
  /* POSIX doesn't define any errors for umask(); we defensively
   * shield ourselves from implementations that set errno.  */
  int saved_errno = errno;
  /* To get the mask, you have to set it.  So we set it to 0 and then
   * restore it.  */
  mode_t r = umask(0);
  umask(r);
  errno = saved_errno;
  return r;
}

mode_t os_set_umask(mode_t mask) {
  int saved_errno = errno;
  mode_t r = umask(mask);
  errno = saved_errno;
  return r;
}

void os_getcwd(char *path, int maxlen) {
  int saved_errno = errno;
  /* POSIX says a cancellation point may occur in getcwd:  */
  while (getcwd(path, maxlen) != path) {
    if (errno != EINTR) os_error("getcwd");  /* non-EINTR failure */
    /* (On EINTR, retry the getcwd.)  */
  }
  errno = saved_errno;  /* success */
}

/*
 *  Not every function in POSIX that takes a "path" argument is
 *  listed as a possible cancellation point, but given the hazards
 *  of path resolution across a wide range of Unix flavours and
 *  filesystem types (including networked filesystems), and the
 *  possibility of such errors as EACCES, EIO, and ELOOP, perhaps
 *  most of them should be.  In an abundance of caution, we are
 *  treating EINTR as a possibility in such cases, and marking
 *  those as "unofficial" cancellation points.
 */

void os_chdir(const char *path) {
  int saved_errno = errno;
  /* Unofficial cancellation point, not listed by POSIX:  */
  while (chdir(path) == -1) {
    if (errno != EINTR) return;  /* non-EINTR failure */
    /* (On EINTR, retry the chdir.)  */
  }
  errno = saved_errno;  /* success */
}

int os_glob(const char *pattern, int flags,
             int (*errfunc) (const char *epath, int eerrno),
              glob_t *pglob) {
  int saved_errno = errno;
  errno = 0;  /* because glob() may return non-0 without setting errno */
  /* POSIX says a cancellation point may occur in glob:  */
  while (true) {
    int r = glob (pattern, flags, errfunc, pglob);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    if (errno != EINTR) return r;  /* non-EINTR failure */
    /* (On EINTR, retry the glob.)  */
  }
}

void os_globfree(glob_t *pglob) {
  int saved_errno = errno;
  globfree (pglob);
  errno = saved_errno;
}

int os_stat(const char *path, struct stat *buf) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in stat:  */
    int r = stat (path, buf);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the stat.)  */
  }
}

int os_fstat(int fd, struct stat *buf) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in fstat:  */
    int r = fstat (fd, buf);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the fstat.)  */
  }
}

int os_lstat(const char *path, struct stat *buf) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in lstat:  */
    int r = lstat (path, buf);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the lstat.)  */
  }
}

int os_link(const char *oldpath, const char *newpath) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in link:  */
    int r = link (oldpath, newpath);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the link.)  */
  }
}

int os_symlink(const char *oldpath, const char *newpath) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in symlink:  */
    int r = symlink (oldpath, newpath);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the symlink.)  */
  }
}

int os_rename(const char *oldpath, const char *newpath) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in rename:  */
    int r = rename (oldpath, newpath);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the rename.)  */
  }
}

ssize_t os_readlink(const char *path, char *buf, size_t bufsiz) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    ssize_t r = readlink (path, buf, bufsiz);
    if (r >= 0) {
      errno = saved_errno;
      return r;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the readlink.)  */
  }
}

int os_mkdir(const char *path, mode_t mode) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = mkdir (path, mode);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the mkdir.)  */
  }
}

int os_mkfifo(const char *path, mode_t mode) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = mkfifo (path, mode);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the mkfifo.)  */
  }
}

int os_mknod(const char *path, mode_t mode, dev_t dev) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = mknod (path, mode, dev);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the mknod.)  */
  }
}

int os_chmod(const char *path, mode_t mode) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = chmod (path, mode);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the chmod.)  */
  }
}

int os_chown(const char *path, uid_t owner, gid_t group) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = chown (path, owner, group);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the chown.)  */
  }
}

int os_unlink(const char *path) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in unlink:  */
    int r = unlink (path);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the unlink.)  */
  }
}

int os_rmdir(const char *path) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = rmdir (path);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the rmdir.)  */
  }
}

int os_remove(const char *path) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = remove (path);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the remove.)  */
  }
}

int os_truncate(const char *path, off_t length) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = truncate (path, length);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the truncate.)  */
  }
}

int os_ftruncate(int fd, off_t length) {
  int saved_errno = errno;
  while (true) {
    /* Unofficial cancellation point, not listed by POSIX:  */
    int r = ftruncate (fd, length);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the ftruncate.)  */
  }
}

/*
 *  You might think the following overkill, but the possibility
 *  of EINTR can be demonstrated even in such unlikely contexts as
 *  printing error messages on SETL program crashes (runerr() calls),
 *  and that can cause missed output if the stdio FILE-oriented output
 *  call isn't retried.  Note that POSIX says fputc() returns EOF on
 *  failure (with no data transferred); and printf() etc. return an
 *  unspecified negative value when there has been an output error.
 */

int os_fputc(int c, FILE *fp) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in fputc:  */
    int r = fputc(c, fp);
    if (r == (uchar)c) {
      errno = saved_errno;
      return r;
    }
    assert (r == EOF);
    if (errno != EINTR) return EOF;  /* non-EINTR failure */
    /* (On EINTR, retry the fputc.)  */
  }
}

int os_printf(const char *fmt, ...) {
  int r;
  va_list ap;
  va_start(ap, fmt);
  r = os_vprintf(fmt, ap);
  va_end(ap);
  return r;
}

int os_fprintf(FILE *fp, const char *fmt, ...) {
  int r;
  va_list ap;
  va_start(ap, fmt);
  r = os_vfprintf(fp, fmt, ap);
  va_end(ap);
  return r;
}

int os_vprintf(const char *fmt, va_list ap) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in vprintf:  */
    int r;
    va_list ap2;
    va_copy(ap2, ap);
    r = vprintf(fmt, ap2);
    va_end(ap2);
    if (r >= 0) {
      errno = saved_errno;
      return r;
    }
    /* r < 0 */
    if (errno != EINTR) return r;  /* non-EINTR failure */
    /* (On EINTR, retry the vprintf().)  */
  }
}

int os_vfprintf(FILE *fp, const char *fmt, va_list ap) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in vfprintf:  */
    int r;
    va_list ap2;
    va_copy(ap2, ap);
    r = vfprintf(fp, fmt, ap2);
    va_end(ap2);
    if (r >= 0) {
      errno = saved_errno;
      return r;
    }
    /* r < 0 */
    if (errno != EINTR) return r;  /* non-EINTR failure */
    /* (On EINTR, retry the vfprintf().)  */
  }
}

#undef POSIX_OPEN_MAX
#ifdef _POSIX_OPEN_MAX
#define POSIX_OPEN_MAX _POSIX_OPEN_MAX
#else
#define POSIX_OPEN_MAX 20
#endif
static void set_max_open_files(void) {
  int saved_errno = errno;
#if defined _SC_OPEN_MAX && HAVE_SYSCONF && HAVE_DECL_SYSCONF
  int nfd = sysconf(_SC_OPEN_MAX);
  if (nfd < POSIX_OPEN_MAX) {
    os_error("sysconf");  /* hopefully errno was set by sysconf() */
  }
#elif defined RLIMIT_NOFILE && HAVE_GETRLIMIT && HAVE_DECL_GETRLIMIT
  int nfd;
  struct rlimit fd_limits;
  if (getrlimit(RLIMIT_NOFILE, &fd_limits) != 0) {
    os_error("getrlimit");
  }
  nfd = fd_limits.rlim_cur;
#elif HAVE_GETDTABLESIZE && HAVE_DECL_GETDTABLESIZE
  int nfd = getdtablesize();
  if (nfd < POSIX_OPEN_MAX) {
    os_error("getdtablesize");  /* hopefully getdtablesize() set errno */
  }
#elif defined OPEN_MAX
  int nfd = OPEN_MAX;
#else
  int nfd = POSIX_OPEN_MAX;  /* as #defined above */
#endif
  max_open_files = nfd;
  errno = saved_errno;
}

int os_max_open_files(void) {
  /*
   *  If setrlimit() is ever used in the future to change the limit,
   *  this number needs to be reset.  (And a policy would be needed for
   *  existing fds past the new limit.  Probably never shrinking 'files'
   *  would be good.  You'd have to distinguish its size from the
   *  current max_open_files then.)
   */
  if (max_open_files == 0) {
    set_max_open_files();
  }
  return max_open_files;
}

void os_check_fd(int fd) {
  int nfd = os_max_open_files();
  if (0 <= fd && fd < nfd) return;  /* ok */
  /*
   *  This could happen if a setrlimit() call was introduced
   *  somewhere that raised the fd limit without "notifying"
   *  os_max_open_files() (say by zeroing max_open_files):
   */
  panic("fd %d is outside range 0 <= fd < %d",
            fd,                          nfd);
}

int os_open(const char *path, int oflag) {
  int saved_errno = errno;
  while (true) {
    /* O_NOCTTY_COND is O_NOCTTY if the TIOCSCTTY ioctl appears to be
     * available (and thus the SETL primitive SETCTTY is functional).
     * Otherwise, it is 0 so that the SETL program still has at least
     * an old-fashioned way of acquiring a controlling terminal.  */
    int fd = open(path, oflag | O_NOCTTY_COND, 0666);  /* cancellation point */
    if (fd != -1) {
      os_check_fd(fd);
      errno = saved_errno;
      return fd;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the open.)  */
  }
}

#if !HAVE_SOCKETPAIR || (defined __QNXNTO__ && _NTO_VERSION < 630)
#warning Using inet_socketpair in place of socketpair.
/*
 *  This is carefully hacked from a code sample in a thread in the
 *  qdn.public.porting newsgroup entitled "socketpair problem".
 *  The original code tried for port 2345 and then worked its way up
 *  until it got a port that was not in use.  My modification asks
 *  for port 0 which instructs the system to choose a port number.
 *
 *  The use of perror() and direct fprintf to stderr is regrettable,
 *  but this is meant as a temporary measure until QNX grows up a bit.
 *  (Which it now has done.  QNX 6.3.0 is ancient history.  So this old
 *  bending-over compatibility code is probably utterly useless now.)
 *
 *  There is a theoretical possibility that this function could fail
 *  on an EINTR during the calls that could block, but that is
 *  extremely unlikely given the use of the loopback interface.
 *  Could be fixed by using os_*() versions of those calls if you care.
 */
static int
inet_socketpair (d, type, protocol, sv)  /* pre-ANSI! */
 int d, type, protocol;
 int sv[2];
{
    int saved_errno = errno;
    struct sockaddr_in addr1, addr2;
    int addr1_len = sizeof addr1;
    int fd;
    uint16_t port_no = 0;  /* have system assign port# */
    if (d != AF_INET || type != SOCK_STREAM || protocol)
    {
        fprintf (stderr, "** bad param in inet_socketpair.\n");
        return -1;
    }
    if ((sv [0] = socket (d, SOCK_STREAM, 0)) < 0
        || (sv [1] = socket (d, SOCK_STREAM, 0)) < 0)
    {
        perror ("** cannot create sockets; reason");
        if (sv[0] >= 0) close (sv[0]);
        if (sv[1] >= 0) close (sv[1]);
        return -1;
    }
    addr1.sin_port = htons (port_no);
    addr1.sin_family = d;
    addr1.sin_addr.s_addr = htonl (INADDR_LOOPBACK);
    if (bind (sv[0], (struct sockaddr *) &addr1, sizeof addr1) < 0)
    {
        perror ("** cannot bind; reason");
        close (sv[0]);
        close (sv[1]);
        return -1;
    }

    if (getsockname (sv[0], (struct sockaddr *) &addr1, &addr1_len) < 0)
    {
        perror ("** cannot getsockname; reason");
        close (sv[0]);
        close (sv[1]);
        return -1;
    }
    port_no = ntohs(addr1.sin_port);  /* assigned port# */

    if (listen (sv[0], 1) < 0)
    {
        perror ("** cannot listen; reason");
        close (sv[0]);
        close (sv[1]);
        return -1;
    }
    addr2.sin_port = htons (port_no);
    addr2.sin_family = d;
    addr2.sin_addr.s_addr = htonl (INADDR_LOOPBACK);
    if (connect (sv[1], (struct sockaddr *) &addr2, sizeof addr2) < 0)
    {
        perror ("** cannot connect; reason");
        close (sv[0]);
        close (sv[1]);
        return -1;
    }

    if ((fd = accept (sv[0], NULL, NULL)) < 0)
    {
        perror ("** cannot accept; reason");
        close (sv[0]);
        close (sv[1]);
        return -1;
    }
    if (close (sv[0]) < 0)
    {
        perror ("** cannot close; reason");
        close (sv[1]);
        close (fd);
        return -1;
    }
    sv[0] = fd;
    errno = saved_errno;
    return 0;
}
#endif

/*
 *  This will attempt to get you a full-duplex pipe, and should succeed
 *  for anything >= 4.2 in the BSD world or >= SVR4 in the other one.
 *  We will #warn if we have to fall back to a pipe() call, in which
 *  case you should assume p[0] is read-only and p[1] is write-only.
 */
int os_pipe(int p[2]) {
  int saved_errno = errno;
#if HAVE_SOCKETPAIR && (!defined __QNXNTO__ || _NTO_VERSION >= 630)
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, p) == -1) return -1;
#elif defined __QNXNTO__ && _NTO_VERSION < 630
  /*
   * QNX 6.1 lacks Unix-domain sockets.  See also the "#else" case
   * below; it seems unlikely that we'll be able to pass file
   * descriptors across these Internet-domain sockets that I am
   * using as a substitute, but you never know...
   */
  if (inet_socketpair(AF_INET, SOCK_STREAM, 0, p) == -1) return -1;
#else
  /*
   *  Now that I want to be able to use these things for passing
   *  file descriptors, I have the additional requirement (beyond
   *  full-duplexness) that they be true Unix-domain sockets.  If it
   *  turns out that they are not, on some systems of current interest,
   *  the following pipe() call should be replaced by some spam to
   *  make a couple of /dev/spx clones and hook them together, as in
   *  Stevens (1990), p. 383.
   *
   *  More on that:  in the "portability.h" header of NCSA httpd 1.5.1,
   *  the only example of a system that caused NEED_SPIPE to be #defined
   *  (indicating the need for exactly the above /dev/spx clone pair)
   *  was SCO3, and this was only after SCO had been checked for.
   *  If SCO3 is not in fact obsolete, perhaps someone would like to
   *  conditionally include the definition of s_pipe() from http_ipc.c
   *  and use it here for that special case:
   */
# warning no socketpair() found, substituting pipe()
  if (pipe(p) == -1) return -1;
#endif
  os_check_fd(p[0]);
  os_check_fd(p[1]);
  errno = saved_errno;
  return 0;
}

int os_dup(int fd) {
  int saved_errno = errno;
  int rfd = dup(fd);
  if (rfd != -1) {
    os_check_fd(rfd);
    errno = saved_errno;
  }
  return rfd;
}

int os_dup2(int fd1, int fd2) {
  int saved_errno = errno;
  /*
   *  Although POSIX does not list dup2() as a possible cancellation
   *  point, it does show EINTR among the possible error returns (as
   *  does the Linux man page, for example).
   */
  while (true) {
    int rfd = dup2(fd1,fd2);
    if (rfd != -1) {
      os_check_fd(rfd);
      assert (rfd == fd2);
      errno = saved_errno;
      return rfd;
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the dup2.)  */
  }
}

int os_mkstemp(char *templat) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in mkstemp:  */
    int fd = mkstemp(templat);
    if (fd != -1) {
      os_check_fd(fd);
      errno = saved_errno;
      return fd;
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the mkstemp.)  */
  }
}

const char *os_tmpnam(void) {
  int saved_errno = errno;
  while (true) {
    /* TMPNAM is deprecated and obsolescent, but still supported
     * for the time being, as MKSTEMP was a later arrival.  */
    const char *r = tmpnam(NULL);  /* sorry for linker warning if any */
    if (r != NULL) {
      errno = saved_errno;
      return r;
    }
    if (errno != EINTR) return NULL;  /* non-EINTR failure */
    /* (On EINTR, retry the tmpnam.)  */
  }
}

int os_fcntl(int fd, int cmd, int arg) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in fcntl:  */
    int r = fcntl(fd, cmd, arg);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) os_error("fcntl");  /* non-EINTR failure */
    /* (On EINTR, retry the fcntl.)  */
  }
}

/* When is an int a fd?  When fcntl(fd, F_GETFD) says so.
 * Synonymous with "being open at the POSIX level".  */
bool os_is_fd(int fd) {
  /* Don't take fcntl(fd, F_GETFD) to be a cancellation point. */
  /* Do leave errno set if fd isn't a fd.  */
  int saved_errno = errno;
  bool r = (fcntl(fd, F_GETFD) != -1);
  if (r) errno = saved_errno;
  return r;
}

int os_ioctl_int(int fd, int cmd, int arg, const char *what) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in ioctl:  */
    int r = ioctl(fd, cmd, arg);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) os_error(what);  /* non-EINTR failure */
    /* (On EINTR, retry the ioctl.)  */
  }
}

int os_ioctl_addr(int fd, int cmd, void *arg, const char *what) {
  int saved_errno = errno;
  while (true) {
    /* POSIX says a cancellation point may occur in ioctl:  */
    int r = ioctl(fd, cmd, arg);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) os_error(what);  /* non-EINTR failure */
    /* (On EINTR, retry the ioctl.)  */
  }
}

/*
 *  In os_close(), EINTR does not occasion a retry of the underlying
 *  system function as it does with practically everything else that
 *  is suspected of being able to block, because POSIX says close()
 *  leaves the fd in an unspecified state for EINTR.
 *
 *  A previous implementation of os_close() did do the retry thing,
 *  however, and that implementation also insisted that any failure
 *  of close() other than EINTR was erroneous.
 *
 *  Unfortunately, POSIX also describes a plausible way to get EIO
 *  from close() without its being a program error.
 *
 *  So os_close() now gives a bool saying whether close() succeeded,
 *  except that per the paranoid convention that appears to reign
 *  in os.c, errno is restored on a successful return from close(),
 *  not just left alone in the faith that close() didn't touch it.
 *
 *  The SETL programmer who notices LAST_ERROR being set to other than
 *  NO_ERROR after a CLOSE call, and wants to make sure the fd has
 *  really been retired, can use IS_OPEN and/or retry the CLOSE in the
 *  hope of an EBADF ("Bad file descriptor").
 */
bool os_close(int fd) {
  int saved_errno = errno;
  int rc = close(fd);
  if (rc != -1) {
    assert (rc == 0);
    errno = saved_errno;
  }
  return rc == 0;
}

void os_shutdown(int fd, int how) {
  int saved_errno = errno;
  /* POSIX does not admit shutdown() as a cancellation point, but I
   * don't trust all Unixes not to block while doing some low-level
   * network flushing (say) in a context like this.  */
  while (shutdown(fd, how) == -1) {
    if (errno != EINTR) return;  /* non-EINTR failure */
    /* (On EINTR, retry the shutdown.)  */
  }
  errno = saved_errno;  /* success */
}

off_t os_seek(int fd, off_t offset, int whence) {
  int saved_errno = errno;
  while (true) {
    off_t r = lseek(fd, offset, whence);
    if (r != (off_t)-1) {
      errno = saved_errno;
      return r;  /* success */
    }
    /* This error response may seem harsh, but I think it is OK to
     * take the only realistically possible errors (attempted offsets
     * out of range 0 to OFF_T_MAX) to be programming errors.  */
    if (errno != EINTR) os_error("lseek");  /* non-EINTR failure */
    /* (On EINTR, retry the lseek.)  */
  }
}

ssize_t os_read(int fd, void *buf, size_t nbytes) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = read(fd, buf, nbytes);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success or EOF */
    }
    if (errno != EINTR) return -1;  /* non-EINTR error */
    /* (On EINTR, retry the read.)  */
  }
}

ssize_t os_recv(int fd, void *buf, size_t nbytes, int flags) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = recv(fd, buf, nbytes, flags);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success or EOF */
    }
    if (errno != EINTR) return -1;  /* non-EINTR error */
    /* (On EINTR, retry the recv.)  */
  }
}

ssize_t os_recvfrom(int fd, void *buf, size_t nbytes, int flags,
                     struct sockaddr *from, socklen_t *fromlen) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = recvfrom(fd, buf, nbytes, flags, from, fromlen);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success or EOF */
    }
    if (errno != EINTR) return -1;  /* non-EINTR error */
    /* (On EINTR, retry the recvfrom.)  */
  }
}

ssize_t os_recvmsg(int fd, struct msghdr *msg, int flags) {
#if HAVE_RECVMSG
  int saved_errno = errno;
  while (true) {
    ssize_t r = recvmsg(fd, msg, flags);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success or "EOF" */
    }
    if (errno != EINTR) return -1;  /* non-EINTR error */
    /* (On EINTR, retry the recvmsg.)  */
  }
#else
  errno = ENOSYS;
  return -1;
#endif
}

#define CHECK_BYTE 28
#define MATE_BYTE 101

#define USE_ANCILLARY_DATA  \
 ( !HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS && HAVE_STRUCT_CMSGHDR_CMSG_LEN )

static void init_msg (struct msghdr *msg,
                      struct iovec iov[1],
                      char buf[1]) {
  iov[0].iov_base   = buf;
  iov[0].iov_len    = 1;
  memset(msg, 0, sizeof *msg);
  msg->msg_iov      = iov;
  msg->msg_iovlen   = 1;
  msg->msg_name     = (void *) NULL;
  msg->msg_namelen  = 0;
}

int os_recv_fd(int fd) {
#if HAVE_RECVMSG
  int received_fd = -1;
  struct iovec iov[1];
  struct msghdr msg;
  ssize_t r;  /* return value from recvmsg() */
  char buf[1];
# if USE_ANCILLARY_DATA
  union {
    struct cmsghdr hdr;
    char cmsg[CMSG_SPACE(sizeof received_fd)] ALIGNMENT(ALIGNOF_INT);
  } cmsg_storage;
  struct cmsghdr *p_cmsg = &cmsg_storage.hdr;
  int *pfd = (int *)CMSG_DATA(p_cmsg);  /* ptr to fd */
# endif
  init_msg (&msg, iov, buf);
# if USE_ANCILLARY_DATA
  msg.msg_control      = p_cmsg;  /* AIX 4.x may give ptr warning */
                        /* Likewise Apple (Darwin 6.8 ppc, gcc 3.1) */
  msg.msg_controllen   = sizeof cmsg_storage;
                        /* was CMSG_LEN(sizeof received_fd) */
  msg.msg_flags        = 0;
# else
  msg.msg_accrights    = (caddr_t) &received_fd;
  msg.msg_accrightslen = sizeof received_fd;
# endif
  r = os_recvmsg (fd, &msg, 0);
  switch (r) {
  case 1:
    if (buf[0] != CHECK_BYTE) {
      /* Incorrect check byte received.  This is probably a
       * programming error but since that cannot be known for sure,
       * treat it the same as an EOF (case 0 below, no check byte
       * received), returning -1 without setting errno.  */
      return -1;
    }
# if USE_ANCILLARY_DATA
    /* Empirically, Cygwin appears to set msg.msg_controllen to 0 to
     * indicate it doesn't support fd passing.  (The Cygwin User Guide
     * as of 2021 also says it isn't supported, but does not elaborate.)
     * So we look for such a condition and print a friendlier diagnostic
     * than a recvmsg "panic".  */
    if (msg.msg_controllen <= sizeof *p_cmsg) {  /* nowt beyond header */
      runerr("File descriptor passing not supported on this system");
    }
    /* The panic() response to failed checks on p_cmsg fields below is
     * technically too strict (which is an opp for DoS if the peer is
     * hostile), but we _really_ want to know if they ever happen.  */
    {
      /* Linux (as of 2016) types cmsg_len as size_t (POSIX requires
       * socklen_t).  They are convertible, and size_t is always big;
       * this intermediary lets us avoid antagonizing the printf format
       * checker for panic():  */
      const size_t cmsg_len = p_cmsg->cmsg_len;
      if (cmsg_len != CMSG_LEN(sizeof received_fd)) {
        panic("recvmsg: expected %zu ancillary bytes but got %zu",
         CMSG_LEN(sizeof received_fd),                  cmsg_len);
      }
    }
    if (p_cmsg->cmsg_level != SOL_SOCKET) {
      panic("recvmsg: expected level SOL_SOCKET but got 0x%x",
                                          p_cmsg->cmsg_level);
    }
    if (p_cmsg->cmsg_type != SCM_RIGHTS) {
      panic("recvmsg: expected type SCM_RIGHTS but got 0x%x",
                                          p_cmsg->cmsg_type);
    }
    received_fd = *pfd;
# endif
    os_check_fd (received_fd);
    buf[0] = MATE_BYTE;
    init_msg (&msg, iov, buf);
    {
      /* Tell the sender we received the fd.  Don't care whether that
       * ack got through, and don't let sendmsg() alter errno.  */
      int saved_errno = errno;
      os_sendmsg (fd, &msg, 0);
      errno = saved_errno;
    }
    return received_fd;
  case 0:
    /* No check byte received.  Sender apparently did an "orderly
     * shutdown" before the expected SEND_FD.  */
    return -1;  /* not a valid fd; errno probably not set */
  case -1:
    return -1;  /* let caller handle it; errno has been set */
  default:
    panic("recvmsg unexpectedly returned %zd", r);
  }
#elif HAVE_STROPTS_H
  struct strrecvfd r;
  os_ioctl_addr(fd, I_RECVFD, &r, "ioctl(I_RECVFD)");
  os_check_fd(r.fd);
  return r.fd;
#else
  runerr("Sorry, RECV_FD not implemented on this system");
#endif
} /* end os_recv_fd */

ssize_t os_write(int fd, const void *buf, size_t nbytes) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = write(fd, buf, nbytes);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the write.)  */
  }
}

ssize_t os_send(int fd, const void *buf, size_t nbytes, int flags) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = send(fd, buf, nbytes, flags);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the send.)  */
  }
}

ssize_t os_sendto(int fd, const void *buf, size_t nbytes, int flags,
              const struct sockaddr *to, socklen_t tolen) {
  int saved_errno = errno;
  while (true) {
    ssize_t r = sendto(fd, buf, nbytes, flags, to, tolen);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the sendto.)  */
  }
}

ssize_t os_sendmsg(int fd, const struct msghdr *msg, int flags) {
#if HAVE_SENDMSG
  int saved_errno = errno;
  while (true) {
    ssize_t r = sendmsg(fd, msg, flags);
    if (r != -1) {
      errno = saved_errno;
      return r;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR error */
    /* (On EINTR, retry the sendmsg.)  */
  }
#else
  errno = ENOSYS;
  return -1;
#endif
}

int os_send_fd(int fd, int fd_to_send) {
#if HAVE_SENDMSG
  struct iovec iov[1];
  struct msghdr msg;
  ssize_t r;  /* return value from sendmsg() */
  char buf[1];
# if USE_ANCILLARY_DATA
  union {
    struct cmsghdr hdr;
    char cmsg[CMSG_SPACE(sizeof fd_to_send)] ALIGNMENT(ALIGNOF_INT);
  } cmsg_storage;
  struct cmsghdr *p_cmsg = &cmsg_storage.hdr;
  int *pfd = (int *)CMSG_DATA(p_cmsg);  /* ptr to fd */
# endif
  buf[0] = CHECK_BYTE;
  init_msg (&msg, iov, buf);
# if USE_ANCILLARY_DATA
  p_cmsg->cmsg_len   = CMSG_LEN(sizeof fd_to_send);
  p_cmsg->cmsg_level = SOL_SOCKET;
  p_cmsg->cmsg_type  = SCM_RIGHTS;
  *pfd               = fd_to_send;
  msg.msg_control    = p_cmsg;  /* AIX 4.x may give ptr warning */
                      /* Likewise Apple (Darwin 6.8 ppc, gcc 3.1) */
  msg.msg_controllen = p_cmsg->cmsg_len;
  msg.msg_flags      = 0;
# else
  msg.msg_accrights    = (caddr_t) &fd_to_send;
  msg.msg_accrightslen = sizeof fd_to_send;
# endif
  r = os_sendmsg (fd, &msg, 0);
  switch (r) {
  case 1:
    init_msg (&msg, iov, buf);
    {
      int saved_errno = errno;
      r = os_recvmsg (fd, &msg, 0);
      switch (r) {
      case 1:
        if (buf[0] != MATE_BYTE) {
          panic("recvmsg: expected ack byte 0x%x but got 0x%x",
                                       MATE_BYTE,      buf[0]);
        }
        return 0;  /* success */
      case 0:
        /* Receiver terminated connection before sending back ack.
         * Nevertheless, we did apparently succeed in sending the fd,
         * and the important thing is that the recvmsg() unblocked,
         * letting us proceed.  */
        return 0;  /* success or close enough */
      case -1:
        /* This too comes after success at sending the fd, but here
         * we also need to undo the setting of errno by recvmsg().  */
        errno = saved_errno;
        return 0;  /* success, just barely :) */
      default:
        panic("recvmsg unexpectedly returned %zd", r);
      }
    }
  case 0:
    /* This case is probably a fiction, as a failure to send 1 byte
     * is more likely to be seen as an error (with errno giving the
     * fine discrimination) than as evidence of an orderly,
     * anticipated shutdown by the receiver.  */
    return -1;  /* failure, but with errno probably not set */
  case -1:
    return -1;  /* let caller handle it; errno has been set */
  default:
    panic("sendmsg unexpectedly returned %zd", r);
  }
#elif HAVE_STROPTS_H
  os_ioctl_int(fd, I_SENDFD, fd_to_send, "ioctl(I_SENDFD)");
  return 0;  /* success */
#else
  runerr("Sorry, SEND_FD not implemented on this system");
#endif
} /* end os_send_fd */

/*
 *  The os_pselect() function differs from pselect(2) in its
 *  response to signals coming in before any fd goes ready or the
 *  timer expires.  It returns as if the timer had expired (even
 *  if no timer was asked for!), which is to say it returns 0,
 *  the number of ready file descriptors.
 *
 *  The rationale for this is to help the high-level SELECT support
 *  an I/O interface to signals.  They are to appear to the SETL
 *  programmer as input, which means the implementation of the
 *  high-level SELECT must be able to regain control when signals
 *  occur during a blocked select(), so it can cough up pseudo-fds.
 */
int os_pselect(int nfds,
               fd_set *readfds,
               fd_set *writefds,
               fd_set *exceptfds,
               const struct timespec *timeout,
               const sigset_t *sigmask) {
  int saved_errno = errno;
#if HAVE_PSELECT && HAVE_DECL_PSELECT
  int r = pselect(nfds, readfds,
                        writefds,
                        exceptfds, timeout, sigmask);
  if (r != -1) {
    errno = saved_errno;
    return r;  /* success */
  }
  if (errno != EINTR) os_error("pselect");  /* non-EINTR failure */
#else  /* missing pselect() or its declaration */
  #warning *** Using race-prone backup version of pselect() ***
  /*
   * This fake ersatz substitute charlatan stand-in for pselect()
   * suffers from the very race condition pselect() was intended to
   * prevent.  But in that respect it is no worse than the pselect()
   * that had exactly the same defect in some span of glibc versions
   * in days gone by.  The SETL programmer on such a horrid old OS
   * (or on QNX through at least version 6.5.0) can work around the
   * defect by opening a timer stream with some suitable period.
   * It is not even necessary to read the events from that stream:
   * the stream's very existence causes the signals to be generated at
   * at least that frequency, and any signal received when select() is
   * blocked will cause it to wake up with an EINTR, which is mapped
   * to our returning as if a timeout had occurred even if there is
   * no timeout arg.  That is reflected at the SETL level as no fds
   * going ready, a spurious case resembling a timeout.  But if the
   * natural thing after that is to do nothing but then check for the
   * signal again (another SELECT), everything comes out OK except for
   * that signal delivery delay of up to the artificial timer stream
   * period that was suffered during the blockage.  Better you should
   * make decent arrangements for dealing with the chance of missed
   * signals, which POSIX allows when they aren't queuing ("real-time")
   * signals anyway.  A cleverer implementation of timers might choose
   * to block SIGALRM whenever all timer streams have unread events,
   * and then use POSIX timer_getoverrun() to figure out the number of
   * events when a read on one of those streams eventually is issued
   * by the SETL program.  But we are getting ahead of ourselves.
   * Here in all its deplorable scabrous squalour is the advertised
   * pselect() imposter:
   */
  int select_errno;
  int r;
  struct timeval tv, *tvp;
  sigset_t oldmask;
  if (timeout == NULL) {
    tvp = NULL;
  } else {
    tv = timespec_to_timeval(*timeout);
    tvp = &tv;
  }
  sigprocmask(SIG_SETMASK, sigmask, &oldmask);
  r = select(nfds, readfds, writefds, exceptfds, tvp);
  select_errno = errno;
  sigprocmask(SIG_SETMASK, &oldmask, NULL);
  if (r != -1) {
    errno = saved_errno;
    return r;  /* success */
  }
  if (select_errno != EINTR) os_error("select");  /* non-EINTR failure */
#endif /* !HAVE_PSELECT */
  errno = saved_errno;
  return 0;  /* EINTR is like the empty set of fds going ready */
}

int os_accept(int fd, struct sockaddr *sa, socklen_t *salen) {
  int saved_errno = errno;
  while (true) {
    int gd = accept(fd, sa, salen);
    if (gd != -1) {
      os_check_fd(gd);
      errno = saved_errno;
      return gd;  /* success */
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the accept.)  */
  }
}

int os_bind(int fd, const struct sockaddr *sa, socklen_t salen) {
  int saved_errno = errno;
  /* Not a POSIX cancellation point, but the Linux bind(2) page
   * distinguishes an O_NONBLOCK case, suggesting the possibility of
   * blocking in practice.  So we do the usual retry on EINTR here.  */
  while (true) {
    int r = bind(fd, sa, salen);
    if (r == 0) {
      errno = saved_errno;
      return 0;
    }
    assert (r == -1);
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the bind.)  */
  }
}

int os_connect(int fd, const struct sockaddr *sa, socklen_t salen) {
  int saved_errno = errno;
  /*
   *  connect() isn't quite like other blocking system calls that
   *  may return EINTR.  POSIX specifies that for connect(), you
   *  can't just retry the call, but must wait for the connection
   *  to be completed by watching for the fd to go writable.
   */
  int r = connect(fd, sa, salen);
  if (r == -1 && errno == EINTR) {
    fd_bitset w;
    new_fd_bitset(&w, fd+1);
    while (r == -1 && errno == EINTR) {
      zero_fd_bitset(&w, fd+1);
      set_fd_bit(&w, fd, fd+1);
      r = select(fd+1, NULL, fd_set_ptr(&w), NULL, NULL);
    }
    free_fd_bitset(&w, fd+1);
  }
  if (r >= 0) {  /* from connect() or select() */
    r = 0;  /* success */
    errno = saved_errno;
  } else {
    /* errno remains as set by connect() or select() */
  }
  return r;
}

int os_listen(int fd, int backlog) {
  int saved_errno = errno;
  int r = listen(fd, backlog);
  if (r != -1) {
    assert (r == 0);
    errno = saved_errno;
  }
  return r;
}

int os_socket(int family, int type, int protocol) {
  int saved_errno = errno;
  int fd = socket(family, type, protocol);
  if (fd != -1) {
    os_check_fd(fd);
    errno = saved_errno;
  }
  return fd;
}

void os_setsockopt(int fd, int level, int optname,
                    const void *optval, socklen_t optlen) {
  int saved_errno = errno;
  if (setsockopt(fd, level, optname, optval, optlen) == -1)
                                        os_error("setsockopt");
  errno = saved_errno;
}

int os_getsocktype(int fd) {
  int saved_errno = errno;
  int socktype;
  socklen_t length = sizeof socktype;
  if (getsockopt(fd, SOL_SOCKET, SO_TYPE, &socktype, &length) == -1)
                                        os_error("getsockopt");
  errno = saved_errno;
  return socktype;
}

bool os_getsockname(int fd, struct sockaddr *sa, socklen_t *salen) {
  int saved_errno = errno;
  int rc = getsockname(fd, sa, salen);
  if (rc != -1) {
    assert (rc == 0);
    errno = saved_errno;
  }
  return rc == 0;
}

bool os_getpeername(int fd, struct sockaddr *sa, socklen_t *salen) {
  int saved_errno = errno;
  int rc = getpeername(fd, sa, salen);
  if (rc != -1) {
    assert (rc == 0);
    errno = saved_errno;
  }
  return rc == 0;
}

/*
 *  This gethostname() wrapper ensures that the result (a C string
 *  name) is initialized to the empty string before the gethostname()
 *  call and has a terminating nul after.
 *
 *  POSIX defines no errors for gethostname(); those defined on Linux
 *  are of no more than theoretical interest, but just to be sure, we
 *  hide even those.
 */
int os_gethostname(char *name, size_t len) {
  int saved_errno;
  assert (len > 0);
  saved_errno = errno;
  name[0] = '\0';  /* init hostname to empty C string */
  while (true) {
    /* POSIX allows gethostname() to be a cancellation point.  */
    int r = gethostname(name, len);
    name[len-1] = '\0';  /* POSIX allows fill without \0-termination */
    if (r == 0 || errno != EINTR) {
      errno = saved_errno;
      return 0;
    }
    /* (On EINTR, retry the gethostname.)  */
  }
}

/*
 *  The assumption that the POSIX-defined external variable 'errno'
 *  can be assigned values outside of its defined range is not strictly
 *  legitimate, but is believed to be safe on all known POSIX impls;
 *  it is defined as an lvalue to which int values may be assigned,
 *  and the E... constants #defined by <errno.h> are required to be
 *  positive, so that all helps, but shoehorning the EAI_... codes in
 *  this way as negative errno values (rather than doing a fussy little
 *  struct or something) is still a hack.
 */
static void gai_errno(int r) {  /* set errno to r or -r if appropriate */
  if (r != EAI_SYSTEM) {
#if EAI_FAIL < 0
    assert (r < 0);  /* assume all EAI_... error codes < 0 */
    errno = r;  /* give errno a negative value */
#else
    assert (r > 0);  /* assume all EAI_... error codes > 0 */
    errno = -r;  /* give errno a negative value */
#endif
  } else {
    /* r == EAI_SYSTEM, and errno already gives the error */
  }
}

/*
 *  This signature follows the older glibc convention of size_t
 *  for the type of nodelen and servlen, which is appropriate given
 *  that they are the lengths of character buffers.  In the POSIX
 *  signature, they are rather less appropriately typed as socklen_t.
 *  This should cause no sweat when they are passed to getnameinfo(),
 *  as both types presumably encompass the integers needed.
 */
int os_getnameinfo(const struct sockaddr *sa, socklen_t salen,
                   char *nodename, size_t nodelen,
                   char *servname, size_t servlen, int flags) {
  int saved_errno = errno;
  while (true) {
    int r = getnameinfo(sa, salen, nodename, nodelen, servname, servlen,
                         flags);
    if (r == 0) {
      errno = saved_errno;  /* success */
      return 0;
    }
    gai_errno(r);  /* set errno to r or -r if appropriate */
    if (r != EAI_SYSTEM || errno != EINTR) return r;  /* non-EINTR error */
    /* (On EINTR, retry the getnameinfo.)  */
  }
}

int os_getaddrinfo(const char *nodename, const char *servname,
                   const struct addrinfo *hints, struct addrinfo **res) {
  int saved_errno = errno;
  while (true) {
    int r = getaddrinfo(nodename, servname, hints, res);
    if (r == 0) {
      errno = saved_errno;  /* success */
      return 0;
    }
    gai_errno(r);  /* set errno to r or -r if appropriate */
    if (r != EAI_SYSTEM || errno != EINTR) return r;  /* non-EINTR error */
    /* (On EINTR, retry the getaddrinfo.)  */
  }
}

void os_freeaddrinfo(struct addrinfo *list) {
  int saved_errno = errno;
  freeaddrinfo (list);
  errno = saved_errno;
}


/*
 *  Elapsed time since the beginning of 1970, as a timespec.
 *
 *  May jump around at the whim of the system administrator.
 */
struct timespec os_tod(void) {
  struct timespec r;
  int saved_errno = errno;
#if HAVE_CLOCK_GETTIME
  if (clock_gettime(CLOCK_REALTIME, &r) != 0) {
    os_error("clock_gettime(CLOCK_REALTIME)");
  }
#elif HAVE_GETTIMEOFDAY
  struct timeval t;
  gettimeofday(&t, NULL);
  r.tv_sec = t.tv_sec;
  r.tv_nsec = t.tv_usec * 1000;
#elif HAVE_FTIME
  struct timeb t;
  ftime(&t);
  r.tv_sec = t.time;
  r.tv_nsec = t.millitm * 1000000;
#else
#warning TOD and CLOCK time will be at only 1-second resolution.
  r.tv_sec = time(NULL);
  r.tv_nsec = 0;
#endif
  errno = saved_errno;
  return r;
}

/*
 *  Elapsed time since an arbitrary fixed reference, as a timespec.
 *
 *  Advances monotonically no matter what the sysadmin does (unless
 *  the required support in the OS is not evident, in which case you
 *  get os_tod() as a fallback, with a build-time #warning).
 */
struct timespec os_mono(void) {
#if HAVE_CLOCK_GETTIME && defined CLOCK_MONOTONIC
  struct timespec r;
  int saved_errno = errno;
  if (clock_gettime(CLOCK_MONOTONIC, &r) != 0) {
    os_error("clock_gettime(CLOCK_MONOTONIC)");
  }
  errno = saved_errno;
  return r;
#else
#warning No monotonic clock - falling back on CLOCK_REALTIME or worse.
  return os_tod();
#endif
}

/*
 *  Establish a reference point for os_elapsed() to use.
 *
 *  Should be called when the SETL VM starts executing the current
 *  program, and at the beginning of each forked child process.
 */
void os_set_time_base(void) {
  time_base = os_mono();
}

/*
 *  Elapsed time since SETL VM execution began in this process.
 */
struct timespec os_elapsed(void) {
  return timespec_minus(os_mono(), time_base);  /* now - time_base */
}

/*
 *  Total CPU time (including completed child processes) since the
 *  beginning of program execution.
 */
struct timeval os_cputime(void) {
  struct timeval r;
  int saved_errno = errno;
#if HAVE_GETRUSAGE  /* e.g., in XSI */
  struct rusage t;
  struct timeval self, kids;
  getrusage(RUSAGE_SELF, &t);
  add_timevals(&self, &t.ru_utime, &t.ru_stime);
  getrusage(RUSAGE_CHILDREN, &t);
  add_timevals(&kids, &t.ru_utime, &t.ru_stime);
  add_timevals(&r, &self, &kids);  /* r := self + kids */
#elif HAVE_TIMES
  struct tms t;
  clock_t ticks;
# if defined CLK_TCK
  const long freq = CLK_TCK;
#  if CLK_TCK > 100 && SIZEOF_LONG <= 4
#  warning CPU time as reported by TIME may easily "wrap around".
#  endif
# elif defined CLOCKS_PER_SEC
  const long freq = CLOCKS_PER_SEC;
#  if CLOCKS_PER_SEC > 100 && SIZEOF_LONG <= 4
#  warning CPU time as reported by TIME may easily "wrap around".
#  endif
# elif HAVE_SYSCONF
  const long freq = sysconf(_SC_CLK_TCK);
# else
# warning Guessing clock frequency of 100 Hz.
  const long freq = 100;  /* a wild but historical guess */
# endif
  (void) times(&t);
  ticks = t.tms_utime +
          t.tms_stime +
          t.tms_cutime +
          t.tms_cstime;
  /*
   *  It is of course fairly easy for the number of ticks of CPU time
   *  to "wrap around", a fundamental defect in times().  The effect
   *  of this may be seen in the seconds field, r->tv_sec.
   *
   *  However, it would be rather rotten of us to cause an erroneous
   *  overflow in the calculation of the microseconds field,
   *  r->tv_usec, through sheer sloppiness in its computation, which
   *  is supposed to produce
   *
   *   ((ticks * 1000000) DIV freq) MOD 1000000
   *
   *  So it is done cautiously but in such a way that the result only
   *  has millisecond resolution (which is all its clients ultimately
   *  want anyway).
   *
   *  Note that it would not do to perform
   *
   *   ((ticks % freq) * 1000000) / freq
   *
   *  if freq were 1000000 (the maximum CLK_TCK or CLOCKS_PER_SEC
   *  defined anywhere) on a 32-bit machine, though for a freq of 100
   *  it would work.
   *
   *
   *  See the line "r.tv_usec = ... " below for how it is really done.
   *
   *
   *  Examples, with ticks = 1111111111 (10 1's, 1.1 billion + change):
   *
   *  If freq = 1000000 Hz, the intermediate expressions in the
   *  calculation of r->tv_usec just below are as follows:
   *
   *   ticks % freq = 111111
   *         * 1000 = 111111000
   *         / freq = 111
   *         * 1000 = 111000
   *
   *  If freq = 100 Hz (the other common value of CLK_TCK):
   *
   *   ticks % freq = 11
   *         * 1000 = 11000
   *         / freq = 110
   *         * 1000 = 110000
   */
  r.tv_sec = (ulong)ticks / freq;
  r.tv_usec = ((((ulong)ticks % freq) * 1000) / freq) * 1000;
#else
  clock_t t = clock();  /* CPU time in CLOCKS_PER_SEC */
# ifndef CLOCKS_PER_SEC
# define CLOCKS_PER_SEC 1000000  /* the XSI definition */
# endif
# if CLOCKS_PER_SEC > 100 && SIZEOF_CLOCK_T <= 4
# warning CPU time as reported by TIME may easily "wrap around".
# endif
  r.tv_sec = t / CLOCKS_PER_SEC;
  r.tv_usec = t % CLOCKS_PER_SEC;
#endif
  errno = saved_errno;
  return r;
}

struct tm *os_localtime(const time_t *t) {
  int saved_errno = errno;
  while (true) {
    /* POSIX actually lists localtime_r() but not localtime() as a
     * possible cancellation point.  Checking for EINTR is probably
     * silly either way, but oh well...  */
    struct tm *r = localtime(t);
    if (r != NULL) {
      errno = saved_errno;
      return r;
    }
    if (errno == EOVERFLOW) {
      return NULL;  /* too many years for tm int tm_year, most likely */
    }
    if (errno != EINTR) {
      os_error("localtime");  /* non-EINTR failure */
    }
    /* (On EINTR, retry the localtime call.)  */
  }
}

size_t os_strftime(char *s, size_t n, const char *fmt,
                                       const struct tm *t) {
  int saved_errno = errno;
  /* Since no errors are defined for strftime(), we wouldn't know when
   * to check for the exceedingly unlikely possibility of EINTR, even
   * though POSIX lists strftime() as a possible cancellation point.  */
  size_t r = strftime (s, n, fmt, t);
  errno = saved_errno;
  return r;
}

#if USE_POSIX_RT

timer_t os_timer_create() {
  timer_t t;
  int saved_errno = errno;
  int r = timer_create(CLOCK_REALTIME, NULL, &t);
  if (r != 0) os_error("timer_create");
  errno = saved_errno;
  return t;
}

void os_timer_delete(timer_t t) {
  int saved_errno = errno;
  /* On Cygwin (and possibly older versions of Linux), timer_delete()
   * can return -1 but leave errno 0 on a valid timer id.  So we
   * work around that by accepting that case as "success".  */
  int r;
  errno = 0;
  r = timer_delete(t);
  if (r != 0 && errno != 0) os_error("timer_delete");
  errno = saved_errno;
}

void os_timer_delete_quietly(timer_t t) {
  int saved_errno = errno;
  (void) timer_delete(t);
  errno = saved_errno;
}

void os_timer_gettime(timer_t t, struct itimerspec *cur) {
  int saved_errno = errno;
  int r = timer_gettime(t, cur);
  if (r != 0) os_error("timer_gettime");
  errno = saved_errno;
}

void os_timer_settime(timer_t t, int flags,
                      const struct itimerspec *new_spec,
                            struct itimerspec *old_spec) {
  int saved_errno = errno;
  int r = timer_settime(t, flags, new_spec, old_spec);
  if (r != 0) os_error("timer_settime");
  errno = saved_errno;
}

#else

/* Suppress the warning attending the first parameter to getitimer()
 * and setitimer() on Linux, where <sys/time.h> is a bit too clever
 * and uses an enum in the prototypes; the int called for by POSIX
 * would not be promoted to that enum in C++, though it is in C:  */
GCC_DIAG_OFF(c++-compat)

void os_get_timer(int which, struct itimerval *t) {
  int saved_errno = errno;
  if (getitimer(which, t) != -1) errno = saved_errno;
}

void os_set_timer(int which, const struct itimerval *t) {
  int saved_errno = errno;
  if (setitimer(which, t, NULL) != -1) errno = saved_errno;
}

GCC_DIAG_ON(c++-compat)

#endif

void os_signal(int sig, const sig_disp handler, struct sigaction *oldact) {
  struct sigaction newact;
  sigset_t no_signals;
  os_sigemptyset(&no_signals);
  newact.sa_handler = handler;
  newact.sa_mask    = no_signals;  /* no extra blocking */
  /*
   *  Note that we don't specify the SA_RESTART flag here, because
   *  in os_pselect(), we want to regain control upon interruption
   *  of a system call by a signal.  Because of that, we deal with
   *  the possibility of EINTR as an error return for a great many
   *  system functions called here in os.c rather than enjoying the
   *  benefit of automatic internal restart courtesy of SA_RESTART.
   */
  newact.sa_flags   = 0;
  /* Copy old, plant new:  */
  os_sigaction(sig, &newact, oldact);
}

void os_sigaction(int sig, const struct sigaction *act,
                                 struct sigaction *oldact) {
  int saved_errno = errno;
  if (sigaction(sig, act, oldact) == -1) os_error("sigaction");
  errno = saved_errno;
}

void os_sigrestore(int sig, const struct sigaction *oldact) {
  os_sigaction(sig, oldact, NULL);
}

void os_sigprocmask(int how, const sigset_t *mask, sigset_t *old_mask) {
  int saved_errno = errno;
  if (sigprocmask(how, mask, old_mask) == -1) os_error("sigprocmask");
  errno = saved_errno;
}

void os_sigblock(int sig, sigset_t *old_mask) {
  sigset_t mask;
  os_sigemptyset(&mask);
  os_sigaddset(&mask, sig);  /* mask := {sig} */
  os_sigprocmask(SIG_BLOCK, &mask, old_mask);
}

/* Useful for restoring the signal mask.  */
void os_sigsetmask(const sigset_t *mask) {
  os_sigprocmask(SIG_SETMASK, mask, NULL);
}

void os_sigsuspend(const sigset_t *mask) {
  /*
   *  The normal return for sigsuspend() is -1, with errno set to EINTR.
   *  We reflect normalcy to our caller by leaving errno alone.
   */
  int saved_errno = errno;
  if (sigsuspend(mask) != -1 || errno != EINTR) os_error("sigsuspend");
  errno = saved_errno;
}

void os_sigemptyset(sigset_t *s) {
  int saved_errno = errno;
  if (sigemptyset(s) == -1) os_error("sigemptyset");
  errno = saved_errno;
}

void os_sigaddset(sigset_t *s, int sig) {
  int saved_errno = errno;
  if (sigaddset(s, sig) == -1) os_error("sigaddset");
  errno = saved_errno;
}

int os_sigismember(sigset_t *s, int sig) {
  int saved_errno = errno;
  int r = sigismember(s, sig);
  if (r == -1) os_error("sigismember");
  errno = saved_errno;
  return r;
}

int os_kill(pid_t pid, int sig) {
  int saved_errno = errno;
  int r = kill(pid, sig);
  if (r != -1) errno = saved_errno;
  return r;
}

pid_t os_fork(void) {
  int saved_errno = errno;
  while (true) {
    pid_t pid = fork();
    switch (pid) {
    case -1:                /* error */
      /* The POSIX rationale for fork() says that erroring out with
       * EINTR is allowable though not recommended.  */
      if (errno != EINTR) return -1;  /* non-EINTR failure */
      break;  /* (On EINTR, retry the fork.) */
    case 0:                 /* child process */
      os_set_time_base();   /* new real-time clock base for this child */
      errno = 0;            /* a state of no original error */
      /*
       *  If this child returns from the execute() call in go(), we do
       *  not want go() to process any more lines that are intended
       *  for the parent VM, we want this child to make a normal return
       *  from go(), run(), and main().  Hence this horrid flag:
       */
      exit_please = true;   /* no more input to SETL VM please */
      return 0;
    default:                /* parent process */
      errno = saved_errno;  /* paranoically restore errno */
      return pid;           /* pid of child */
    }
  }
}

void os_execvp(const char *path, char *const argv[]) {
  int r;
  errno = 0;
  /* Unofficial cancellation point, not listed by POSIX:  */
  while (-1 == (r = execvp(path, argv))) {
    if (errno != EINTR) os_error("execvp");  /* non-EINTR failure */
    /* (On EINTR, retry the execvp.)  */
  }
  panic("execvp returned %d", r);
}

void os_execve(const char *path, char *const argv[], char *const envp[]) {
  int r;
  errno = 0;
  /* Unofficial cancellation point, not listed by POSIX:  */
  while (-1 == (r = execve(path, argv, envp))) {
    if (errno != EINTR) os_error("execve");  /* non-EINTR failure */
    /* (On EINTR, retry the execve.)  */
  }
  panic("execve returned %d", r);
}

void os_exec_sh(const char *cmd) {
  int r;
  /* Unofficial cancellation point, not listed by POSIX
   * (the (char *) cast is required by POSIX, though):  */
  while (-1 == (r = execl("/bin/sh", "sh", "-c", cmd, (char *)NULL))) {
    if (errno != EINTR) os_error("execl");  /* non-EINTR failure */
    /* (On EINTR, retry the execl.)  */
  }
  panic("execl returned %d", r);
}

pid_t os_waitpid(pid_t pid, int *statusp, int options) {
  int saved_errno = errno;
  while (true) {
    int raw_status;
    pid_t rpid = waitpid(pid, &raw_status, options);
    if (rpid != -1) {
      if (statusp != NULL) {
        *statusp = raw_status;
      }
      errno = saved_errno;
      return rpid;
    }
    if (errno != EINTR) return -1;  /* non-EINTR failure */
    /* (On EINTR, retry the waitpid.)  */
  }
}

pid_t os_getpid(void) {
  int saved_errno = errno;
  pid_t r = getpid();
  errno = saved_errno;
  return r;
}

pid_t os_getppid(void) {
  int saved_errno = errno;
  pid_t r = getppid();
  errno = saved_errno;
  return r;
}

pid_t os_getpgid(pid_t pid) {  /* obsolescent, as GETPGID is deprecated */
  int saved_errno = errno;
  pid_t r = getpgid(pid);
  if (r != -1) errno = saved_errno;
  return r;
}

pid_t os_getpgrp(void) {
  /* POSIX and SYSV have a getpgrp(void), but BSD doesn't.  */
  pid_t r = getpgid(0);  /* avoid calling getpgrp() */
  if (r == -1) os_error("getpgrp");  /* pretend getpgid(0) was getpgrp() */
  return r;
}

void os_setpgid(pid_t pid, pid_t pgid) {
  int saved_errno = errno;
  if (setpgid(pid, pgid) != -1) errno = saved_errno;
}

void os_setpgrp(void) {
  os_setpgid(0, 0);
}

pid_t os_getsid(pid_t pid) {
  int saved_errno = errno;
  pid_t r = getsid(pid);
  if (r != -1) errno = saved_errno;
  return r;
}

void os_setsid(void) {
  int saved_errno = errno;
  if (setsid() != -1) errno = saved_errno;
}

pid_t os_tcgetpgrp(int fd) {
  int saved_errno = errno;
  pid_t r = tcgetpgrp(fd);
  if (r != -1) errno = saved_errno;
  return r;
}

void os_tcsetpgrp(int fd, pid_t pgid) {
  int saved_errno = errno;
  if (tcsetpgrp(fd, pgid) != -1) errno = saved_errno;
}

void os_setctty(int fd) {
#ifdef TIOCSCTTY
  /* The non-POSIX but widely supported BSD convention.  */
  int saved_errno = errno;
  /* Linux describes this use of this ioctl thus:
      Make the given terminal the controlling terminal of the calling
      process.  The calling process must be a session leader and not
      have a controlling terminal already.  */
  if (ioctl(fd, TIOCSCTTY, 0) != -1) errno = saved_errno;
#else
  /* Controlling terminal acquired on first open in session.  */
  errno = ENOSYS;  /* because SETCTTY is effectively unimplemented */
  (void)fd;
#endif
}

void os_unsetctty(int fd) {
#ifdef TIOCNOTTY
  /* The non-POSIX but widely supported BSD convention.  */
  int saved_errno = errno;
  /* Linux describes this ioctl thus:
      If the given terminal was the controlling terminal of the
      calling process, give up this controlling terminal.  If the
      process was session leader, then send SIGHUP and SIGCONT to
      the foreground process group and all processes in the current
      session lose their controlling terminal.  */
  if (ioctl(fd, TIOCNOTTY, 0) != -1) errno = saved_errno;
#else
  /* Only setsid can relinquish controlling terminal.  */
  errno = ENOSYS;  /* because UNSETCTTY is effectively unimplemented */
  (void)fd;
#endif
}


uid_t os_getuid(void) {
  int saved_errno = errno;
  uid_t r = getuid();
  if (r == (uid_t)-1) os_error("getuid");
  errno = saved_errno;
  return r;
}

void os_setuid(uid_t uid) {
  int saved_errno = errno;
  if (setuid(uid) != -1) errno = saved_errno;
}

gid_t os_getgid(void) {
  int saved_errno = errno;
  gid_t r = getgid();
  if (r == (gid_t)-1) os_error("getgid");
  errno = saved_errno;
  return r;
}

void os_setgid(gid_t gid) {
  int saved_errno = errno;
  if (setgid(gid) != -1) errno = saved_errno;
}

uid_t os_geteuid(void) {
  int saved_errno = errno;
  uid_t r = geteuid();
  if (r == (uid_t)-1) os_error("geteuid");
  errno = saved_errno;
  return r;
}

void os_seteuid(uid_t uid) {
  int saved_errno = errno;
  if (seteuid(uid) != -1) errno = saved_errno;
}

gid_t os_getegid(void) {
  int saved_errno = errno;
  gid_t r = getegid();
  if (r == (gid_t)-1) os_error("getegid");
  errno = saved_errno;
  return r;
}

void os_setegid(gid_t gid) {
  int saved_errno = errno;
  if (setegid(gid) != -1) errno = saved_errno;
}


/*
 *  Open master ('ptym_open') and slave ('ptys_open') pseudo-tty.
 *
 *  After Stevens originally, though they've morphed a bit since then.
 */

static void pty_open_error(const char *name) NO_RETURN;

static void pty_open_error(const char *name) {
  char msg[40];
  strcpy(msg,"open of ");
  strcat(msg,name);
  os_error(msg);
} /* end pty_open_error */

#if HAVE_GRANTPT && HAVE_UNLOCKPT && HAVE_PTSNAME && \
    HAVE_DECL_GRANTPT && HAVE_DECL_UNLOCKPT && HAVE_DECL_PTSNAME

/* The preferred (SysV, UNIX 98, now POSIX) case */

#if HAVE_POSIX_OPENPT && HAVE_DECL_POSIX_OPENPT
#define posix_openpt_error  os_error("posix_openpt")
#else
#define PTY_MASTER  "/dev/ptmx"   /* the "cloning" device */
#define posix_openpt_error  pty_open_error(PTY_MASTER)
static int posix_openpt(int flags) {
  return os_open(PTY_MASTER, flags);  /* clone pty master */
}
#endif

/*
 *  The pts_name appears in the signatures here only because in BSD,
 *  the slave's device name is passed around that way.  Here in
 *  SysV, the slave name is picked up in ptys_open() by ptsname().
 */
int ptym_open(char *pts_name /* arg ignored, almost */) {
  int mfd = posix_openpt (O_RDWR | O_NOCTTY_COND);
  if (mfd < 0) posix_openpt_error;
  pts_name[0] = '\0';  /* for definiteness */
  return mfd;
} /* end ptym_open */

int ptys_open(int mfd, const char *pts_name /* arg ignored */) {
  int saved_errno = errno;
  int sfd;
  char *name;  /* slave name */
#ifdef I_PUSH  /* assume we should try to push STREAMS modules */
  char ptem[sizeof "ptem"];
  char ldterm[sizeof "ldterm"];
#endif
  /* Attempt a grantpt(), which should update the uid, gid, and mode of
   * the slave device before we open it; but don't abend the program on
   * disappointment.  Some OSses, notably QNX up to at least 6.5.0, do
   * what appears to be SysV fakery using the BSD tools when you use
   * these now-POSIX functions, and won't let a normal user change
   * things like ownership and mode on a /dev/tty?? device without
   * generating an EACCES, though they will let root do so.  Conversely,
   * the mode of all those devices on that edition of QNX is normally
   * rw-rw-rw- and they are owned by root, so normal users can use them
   * albeit with no security; and if root runs the program, the slave
   * mode does get changed to rw--w---- during execution.  */
  grantpt(mfd);  /* try to improve the slave pty's uid, gid, and mode */
  /* Apply the same treatment to unlockpt().  Its purpose is fairly
   * obscure (perhaps if it isn't supported, it isn't needed); what
   * really matters is whether the os_open() succeeds in producing a
   * working fd.  */
  unlockpt(mfd);  /* try to unlock the slave pty */
  name = ptsname(mfd);  /* get slave name */
  if (!name) os_error("ptsname");
  sfd = os_open(name, O_RDWR);
  if (sfd < 0) pty_open_error(name);  /* can't do much without this */
#ifdef I_PUSH
  strcpy(ptem,"ptem");
  ioctl(sfd, I_PUSH, ptem);  /* push STREAMS module if possible */
  strcpy(ldterm,"ldterm");
  ioctl(sfd, I_PUSH, ldterm);  /* push STREAMS module if possible */
#endif
  errno = saved_errno;
  return sfd;
} /* end ptys_open */

#else  /* !HAVE_GRANTPT etc. */

/* The old BSD case, in case you still need it somewhere */

int ptym_open(char *pts_name) {
  int saved_errno = errno;
  int mfd;
  const char *ptr1, *ptr2;
  strcpy(pts_name, "/dev/ptyXY");
  for (ptr1 = "pqrstuvwxyzPQRST"; *ptr1; ptr1++) {
    pts_name[8] = *ptr1;
    for (ptr2 = "0123456789abcdef"; *ptr2; ptr2++) {
      pts_name[9] = *ptr2;
      mfd = os_open(pts_name, O_RDWR);
      if (mfd >= 0) {
        pts_name[5] = 't';      /* change "pty" to "tty" */
        errno = saved_errno;    /* cancel effects of open failures */
        return mfd;             /* got it, return fd of master */
      } else if (errno == ENOENT) {
        /* Take it that we have run out of names to try.  */
        pty_open_error(pts_name);
      }
      /* Go around again.  We haven't been told that the name doesn't
       * exist, just that we couldn't open it for some reason.  */
    }
  }
  panic("ptym_open: out of pty devices");
} /* end ptym_open */

int ptys_open(int mfd, const char *pts_name) {
  int saved_errno = errno;
  int sfd;
  gid_t gid;
  struct group *grptr;
  grptr = getgrnam("tty");
  gid = grptr ? grptr->gr_gid : (gid_t)-1;
  os_chown(pts_name, getuid(), gid);  /* only effective if we're root */
  os_chmod(pts_name, S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH);
  sfd = os_open(pts_name, O_RDWR);
  if (sfd < 0) pty_open_error(pts_name);
  errno = saved_errno;  /* chown or chmod may have innocently failed */
  return sfd;
} /* end ptys_open */

#endif  /* end of ptym_open() and ptys_open() definitions */


/*
 *  Put a tty into "raw" mode (again after Stevens).
 */

#if HAVE_TERMIOS_H

void tty_rawmode(int fd) {
  int saved_errno = errno;
  struct termios mode;
  if (tcgetattr(fd, &mode) < 0) os_error("tcgetattr");
  mode.c_iflag = 0;  /* as revealed by careful study of termios page */
  mode.c_oflag &= ~(OPOST); /* | OLCUC | ONLCR | OCRNL | ONOCR | ONLRET
                               | OFILL | OFDEL */
  mode.c_cflag &= ~(CSIZE | PARENB);
  mode.c_cflag |= CS8;
  mode.c_lflag &= ~(ISIG | ICANON | ECHO | PENDIN | IEXTEN);
  mode.c_cc[VMIN] = 1;  /* min #chars to satisfy read */
  mode.c_cc[VTIME] = 0;  /* tenths of secs between chars */
  if (tcsetattr(fd, TCSANOW, &mode) < 0) os_error("tcsetattr");
  errno = saved_errno;
} /* end tty_rawmode */

#elif HAVE_TERMIO_H

void tty_rawmode(int fd) {
  struct termio mode;
  os_ioctl_addr(fd, TCGETA, &mode, "ioctl(TCGETA)");
  mode.c_iflag = 0;  /* turn off all input control */
  mode.c_oflag &= ~OPOST;  /* disable output post-processing */
  mode.c_lflag &= ~(ISIG | ICANON | ECHO | XCASE);  /* disable
       signal generation, canonical input, echo, upper/lower output */
  mode.c_cflag &= ~(CSIZE | PARENB);  /* clear char size, no parity */
  mode.c_cflag |= CS8;  /* 8-bit chars */
  mode.c_cc[VMIN] = 1;  /* min #chars to satisfy read */
  mode.c_cc[VTIME] = 0;  /* tenths of secs between chars */
  os_ioctl_addr(fd, TCSETA, &mode, "ioctl(TCSETA)");
} /* end tty_rawmode */

#elif HAVE_BSD_SGTTY_H

void tty_rawmode(int fd) {
  struct sgttyb mode;
  os_ioctl_addr(fd, TIOCGETP, &mode, "ioctl(TIOCGETP)");
  mode.sg_flags |= RAW;    /* turn on RAW */
  mode.sg_flags &= ~ECHO;  /* turn off ECHO */
  os_ioctl_addr(fd, TIOCSETP, &mode, "ioctl(TIOCSETP)");
} /* end tty_rawmode */

#endif

bool os_isatty(int fd) {
  int saved_errno = errno;
  int r = isatty(fd);
  errno = saved_errno;
  return r==1;
} /* end os_isatty */


const char *os_strerror(int errnum) {
  int saved_errno = errno;
  const char *r;
  if (errnum >= 0) {
    errno = 0;
    r = strerror(errnum);
    if (errno == EINVAL || r[0] == '\0') {
      static char s[40];
      snprintf (s, sizeof s, "Unknown error %d", errnum);
      r = s;
    }
  } else {
#if EAI_FAIL > 0
    errnum = -errnum;  /* assume all EAI_... error codes > 0 */
#endif
    r = gai_strerror(errnum);
  }
  errno = saved_errno;  /* lest set by strerror() or gai_strerror() */
  return r;
} /* end os_strerror */

#if 0
/* This can be used to help placate Autoconf-spewed configure scripts
 * when cross-compiling, if someone has misguidedly called
 * AC_FUNC_MALLOC and/or AC_FUNC_REALLOC in configure.ac (which would
 * be misguided because we avoid calling malloc() or realloc() with a
 * size arg of 0 in GNU SETL anyway):  */
#undef malloc
#undef realloc
void *rpl_malloc (size_t siz);  /* avoid warning of no prototype */
void *rpl_malloc (size_t siz) {
  extern void *malloc (size_t);
  return malloc (siz);
}
void *rpl_realloc (void *old, size_t siz);
void *rpl_realloc (void *old, size_t siz) {
  extern void *realloc (void *, size_t);
  return realloc (old, siz);
}
#endif
