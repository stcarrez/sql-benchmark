dnl Autoconf configure script for tool

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/tool.ads)

AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
TOOL_MAJOR_VERSION=0
TOOL_MINOR_VERSION=1
TOOL_MICRO_VERSION=0
TOOL_VERSION=$TOOL_MAJOR_VERSION.$TOOL_MINOR_VERSION.$TOOL_MICRO_VERSION
AM_GNAT_CHECK_GPRBUILD
# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

# Set the version number of the project
AC_SUBST(TOOL_VERSION)
AC_SUBST(TOOL_MAJOR_VERSION)
AC_SUBST(TOOL_MINOR_VERSION)
AC_SUBST(TOOL_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
TOOL_LIBDIR="lib"
AC_SUBST(TOOL_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)
##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building tool requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_util}
  ])
AC_SUBST(UTIL_DIR)

AC_OUTPUT(
Makefile tool.gpr
)
