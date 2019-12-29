dnl Autoconf configure script for tool

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/tool.ads)

# Current release settings
TOOL_MAJOR_VERSION=0
TOOL_MINOR_VERSION=2
TOOL_MICRO_VERSION=0
TOOL_VERSION=$TOOL_MAJOR_VERSION.$TOOL_MINOR_VERSION.$TOOL_MICRO_VERSION

AM_GNAT_LIBRARY_PROJECT(tools)

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

AM_GNAT_FIND_ADA_UTIL([Tools])

AC_OUTPUT(
Makefile tool.gpr
)
