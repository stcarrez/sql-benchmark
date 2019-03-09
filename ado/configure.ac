dnl Autoconf configure script for sqlbench

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/sqlbench.ads)

AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
SQLBENCH_MAJOR_VERSION=0
SQLBENCH_MINOR_VERSION=2
SQLBENCH_MICRO_VERSION=0
SQLBENCH_VERSION=$SQLBENCH_MAJOR_VERSION.$SQLBENCH_MINOR_VERSION.$SQLBENCH_MICRO_VERSION
AM_GNAT_CHECK_GPRBUILD
# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

# Set the version number of the project
AC_SUBST(SQLBENCH_VERSION)
AC_SUBST(SQLBENCH_MAJOR_VERSION)
AC_SUBST(SQLBENCH_MINOR_VERSION)
AC_SUBST(SQLBENCH_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
SQLBENCH_LIBDIR="lib"
AC_SUBST(SQLBENCH_LIBDIR)

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
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building sqlbench requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building sqlbench requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)
AC_OUTPUT(
Makefile sqlbench.gpr
)
