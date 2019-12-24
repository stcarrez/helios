dnl Autoconf configure script for helios

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/helios.ads)

# Current release settings
HELIOS_MAJOR_VERSION=0
HELIOS_MINOR_VERSION=1
HELIOS_MICRO_VERSION=0
HELIOS_VERSION=$HELIOS_MAJOR_VERSION.$HELIOS_MINOR_VERSION.$HELIOS_MICRO_VERSION

# Set the version number of the project
AC_SUBST(HELIOS_VERSION)
AC_SUBST(HELIOS_MAJOR_VERSION)
AC_SUBST(HELIOS_MINOR_VERSION)
AC_SUBST(HELIOS_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
AM_GNAT_LIBRARY_PROJECT(helios)
# checking for local tools
AC_PROG_CC

HELIOS_LIBDIR="lib"
AC_SUBST(HELIOS_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building helios requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Swagger library
##########################################
AM_GNAT_FIND_PROJECT([swagger-ada],[Swagger Ada Library],[swagger],
  [git@github.com:stcarrez/swagger-ada.git],
  [Building Hyperion requires the Ada Swagger Library.],
  [
    SERVLET_DIR=${gnat_project_dir_swagger}
  ])
AC_SUBST(SWAGGER_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building helios requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)
AC_OUTPUT(
Makefile.conf
helios.gpr
)
