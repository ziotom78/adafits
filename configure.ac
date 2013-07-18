# Process this file with autoconf to produce a configure script.

# AdaFITS: a set of Ada 2012 bindings to the CFITSIO library
# Copyright (C) 2013 Maurizio Tomasi
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

AC_INIT([AdaFITS],
	[1.0],
	[ziotom78@gmail.com],
	,
	[http://github.com/ziotom78/adafits])

AC_PROG_INSTALL

######################################################################
# Decide whether to link statically or dinamically to CFITSIO

AC_MSG_CHECKING([how to link to CFITSIO])
ac_library_kind=static
AC_ARG_ENABLE(shared-linking,
[  --enable-shared-linking         Enable shared linking to CFITSIO (disabled)],
[case "${enableval}" in
  no|none)  ac_library_kind=static ;;
  *)        ac_library_kind=dynamic ;;
esac])dnl

AC_MSG_RESULT(${ac_library_kind})
LIBRARY_KIND=$ac_library_kind
AC_SUBST(LIBRARY_KIND)

######################################################################
# Check for the existence of CFITSIO

AC_MSG_CHECKING([the path to CFITSIO])
CFITSIO_LIBRARY_PATH=/usr/lib
AC_ARG_WITH([cfitsio],
            AS_HELP_STRING([--with-cfitsio=PATH],
                           [Specify the directory containing the CFITSIO
                             library file (with extension .a, .so, or .dylib)]),
            [
                CFITSIO_LIBRARY_PATH=$withval
            ])
AC_MSG_RESULT(${CFITSIO_LIBRARY_PATH})

case "${ac_library_kind}" in
  static)  library_extension=.a ;;
  dynamic) library_extension=.so ;;
esac

cfitsio_lib=${CFITSIO_LIBRARY_PATH}/libcfitsio${library_extension}
AC_CHECK_FILE([${cfitsio_lib}],
              [],
              [AC_ERROR([No CFITSIO library ${cfitsio_lib} found])])
AC_SUBST(CFITSIO_LIBRARY_PATH)

AC_CONFIG_FILES([cfitsio.gpr])
AC_OUTPUT
