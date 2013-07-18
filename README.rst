AdaFITS User's Manual
=====================

Copyright © 2013 Maurizio Tomasi.

This is the repository of the AdaFITS library, a set of Ada 2005 bindings to
the CFITSIO library (http://heasarc.gsfc.nasa.gov/fitsio/). The
library is licensed under the GPL 2 – see the file ``LICENSE`` for
more information.

Project status
--------------

Currently there are bindings only for reading ASCII/binary tables.
Support for images is yet to come. There is no support for reading
logical/string columns from tables yet.

AdaFITS has been developed under Linux using GNAT. It has never been
tested on other systems/compilers.

The library contains a complete set of unit tests, implemented using
AUnit (http://libre.adacore.com/tools/aunit/). You can run them by
entering the ``gnattest/harness`` directory and running::

    gprbuild -Ptest_driver.gpr
    ./test_runner

Compiling and installing the library
------------------------------------

To compile the library, run the following commands from the terminal::

    ./configure
    gprbuild -Padafits

A few caveats:

1. It is currently not allowed to run ``configure`` in a directory
   different than the one where this file is located.
2. By default, the CFITSIO library is linked statically. To enable
   dynamical linking, you can pass the ``--enable-shared-linking`` flag
   to ``configure``.
3. If the CFITSIO library file is in a non-standard location (i.e., it
   is not in ``/usr/lib``), you can specify its path with the
   ``--with-cfitsio=PATH`` flag to ``configure``.

Using the library in your application
-------------------------------------

The easiest way to use AdaFITS in your application is to include the
project file ``adafits.gpr`` at the beginning of your project file:

.. code-block:: ada

    with "adafits";

Be sure to include the path to ``adafits.gpr`` in the Ada library
search path.

Documentation
-------------

You can find the documentation for the library at these locations:

1. Look at the files ``adafits.ads`` (general-purpose routines) and
   ``adafits-column-routines.ads`` (table access routines) to learn
   which functions have been defined and what is their prototype.
2. If you are unsure how to call a function, refer to the unit tests
   in the ``gnattest/tests`` directory (there is one unit test for
   each routine implemented in AdaFITS).
3. It is useful if you get acquainted with the original CFITSIO user's
   manual.

Differences from CFITSIO
------------------------

Usually the AdaFITS functions are closely related to their counterpart
in the CFITSIO library, apart from the obvious fact that the former
accept Ada types as function parameters (e.g., a ``String`` instead of
a ``char *``).

There is however one function which is implemented differently in
AdaFITS than in CFITSIO: ``Create_Table``. I was urged to change the
convention on the parameters because of an intrinsic weakness in the
original CFITSIO definition of ``fits_create_tbl``. Let's consider the
following C code which creates a table in a FITS file:

.. code-block:: c

  char * ttype = { "LOCX", "LOCY", "LOCZ", "VELX", "VELY", "VELZ", "MASS" };
  char * tform = { "1D", "1D", "1D", "1D", "1D", "1D" };
  char * tunit = { "[m]", "[m]", "[m]", "[m/s]", "[m/s]", "[m/s]", "[kg]" };
  int status = 0;

  fits_create_tbl(fptr, BINARY, 0, 7,
                  ttype, tform, tunit, "PART", &status);

This code is likely to crash when executed. Can you spot where the
problem is?

The problem is that the array ``tform`` has one element less than
``ttype`` and ``tunit`` (6 elements instead of 7), and since
``fits_create_tbl`` is going to look at the seventh element, it will
surely read garbage. The AdaFITS's ``Create_Table`` function does not
suffer from such weakness because instead of accepting *three* vectors
it requires to specify *one* vector of ``Column_Def`` types. Moreover,
Ada vectors carry their own length, and therefore the code is
prevented from running out of the array's boundaries. 

Here is an example of how the C code shown above might be translated
in Ada using AdaFITS:

.. code-block:: ada

    procedure Create_Table (File : Fits_File) is

      package UStr renames Ada.Strings.Unbounded;

      Columns : constant Column_Def_Array (1 .. 7) :=
        ((Name => UStr ("LOCX"), Format => UStr ("1D"), Unit => UStr ("[m]")),
         (Name => UStr ("LOCY"), Format => UStr ("1D"), Unit => UStr ("[m]")),
         (Name => UStr ("LOCZ"), Format => UStr ("1D"), Unit => UStr ("[m]")),
         (Name => UStr ("VELX"), Format => UStr ("1D"), Unit => UStr ("[m/s]")),
         (Name => UStr ("VELY"), Format => UStr ("1D"), Unit => UStr ("[m/s]")),
         (Name => UStr ("VELZ"), Format => UStr ("1D"), Unit => UStr ("[m/s]")),
         (Name => UStr ("MASS"), Format => UStr ("1D"), Unit => UStr ("[kg]")));

      begin
         Create_Table (File, "PART", Columns);
      end;

(Note that an higher level of safety could have been achieved in C by
defining a ``struct column_def_t`` and modifying the prototype of
``fits_create_tbl`` accordingly. This would however not prevent the
user from passing a wrong number of columns – e.g., 8 instead of 7 – to the
``fits_create_tbl`` function.)


Usage examples
--------------

In this section we provide some example showing how to use the AdaFITS
library. All the examples assume ``use`` clauses for the library,
e.g.:

.. code-block:: ada

    with AdaFITS; use AdaFITS;
    -- The following is required only if you access ASCII/binary tables
    with AdaFITS.Column_Routines; use AdaFITS.Column_Routines;


Opening a file
**************

The following code shows how to use the library to open an existing
file named ``test.fits`` (the source code for this example is
available in ``examples/open_file.adb``). Note that AdaFITS traps
CFITSIO error conditions by means of exceptions. The exception message
is the string returned by the CFITSIO function `ffgmsg`_.

.. code-block:: ada

    procedure Open_File is

       File : Fits_File;

    begin
       File := Open_Table (File_Name => "test.fits",
                           Mode => READ_ONLY);
       --  Do whatever you want with "File"
       Close_File (File);
    exception
       when E : AdaFITS_Error =>
          Ada.Text_IO.Put_Line ("Error reading the file: "
                                & Ada.Exceptions.Exception_Message (E));
    end Open_File;

.. _ffgmsg: http://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/node34.html#ffgmsg


Reading table columns from a file
*********************************

The following example (file ``examples/read_table.adb``) reads a
vector of double values from a FITS file included in the source
distribution of AdaFITS.

.. code-block:: ada

    procedure Read_Table is
    
       File : Fits_File;
       Num_Of_Rows : Natural;
    
    begin
       File := Open_Table (File_Name => "../test_data/tables.fits",
                           Mode => AdaFITS.READ_ONLY);
       
       Move_To_HDU (File, 4);
       
       Num_Of_Rows := Get_Number_Of_Rows (File);
       declare
          Values : Double_Array (1 .. Num_Of_Rows);
       begin
          Read_Column_Of_Doubles (File,
                                  Column_Number => 2,
                                  First_Row => 1,
                                  First_Element => 1,
                                  Num_Of_Elements => Values'Length,
                                  Destination => Values);
    
          for I in Values'Range loop
              Ada.Text_IO.Put_Line (Natural'Image (I) & ": " &
                                    Double'Image (Values (I)));
          end loop;
       end;
    
       Close_File (File);
    
    exception
       when E : AdaFITS_Error =>
          Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
    end Read_Table;

In this example we use the ``Double`` type defined in
``AdaFITS.Column_Routines`` as we need to closely match the C type
used by CFITSIO. Several types are defined there: ``Short``, ``Long``,
``Float``, and ``Double``. If you need more types, you can instantiate
the generic package ``AdaFITS.Generic_Column_Routines``. (Have a look
at the ``adafits-column_routines.ads`` file to see how to do it.)

A few caveats:

1. In this example we allocate the array on the stack, as we already
   know that the number of values to be loaded is small. If you expect
   to load large quantities of data, you should allocate the array on
   the heap.
2. If you read/write many columns at once, and the number of rows is
   large, you should also read the elements in bunches of ``N``, where
   ``N`` is the value returned by the function
   ``AdaFITS.Get_Number_Of_Rows_For_Optimal_IO``. This can
   dramatically increase the speed of the program. See chapter
   "Optimizing programs" of the CFITSIO manual
   (http://heasarc.gsfc.nasa.gov/docs/software/fitsio/c/c_user/node1.html).
