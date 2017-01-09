The (undocumented) startup parameter -zn is required to compile the WinKit Migration Utility (src/tools/winkit/winkitmtk.w).

After the compilation of the WinKit Migration Utility the -zn startup parameter could (and should) be removed. The startup parameter is not required to run the utility of your application.

The purpose of the startup parameter is to enable the compilation of files the define temp-tables or buffer names that start with the underscore "_". This is required for working with the AppBuilder internal temp-tables.

