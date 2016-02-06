The assembly in this folder is part of the support for the ZeroMQ (0MQ) 
messaging solution.

http://zeromq.org/
https://github.com/zeromq/clrzmq
http://packages.nuget.org/Packages/clrzmq

The assembly is not strong named, so the entry in assemblies.xml does only 
contain the assembly name, not the version, culture, vendor key.

The assembly has dependencies on the libzmq.dll which is available in both
32 and 64 bit versions in sub folders of the folder containing this readme.txt 
file.

This assembly is build using the .NET framework version 4.0 and thus not 
compatible with OpenEdge 10. The previous - 32 bit only - version of ZeroMQ
is used on OpenEdge 10.
