@ECHO OFF
ECHO Creating program library (.pl) files...

SET DLC=C:\Progress\OE116_64
SET DLCBIN=%DLC%\bin

C:
CD /asigui/build
DEL /s /q PLBuild\*.* >NUL
RMDIR /s /q PLBuild\Graphics >NUL
RMDIR /s /q PLBuild\adm >NUL
RMDIR /s /q PLBuild\adm2 >NUL

MKDIR PLBuild\Graphics
MKDIR PLBuild\adm
MKDIR PLBuild\adm2

XCOPY c:\asigui\environments\Devel\Resources\Graphics\*.* .\PLBuild\Graphics /q /s >NUL
XCOPY c:\asigui\environments\Devel\Programs\adm\*.* .\PLBuild\adm /q /s >NUL
XCOPY c:\asigui\environments\Devel\Programs\adm2\*.* .\PLBuild\adm2 /q /s >NUL

CD PLBuild
%DLCBIN%/prolib asiGraphics.pl -create
%DLCBIN%/prolib asiObjects.pl -create

%DLCBIN%/prolib asiGraphics.pl -add Graphics/*.*
%DLCBIN%/prolib asiGraphics.pl -add Graphics/16x16/*.*
%DLCBIN%/prolib asiGraphics.pl -add Graphics/24x24/*.*
%DLCBIN%/prolib asiGraphics.pl -add Graphics/32x32/*.*
%DLCBIN%/prolib asiGraphics.pl -add Graphics/48x48/*.*

%DLCBIN%/prolib asiObjects.pl -add adm/objects/*.*
%DLCBIN%/prolib asiObjects.pl -add adm2/*.*
%DLCBIN%/prolib asiObjects.pl -add adm2/custom/*.*
%DLCBIN%/prolib asiObjects.pl -add adm2/image/*.*
%DLCBIN%/prolib asiObjects.pl -add adm2/support/*.*






