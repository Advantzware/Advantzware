The "Business Entity Designer Launcher.exe" allows launching .bedgm files by 
double clicking them in the Windows Explorer or from the Progress Developer 
Studio Project Explorer or related views.

Users should associate in the Windows Explorer the .bedgm file extension with
the "Business Entity Designer Launcher.exe" process by using the "Open With" 
context menu option on a .bedgm file in Windows Explorer.

After doing this the Windows Explorer (and other programs) will open .bedgm 
files with launcher process. The launcher process will look for a closest file
called ".BusinessEntityDesignerSettings.xml" in any parent folder of the .bedgm
file that you are trying to open. When a settings file was found, the folder 
containing the settings file will be used as the startup folder for the 
Business Entity Designer that opens the selected .bedgm file. 

The command line parameters (including the path to the proper version of
prowin32.exe, the ini file, pf files connecting the databases etc. need to 
be specified in the Business Entity Designer options on the "Launcher" tab. The 
launcher will use those command line parameters to start the Business Entity
Designer with the right configuration. Without this setting the launcher will
not be able to start the Business Entity Designer.

Please add the parameter -param open=%filename% like this 

-param DebugMode,open=%filename%

or 

-param open=%filename%  

to the parameters. %filename% will be replaced with the name of the design to 
open.

Developers can also use the Progress Developer Studio for OpenEdge preferences
under General -> Editors -> File Associations to register the Launcher as a 
custom editor in the IDE.