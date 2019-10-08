The SmartComponent Library Desktop is a launcher Application for various tools 
of the SmartComponent Library framework. The Desktop application will make it 
easier for us to roll out "shortcuts" to launching new tools or resources to our 
customers. Customers will only need to create a single desktop shortcut to the 
"SmartComponent Library Desktop" and we can deploy additional items to this desktop.

See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+the+SmartComponent+Library+Desktop


Sample items in the Desktop:
----------------------------

- Business Entity Designer
- Business Entity Tester
- Business Entity Browser
- proenv
- Windows Explorer
- Confluence
- Class Reference
- JIRA
- ESD

The Desktop will be configured using an XML file based on ttDesktopItems.i. The
default item definitions are stored in Consultingwerk/Studio/Desktop/items.xml

Alternatively items can be configured by using the -icfparam startup parameter and 
pointing to a different XML file.

UltraWinCarousel dependency:
----------------------------

As the Desktop is based on the UltraCarousel control which is only available form 
OpenEdge 11.5 on, we do not support the Desktop on earlier versions of OpenEdge.

- http://help.infragistics.com/doc/WinForms/2014.2/CLR4.0/?page=Whats_New_in_2014_Volume_2.html
- http://help.infragistics.com/doc/WinForms/2014.2/CLR4.0/?page=WinCarousel.html 

Our sample .NET assemblies package and assemblies.xml do currently not contain the 
assembly or a reference to this. Customers on OpenEdge 11.5 will find a version to 
this assembly in their "%DLC%\bin\infragistics\winforms" folder or in the Infragistics
install folder "c:\Program Files (x86)\Infragistics\2014.2\Windows Forms".

Assembly versions:
------------------

OpenEdge 11.5:   14.2.20142.2010
OpenEdge 11.5.1: 14.2.20142.2092
