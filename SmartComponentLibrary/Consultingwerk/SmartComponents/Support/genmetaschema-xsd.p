/*------------------------------------------------------------------------
    File        : genmetaschema-xsd.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 02 17:18:08 CEST 2009
    Notes       : C:\Work\SmartComponents4NET\Trunk\CS\Consultingwerk.SmartComponents.Design\
                  Consultingwerk.SmartComponents.Design>"c:\Program Files\
                  Microsoft Visual Studio 8\SDK\v2.0\bin\xsd.exe" 
                  c:\work\SmartComponents4NET\Trunk\ABL\Consultingwerk\SmartComponents\Support\dsMetaschema.xsd 
                  /dataset /l:cs /n:Consultingwerk.SmartComponents.Design        
        
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{Consultingwerk/SmartComponents/Support/dsMetaschema.i}

DATASET dsMetaschema:WRITE-XMLSCHEMA ("FILE":U, 
                                      "Consultingwerk/SmartComponents/Support/dsMetaschema.xsd":U,
                                      TRUE, 
                                      ?, 
                                      FALSE) . 