/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : getclassnames.p
    Purpose     : Used by the ClassHelper to spawn off the search for 
                  ABL classes in a separate ABL session

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 27 20:25:29 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Util.* FROM PROPATH.

DEFINE VARIABLE cBaseType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPath            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeAbstract AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPrefix          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPropath         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogfileName     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cTempFileName AS CHARACTER NO-UNDO.

{Consultingwerk/products.i}

/* Mike Fechner, Consultingwerk Ltd. 13.11.2014
   Uncomment the following line, to enable debug output in 
   %SESSION-TEMP%\getclassnames.log and getclassnames.txt */
/*&GLOBAL-DEFINE DebugClassHelper*/

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 30.12.2011
   This procedure is part of the Visual Designer environment. 
   It's not intended to be executed on UNIX - the ClassHelper referenced
   here (the only program that should execute this .p file) is not
   available on UNIX */
&IF DEFINED (DotNetAccessible) NE 0 &THEN

{Consultingwerk/Util/TempTables/ttClassNames.i}

&IF DEFINED (DebugClassHelper) NE 0 &THEN
LOG-MANAGER:LOGFILE-NAME = SESSION:TEMP-DIRECTORY + "getclassnames.log":U . 
LOG-MANAGER:CLEAR-LOG () .
LOG-MANAGER:LOG-ENTRY-TYPES = "4GLMessages:2,4GLTrace:5":U .
ClassHelper:LogfileName = SESSION:TEMP-DIRECTORY + "getclassnames.txt":U .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 02.01.2012
   Set PROPATH for access to Consultingwerk.Util classes,
   passed from foreground runtime via SESSION:PARAMETER */
ASSIGN cPropath         = ENTRY (5, SESSION:PARAMETER, "|":U) . 

PROPATH = cPropath . 

ASSIGN cBaseType        = ENTRY (1, SESSION:PARAMETER, "|":U)
       cPath            = ENTRY (2, SESSION:PARAMETER, "|":U)
       lIncludeAbstract = Consultingwerk.Util.DataTypeHelper:ToLogical (ENTRY (3, SESSION:PARAMETER, "|":U))
       cPrefix          = ENTRY (4, SESSION:PARAMETER, "|":U)
       cLogfileName     = ENTRY (6, SESSION:PARAMETER, "|":U)
       .

IF cLogfileName > "":U THEN 
    Consultingwerk.Util.ClassHelper:LogfileName = cLogfileName . 

Consultingwerk.Util.ClassHelper:GetClassNames (cBaseType,
                                               cPath,
                                               lIncludeAbstract,
                                               cPrefix, 
                                               OUTPUT TABLE ttClassNames) .

ASSIGN cTempFileName = SESSION:TEMP-DIRECTORY + GUID + ".xml":U .

&IF DEFINED (DebugClassHelper) NE 0 &THEN
LogManager:WriteFormattedMessage ("Writing classes to: &1":U, cTempFileName) .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 10.01.2012
   Bug 2776: Unformatted WRITE-XML causes issues with READ-XML on OpenEdge 11.0 */
TEMP-TABLE ttClassNames:WRITE-XML ("FILE":U, cTempFileName, TRUE)  .

PUT UNFORMATTED cTempFileName . 

&IF DEFINED (DebugClassHelper) NE 0 &THEN
LogManager:WriteMessage ("Done":U) .
&ENDIF

&ENDIF
