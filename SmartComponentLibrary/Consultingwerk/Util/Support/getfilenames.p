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
    File        : getfilenames.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 14 21:46:46 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/Util/TempTables/ttFileNames.i &REFERENCE-ONLY=REFERENCE-ONLY}

DEFINE INPUT PARAMETER pcDirectory AS CHARACTER NO-UNDO . 
DEFINE INPUT PARAMETER pcFileMask  AS CHARACTER NO-UNDO . 
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFileNames .

/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cFileShort AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFileLong  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType      AS CHARACTER NO-UNDO.

INPUT FROM OS-DIR (pcDirectory) .

REPEAT:
    IMPORT cFileShort cFileLong cType . 
    
    IF cType MATCHES "*F*":U AND 
       cFileShort MATCHES pcFileMask THEN DO:
           
        CREATE ttFileNames .
        ASSIGN ttFileNames.FileName = cFileLong .    
    END.
    ELSE IF cType MATCHES "*D*":U AND cFileShort <> ".":U AND cFileShort <> "..":U THEN 
        RUN Consultingwerk/Util/Support/getfilenames.p
                (cFileLong, 
                 pcFileMask,
                 INPUT-OUTPUT TABLE ttFileNames BY-REFERENCE) .
END.    



