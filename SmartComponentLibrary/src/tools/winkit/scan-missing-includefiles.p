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
    File        : scan-missing-includefiles.p
    Purpose     : 

    Syntax      :

    Description : This file is part of the WinKit MTK 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 19 12:57:04 CET 2012
    Notes       : Generates the following report files:
                  report-missing-embedwindow.txt         - a list of programs with no embedwindow.i at all
                  report-missing-embedfinalize.txt       - a list of programs with no embedfialize.i at all
                  report-missing-closewindow.txt         - a list of programs with no closewindow.i at all
                  report-stil-with-LOAD-ICON.txt         - a list of programs that still contain the LOAD-ICON method
                  report-more-than-one-embedfinalize.txt - a list of programs with more than one embedfialize.i
                  report-more-than-one-closewindow.txt   - a list of programs with more then one closewindow.i 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE STREAM s1 .
DEFINE STREAM s2 . 
DEFINE STREAM s3 .
DEFINE STREAM s4 .
DEFINE STREAM s5 .
DEFINE STREAM s6 .

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
DEFINE VARIABLE lcFile    AS LONGCHAR  NO-UNDO . 

/* ***************************  Main Block  *************************** */

DEFAULT-WINDOW:WIDTH = 150  .

INPUT FROM wowifiles.txt .

OUTPUT STREAM s1 TO report-missing-embedwindow.txt .
OUTPUT STREAM s2 TO report-missing-embedfinalize.txt .
OUTPUT STREAM s3 TO report-missing-closewindow.txt .
OUTPUT STREAM s4 TO report-stil-with-LOAD-ICON.txt .
OUTPUT STREAM s5 TO report-more-than-one-embedfinalize.txt .
OUTPUT STREAM s6 TO report-more-than-one-closewindow.txt .

DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

REPEAT:
    IMPORT UNFORMATTED cFileName . 
    
    DISPL cFileName FORMAT "x(120)":U WITH WIDTH 140 . 
    PAUSE 0. 
    PROCESS EVENTS . 
    
    COPY-LOB FROM FILE cFileName TO lcFile . 
    
    IF INDEX (lcFile, "embedwindow.i":U) > 0 THEN . 
    ELSE 
        PUT STREAM s1 UNFORMATTED cFileName SKIP . 

    IF INDEX (lcFile, "embedfinalize.i":U) > 0 THEN DO:
        iPos = INDEX (lcFile, "embedfinalize.i":U) . 
        
        IF INDEX (lcFile, "embedfinalize.i":U, iPos + 1) > 0 THEN 
            PUT STREAM s5 UNFORMATTED cFileName SKIP . 
    END.
    ELSE 
        PUT STREAM s2 UNFORMATTED cFileName SKIP . 

    IF INDEX (lcFile, "closewindow.i":U) > 0 THEN DO:
        iPos = INDEX (lcFile, "closewindow.i":U) . 
        
        IF INDEX (lcFile, "closewindow.i":U, iPos + 1) > 0 THEN 
            PUT STREAM s6 UNFORMATTED cFileName SKIP . 
    END.
    ELSE 
        PUT STREAM s3 UNFORMATTED cFileName SKIP . 

    IF INDEX (lcFile, "LOAD-ICON":U) > 0 THEN  
        PUT STREAM s4 UNFORMATTED cFileName SKIP . 
    
END.    

