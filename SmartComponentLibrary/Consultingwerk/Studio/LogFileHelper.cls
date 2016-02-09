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
    File        : LogFileHelper
    Purpose     : Supports parsing entries from a OpenEdge logile
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 09:42:19 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.* FROM PROPATH .
USING Progress.Lang.*         FROM PROPATH .

CLASS Consultingwerk.Studio.LogFileHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LogFileHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Parses a client logfile line                                                                        
        Notes:     
        @param pcLine The line to parse
        @param pdtDate OUTPUT The date time of the message
        @param piProcess OUTPUT The process id
        @param piThread OUTPUT The thread number
        @param piSeverity OUTPUT The message severity
        @param pcGroup  OUTPUT The group
        @param pcSubGroup OUTPUT The sub group
        @param pcText OUTPUT The log file message                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ParseClientLogLine (pcLine AS CHARACTER,
                                                  OUTPUT pdtDate AS DATETIME-TZ,
                                                  OUTPUT piProcess AS INTEGER,
                                                  OUTPUT piThread AS INTEGER, 
                                                  OUTPUT piSeverity AS INTEGER,
                                                  OUTPUT pcGroup AS CHARACTER,
                                                  OUTPUT pcSubGroup AS CHARACTER,
                                                  OUTPUT pcText AS CHARACTER):
            
        ASSIGN pdtDate = ? . /* not yet supported */
        
        IF NUM-ENTRIES (pcLine, " ":U) > 1 THEN 
            ASSIGN piProcess = LogFileHelper:ParseProcessOrThread (ENTRY (2, pcLine, " ":U)) .
        
        IF NUM-ENTRIES (pcLine, " ":U) > 2 THEN 
            ASSIGN piProcess = LogFileHelper:ParseProcessOrThread (ENTRY (3, pcLine, " ":U)) .
        
        IF NUM-ENTRIES (pcLine, " ":U) > 3 THEN
            ASSIGN piSeverity = INTEGER (ENTRY (4, pcLine, " ":U)) .  

        ParseClientLogLine (pcLine, 
                            OUTPUT pcGroup,
                            OUTPUT pcSubGroup,
                            OUTPUT pcText) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a client logfile line                                                                     
        Notes:        
        @param pcLine The logfile line to parse
        @param pcGroup  OUTPUT The group
        @param pcSubGroup OUTPUT The sub group
        @param pcText OUTPUT The log file message                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ParseClientLogLine (pcLine AS CHARACTER,
                                                  OUTPUT pcGroup AS CHARACTER,
                                                  OUTPUT pcSubGroup AS CHARACTER,
                                                  OUTPUT pcText AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.          
            
        IF NUM-ENTRIES (pcLine, " ":U) > 4 THEN
            ASSIGN pcGroup = ENTRY (5, pcLine, " ":U) .

        IF NUM-ENTRIES (pcLine, " ":U) > 5 THEN
            ASSIGN pcSubGroup = ENTRY (6, pcLine, " ":U) .

        pcLine = SUBSTRING (pcLine, 50) .

        ASSIGN iIndex = INDEX (pcLine, " ":U)
               iIndex = INDEX (pcLine, " ":U, iIndex + 1) .
               
        ASSIGN pcText = TRIM (SUBSTRING (pcLine, iIndex + 1)) .   
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Parses the process identifier                                                                        
        Notes:  
        @param pcProcess The process identifier from the logfile
        @return The process or thread id                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC INTEGER ParseProcessOrThread (pcProcess AS CHARACTER):
        
        RETURN INTEGER (ENTRY (2, pcProcess, "-":U)) .

    END METHOD.

END CLASS.
