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
    File        : EventLogHelper
    Purpose     : Provides support for writing to the Windows Appliation Event Log
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 22 12:19:54 CEST 2012
    Notes       : This class is intended for GUI for .NET use only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Diagnostics.*  FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.EventLogHelper: 
    
    DEFINE STATIC VARIABLE cDisabledEventSources AS CHARACTER NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EventLogHelper class
        Notes:   Disallow instance creation                                                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED EventLogHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Creates an Event Source                                                                        
        Notes:                                                                        
        @param pcEventSource The name of the Event Source to create      
        @return Logical value indicating if the EventSource has been created                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL CreateEventSource (pcEventSource AS CHARACTER):

        /* Don't try to create Event source when already existing */ 
        IF EventLog:SourceExists (pcEventSource) THEN RETURN TRUE . 
        
        EventLog:CreateEventSource (pcEventSource, "Application":U) . 
        
        RETURN EventLog:SourceExists (pcEventSource) .

        CATCH err AS System.Security.SecurityException:
            /* When on Vista, Windows 7, ... try to create Event Source as Administrator */
            IF System.Environment:OSVersion:Version:Major >= 6 THEN DO ON ERROR UNDO, THROW:
                EventLogHelper:CreateEventSourceAsAdministrator (pcEventSource) .

                /* Now the new source should exist and we should be able to query it */
                RETURN EventLog:SourceExists (pcEventSource) .        

                CATCH err2 AS System.Security.SecurityException:
                    RETURN FALSE .     
                END CATCH.
            END.    

            RETURN FALSE . 
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Starts a new AVM with the "Run As Administrator" flag to create an 
                 Event Source                                                                        
        Notes:  
        @param pcEventSource The name of the Event Source to create        
        @return Logical value indicating if the EventSource has been created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC LOGICAL CreateEventSourceAsAdministrator (pcEventSource AS CHARACTER):
        
        DEFINE VARIABLE oProcess   AS System.Diagnostics.Process NO-UNDO . 
        DEFINE VARIABLE oStartInfo AS ProcessStartInfo           NO-UNDO .
        DEFINE VARIABLE cProcedure AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE cResult    AS CHARACTER                  NO-UNDO .

        /* Avoid infinitive loop */
        IF SESSION:PARAMETER = pcEventSource AND SESSION:BATCH THEN 
            RETURN FALSE . 

        FILE-INFO:FILE-NAME = "Consultingwerk/Util/Support/create-event-source.p":U .

        ASSIGN cProcedure = FILE-INFO:FULL-PATHNAME .

        IF cProcedure = ? THEN DO:
            FILE-INFO:FILE-NAME = "Consultingwerk/Util/Support/create-event-source.r":U .

            ASSIGN cProcedure = FILE-INFO:FULL-PATHNAME .
        END.

        IF cProcedure = ? THEN
            UNDO, THROW NEW AppError ("Unable to find Consultingwerk/Util/Support/create-event-source.p":U, 0) .

        oStartInfo = NEW System.Diagnostics.ProcessStartInfo () .
        oStartInfo:FileName = Consultingwerk.Util.SessionHelper:ExecutablePath() .
        oStartInfo:Arguments = Consultingwerk.Util.SessionHelper:StartupParameters()
                + SUBSTITUTE (" -b -p ~"&1~" -param ~"&2~"":U,
                              cProcedure,
                              pcEventSource) .
        
        /* Set the Run As Administrator flag! */        
        oStartInfo:Verb = "runas":U .
        oStartInfo:UseShellExecute = TRUE .

        oStartInfo:WorkingDirectory = Consultingwerk.Util.SessionHelper:CurrentDirectory() .

        oProcess = System.Diagnostics.Process:Start (oStartInfo) .

        IF VALID-OBJECT (oProcess) THEN DO:
            oProcess:WaitForExit() .
            
            IF oProcess:ExitCode = 100 THEN 
                RETURN TRUE . 
        END.
            
        RETURN FALSE . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Verifies if an EventSource exists                                                              
        Notes: 
        @param pcEventSource The name of the Event Source 
        @return Logical value indicating if the EventSource exists
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL EventSourceExists (pcEventSource AS CHARACTER):
        
        RETURN EventLog:SourceExists (pcEventSource) . 
        
        CATCH err AS System.Security.SecurityException:
            RETURN FALSE .     
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes a Message to the EventLog                                                                        
        Notes:   Prevents from writing to Event Sources that are known to be not 
                 available in the session  
        @param pcMessage The message to be written to the EventLog 
        @param pcEventSource The event source to be used for the message, will be created if required
        @param poEntryType The System.Diagnostics.EventLogEntryType for the message
        @return Logical value indicating if the message has been written to the EventLog                                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pcMessage AS CHARACTER,
                                               pcEventSource AS CHARACTER,
                                               poEntryType AS System.Diagnostics.EventLogEntryType):
        
        IF LOOKUP (pcEventSource, cDisabledEventSources) > 0 THEN 
            RETURN FALSE. 

        IF EventLogHelper:EventSourceExists (pcEventSource) OR 
           EventLogHelper:CreateEventSource (pcEventSource) THEN DO:
            EventLog:WriteEntry (pcEventSource, pcMessage, poEntryType) .
            
            RETURN TRUE . 
        END.

        RETURN FALSE . 

        CATCH err AS Progress.Lang.Error:
            ASSIGN cDisabledEventSources = TRIM (cDisabledEventSources + ",":U + pcEventSource, ",":U) .    
        
            RETURN FALSE . 
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes a Message to the EventLog                                                                     
        Notes:   Prevents from writing to Event Sources that are known to be not 
                 available in the session  
        @param pcMessage The message to be written to the EventLog 
        @param pcEventSource The event source to be used for the message, will be created if required
        @return Logical value indicating if the message has been written to the EventLog                                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pcMessage AS CHARACTER,
                                               pcEventSource AS CHARACTER):
                                                   
        RETURN EventLogHelper:WriteMessage (pcMessage,
                                            pcEventSource,
                                            EventLogEntryType:Information) .                                           
                                                   
    END METHOD  .
&ENDIF
END CLASS.

