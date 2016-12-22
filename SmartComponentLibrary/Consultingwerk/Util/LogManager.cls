/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LogManager
    Purpose     : 
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 05 19:52:55 CEST 2008
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{ Consultingwerk/products.i }

USING Consultingwerk.*                   FROM PROPATH . 
USING Consultingwerk.Exceptions.*        FROM PROPATH .
USING Consultingwerk.Framework.*         FROM PROPATH . 
USING Consultingwerk.Framework.Base.*    FROM PROPATH.
USING Consultingwerk.Framework.Enum.*    FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH . 
USING Consultingwerk.OERA.*              FROM PROPATH . 
USING Consultingwerk.OERA.Enum.*         FROM PROPATH . 
USING Consultingwerk.Util.*              FROM PROPATH .
USING Consultingwerk.Util.Enum.*         FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH . 

CLASS Consultingwerk.Util.LogManager : 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogManager class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC LogManager ():
		
        LogManager:CustomLogEntries = "LogManager":U .  
        		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Proteced default constructor. 
        Notes:   There's no need to create instances of the helper classes
                 Overridden in Consultingwerk.Dynamics.Framework.LogManager                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED LogManager ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Allows to toggle output of application specific logfile entries                                                                      
        Notes:   CustomLogEntries can be set from the icfconfig.xml file in 
                 Dynamics4.NET (see d4start.p)                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY CustomLogEntries AS CHARACTER NO-UNDO 
    GET.
    SET (arg AS CHARACTER):
        LogManager:CustomLogEntries = arg . 
        
        LogManager:WriteMessage (SUBSTITUTE ("CustomLogEntries set to: &1"{&TRAN}, arg), "LogManager"{&TRAN}) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the current open logfile                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LogFileName AS CHARACTER NO-UNDO 
    GET():
        RETURN LOG-MANAGER:LOGFILE-NAME .
        
        CATCH e AS Progress.Lang.Error :
            RETURN ? .    
        END CATCH.
    END GET.
    SET(INPUT arg AS CHARACTER):
        LOG-MANAGER:LOGFILE-NAME = arg .
        
        /* no catch block here. Error should be catched by caller */
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the list of available EntryTypes for the current runtime 
                 session                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY EntryTypesList AS CHARACTER NO-UNDO 
    GET():
        RETURN LOG-MANAGER:ENTRY-TYPES-LIST .
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Adds a comma delimited list of custom log entries to the 
                 CustomLogEntries List  
        Notes:   Does silently ignore entries that are already part of the list   
        @param pcEntries The comma delimited list of custom log entries to add
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID AddCustomLogEntries (pcEntries AS CHARACTER):
		
		DEFINE VARIABLE cEntries AS CHARACTER NO-UNDO.

        ASSIGN LogManager:CustomLogEntries = ListHelper:AddUniqueEntries (LogManager:CustomLogEntries,
                                                                          pcEntries) . 
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes a comma delimited list of custom log entries from the 
                 CustomLogEntries List  
        Notes:   Does silently ignore entries that are not part of the list   
        @param pcEntries The comma delimited list of custom log entries to remove
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID RemoveCustomLogEntries (pcEntries AS CHARACTER):
		
        DEFINE VARIABLE cEntry      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iEntry      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cLogEntries AS CHARACTER NO-UNDO.
        
        ASSIGN cLogEntries = LogManager:CustomLogEntries .
        
        DO i = 1 TO NUM-ENTRIES (pcEntries):
            ASSIGN cEntry = ENTRY (i, pcEntries) .
            
            iEntry = LOOKUP (cEntry, cLogEntries) .
            
            IF iEntry > 0 THEN 
               cLogEntries = ListHelper:RemoveEntry (cLogEntries, iEntry) . 
        END.

        ASSIGN LogManager:CustomLogEntries = cLogEntries . 

	END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param pcLogentryTypes The log-entry types to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            pcLogentryTypes AS CHARACTER):
        
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = pcLogentryTypes .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryTypes The array of log-entry types to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryTypes AS LogEntryTypesEnum EXTENT):
        
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (poLogentryTypes, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum):
        
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = SubstituteEntryType (poLogentryType1) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 2 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 3 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 4 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 5 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
       @param poLogentryType6 The sixth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum,
                                            poLogentryType6 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 6 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               cLogentryTypes[6] = SubstituteEntryType (poLogentryType6) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
       @param poLogentryType6 The sixth log-entry type to activate
       @param poLogentryType7 The seventh log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum,
                                            poLogentryType6 AS LogEntryTypesEnum,
                                            poLogentryType7 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 7 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               cLogentryTypes[6] = SubstituteEntryType (poLogentryType6) 
               cLogentryTypes[7] = SubstituteEntryType (poLogentryType7) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
       @param poLogentryType6 The sixth log-entry type to activate
       @param poLogentryType7 The seventh log-entry type to activate
       @param poLogentryType8 The eigth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum,
                                            poLogentryType6 AS LogEntryTypesEnum,
                                            poLogentryType7 AS LogEntryTypesEnum,
                                            poLogentryType8 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 8 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               cLogentryTypes[6] = SubstituteEntryType (poLogentryType6) 
               cLogentryTypes[7] = SubstituteEntryType (poLogentryType7) 
               cLogentryTypes[8] = SubstituteEntryType (poLogentryType8) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
       @param poLogentryType6 The sixth log-entry type to activate
       @param poLogentryType7 The seventh log-entry type to activate
       @param poLogentryType8 The eigth log-entry type to activate
       @param poLogentryType9 The nineth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum,
                                            poLogentryType6 AS LogEntryTypesEnum,
                                            poLogentryType7 AS LogEntryTypesEnum,
                                            poLogentryType8 AS LogEntryTypesEnum,
                                            poLogentryType9 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 9 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               cLogentryTypes[6] = SubstituteEntryType (poLogentryType6) 
               cLogentryTypes[7] = SubstituteEntryType (poLogentryType7) 
               cLogentryTypes[8] = SubstituteEntryType (poLogentryType8) 
               cLogentryTypes[9] = SubstituteEntryType (poLogentryType9) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Starts Logging to a (new) client logfile
       Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
       @param pcLogfileName The name of the Logfile to write to
       @param poLogentryType1 The first log-entry type to activate
       @param poLogentryType2 The second log-entry type to activate
       @param poLogentryType3 The third log-entry type to activate
       @param poLogentryType4 The fourth log-entry type to activate
       @param poLogentryType5 The fifth log-entry type to activate
       @param poLogentryType6 The sixth log-entry type to activate
       @param poLogentryType7 The seventh log-entry type to activate
       @param poLogentryType8 The eigth log-entry type to activate
       @param poLogentryType9 The nineth log-entry type to activate
       @param poLogentryType10 The tenth log-entry type to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
                                            poLogentryType1 AS LogEntryTypesEnum,
                                            poLogentryType2 AS LogEntryTypesEnum,
                                            poLogentryType3 AS LogEntryTypesEnum,
                                            poLogentryType4 AS LogEntryTypesEnum,
                                            poLogentryType5 AS LogEntryTypesEnum,
                                            poLogentryType6 AS LogEntryTypesEnum,
                                            poLogentryType7 AS LogEntryTypesEnum,
                                            poLogentryType8 AS LogEntryTypesEnum,
                                            poLogentryType9 AS LogEntryTypesEnum,
                                            poLogentryType10 AS LogEntryTypesEnum):
        
        DEFINE VARIABLE cLogentryTypes AS CHARACTER EXTENT 10 NO-UNDO.
        
        ASSIGN cLogentryTypes[1] = SubstituteEntryType (poLogentryType1)
               cLogentryTypes[2] = SubstituteEntryType (poLogentryType2) 
               cLogentryTypes[3] = SubstituteEntryType (poLogentryType3) 
               cLogentryTypes[4] = SubstituteEntryType (poLogentryType4) 
               cLogentryTypes[5] = SubstituteEntryType (poLogentryType5) 
               cLogentryTypes[6] = SubstituteEntryType (poLogentryType6) 
               cLogentryTypes[7] = SubstituteEntryType (poLogentryType7) 
               cLogentryTypes[8] = SubstituteEntryType (poLogentryType8) 
               cLogentryTypes[9] = SubstituteEntryType (poLogentryType9) 
               cLogentryTypes[10] = SubstituteEntryType (poLogentryType10) 
               .
            
        LogManager:LogFileName = pcLogfileName . 
        LogManager:ClearLogFile() .
        
        LOG-MANAGER:LOG-ENTRY-TYPES = ArrayHelper:ToList (cLogentryTypes) .

    END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Starts Logging to a (new) client logfile
	   Notes:   Sets LOG-ENTRY-TYPES and clears the logfile
	   @param pcLogfileName The name of the Logfile to write to
	   @param pcLogentryTypes The log-entry types to activate
       @param pcCustomLogEntries The custom log-entries to activate
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID StartLogging (pcLogfileName AS CHARACTER,
	                                        pcLogentryTypes AS CHARACTER,
	                                        pcCustomLogEntries AS CHARACTER):
		
		LogManager:LogFileName = pcLogfileName . 
		LogManager:ClearLogFile() .
		
        LOG-MANAGER:LOG-ENTRY-TYPES = pcLogentryTypes .

        LogManager:CustomLogEntries = pcCustomLogEntries . 

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Replaces the log entry type value of the LogEntryTypesEnum with the 
	             matching ABL log-entry-type 
	    Notes:
	    @param poEntryType The LogEntryTypesEnum value
	    @return The log-entry-type matching with LogEntryTypesEnum
	------------------------------------------------------------------------------*/
	METHOD PROTECTED STATIC CHARACTER SubstituteEntryType (poEntryType AS LogEntryTypesEnum):

        RETURN REPLACE (REPLACE (REPLACE (STRING (poEntryType), "_":U, ".":U), 
                                                                "#":U, "*":U), 
                                                                "Four":U, "4":U) .		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Dumps a buffer's field to the current logfile.                                                                        
        Notes:         
        @param phBuffer The handle to the buffer
        @param plFindFirst When plFindFirst is TURE, the method will create an additional buffer for the table and run a FIND-FIRST on that buffer to not change the orignal buffer's position                                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteBuffer (phBuffer AS HANDLE, 
                                           plFindFirst AS LOGICAL):
        
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN . 
        ELSE 
            RETURN .  
                    
        IF NOT VALID-HANDLE (phBuffer) OR phBuffer:TYPE <> "BUFFER":U THEN 
            RETURN . 
        
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN .
        ELSE RETURN . 
        
        IF plFindFirst THEN DO ON ERROR UNDO, THROW:
            
            CREATE BUFFER hBuffer FOR TABLE phBuffer .

            hBuffer:FIND-FIRST () NO-ERROR .
            
            WriteBuffer (hBuffer) .            
            
            FINALLY:
                IF VALID-HANDLE (hBuffer) THEN 
                    DELETE OBJECT hBuffer .         
            END FINALLY.            
        END.
        ELSE 
            WriteBuffer (phBuffer) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Dumps a buffer's field to the current logfile.                                                                       
        Notes:   When no record is availble in that buffer, a "no record is available"
                 message is written instead.                                                                        
        @param phBuffer The handle to the buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID WriteBuffer (phBuffer AS HANDLE):

        DEFINE VARIABLE iField  AS INTEGER NO-UNDO.        
        DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
        DEFINE VARIABLE hField  AS HANDLE  NO-UNDO.
        
        DEFINE VARIABLE cLogValue AS CHARACTER NO-UNDO.

        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN . 
        ELSE 
            RETURN .  
        
        IF NOT phBuffer:AVAILABLE THEN DO:
            
            LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("Buffer &1: <no record is available>"{&TRAN}, phBuffer:NAME), "BUFFER":U) .
            
            RETURN .
        END.

        LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("Buffer &1: record with ROWID &2"{&TRAN}, 
                                               phBuffer:NAME, 
                                               phBuffer:ROWID), "BUFFER":U) .

        IF NOT BufferHelper:IsDatabaseBuffer(phBuffer) THEN 
            LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE (" ROW-STATE: &1"{&TRAN}, 
                                                   RowStateEnum:FromInteger(phBuffer:ROW-STATE):ToAblConstant())) . 


        DO iField = 1 TO phBuffer:NUM-FIELDS:
            
            ASSIGN hField = phBuffer:BUFFER-FIELD (iField) .
            
            IF hField:EXTENT >= 1 THEN DO:
                DO iExtent = 1 TO hField:EXTENT:
                    CASE hField:DATA-TYPE:
                        WHEN "BLOB":U THEN 
                            ASSIGN cLogValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE(iExtent))) .

                        WHEN "CLOB":U THEN 
                            ASSIGN cLogValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE)) .
                        
                        OTHERWISE 
                            ASSIGN cLogValue = STRING (hField:BUFFER-VALUE(iExtent)) . 
                    
                    END.
                    
                    LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1.&2[&5] <&3>: &4":U,
                                                           phBuffer:NAME, 
                                                           hField:NAME,
                                                           hField:DATA-TYPE,
                                                           cLogValue,
                                                           iExtent)) .
                END.
            END.
            ELSE DO:
                CASE hField:DATA-TYPE:
                    WHEN "BLOB":U THEN 
                        ASSIGN cLogValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE)) .

                    WHEN "CLOB":U THEN 
                        ASSIGN cLogValue = SUBSTITUTE ("Length: &1"{&TRAN}, LENGTH (hField:BUFFER-VALUE)) .
                    
                    OTHERWISE 
                        ASSIGN cLogValue = STRING (hField:BUFFER-VALUE) . 
                
                END.
                
                LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1.&2 <&3>: &4":U,
                                                       phBuffer:NAME, 
                                                       hField:NAME,
                                                       hField:DATA-TYPE,
                                                       cLogValue)) .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes details about an Error object (exception) to the client
                 logfile                                                                        
        Notes:   Uses Consultingwerk.Util.ErrorHelper to format the error message
                 and stack trace  
        @param poError The reference to the error object                                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteError (poError AS Progress.Lang.Error):
        
        DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO . 
        DEFINE VARIABLE cTypeName AS CHARACTER NO-UNDO.
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN . 
        ELSE 
            RETURN .  
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF TYPE-OF (poError, System.Exception) THEN 
            ASSIGN cTypeName = CAST (poError, System.Exception):GetType():FullName . 
        ELSE 
&ENDIF
            ASSIGN cTypeName = CAST (poError, Progress.Lang.Object):GetClass():TypeName .

        ASSIGN cMessage = SUBSTITUTE ("An '&1' has occurred.&4&2&4  &3"{&TRAN},
                                      cTypeName, 
                                      Consultingwerk.Util.ErrorHelper:FormattedErrorMessages (poError),
                                      REPLACE (Consultingwerk.Util.ErrorHelper:StackTrace (poError), CHR(10), CHR (10) + "  ":U),
                                      "~n":U) . 
        
        WriteMessage (cMessage) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam A character message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pcMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATE):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date time message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATETIME):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date time TZ message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATETIME-TZ):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdeMessageParam A decimal message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdeMessageParam AS DECIMAL):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdeMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param piMessageParam An integer message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        piMessageParam AS INTEGER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, piMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param piMessageParam An int64 message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        piMessageParam AS INT64):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, piMessageParam)) .
    
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param plMessageParam An int64 message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        plMessageParam AS LOGICAL):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, plMessageParam)) .
    
    END.
        
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param preMessageParam A recid message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        preMessageParam AS RECID):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, preMessageParam)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param proMessageParam A rowid message parameter to substitute &1
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        proMessageParam AS ROWID):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, proMessageParam)) .
    
    END.
        
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATE,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date time message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATETIME,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdtMessageParam A date time TZ message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdtMessageParam AS DATETIME-TZ,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdtMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pdeMessageParam A decimal message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pdeMessageParam AS DECIMAL,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pdeMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param piMessageParam An int64 message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        piMessageParam AS INT64,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, piMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param piMessageParam An integer message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        piMessageParam AS INTEGER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, piMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param plMessageParam An int64 message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        plMessageParam AS LOGICAL,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, plMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param preMessageParam A recid message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        preMessageParam AS RECID,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, preMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param proMessageParam A rowid message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        proMessageParam AS ROWID,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, proMessageParam), 
                                            STRING (poMessageSubSystem)) .
    
    END.
        
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4)) .
        
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @param pcMessageParam7 A character message parameter to substitute &7
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @param pcMessageParam7 A character message parameter to substitute &7
        @param pcMessageParam8 A character message parameter to substitute &8
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER,
                                                        pcMessageParam8 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7,
                                                        pcMessageParam8)) .
    
    END.
                                                      
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &2
        @param pcMessageParam4 A character message parameter to substitute &2
        @param pcMessageParam5 A character message parameter to substitute &2
        @param pcMessageParam6 A character message parameter to substitute &2
        @param pcMessageParam7 A character message parameter to substitute &2
        @param pcMessageParam8 A character message parameter to substitute &2
        @param pcMessageParam9 A character message parameter to substitute &2
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER,
                                                        pcMessageParam8 AS CHARACTER,
                                                        pcMessageParam9 AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7,
                                                        pcMessageParam8,
                                                        pcMessageParam9)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam A character message parameter to substitute &1
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, pcMessageParam),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4),
                                            STRING (poMessageSubSystem)) .
        
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @param pcMessageParam7 A character message parameter to substitute &7
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &3
        @param pcMessageParam4 A character message parameter to substitute &4
        @param pcMessageParam5 A character message parameter to substitute &5
        @param pcMessageParam6 A character message parameter to substitute &6
        @param pcMessageParam7 A character message parameter to substitute &7
        @param pcMessageParam8 A character message parameter to substitute &8
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER,
                                                        pcMessageParam8 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7,
                                                        pcMessageParam8),
                                            STRING (poMessageSubSystem)) .
    
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                            
        @param pcMessagePattern A character message to write to the log file
        @param pcMessageParam1 A character message parameter to substitute &1
        @param pcMessageParam2 A character message parameter to substitute &2
        @param pcMessageParam3 A character message parameter to substitute &2
        @param pcMessageParam4 A character message parameter to substitute &2
        @param pcMessageParam5 A character message parameter to substitute &2
        @param pcMessageParam6 A character message parameter to substitute &2
        @param pcMessageParam7 A character message parameter to substitute &2
        @param pcMessageParam8 A character message parameter to substitute &2
        @param pcMessageParam9 A character message parameter to substitute &2
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteFormattedMessage (pcMessagePattern AS CHARACTER,
                                                        pcMessageParam1 AS CHARACTER,
                                                        pcMessageParam2 AS CHARACTER,
                                                        pcMessageParam3 AS CHARACTER,
                                                        pcMessageParam4 AS CHARACTER,
                                                        pcMessageParam5 AS CHARACTER,
                                                        pcMessageParam6 AS CHARACTER,
                                                        pcMessageParam7 AS CHARACTER,
                                                        pcMessageParam8 AS CHARACTER,
                                                        pcMessageParam9 AS CHARACTER,
                                                        poMessageSubSystem AS MessageSubSystem):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (SUBSTITUTE (pcMessagePattern, 
                                                        pcMessageParam1, 
                                                        pcMessageParam2,
                                                        pcMessageParam3,
                                                        pcMessageParam4,
                                                        pcMessageParam5,
                                                        pcMessageParam6,
                                                        pcMessageParam7,
                                                        pcMessageParam8,
                                                        pcMessageParam9),
                                            STRING (poMessageSubSystem)) .
    
    END.
         
    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the client logfiles                                                                        
        Notes:   The second character parameter allows to specify that this message
                 should only appear in the client logfile when a certain custom
                 log entry type (CustomLogEntries) is activated.     
        @param pcMessage The message text
        @param pcMessageSubSys The custom log entry type for the message                                                                                    
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pcMessage AS CHARACTER,
                                               pcMessageSubSys AS CHARACTER):
          
        /* Mike Fechner, Consultingwerk Ltd. 21.03.2011
           AppServers do always have a client logfile */
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN DO:
            IF pcMessageSubSys <> "":U AND pcMessageSubSys <> "APPL":U AND pcMessageSubSys <> "USER":U THEN DO: 
                IF ListHelper:EntryIsInList (pcMessageSubSys, LogManager:CustomLogEntries) OR pcMessageSubSys = "LogManager":U THEN DO:
                    ASSIGN pcMessageSubSys = ENTRY (LOOKUP (pcMessageSubSys, LogManager:CustomLogEntries), LogManager:CustomLogEntries) .
                     
                    LOG-MANAGER:WRITE-MESSAGE (pcMessage, pcMessageSubSys) .
                END.
            END.
            ELSE    
                LOG-MANAGER:WRITE-MESSAGE (pcMessage, pcMessageSubSys) .

            RETURN TRUE . 
        END.             
        
        RETURN FALSE.
        
        CATCH e AS Progress.Lang.Error :
            RETURN FALSE.
        END CATCH.                                               
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the client logfiles                                                                        
        Notes:   The second character parameter allows to specify that this message
                 should only appear in the client logfile when a certain custom
                 log entry type (CustomLogEntries) is activated.     
        @param pcMessage The message text
        @param poMessageSubSystem The MessageSubSystem of the custom log entry type for the message
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pcMessage AS CHARACTER,
                                               poMessageSubSystem AS MessageSubSystem):

        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LogManager:WriteMessage (pcMessage, 
                                            STRING (poMessageSubSystem)) .  

    END METHOD .     
    
    /*------------------------------------------------------------------------------
        Purpose: Writes a character message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:      
        @param pcMessage A character message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pcMessage AS CHARACTER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (pcMessage) .
          
        RETURN FALSE .                                                
    END.
   
    /*------------------------------------------------------------------------------
        Purpose: Writes an date message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                                                                        
        @param pdaMessage A date message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pdaMessage AS DATE):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (STRING (pdaMessage)) .
          
        RETURN FALSE .                                                
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes an datetime message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:              
        @param pdaMessage A datetime message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pdaMessage AS DATETIME):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (STRING (pdaMessage)) .
          
        RETURN FALSE .                                                
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes an decimal message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:     
        @param pdeMessage A decimal message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (pdeMessage AS DECIMAL):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (STRING (pdeMessage)) .
          
        RETURN FALSE .                                                
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Writes an integer message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:      
        @param piMessage A integer message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (piMessage AS INTEGER):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (STRING (piMessage)) .
          
        RETURN FALSE .                                                
    END.

    /*------------------------------------------------------------------------------
        Purpose: Writes an logical message to the client logile and the default 
                 logging sub system.                                                                      
        Notes:                 
        @param plMessage A logical message to write to the log file
        @return Logical value indicating if the message has been written to the log file                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL WriteMessage (plMessage AS LOGICAL):
    
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
            RETURN LOG-MANAGER:WRITE-MESSAGE (STRING (plMessage)) .
          
        RETURN FALSE .                                                
    END.

    /*------------------------------------------------------------------------------
        Purpose: Clears the logfile.                                                                      
        Notes:   Errors from the LOG-MANAGER will be caught and ignored.
        @return Logical value indicating if the method was successful                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL ClearLogFile () :

        IF LOG-MANAGER:LOGFILE-NAME <> ? THEN DO:
            LOG-MANAGER:CLEAR-LOG () .
            RETURN TRUE.
        END.
        
        RETURN FALSE.
        
        CATCH e AS Progress.Lang.Error :
            RETURN FALSE.                
        END CATCH.        
        
    END METHOD . 
           
    /*------------------------------------------------------------------------------
        Purpose: Returns if currently a logfile is open.                                                                      
        Notes:
        @return Logical value indicating if a log file is open                                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL LogFileIsOpen () :
        
        RETURN LOG-MANAGER:LOGFILE-NAME <> ? .
        
        CATCH e AS Progress.Lang.Error :
            RETURN FALSE . 
        END CATCH.
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the currently active logging level for a certain log file
                 entry                                                                       
        Notes:   Errors from the LOG-MANAGER will be ignored      
        @param pcEntry The entry type
        @return The current logging level of the entry type                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER GetLoggingLevel (pcEntry AS CHARACTER):
        
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntry    AS CHARACTER NO-UNDO.
        
        DO i = 1 TO NUM-ENTRIES(LOG-MANAGER:LOG-ENTRY-TYPES):
            ASSIGN cEntry = ENTRY(i, LOG-MANAGER:LOG-ENTRY-TYPES) .
            
            IF cEntry = pcEntry THEN RETURN LOG-MANAGER:LOGGING-LEVEL .
             
            ELSE IF NUM-ENTRIES(cEntry, ":":U) = 2 AND ENTRY(1, cEntry, ":":U) = pcEntry THEN 
                RETURN INTEGER (ENTRY(2, cEntry, ":":U)) .
        END.
        
        CATCH e AS Progress.Lang.Error :
            RETURN 0 .                
        END CATCH.        
    END.
    
    /*------------------------------------------------------------------------------
        Purpose: Sets the active logging level for a certain log file entry                                                                       
        Notes:   Errors from the LOG-MANAGER will be thrown to the caller.                                                                        
        @param pcEntry The entry type
        @param piValue The new logging level of the entry type                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetLoggingLevel (pcEntry AS CHARACTER, 
                                               piValue AS INTEGER):
        
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cEntry           AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO .
        DEFINE VARIABLE cNewValue        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cCurrentSettings AS CHARACTER NO-UNDO .
        
        DEFINE VARIABLE oError           AS AppError  NO-UNDO . 
        
        ASSIGN cCurrentSettings = LOG-MANAGER:LOG-ENTRY-TYPES .
        
        IF cCurrentSettings > "":U THEN . 
        ELSE 
            ASSIGN cCurrentSettings = "":U .
        
        DO i = 1 TO NUM-ENTRIES(cCurrentSettings):
            ASSIGN cEntry = ENTRY(i, cCurrentSettings) .
            
            IF cEntry = pcEntry THEN DO:
                ASSIGN lFound = TRUE .
                LEAVE .
            END. 
                 
            ELSE IF NUM-ENTRIES(cEntry, ":":U) = 2 AND ENTRY(1, cEntry, ":":U) = pcEntry THEN DO:
                ASSIGN lFound = TRUE . 
                LEAVE . 
            END.                 
        END.        

        IF piValue = 0 THEN DO:
            IF lFound THEN DO:
                ASSIGN cNewValue = cCurrentSettings 
                
                       ENTRY(i, cNewValue) = "":U 
                       cNewValue = REPLACE(cNewValue, ",,":U, ",":U)
                       
                       LOG-MANAGER:LOG-ENTRY-TYPES = cNewValue . 
            END. 
            RETURN .             
        END.

        IF piValue = LOG-MANAGER:LOGGING-LEVEL THEN 
            ASSIGN cEntry = pcEntry .  
        ELSE ASSIGN cEntry = pcEntry + ":":U + STRING(piValue) .

        IF lFound THEN  
            ASSIGN cNewValue = cCurrentSettings
                   ENTRY(i, cNewValue) = cEntry .         
        ELSE DO:
            ASSIGN cNewValue = cCurrentSettings + 
                               (IF cCurrentSettings > "":U THEN ",":U ELSE "":U) +
                               cEntry . 
        END. 

        LOG-MANAGER:LOG-ENTRY-TYPES = cNewValue NO-ERROR . 

        IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            oError = NEW AppError () . 
            
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                oError:AddMessage (ERROR-STATUS:GET-MESSAGE (1),
                                   ERROR-STATUS:GET-NUMBER (1)) .
            END.
            
            UNDO, THROW oError .                                    
        END.                                      

        /* catch should be in calling method, to enable error handling */
    END.
                                                   
    /*------------------------------------------------------------------------------
        Purpose: Sets the active logging level for a certain log file entry                                                                       
        Notes:   Errors from the LOG-MANAGER will be thrown to the caller.
        @param pcEntry The log entry type 
        @param poLoggingLevel The LoggingLevelEnum to set for the log entry type                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetLoggingLevel (pcEntry AS CHARACTER, 
                                               poLoggingLevel AS LoggingLevelEnum):
    
        IF VALID-OBJECT (poLoggingLevel) THEN     
            LogManager:SetLoggingLevel (pcEntry, INTEGER(poLoggingLevel:GetValue())) .
        ELSE 
            LogManager:SetLoggingLevel (pcEntry, 0) .
    END .                                     

	/*------------------------------------------------------------------------------
        Purpose: Writes the properties of a class based object to the open logfile
        Notes:   Returns silently when no logfile is open
        @param poObject The reference of the object to write 
        @param pcTitle The title used as a prefix of the logfile output
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID WriteObjectProperties (poObject AS Progress.Lang.Object,
	                                                 pcTitle AS CHARACTER):
		
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN . 
        ELSE 
            RETURN .  

        LogManager:WriteObjectProperties (poObject, pcTitle, 0) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes the properties of a class based object to the open logfile
        Notes:   Returns silently when no logfile is open
        @param poObject The reference of the object to write 
        @param pcTitle The title used as a prefix of the logfile output
        @param piIndent Number of characters to indent the output for recursive calls 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID WriteObjectProperties (poObject AS Progress.Lang.Object,
                                                        pcTitle AS CHARACTER,
                                                        piIndent AS INTEGER):
    
&IF DEFINED (AblReflection) NE 0 &THEN
        DEFINE VARIABLE oProperty   AS Progress.Reflect.Property NO-UNDO . 
        DEFINE VARIABLE oProperties AS Progress.Reflect.Property NO-UNDO EXTENT .
        DEFINE VARIABLE cFill       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cType       AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cValue      AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE oValue      AS Progress.Lang.Object      NO-UNDO .
        DEFINE VARIABLE iIndex      AS INTEGER                   NO-UNDO . 
        
        ASSIGN cFill = FILL ("    ":U, piIndent) .
    
        IF NOT VALID-OBJECT (poObject) THEN DO: 
            LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1&2 : <invalid object reference>":U,
                                                   cFill, 
                                                   pcTitle)) .  
                                                   
            RETURN . 
        END.
        
        IF piIndent = 0 THEN 
            LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1&2 <Object of Type: &3>:":U,
                                                   cFill, 
                                                   pcTitle,
                                                   poObject:GetClass():TypeName)) .  
        
        
        oProperties = poObject:GetClass ():GetProperties(Progress.Reflect.Flags:Public:SetFlag (Progress.Reflect.Flags:Instance)) .
        
        DO i = 1 TO EXTENT (oProperties):
            ASSIGN oProperty = oProperties [i] 
                   cValue    = ? 
                   oValue    = ?.
            
            /* Skip Next-Sibling and Prev-Sibling */
            IF oProperty:Name = "Next-Sibling":U OR oProperty:Name = "Prev-Sibling":U THEN  
                NEXT .              
            
            IF VALID-OBJECT (oProperty:DataType) AND oProperty:DataType <> Progress.Reflect.DataType:Object THEN 
                ASSIGN cType = STRING (oProperty:DataType) .
            ELSE 
                ASSIGN cType = oProperty:DataTypeName .

            CASE oProperty:DataType:
                
                WHEN Progress.Reflect.DataType:Object THEN DO ON ERROR UNDO, THROW:
                    oValue = oProperty:Get (poObject) .
                    
                    IF VALID-OBJECT (oValue) THEN 
                        cValue = oValue:ToString () .
                    ELSE 
                        cValue = "<invalid object reference>":U . 
                    
                    CATCH err AS Progress.Lang.Error :
                        cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .   
                    END CATCH.
                END .
                OTHERWISE DO ON ERROR UNDO, THROW:
                    cValue = STRING (oProperty:Get (poObject)) .
                    
                    CATCH err AS Progress.Lang.Error :
                    	cValue = SUBSTITUTE ("Error: &1":U, err:GetMessage (1)) .	
                    END CATCH.
                END.
                
            END CASE . 
            
            LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1&2:&3 <&4> = &5":U,
                                                   cFill, 
                                                   pcTitle,
                                                   oProperty:Name,
                                                   cType, 
                                                   cValue)) .  

            /* Recurse Objects, but not Next-Sibling and Prev-Sibling */
            IF oProperty:DataTypeName > "":U THEN 
                LogManager:WriteObjectProperties (oProperty:Get (poObject), oProperty:Name, piIndent + 1) .

            /* Avoid infinite recursion caused by objects referencing themselves (circular references) */
            IF piIndent < 50 THEN DO:
            
                /* Iterate IEnumerable, but not TableModel as that would dump all records */ 
                IF VALID-OBJECT (oValue) AND TYPE-OF (oValue, IEnumerable) AND NOT TYPE-OF (oValue, TableModel) THEN DO:
                    ASSIGN iIndex = 1 .
    
                    {Consultingwerk/foreachABL.i Progress.Lang.Object oItem in oValue}
    
                        LogManager:WriteObjectProperties (oItem, SUBSTITUTE ("&1[&2]":U, oProperty:Name, iIndex), piIndent + 2) .
                        
                        iIndex = iIndex + 1 .
                    END.
                END.
            END.    
        END.
        
&ELSE 
        UNDO, THROW NEW NotSupportedException ("WriteObjectProperties":U,
                                               "Consultingwerk.Util.LogManager":U,
                                               "The use of WriteObjectPropertise requires OpenEdge 11.6 or later."{&TRAN}) .
&ENDIF

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Writes a line full of ------------- into the current logfile 
	    Notes:   
	------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID WriteSeparator ():
	    
	    IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN 
	        LOG-MANAGER:WRITE-MESSAGE (FILL ("-":U, 40)) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes the current stack trace to the log file 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteStackTrace ():
        
        DEFINE VARIABLE cStacktrace AS CHARACTER NO-UNDO.

        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN DO: 
            cStacktrace = Consultingwerk.Util.ErrorHelper:CurrentStacktrace .
            
            LogManager:WriteMessage (cStacktrace) .
        END.

    END METHOD .

&IF NOT PROVERSION BEGINS "10.2":U &THEN
    /*------------------------------------------------------------------------------
        Purpose: Writes a Character Message to the AppServer Logfile 
        Notes:   Invokable from the client, only supported from OpenEdge 11 on
        @param pcMessage The Message to write to the AppServer Logfile
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID WriteToServerLogfile (pcMessage AS CHARACTER):
		
		DEFINE VARIABLE oMessage AS CharacterHolder NO-UNDO . 

        oMessage = NEW CharacterHolder (pcMessage) .
        
        FrameworkSettings:ServiceAdapter:InvokeTask 
                ("":U,
                 "Consultingwerk.Framework.Server.ServerHelperTask":U,
                 "WriteToLogFile":U,
                 oMessage,
                 SessionManager:ContextDataset) .

	END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Writes the properties of a widget handle to the open logfile
        Notes:   Returns silently when no logfile is open
        @param phWidget The handle of the widget 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID WriteWidgetProperties (phWidget AS HANDLE):

        LogManager:WriteWidgetProperties (phWidget, ?) .
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Writes the properties of a widget handle to the open logfile
        Notes:   Returns silently when no logfile is open
        @param phWidget The handle of the widget 
        @param pcTitle The title used as a prefix of the logfile output 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID WriteWidgetProperties (phWidget AS HANDLE,
	                                                 pcTitle AS CHARACTER):
		
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cText       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAttributes AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAttribute  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE    NO-UNDO .
        
        IF LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE OR SESSION:CLIENT-TYPE = "WEBSPEED":U THEN . 
        ELSE 
            RETURN .  
        
        IF NOT VALID-HANDLE (phWidget) THEN DO: 
            LOG-MANAGER:WRITE-MESSAGE ("<invalid widget handle>"{&TRAN}) .
            RETURN .  
        END. 

        IF pcTitle > "":U THEN
            ASSIGN pcTitle = SUBSTITUTE ("&1 - &2":U, pcTitle, phWidget:TYPE) . 
        ELSE 
            ASSIGN pcTitle = phWidget:TYPE . 

        ASSIGN cAttributes = ListHelper:SortList(LIST-QUERY-ATTRS (phWidget)) .

        loop: DO i = 1 TO NUM-ENTRIES (cAttributes):

            ASSIGN cAttribute = ENTRY (i, cAttributes) . 

            /* Mike Fechner, Consultingwerk Ltd. 13.07.2015
               Need to verify as some methods called as attributes (like SERIALIZE-ROW on the buffer object handle)
               may kill the AVM (call #00318812)  */
            IF NOT WidgetAttributes:IsValid (cAttribute) THEN 
                NEXT loop . 

            DO ON ERROR UNDO, THROW:
                CREATE CALL hCall.
                
                ASSIGN hCall:IN-HANDLE = phWidget 
                       hCall:CALL-TYPE = GET-ATTR-CALL-TYPE  
                       hCall:CALL-NAME = cAttribute . 
        
                hCall:INVOKE () NO-ERROR . 

                IF ERROR-STATUS:NUM-MESSAGES > 0 THEN 
                    NEXT loop . 

                LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("&1 : &2 = &3":U, 
                                                       pcTitle,
                                                       cAttribute,
                                                       hCall:RETURN-VALUE)) .
                
                CATCH err AS Progress.Lang.Error:
                    NEXT loop .     
                END CATCH.
                
                FINALLY:
                    Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hCall) .       
                END FINALLY.
            END. 
        END.

	END METHOD .

END CLASS.
