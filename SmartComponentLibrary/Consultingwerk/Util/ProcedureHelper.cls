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
    File        : ProcedureHelper
    Purpose     : Provides support routines dealing with access to procedures 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 08 15:27:16 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.Util.ProcedureHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED ProcedureHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Searches for the first instance of a persistent procedure with the 
                 given name
        Notes:   
        @param pcFileName The file name of the persistent procedure to search for
        @return The handle of the persistent procedure instance of ? when no instance is found
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC HANDLE FindPersistentProcedure (pcFileName AS CHARACTER):

        DEFINE VARIABLE hProcedure AS HANDLE NO-UNDO.

        ASSIGN hProcedure = SESSION:FIRST-PROCEDURE.

        DO WHILE VALID-HANDLE(hProcedure):
            IF hProcedure:FILE-NAME = pcFileName THEN 
                RETURN hProcedure . 
            
            ASSIGN hProcedure = hProcedure:NEXT-SIBLING . 
        END.  
        
        RETURN ? . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:    
        @param phProcedure The handle of the persistent procedure
        @param pcEntry The name of an internal procedure or user defined function                                                                    
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntry (phProcedure AS HANDLE,
                                           pcEntry AS CHARACTER):
        
        DEFINE VARIABLE hSuperProcedure AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i               AS INTEGER NO-UNDO.
        
        IF ListHelper:EntryIsInList (pcEntry, phProcedure:INTERNAL-ENTRIES) THEN 
            RETURN TRUE. 
        
        DO i = 1 TO NUM-ENTRIES(phProcedure:SUPER-PROCEDURES):
            ASSIGN hSuperProcedure = WIDGET-HANDLE(ENTRY(i, phProcedure:SUPER-PROCEDURES)) .
            
            IF NOT VALID-HANDLE(hSuperProcedure) THEN 
                NEXT . 

            IF ListHelper:EntryIsInList(pcEntry, hSuperProcedure:INTERNAL-ENTRIES) THEN 
                RETURN TRUE. 
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:  
        @param phProcedure The handle of the persistent procedure
        @param pcEntry The name of an internal procedure or user defined function                                                                    
        @param poType The entry type as an EntryTypeEnum member 
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntry (phProcedure AS HANDLE,
                                           pcEntry AS CHARACTER,
                                           poType AS EntryTypeEnum):
                                               
        RETURN ProcedureHelper:HasEntry (phProcedure,
                                         pcEntry,
                                         poType:ToString()) .                                             
                                               
    END METHOD .                                                
                                               
    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:  
        @param phProcedure The handle of the persistent procedure
        @param pcEntry The name of an internal procedure or user defined function                                                                    
        @param pcType The entry type, PROCEDURE or FUNCTION       
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntry (phProcedure AS HANDLE,
                                           pcEntry AS CHARACTER,
                                           pcType AS CHARACTER):
        
        DEFINE VARIABLE hSuperProcedure AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i               AS INTEGER NO-UNDO.
        
        IF ListHelper:EntryIsInList (pcEntry, phProcedure:INTERNAL-ENTRIES) AND 
           ENTRY (1, phProcedure:GET-SIGNATURE(pcEntry)) = pcType THEN 
            RETURN TRUE. 
        
        DO i = 1 TO NUM-ENTRIES(phProcedure:SUPER-PROCEDURES):
            ASSIGN hSuperProcedure = WIDGET-HANDLE(ENTRY(i, phProcedure:SUPER-PROCEDURES)) .
            
            IF NOT VALID-HANDLE(hSuperProcedure) THEN 
                NEXT . 

            IF ListHelper:EntryIsInList (pcEntry, hSuperProcedure:INTERNAL-ENTRIES) AND 
               ENTRY (1, hSuperProcedure:GET-SIGNATURE(pcEntry)) = pcType THEN 
                RETURN TRUE. 
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:   Uses MATCHES to compare pcEntry to the name of the internal entries           
        @param phProcedure The handle of the procedure                                                          
        @param pcEntry The search pattern for internal entries
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntryMatching (phProcedure AS HANDLE,
                                                   pcEntry AS CHARACTER):
        
        DEFINE VARIABLE hSuperProcedure AS HANDLE    NO-UNDO.
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO.
        
        IF HasEntryMatchingInternal (phProcedure, pcEntry) > "":U THEN 
            RETURN TRUE. 
        
        DO i = 1 TO NUM-ENTRIES(phProcedure:SUPER-PROCEDURES):
            ASSIGN hSuperProcedure = WIDGET-HANDLE(ENTRY(i, phProcedure:SUPER-PROCEDURES)) .
            
            IF NOT VALID-HANDLE(hSuperProcedure) THEN 
                NEXT . 
        
            IF HasEntryMatchingInternal (phProcedure, pcEntry) > "":U THEN 
                RETURN TRUE. 
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:   Uses MATCHES to compare pcEntry to the name of the internal entries                                                                     
        @param phProcedure The handle of the procedure                                                          
        @param pcEntry The search pattern for internal entries
        @param poType The entry type as an EntryTypeEnum member                                                                     
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntryMatching (phProcedure AS HANDLE,
                                                   pcEntry AS CHARACTER,
                                                   poType AS EntryTypeEnum):

        RETURN ProcedureHelper:HasEntryMatching (phProcedure,
                                                 pcEntry,
                                                 poType:ToString()) .                                             

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 given entry (internal procedures or user defined functions) and thus 
                 the given entry is callable (RUN or DYNAMIC-FUNCTION).                                                                       
        Notes:   Uses MATCHES to compare pcEntry to the name of the internal entries                                                                     
        @param phProcedure The handle of the procedure                                                          
        @param pcEntry The search pattern for internal entries
        @param pcType The entry type, e.g. PROCEDURE or FUNCTION
        @return TRUE when an entry is found, else FALSE 
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasEntryMatching (phProcedure AS HANDLE,
                                                   pcEntry AS CHARACTER,
                                                   pcType AS CHARACTER):
        
        DEFINE VARIABLE hSuperProcedure AS HANDLE  NO-UNDO.
        DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        
        cEntry = HasEntryMatchingInternal (phProcedure, pcEntry) .
        
        IF cEntry > "":U AND 
           ENTRY (1, phProcedure:GET-SIGNATURE(cEntry)) = pcType THEN 
            RETURN TRUE. 
        
        DO i = 1 TO NUM-ENTRIES(phProcedure:SUPER-PROCEDURES):
            ASSIGN hSuperProcedure = WIDGET-HANDLE(ENTRY(i, phProcedure:SUPER-PROCEDURES)) .
            
            IF NOT VALID-HANDLE(hSuperProcedure) THEN 
                NEXT . 

            cEntry = HasEntryMatchingInternal (hSuperProcedure, pcEntry) .
        
            IF cEntry > "":U  AND 
               ENTRY (1, hSuperProcedure:GET-SIGNATURE(cEntry)) = pcType THEN 
                RETURN TRUE. 
        END.
        
        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Searches the passed in procedure handle for an entry using a search    
                 pattern.                                                                      
        Notes:   Uses MATCHES to compare pcEntry to the name of the internal entries                                                                     
        @param phProcedure The handle of the procedure                                                          
        @param pcEntry The search pattern for internal entries
        @return The name of the matching entry name or ? if none is found
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC CHARACTER HasEntryMatchingInternal (phProcedure AS HANDLE,
                                                                pcEntry AS CHARACTER):
        
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntry      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iNumEntries AS INTEGER   NO-UNDO.

        ASSIGN iNumEntries = NUM-ENTRIES (phProcedure:INTERNAL-ENTRIES) .

        DO i = 1 TO iNumEntries:
            ASSIGN cEntry = ENTRY (i, phProcedure:INTERNAL-ENTRIES) .
            
            IF cEntry MATCHES pcEntry THEN 
                RETURN cEntry . 
        END.

        RETURN ? . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 named internal procedures and thus the named internal procedue is 
                 callable (RUN).                                                                      
        Notes:
        @param phProcedure The handle of the procedure to validate
        @param pcEntry The name of the internal procedure to test for
        @return Logical value indicating if the internal procedure exists                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasInternalProcedure (phProcedure AS HANDLE,
                                                       pcEntry AS CHARACTER):
        
        RETURN HasEntry (phProcedure, pcEntry, "PROCEDURE":U) .
        
    END METHOD.    

    /*------------------------------------------------------------------------------
        Purpose: Returns true if the procedure or it's super-procedures contains the
                 named user defined function internal procedures and thus the user 
                 defined function is callable (DYNAMIC-FUNCTION).                                                                     
        Notes:                                                                        
        @param phProcedure The handle of the procedure to validate
        @param pcEntry The name of the user defined function to test for
        @return Logical value indicating if the user defined function exists                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC LOGICAL HasUserDefinedFunction (phProcedure AS HANDLE,
                                                         pcEntry AS CHARACTER):
        
        RETURN HasEntry (phProcedure, pcEntry, "FUNCTION":U) .
        
    END METHOD.   

    /*------------------------------------------------------------------------------
        Purpose: Returns the file-name of the current procedure handle without the 
                 pathname and without the extension                                                                       
        Notes:
        @param phProcedureHandle The handle to the procedure
        @return The short name of the procedure file                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ShortDotPName (phProcedureHandle AS HANDLE):
        
        DEFINE VARIABLE iSlash     AS INTEGER NO-UNDO.
        DEFINE VARIABLE iBackSlash AS INTEGER NO-UNDO.
        DEFINE VARIABLE iMax       AS INTEGER NO-UNDO.
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phProcedureHandle ""PROCEDURE"":U} .
        
        ASSIGN iSlash = R-INDEX (phProcedureHandle:FILE-NAME, "/":U)
               iBackSlash = R-INDEX (phProcedureHandle:FILE-NAME, "~\":U) .

        iMax = MAX (iSlash, iBackSlash) .

        IF iMax > 0 THEN 
            RETURN ENTRY (1, 
                          SUBSTRING (phProcedureHandle:FILE-NAME, iMax + 1),
                          ".":U) .
    
        RETURN ENTRY (1,
                      phProcedureHandle:FILE-NAME,
                      ".":U) .             
    END METHOD.

 END CLASS.
 