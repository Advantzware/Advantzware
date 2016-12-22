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
    File        : LogFileParser
    Purpose     : Parser for Progress client log files 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 08:37:34 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* .
USING Consultingwerk.Studio.ProjectMinimizer.* . 
USING Progress.Lang.*.

&global-define dlm CHR(10)

CLASS Consultingwerk.Studio.ProjectMinimizer.LogFileParser: 

    { Consultingwerk/Studio/ProjectMinimizer/ttProcedures.i &ACCESS="PRIVATE"}

    DEFINE PRIVATE VARIABLE lcLogFile AS LONGCHAR NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the parsed progress changes                                                                      
        Notes:                                          
        @param sender The reference to the object that raised the event
        @param e The ProgressChangedEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ProgressChanged SIGNATURE VOID 
            (sender AS Progress.Lang.Object, 
             e AS ProgressChangedEventArgs). 

    /*------------------------------------------------------------------------------
        Purpose: Returns the log file name                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LogFileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the ttProcedures buffer handle                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TempTableBuffer AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogFileParser class                                                                         
        Notes:                                          
        @param pcLogFile The file name of the log file to parse                               
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LogFileParser (pcLogFile AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:LogFileName     = pcLogFile 
               THIS-OBJECT:TempTableBuffer = BUFFER ttProcedures:HANDLE 
               .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Extracts the procedure information form a client log file line
        Notes:                                                                 
        @param pcLine The client log file line to parse
        @return The procedure information       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetExecutingProcedure (pcLine AS CHARACTER):

        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

        ASSIGN iIndex = INDEX (pcLine, "[":U) .
        
        IF iIndex = 0 THEN 
            RETURN "".
        ELSE 
            pcLine = SUBSTRING (pcLine, iIndex + 1) . 
        
        ASSIGN iIndex = INDEX (pcLine, "]":U) .
        
        IF iIndex < 2 THEN 
            RETURN "".
        ELSE 
            pcLine = SUBSTRING (pcLine, 1, iIndex - 1) .

        IF INDEX (pcLine, " - ":U) = 0 THEN 
            RETURN pcLine . 
        ELSE DO:
            ASSIGN pcLine = SUBSTRING (pcLine, INDEX (pcLine, " - ":U) + 3)  .
            
            IF INDEX (pcLine, " @ ":U) = 0 THEN 
                RETURN pcLine . 
            ELSE 
                RETURN SUBSTRING (pcLine, 1, INDEX (pcLine, " @ ":U) - 1) .    
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ProgressChanged event                                                                        
        Notes:      
        @param e The ProgressChangedEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnProgressChanged (e AS ProgressChangedEventArgs):
        
        EventArgsAssert:IsValid (e, "ProgressChanged":U) .
        
        THIS-OBJECT:ProgressChanged:Publish (THIS-OBJECT, e) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Starts parsing of the log file                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseLogFile ():
        
        DEFINE VARIABLE i         AS INTEGER                  NO-UNDO.
        DEFINE VARIABLE cLine     AS CHARACTER                NO-UNDO.
        DEFINE VARIABLE cGroup    AS CHARACTER                NO-UNDO.
        DEFINE VARIABLE cSubGroup AS CHARACTER                NO-UNDO.
        DEFINE VARIABLE cText     AS CHARACTER                NO-UNDO.
        DEFINE VARIABLE iCount    AS INTEGER                  NO-UNDO.
        DEFINE VARIABLE e         AS ProgressChangedEventArgs NO-UNDO . 
        
        COPY-LOB FROM FILE THIS-OBJECT:LogFileName TO lcLogFile . 
        
        ASSIGN iCount = NUM-ENTRIES (lcLogFile, {&dlm}) .
        
        DO i = 1 TO iCount:

            ASSIGN cLine = ENTRY (i, lcLogFile, {&dlm}) . 

            IF cLine = ? OR LENGTH (cLine) < 52 THEN NEXT . 

            IF NOT VALID-OBJECT (e) THEN 
                e = NEW ProgressChangedEventArgs (i, iCount) .    
            ELSE 
                ASSIGN e:Value = i
                       e:Maximum = iCount . 
                
            OnProgressChanged (e) .                

            /* Ignore first 50 characters */
            Consultingwerk.Studio.LogFileHelper:ParseClientLogLine (cLine,
                                                                    OUTPUT cGroup,
                                                                    OUTPUT cSubGroup,
                                                                    OUTPUT cText) .

            IF cGroup = "4GL":U AND cSubGroup = "4GLTrace":U THEN 
                ProcessLine (cText) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Processes a single line of the log file                                                                        
        Notes:                                  
        @param pcLine The line of the log file to parse                                      
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessLine (pcLine AS CHARACTER):
        
        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
        DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
        
        ASSIGN cLine = pcLine .

        IF cLine BEGINS "Return from":U THEN DO:
            GetExecutingProcedure (cLine) .
            
            IF GetExecutingProcedure (cLine) = "-":U THEN MESSAGE "- in ":U cLine
                                                              VIEW-AS ALERT-BOX.
            
            StoreProcedure (GetExecutingProcedure (cLine)) .
        END.
        ELSE IF cLine BEGINS "Run":U OR cLine BEGINS "New":U OR cLine BEGINS "Super":U THEN DO:
            StoreProcedure (ENTRY (2, cLine, " ":U)) .
            
            IF GetExecutingProcedure (cLine) = "-":U THEN MESSAGE "- in ":U cLine
                                                              VIEW-AS ALERT-BOX.
            
            StoreProcedure (GetExecutingProcedure (cLine)) .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the procedure name in the ttProcedure temp-table                                                                       
        Notes:                                                          
        @param pcProcedure The procedure name to store in the ttProcedure temp-table              
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreProcedure (pcProcedure AS CHARACTER):
        
        DEFINE BUFFER ttProcedures FOR ttProcedures . 
        
        IF pcProcedure > "":U THEN . 
        ELSE RETURN .
        
        IF CAN-FIND (ttProcedures WHERE ttProcedures.RunName = pcProcedure) THEN
            RETURN . 
            
        CREATE ttProcedures . 
        ASSIGN ttProcedures.RunName = pcProcedure .               

    END METHOD.

END CLASS.
