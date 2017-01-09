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
    File        : LlReport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 14 19:57:02 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                      FROM PROPATH . 
USING Consultingwerk.Assertion.*            FROM PROPATH . 
USING Consultingwerk.Exceptions.*           FROM PROPATH . 
USING Consultingwerk.Util.*                 FROM PROPATH . 
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlReport: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the dataset that provides the data for the 
                 report
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Tile for the Report Designer
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignerTitle AS CHARACTER NO-UNDO INIT "Report Designer"{&TRAN}
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Parent Window 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ParentWindow AS INTEGER NO-UNDO INIT 0  
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the ReportDefinition object used by this report instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ReportDefinition AS ReportDefinition NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Number of Records printed 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RecordsPrinted AS INTEGER NO-UNDO INIT 0
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the path (full pathname preferred) of the sample image
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SampleImagePath AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LlReport class
        Notes:   
        @param poReportDefinition The ReportDefinition object that defines the relations for this report
        @param phDataset The handle of the Dataset that provides the data for the report 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LlReport (poReportDefinition AS ReportDefinition,
                                 phDataset AS HANDLE):
        SUPER ().
        
        ObjectAssert:IsValid (poReportDefinition, "Report Definition"{&TRAN}) .
        Assert:GreaterThanZero (poReportDefinition:ReportBuffers:Count, "Number of Report Buffers"{&TRAN}) .
        
        THIS-OBJECT:ValidateReportBuffers (poReportDefinition, phDataset) .
        
        ASSIGN THIS-OBJECT:ReportDefinition = poReportDefinition
               THIS-OBJECT:DatasetHandle    = phDataset .
        
        THIS-OBJECT:InitializeSampleImage () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Defines Buffer Fields as List & Label Variables
        Notes:   
        @param piJob The List And Label Job Handle
        @param poReportBuffer The ReportBuffer Definition
        @param plAllFields If true all fields will be defined, if false only those fields that are used
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DefineBufferFields (piJob AS INTEGER,
                                              poReportBuffer AS ReportBuffer,
                                              plAllFields AS LOGICAL):
        
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iField           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iFrom            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTo              AS INTEGER   NO-UNDO.
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
        
        ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (poReportBuffer:BufferName) .
        
        DO iField = 1 TO hBuffer:NUM-FIELDS:
    
            ASSIGN hField = hBuffer:BUFFER-FIELD(iField) .
            
            IF hField:EXTENT = 0 THEN  
                ASSIGN iFrom = 0
                       iTo   = 0 .
            ELSE
                ASSIGN iFrom = 1
                       iTo   = hField:EXTENT . 

            DO i = iFrom TO iTo:
                
                IF i = 0 THEN 
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2":U,
                                                    poReportBuffer:BufferName,
                                                    hField:NAME) .
                ELSE                                                                          
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2____&3":U,
                                                    poReportBuffer:BufferName,
                                                    hField:NAME,
                                                    i) .

                CASE hField:DATA-TYPE:
                    WHEN "CHARACTER":U THEN
                        LlApi:DefineField (piJob, cFieldName, cFieldName).
                    WHEN "INTEGER":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, "0":U, LlConst:LL_NUMERIC, 0).
                    WHEN "DECIMAL":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, "0.00":U, LlConst:LL_NUMERIC, 0).
                    WHEN "DATE":U THEN 
                        IF SESSION:DATE-FORMAT = "DMY":U THEN
                          LlApi:DefineFieldExt (piJob, cFieldName, "01.01.2012":U, LlConst:LL_DATE_DMY, 0).
                        ELSE
                          LlApi:DefineFieldExt (piJob, cFieldName, "01.01.2012":U, LlConst:LL_DATE_MDY, 0).
                    WHEN "LOGICAL":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, "yes":U, LlConst:LL_BOOLEAN, 0).
                    WHEN "CLOB":U THEN 
                        LlApi:DefineFieldExt (piJob, cFieldName, "{Consultingwerk/Windows/ListAndLabel/rtf.i}":U, LlConst:LL_RTF, 0).
                    WHEN "BLOB":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, THIS-OBJECT:SampleImagePath, LlConst:LL_DRAWING, 0).
                    WHEN "DATETIME":U THEN
                        LlApi:DefineField (piJob, cFieldName, STRING(NOW, "99.99.9999 HH:MM:SS":U)).    
                    OTHERWISE 
                        UNDO, THROW NEW AppError (SUBSTITUTE ("Datatype &1 not yet supported by reporting engine!"{&TRAN},
                                                              hField:DATA-TYPE), 0) .
                END CASE.
            END.
        END. /* iFields */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Defines Buffer Fields as List & Label Variables
        Notes:   
        @param piJob The List And Label Job Handle
        @param poReportBuffer The ReportBuffer Definition
        @param plAllFields If true all fields will be defined, if false only those fields that are used
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DefineBufferVariables (piJob AS INTEGER,
                                                 poReportBuffer AS ReportBuffer,
                                                 plAllFields AS LOGICAL):
        
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
        DEFINE VARIABLE iField           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iFrom            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iTo              AS INTEGER   NO-UNDO.
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
        
        ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (poReportBuffer:BufferName) .
        
        DO iField = 1 TO hBuffer:NUM-FIELDS:
    
            ASSIGN hField = hBuffer:BUFFER-FIELD(iField) .
            
            IF hField:EXTENT = 0 THEN  
                ASSIGN iFrom = 0
                       iTo   = 0 .
            ELSE
                ASSIGN iFrom = 1
                       iTo   = hField:EXTENT . 

            DO i = iFrom TO iTo:
                
                IF i = 0 THEN 
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2":U,
                                                    poReportBuffer:BufferName,
                                                    hField:NAME) .
                ELSE                                                                          
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2____&3":U,
                                                    poReportBuffer:BufferName,
                                                    hField:NAME,
                                                    i) .

                CASE hField:DATA-TYPE:
        
                    WHEN "CHARACTER":U THEN
                        LlApi:DefineVariable (piJob, cFieldName, cFieldName).
                    WHEN "INTEGER":U THEN
                        LlApi:DefineVariableExt (piJob, cFieldName, "0":U, LlConst:LL_NUMERIC, 0).
                    WHEN "DECIMAL":U THEN
                        LlApi:DefineVariableExt (piJob, cFieldName, "0.00":U, LlConst:LL_NUMERIC, 0).
                    WHEN "DATE":U THEN 
                        IF SESSION:DATE-FORMAT = "DMY":U THEN
                            LlApi:DefineVariableExt (piJob, cFieldName, "01.01.2004":U, LlConst:LL_DATE_DMY, 0).
                        ELSE
                            LlApi:DefineVariableExt (piJob, cFieldName, "01.01.2004":U, LlConst:LL_DATE_MDY, 0).
                    WHEN "LOGICAL":U THEN
                        LlApi:DefineVariableExt (piJob, cFieldName, "yes":U, LlConst:LL_BOOLEAN, 0).
                    WHEN "CLOB":U THEN 
                        LlApi:DefineVariableExt (piJob, cFieldName, "{Consultingwerk/Windows/ListAndLabel/rtf.i}":U, LlConst:LL_RTF, 0).
                    WHEN "BLOB":U THEN
                        LlApi:DefineVariableExt (piJob, cFieldName, THIS-OBJECT:SampleImagePath, LlConst:LL_DRAWING, 0).
                    WHEN "DATETIME":U THEN
                        LlApi:DefineVariable (piJob, cFieldName, STRING(NOW, "99.99.9999 HH:MM:SS":U)).    
                    OTHERWISE 
                        UNDO, THROW NEW AppError (SUBSTITUTE ("Datatype &1 not yet supported by reporting engine!"{&TRAN},
                                                              hField:DATA-TYPE), 0) .
                END CASE.
            END.
        END. /* iFields */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the List & Label Designer
        Notes:   
        @param piJob The List And Label Job Handle
        @param plAllFields If true all fields will be defined, if false only those fields that are used
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DefineFieldsAndVariables (piJob AS INTEGER,
                                                    plAllFields AS LOGICAL):
        
        LlApi:DefineVariableStart (piJob) .
        
        {Consultingwerk/foreachABL.i ReportBuffer oReportBuffer in this-object:ReportDefinition:ReportBuffers}
    
            IF oReportBuffer:DefineBufferAs = DefineBufferAsEnum:Variables THEN 
                DefineBufferVariables (piJob, oReportBuffer, plAllFields) .
        END . 

        LlApi:DefineFieldStart (piJob) .

        /* Mike Fechner, Consultingwerk Ltd. 28.10.2012
           Allow for conditional bands */
        LlApi:DefineField (piJob, "CurrentBufferName":U, "CurrentBufferName":U) .

        {Consultingwerk/foreachABL.i ReportBuffer oReportBuffer in this-object:ReportDefinition:ReportBuffers nodefine}

            IF oReportBuffer:DefineBufferAs = DefineBufferAsEnum:Fields THEN 
                DefineBufferFields (piJob, oReportBuffer, plAllFields) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the path to the sample image
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeSampleImage ():
        
        FILE-INFO:FILE-NAME = "Consultingwerk\Windows\ListAndLabel\painting.png":U .
        
        IF FILE-INFO:FILE-NAME > "":U THEN 
            THIS-OBJECT:SampleImagePath = FILE-INFO:FULL-PATHNAME .
        ELSE 
            UNDO, THROW NEW AppError ("Unable to initialize Sample Image"{&TRAN}, 0) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Outputs the Dataset 
        Notes:   
        @param piJob The List & Label Job handle
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OutputDataset (piJob AS INTEGER):
		
		DEFINE VARIABLE hQuery           AS HANDLE  NO-UNDO .
		DEFINE VARIABLE lRelationsActive AS LOGICAL NO-UNDO .

        ASSIGN lRelationsActive = THIS-OBJECT:DatasetHandle:RELATIONS-ACTIVE 
               hQuery           = QueryHelper:CreatePreparedQuery 
                                        (THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (THIS-OBJECT:ReportDefinition:ParentBufferName)) 
               
               THIS-OBJECT:RecordsPrinted = 0 .
                    
                    
        OutputDatasetQuery (piJob, hQuery) .
        
        FINALLY:
            THIS-OBJECT:DatasetHandle:RELATIONS-ACTIVE = lRelationsActive .
            
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.                    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Outputs a Dataset Query 
        Notes:   
        @param piJob The List & Label Job handle
        @param phQuery The query to iterate
        @return Logical value indicating if the user pressed Cancel
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL OutputDatasetQuery (piJob AS INTEGER,
                                                 phQuery AS HANDLE):
        
        DEFINE VARIABLE hQueryBuffer       AS HANDLE       NO-UNDO .
        DEFINE VARIABLE oReportBuffer      AS ReportBuffer NO-UNDO .
        DEFINE VARIABLE oChildBuffer       AS ReportBuffer NO-UNDO . 
        DEFINE VARIABLE iRet               AS INTEGER      NO-UNDO .
        DEFINE VARIABLE i                  AS INTEGER      NO-UNDO .

        DEFINE VARIABLE hDataRelation      AS HANDLE       NO-UNDO .
        DEFINE VARIABLE hDataRelationQuery AS HANDLE       NO-UNDO .

        phQuery:QUERY-OPEN () .
        phQuery:GET-FIRST () .
        
        ASSIGN hQueryBuffer = phQuery:GET-BUFFER-HANDLE(1) .
      
        oReportBuffer = THIS-OBJECT:ReportDefinition:ReportBuffers:GetItemByBufferName (hQueryBuffer:NAME) .
      
        IF NOT VALID-OBJECT (oReportBuffer) THEN 
            RETURN FALSE . 
            
        loop: DO WHILE hQueryBuffer:AVAILABLE:
            
            ASSIGN THIS-OBJECT:RecordsPrinted = THIS-OBJECT:RecordsPrinted + 1 .  

            IF oReportBuffer:DefineBufferAs = DefineBufferAsEnum:Fields THEN DO:
                
                /* TO-DO: Status display */                
                IF NOT (SESSION:REMOTE OR SESSION:BATCH-MODE) THEN DO:

                    iRet = LlApi:PrintSetBoxText (piJob, 
                                                  SUBSTITUTE ("Printing... &1"{&TRAN}, 
                                                              THIS-OBJECT:RecordsPrinted),
                                                  0).

                    IF iRet = LlConst:LL_ERR_USER_ABORTED THEN DO:
                        RETURN TRUE .
                    END.
                END. /* not (remote or batch-mode) */
                
                LlApi:DefineField (piJob, "CurrentBufferName":U, hQueryBuffer:NAME) .
                
                PrintBufferFields (piJob, oReportBuffer, TRUE) .

                /* Print 1:1 Relations */
                childloop: DO i = 1 TO THIS-OBJECT:ReportDefinition:ReportBuffers:Count:
                   
                    ASSIGN oChildBuffer = THIS-OBJECT:ReportDefinition:ReportBuffers:GetItem (i) .
                   
                    IF oChildBuffer:Multiple = FALSE THEN DO:

                        ASSIGN hDataRelation = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (oChildBuffer:BufferName):PARENT-RELATION .
                        
                        IF NOT VALID-HANDLE (hDataRelation) THEN 
                            NEXT childloop . 
                         
                        IF hDataRelation:PARENT-BUFFER:NAME <> oReportBuffer:BufferName THEN
                            NEXT childloop .  
                         
                        ASSIGN hDataRelationQuery = hDataRelation:QUERY . 

                        hDataRelationQuery:QUERY-OPEN (). 
                        hDataRelationQuery:GET-FIRST(). 

                        IF oChildBuffer:DefineBufferAs = DefineBufferAsEnum:Fields THEN  
                            PrintBufferFields (piJob, oChildBuffer, TRUE) .
                        ELSE 
                            PrintBufferVariables (piJob, oChildBuffer, TRUE) .
                    END.
                END.

                iRet = LlApi:PrintFields (piJob) . 
                
                IF iRet < 0 AND iRet <> LlConst:LL_WRN_REPEAT_DATA THEN 
                    UNDO, THROW NEW AppError (LlErrorMessages:GetErrorText (iRet), 0) .
    
                /* If page full then force new page an print record again */
                DO WHILE iRet = LlConst:LL_WRN_REPEAT_DATA:
                    iRet = LlApi:Print (piJob) .
                    iRet = LlApi:PrintFields (piJob) .
                END.
            END.    /* DefineBufferAsEnum:Fields */
                
            /* Print 1:n Relations */
            childloop: DO i = 1 TO THIS-OBJECT:ReportDefinition:ReportBuffers:Count:
               
                ASSIGN oChildBuffer = THIS-OBJECT:ReportDefinition:ReportBuffers:GetItem (i) .
               
                IF oChildBuffer:Multiple = TRUE THEN DO:

                    ASSIGN hDataRelation      = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (oChildBuffer:BufferName):PARENT-RELATION .
                    
                    IF NOT VALID-HANDLE (hDataRelation) THEN 
                        NEXT childloop . 
                     
                    ASSIGN hDataRelationQuery = hDataRelation:QUERY . 
                   
                    IF hDataRelation:PARENT-BUFFER:NAME = oReportBuffer:BufferName THEN 
                        THIS-OBJECT:OutputDatasetQuery (piJob,
                                                        hDataRelationQuery) .
                END.
            END.
                
            phQuery:GET-NEXT().
                
        END. /* loop */             
      
        phQuery:QUERY-CLOSE() .
      
        RETURN FALSE . /* don't cancel */
      
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Outputs the Dataset Variables 
        Notes:   
        @param piJob The List & Label Job handle
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID OutputDatasetVariables (piJob AS INTEGER):
		
        DEFINE VARIABLE hQuery           AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hRelationQuery   AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBuffer          AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hRelation        AS HANDLE  NO-UNDO.

        DEFINE VARIABLE lRelationsActive AS LOGICAL NO-UNDO .

        ASSIGN lRelationsActive = THIS-OBJECT:DatasetHandle:RELATIONS-ACTIVE 
               hQuery           = QueryHelper:CreatePreparedQuery 
                                        (THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (THIS-OBJECT:ReportDefinition:ParentBufferName)) .
		
		{Consultingwerk/foreachABL.i ReportBuffer oReportBuffer in THIS-OBJECT:ReportDefinition:ReportBuffers}
             
             IF oReportBuffer:DefineBufferAs = DefineBufferAsEnum:Variables THEN DO: 

                 hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (oReportBuffer:BufferName) .

                 IF NOT oReportBuffer:BufferName = THIS-OBJECT:ReportDefinition:ParentBufferName THEN DO:
                 
                     IF NOT hBuffer:AVAILABLE THEN DO:
                         hRelation = hBuffer:PARENT-RELATION .

                         IF VALID-HANDLE (hRelation) THEN DO:
                             hRelationQuery = hRelation:QUERY .
                             hRelationQuery:QUERY-OPEN () .
                             hRelationQuery:GET-FIRST () .
                         END.
                         ELSE DO:
                             hBuffer:FIND-FIRST () .
                         END.                         
                     END.    
                 END.                 
                 
                 PrintBufferVariables (piJob, oReportBuffer, FALSE) .              
             END.                 
        END.

        FINALLY:
            THIS-OBJECT:DatasetHandle:RELATIONS-ACTIVE = lRelationsActive .
            
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.             

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prints a buffer as fields 
        Notes:   
        @param piJob The List And Label Job Handle
        @param poReportBuffer The ReportBuffer Definition
        @param plAllFields If true all fields will be defined, if false only those fields that are used
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC INTEGER PrintBufferFields (piJob AS INTEGER,
	                                         poReportBuffer AS ReportBuffer,
	                                         plAllFields AS LOGICAL):
		
        DEFINE VARIABLE hField      AS HANDLE     NO-UNDO .
        DEFINE VARIABLE iField      AS INTEGER    NO-UNDO .
        DEFINE VARIABLE hBuffer     AS HANDLE     NO-UNDO .
        DEFINE VARIABLE cBufferName AS CHARACTER  NO-UNDO .  
        
        DEFINE VARIABLE cLong       AS LONGCHAR   NO-UNDO . 
        DEFINE VARIABLE cShort      AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE m           AS MEMPTR     NO-UNDO .
        DEFINE VARIABLE cFieldName  AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE iFrom       AS INTEGER    NO-UNDO .
        DEFINE VARIABLE iTo         AS INTEGER    NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER    NO-UNDO .
    
        ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (poReportBuffer:BufferName) .
    
        /* Print this buffer */
        ASSIGN cBufferName = hBuffer:NAME .

        DO iField = 1 TO hBuffer:NUM-FIELDS ON ERROR UNDO, NEXT:
        
            ASSIGN hField = hBuffer:BUFFER-FIELD (iField) .

            IF hField:EXTENT = 0 THEN  
                ASSIGN iFrom = 0
                       iTo   = 0 .
            ELSE
                ASSIGN iFrom = 1
                       iTo   = hField:EXTENT . 

            DO i = iFrom TO iTo:
                
                IF i = 0 THEN 
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2":U,
                                                    cBufferName,
                                                    hField:NAME) .
                ELSE                                                                          
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2____&3":U,
                                                    cBufferName,
                                                    hField:NAME,
                                                    i) .

                CASE hField:DATA-TYPE:
                    WHEN "CHARACTER":U THEN
                        LlApi:DefineField (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:BUFFER-VALUE[i] ELSE "":U)).
                    WHEN "INTEGER":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:STRING-VALUE[i] ELSE "0":U), LlConst:LL_NUMERIC, 0).
                    WHEN "DECIMAL":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:BUFFER-VALUE[i] ELSE "0":U), LlConst:LL_NUMERIC, 0) NO-ERROR.
                    WHEN "DATE":U THEN 
                        IF SESSION:DATE-FORMAT = "DMY":U THEN
                          LlApi:DefineFieldExt (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING (hField:BUFFER-VALUE[i], "99.99.9999":U) ELSE "01.01.1900":U), LlConst:LL_DATE_DMY, 0).
                        ELSE
                          LlApi:DefineFieldExt (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING (hField:BUFFER-VALUE[i], "99.99.9999":U) ELSE "01.01.1900":U), LlConst:LL_DATE_MDY, 0).
                    WHEN "LOGICAL":U THEN
                        LlApi:DefineFieldExt (piJob, cFieldName, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING(hField:BUFFER-VALUE[i], "true/false":U) ELSE "false":U), LlConst:LL_BOOLEAN, 0).
                    WHEN "CLOB":U THEN DO:
                        IF hBuffer:AVAILABLE THEN DO:
                            ASSIGN cLong = hField:BUFFER-VALUE[i] .
                            IF LENGTH (cLong) > 0  THEN DO:
                                COPY-LOB FROM cLong TO m .
                                ASSIGN cShort = GET-STRING (m,1).
                                SET-SIZE(m) = 0.
                            END.
                            ELSE ASSIGN cShort = "":U.
                        END.
                        ELSE ASSIGN cShort = "":U .
      
                        LlApi:DefineFieldExt (piJob, cFieldName, cShort, LlConst:LL_RTF, 0).
                    END.  
                    WHEN "BLOB":U THEN DO:
                        IF hBuffer:AVAILABLE THEN DO:
                            IF LENGTH(hField:BUFFER-VALUE[i]) > 0 THEN DO:
                                
                                ASSIGN cShort = LlHelper:GetTempFileName ("tmp":U) .
      
                                COPY-LOB FROM hField:BUFFER-VALUE[i] TO FILE cShort .
                            END.
                            ELSE ASSIGN cShort = "":U .
                        END.
                        ELSE ASSIGN cShort = "":U .
      
                        LlApi:DefineFieldExt (piJob, cFieldName, cShort, LlConst:LL_DRAWING, 0).
                    END.
                    WHEN "DATETIME":U THEN
                        LlApi:DefineField (piJob, cFieldName, 
                                           (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING(hField:BUFFER-VALUE[i], "99.99.9999 HH:MM:SS":U) ELSE "?":U)).                      
                END CASE.
            END. 
        END.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prints a buffer as fields 
        Notes:   
        @param piJob The List And Label Job Handle
        @param poReportBuffer The ReportBuffer Definition
        @param plAllFields If true all fields will be defined, if false only those fields that are used
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER PrintBufferVariables (piJob AS INTEGER,
                                                poReportBuffer AS ReportBuffer,
                                                plAllFields AS LOGICAL):
        
        DEFINE VARIABLE hField      AS HANDLE     NO-UNDO .
        DEFINE VARIABLE iField      AS INTEGER    NO-UNDO .
        DEFINE VARIABLE hBuffer     AS HANDLE     NO-UNDO .
        DEFINE VARIABLE cBufferName AS CHARACTER  NO-UNDO .  
        
        DEFINE VARIABLE cLong       AS LONGCHAR   NO-UNDO . 
        DEFINE VARIABLE cShort      AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE m           AS MEMPTR     NO-UNDO .
        DEFINE VARIABLE cFieldName  AS CHARACTER  NO-UNDO .
        DEFINE VARIABLE iFrom       AS INTEGER    NO-UNDO .
        DEFINE VARIABLE iTo         AS INTEGER    NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER    NO-UNDO .
    
        ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE (poReportBuffer:BufferName) .
    
        /* Print this buffer */
        ASSIGN cBufferName = hBuffer:NAME .

        DO iField = 1 TO hBuffer:NUM-FIELDS ON ERROR UNDO, NEXT:
        
            ASSIGN hField = hBuffer:BUFFER-FIELD (iField) .

            ASSIGN hField = hBuffer:BUFFER-FIELD (iField) .

            IF hField:EXTENT = 0 THEN  
                ASSIGN iFrom = 0
                       iTo   = 0 .
            ELSE
                ASSIGN iFrom = 1
                       iTo   = hField:EXTENT . 

            DO i = iFrom TO iTo:
                
                IF i = 0 THEN 
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2":U,
                                                    cBufferName,
                                                    hField:NAME) .
                ELSE                                                                          
                    ASSIGN cFieldName = SUBSTITUTE ("&1.&2____&3":U,
                                                    cBufferName,
                                                    hField:NAME,
                                                    i) .

                CASE hField:DATA-TYPE:
                    WHEN "CHARACTER":U THEN
                        LlApi:DefineVariable (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:BUFFER-VALUE[i] ELSE "":U)).
                    WHEN "INTEGER":U THEN
                        LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:STRING-VALUE[i] ELSE "0":U), LlConst:LL_NUMERIC, 0).
                    WHEN "DECIMAL":U THEN
                        LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN hField:BUFFER-VALUE[i] ELSE "0":U), LlConst:LL_NUMERIC, 0) NO-ERROR.
                    WHEN "DATE":U THEN 
                        IF SESSION:DATE-FORMAT = "DMY":U THEN
                          LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING(hField:BUFFER-VALUE[i], "99.99.9999":U) ELSE "01.01.1900":U), LlConst:LL_DATE_DMY, 0).
                        ELSE
                          LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING(hField:BUFFER-VALUE[i], "99.99.9999":U) ELSE "01.01.1900":U), LlConst:LL_DATE_MDY, 0).
                    WHEN "LOGICAL":U THEN
                        LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING (hField:BUFFER-VALUE[i], "true/false":U) ELSE "false":U), LlConst:LL_BOOLEAN, 0).
                    WHEN "CLOB":U THEN DO:
                        IF hBuffer:AVAILABLE THEN DO:
                            ASSIGN cLong = hField:BUFFER-VALUE[i] .
                            IF LENGTH (cLong) > 0  THEN DO:
                                COPY-LOB FROM cLong TO m .
                                ASSIGN cShort = GET-STRING (m,1).
                                SET-SIZE(m) = 0.
                            END.
                            ELSE ASSIGN cShort = "":U.
                        END.
                        ELSE ASSIGN cShort = "":U .
      
                        LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, cShort, LlConst:LL_RTF, 0).
                    END.  
                    WHEN "BLOB":U THEN DO:
                        IF hBuffer:AVAILABLE THEN DO:
                            IF LENGTH(hField:BUFFER-VALUE[i]) > 0 THEN DO:
                                
                                ASSIGN cShort = LlHelper:GetTempFileName ("tmp":U) .
      
                                COPY-LOB FROM hField:BUFFER-VALUE[i] TO FILE cShort .
                            END.
                            ELSE ASSIGN cShort = "":U .
                        END.
                        ELSE ASSIGN cShort = "":U .
      
                        LlApi:DefineVariableExt (piJob, cBufferName + ".":U + hField:NAME, cShort, LlConst:LL_DRAWING, 0).
                    END.
                    WHEN "DATETIME":U THEN
                        LlApi:DefineVariable (piJob, cBufferName + ".":U + hField:NAME, 
                                           (IF hBuffer:AVAILABLE AND hField:BUFFER-VALUE[i] <> ? THEN STRING(hField:BUFFER-VALUE[i], "99.99.9999 HH:MM:SS":U) ELSE "?":U)).                      
                END CASE.
            END. 
        END.
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Starts the Report Output with an optional export module
        Notes:   
        @param pcReportDefinitionFile The .lst file with the report definition, when ? the user will be prompted for a file
        @param plPromptFileName Logical value indicating if the routine should prompf for a report file name
        @param poExportModule The ILlExportModule with the settings for the Export Module
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID StartExport (pcReportDefinitionFile AS CHARACTER,
	                                plPromptFileName AS LOGICAL,
	                                poExportModule AS ILlExportModule):
		
        DEFINE VARIABLE iJob              AS INTEGER   NO-UNDO . 
        DEFINE VARIABLE iRet              AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lOk               AS LOGICAL   NO-UNDO .    
        DEFINE VARIABLE cWorkingDirectory AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lPreview          AS LOGICAL   NO-UNDO .
        
        ASSIGN cWorkingDirectory = System.Environment:CurrentDirectory .

        iJob = LlApi:JobOpen (LlLanguage:DEFAULT) .
        
        LlHelper:SetLicenseInfo (iJob) .

        LlApi:SetOption (iJob, LlConst:LL_OPTION_MULTIPLETABLELINES, 1) . 

        LlApi:DefineVariableStart (iJob) .
        LlApi:DefineFieldStart (iJob) .
                
        THIS-OBJECT:DefineFieldsAndVariables (iJob, TRUE) .

        /* Prompt for Report Definition file */
        IF CharacterType:IsNullOrEmpty (pcReportDefinitionFile) OR plPromptFileName THEN DO:
            
            lOk = LlHelper:PrompfForReportFileName (iJob,
                                                    ?,
                                                    THIS-OBJECT:ParentWindow,
                                                    INPUT-OUTPUT pcReportDefinitionFile) .

            IF NOT lOk THEN DO:
                LlApi:JobClose (iJob) .
                
                RETURN . 
            END.
        END.
        ELSE DO:
            FILE-INFO:FILE-NAME = pcReportDefinitionFile . 
            
            IF CharacterType:IsNullOrEmpty (FILE-INFO:FULL-PATHNAME) THEN DO ON ERROR UNDO, THROW:
                            
                UNDO, THROW NEW InvalidParameterValueException (THIS-OBJECT:GetClass():TypeName,
                                                                "pcReportDefinitionFile":U,
                                                                pcReportDefinitionFile) .                            
                FINALLY:
                    LlApi:JobClose (iJob) .
                END FINALLY.
            END . 
            ELSE 
                pcReportDefinitionFile = FILE-INFO:FULL-PATHNAME .
        END . 
        
        /* Start printing */
        LlApi:PrintWithBoxStart (iJob,
                                 LlConst:LL_PROJECT_LIST,
                                 pcReportDefinitionFile,
                                 LlConst:LL_PRINT_USERSELECT,
                                 LlConst:LL_BOXTYPE_EMPTYABORT,
                                 THIS-OBJECT:ParentWindow,
                                 "Printing"{&TRAN}) .

        /* Set Print Option */
        LlApi:PrintSetOption (iJob, LlConst:LL_PRNOPT_PRINTDLG_DEST, LlConst:LL_DESTINATION_PRV).
        LlApi:PrintSetOption (iJob, LlConst:LL_PRNOPT_PRINTDLG_ONLYPRINTERCOPIES, 1).

        IF VALID-OBJECT (poExportModule) THEN DO:
            poExportModule:SetExportProperties (iJob) .
        
            LlApi:PrintSetOptionString (iJob, LlConst:LL_PRNOPTSTR_EXPORT, poExportModule:ExportModule) .
        END.
        ELSE DO:
            /* Show print options dialog */
            IF LlApi:PrintOptionsDialog (iJob, 
                                         THIS-OBJECT:ParentWindow, 
                                         "Configure Printing Options"{&TRAN}) = LlConst:LL_ERR_USER_ABORTED THEN DO:
    
                LlApi:PrintEnd (iJob, 0) .
                LlApi:JobClose (iJob) .
                
                RETURN . 
            END.
        END.

        IF LlApi:PrintGetOptionString (iJob, LlConst:LL_PRNOPTSTR_EXPORT) = "PRV":U THEN 
            ASSIGN lPreview = TRUE .
        ELSE 
            ASSIGN lPreview = FALSE . 
        
        /* Print variables */
        OutputDatasetVariables (iJob) .
        
        LlApi:Print (iJob) .
        
        /* Print fields */            
        OutputDataset (iJob) .         

        LlApi:PrintEnd (iJob, 0) .

        /* In preview mode display preview and delete temporary files */
        IF lPreview THEN DO:
            LlApi:PreviewDisplay (iJob, pcReportDefinitionFile, "":U, THIS-OBJECT:ParentWindow) .
            LlApi:PreviewDeleteFiles (iJob, pcReportDefinitionFile, "":U) .
        END .
    
        /* Close List & Label Job */
        LlApi:JobClose (iJob) .

        FINALLY:
            System.Environment:CurrentDirectory = cWorkingDirectory .  
            LlHelper:CleanupTempFiles () .     
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the Report Output 
        Notes:   
        @param pcReportDefinitionFile The .lst file with the report definition, when ? the user will be prompted for a file
        @param plPromptFileName Logical value indicating if the routine should prompf for a report file name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StartReport (pcReportDefinitionFile AS CHARACTER,
                                    plPromptFileName AS LOGICAL):
        
        THIS-OBJECT:StartExport (pcReportDefinitionFile,
                                 plPromptFileName, 
                                 ?) .

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Starts the List & Label Report Designer
        Notes:   Does not prompt for the file name 
        @param pcDesignerFileName The name of the Report design file (.lst)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StartReportDesigner (pcDesignerFileName AS CHARACTER):
        
        THIS-OBJECT:StartReportDesigner (pcDesignerFileName,
                                         FALSE /* Don't prompt for File Name */) .
        
    END METHOD . 
        
    /*------------------------------------------------------------------------------
        Purpose: Starts the List & Label Report Designer
        Notes:   
        @param pcDesignerFileName The name of the Report design file (.lst)
        @param plPromptFileName Logical value indicating if the routine should prompf for a report file name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StartReportDesigner (pcDesignerFileName AS CHARACTER,
                                            plPromptFileName AS LOGICAL):
        
        DEFINE VARIABLE iJob       AS INTEGER NO-UNDO . 
        DEFINE VARIABLE iRet       AS INTEGER NO-UNDO .
        DEFINE VARIABLE lOk        AS LOGICAL NO-UNDO .    

        iJob = LlApi:JobOpen (LlLanguage:DEFAULT) .
        
        LlHelper:SetLicenseInfo (iJob) .
        
        LlApi:SetOption (iJob, LlConst:LL_OPTION_MULTIPLETABLELINES, 1) . 
                
        THIS-OBJECT:DefineFieldsAndVariables (iJob, TRUE) .

        IF plPromptFileName THEN DO:
            lOk = LlHelper:PrompfForReportFileName (iJob,
                                                    ?,
                                                    THIS-OBJECT:ParentWindow,
                                                    INPUT-OUTPUT pcDesignerFileName) .

            IF NOT lOk THEN DO:
                LlApi:JobClose (iJob) .
                
                RETURN . 
            END.
        END.

        FILE-INFO:FILE-NAME = pcDesignerFileName . 
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            pcDesignerFileName = FILE-INFO:FULL-PATHNAME . 

        LlApi:DefineLayout (iJob, 
                            THIS-OBJECT:ParentWindow, 
                            THIS-OBJECT:DesignerTitle, 
                            LlConst:LL_PROJECT_LIST,
                            pcDesignerFileName) .
        
        LlApi:JobClose (iJob) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates the Report Buffer Definition
        Notes:   
        @param poReportDefinition The ReportDefinition object that defines the relations for this report
        @param phDataset The handle of the Dataset that provides the data for the report 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ValidateReportBuffers (poReportDefinition AS ReportDefinition,   
	                                             phDataset AS HANDLE):
		
		DatasetAssert:HasBuffer (phDataset, poReportDefinition:ParentBufferName) .

        {Consultingwerk/foreachABL.i Consultingwerk.Windows.ListAndLabel.ReportBuffer oBuffer in poReportDefinition:ReportBuffers}
                    DatasetAssert:HasBuffer (phDataset, oBuffer:BufferName) .
        END.

	END METHOD .

END CLASS.



