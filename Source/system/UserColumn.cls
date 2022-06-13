 /*------------------------------------------------------------------------
    File        : system.UserColumn.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Tue Sep 28 20:56:14 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.UserColumn: 
    {system/ttUserColumn.i}
    
	DEFINE PUBLIC PROPERTY BrowseHandle AS HANDLE NO-UNDO 
	GET.
	SET. 

	DEFINE PUBLIC PROPERTY ProgramName AS CHARACTER NO-UNDO 
	GET.
	SET. 

	DEFINE PUBLIC PROPERTY CurrentUser AS CHARACTER NO-UNDO 
	GET.
	SET. 
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC UserColumn ( iphdBrowseHandle AS HANDLE, ipcProgramName AS CHARACTER ):
		SUPER ().
	    
	    ASSIGN
	       BrowseHandle = iphdBrowseHandle
	       ProgramName  = ipcProgramName
	       .
	    
	    RUN spGetSessionParam ("UserID", OUTPUT CurrentUser).
	    
	    SetDefaults().
	    
	    pBuildUserColumn().
	    	
	END CONSTRUCTOR.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SetDefaults(  ):
        DEFINE VARIABLE iColumn        AS INTEGER NO-UNDO.
        DEFINE VARIABLE hdBrowseColumn AS HANDLE  NO-UNDO.
        
        EMPTY TEMP-TABLE ttDefaultUserColumn.
        
        DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
            hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn).
            
            IF NOT VALID-HANDLE (hdBrowseColumn) THEN
                NEXT.
            
            CREATE ttDefaultUserColumn.
            ASSIGN
                ttDefaultUserColumn.colTable    = hdBrowseColumn:TABLE       
                ttDefaultUserColumn.colName     = hdBrowseColumn:NAME
                                                + (IF hdBrowseColumn:INDEX GT 0 THEN "[" + STRING(hdBrowseColumn:INDEX) + "]" ELSE "")     
                ttDefaultUserColumn.colLabel    = hdBrowseColumn:LABEL
                ttDefaultUserColumn.colVisible  = hdBrowseColumn:VISIBLE
                ttDefaultUserColumn.colWidth    = hdBrowseColumn:WIDTH-PIXELS
                ttDefaultUserColumn.colPosition = iColumn
                .
        END.     
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID GetUserColumn( OUTPUT TABLE ttUserColumn ):
        /* Records already populated */
        IF TEMP-TABLE ttUserColumn:HAS-RECORDS THEN
            RETURN.
        
        pBuildUserColumn().
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pBuildUserColumn( ):
		DEFINE VARIABLE lBrowseChanged AS LOGICAL   NO-UNDO.
		DEFINE VARIABLE iPosition      AS INTEGER   NO-UNDO.
		DEFINE VARIABLE cColumnName    AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cColumnTable   AS CHARACTER NO-UNDO.
		 
        DEFINE BUFFER bf-userColumn FOR userColumn. 
        
        EMPTY TEMP-TABLE ttUserColumn.
        
        /* Load from userColumn table */
        FOR EACH bf-userColumn NO-LOCK
            WHERE bf-userColumn.programName EQ ProgramName
              AND bf-userColumn.usrId       EQ CurrentUser:
            CREATE ttUserColumn.
            BUFFER-COPY bf-userColumn TO ttUserColumn.
        END.
        
        /* Delete any field that is removed from browse after user column is saved */
        FOR EACH ttUserColumn:
            ASSIGN
                cColumnTable = IF NUM-ENTRIES (ttUserColumn.colName, ".") EQ 2 THEN
                                   ENTRY(1, ttUserColumn.colName, ".")
                               ELSE
                                   ?
                cColumnName  = IF NUM-ENTRIES (ttUserColumn.colName, ".") EQ 2 THEN
                                   ENTRY(2, ttUserColumn.colName, ".")
                               ELSE
                                   ENTRY(1, ttUserColumn.colName, ".")
                NO-ERROR.
            
            FIND FIRST ttDefaultUserColumn 
                 WHERE ttDefaultUserColumn.colTable EQ cColumnTable
                   AND ttDefaultUserColumn.colName  EQ cColumnName
                 NO-ERROR.
            IF AVAILABLE ttDefaultUserColumn THEN
                NEXT.
                 
            FIND FIRST ttDefaultUserColumn 
                 WHERE ttDefaultUserColumn.colTable EQ ?
                   AND ttDefaultUserColumn.colName  EQ cColumnName
                 NO-ERROR.
            IF NOT AVAILABLE ttDefaultUserColumn THEN DO:
                lBrowseChanged = TRUE.
                DELETE ttUserColumn.
            END.
            ELSE  /* This is more of a conversion of colName from fieldname to tablename.fieldname format, as a browse can have columns with same field names, which is causing a problem while saving. Eg. oe-ordl.po-no and oe-ord.po  */
                ttUserColumn.colName = (IF cColumnTable EQ ? THEN "" ELSE cColumnTable + ".") + cColumnName.                
        END.
        
        /* Add any field that is added to browse after user column is saved */
        FOR EACH ttDefaultUserColumn:
            cColumnName = (IF ttDefaultUserColumn.colTable EQ ? THEN "" ELSE ttDefaultUserColumn.colTable + ".") + ttDefaultUserColumn.colName.
             
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ cColumnName
                 NO-ERROR.
            IF NOT AVAILABLE ttUserColumn THEN DO:
                lBrowseChanged = TRUE.
                
                CREATE ttUserColumn.
                ASSIGN
                    ttUserColumn.programName = ProgramName
                    ttUserColumn.usrID       = CurrentUser
                    ttUserColumn.colName     = cColumnName
                    ttUserColumn.colLabel    = ttDefaultUserColumn.colLabel
                    ttUserColumn.colVisible  = ttDefaultUserColumn.colVisible
                    ttUserColumn.colWidth    = ttDefaultUserColumn.colWidth
                    ttUserColumn.colPosition = ttDefaultUserColumn.colPosition
                    .
            END.
            
            ttUserColumn.defaultColLabel = ttDefaultUserColumn.colLabel.  
            
            IF ttUserColumn.colLabel EQ "" THEN
                ttUserColumn.colLabel = ttDefaultUserColumn.colLabel.
            
            IF ttUserColumn.colVisible EQ ? THEN
                ttUserColumn.colVisible = ttDefaultUserColumn.colVisible.          
        END.
        /* If a new column is added or deleted reposition the duplicate positioned columns */
        IF lBrowseChanged THEN DO:
            FOR EACH ttUserColumn
                BY ttUserColumn.colPosition:
                iPosition = iPosition + 1.
                
                ttUserColumn.colPosition = iPosition.
            END.
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID pBuildUserColumnFromBrowse( ):
        DEFINE VARIABLE iColumn        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hdBrowseColumn AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cColumnName    AS CHARACTER NO-UNDO.
        
        DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
            hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn).
            
            IF NOT VALID-HANDLE (hdBrowseColumn) THEN
                NEXT.
            
            cColumnName = (IF hdBrowseColumn:TABLE EQ ? THEN "" ELSE hdBrowseColumn:TABLE + ".") + hdBrowseColumn:NAME
                        + (IF hdBrowseColumn:INDEX GT 0 THEN "[" + STRING(hdBrowseColumn:INDEX) + "]" ELSE "").
            
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ cColumnName
                 NO-ERROR.
            IF AVAILABLE ttUserColumn THEN
                ASSIGN       
                    ttUserColumn.colLabel    = hdBrowseColumn:LABEL
                    ttUserColumn.colVisible  = hdBrowseColumn:VISIBLE
                    ttUserColumn.colWidth    = hdBrowseColumn:WIDTH-PIXELS
                    ttUserColumn.colPosition = iColumn
                    .
        END.     
    END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID RestoreDefaults( ):
        DEFINE VARIABLE cColumnName AS CHARACTER NO-UNDO.
        
		FOR EACH ttDefaultUserColumn
		      BY ttDefaultUserColumn.colPosition
		    :
            cColumnName = (IF ttDefaultUserColumn.colTable EQ ? THEN "" ELSE ttDefaultUserColumn.colTable + ".") + ttDefaultUserColumn.colName.

		    FIND FIRST ttUserColumn
		         WHERE ttUserColumn.colName EQ cColumnName
		         NO-ERROR.
		    IF AVAILABLE ttUserColumn THEN
                ASSIGN
                    ttUserColumn.colLabel    = ttDefaultUserColumn.colLabel
                    ttUserColumn.colVisible  = ttDefaultUserColumn.colVisible
                    ttUserColumn.colWidth    = ttDefaultUserColumn.colWidth
                    ttUserColumn.colPosition = ttDefaultUserColumn.colPosition
                    .
		END.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SaveUserColumn( ):
		
		SetUserColumn (INPUT TABLE ttUserColumn).

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID SetUserColumn( INPUT TABLE ttUserColumn ):
        DEFINE BUFFER bf-userColumn FOR userColumn. 
        
        FOR EACH bf-userColumn EXCLUSIVE-LOCK
            WHERE bf-userColumn.programName EQ ProgramName
              AND bf-userColumn.usrId       EQ CurrentUser:
            DELETE bf-userColumn.
        END.
        
        FOR EACH ttUserColumn:
            /* For some reason if there is a duplicate colPosition then ignore the column */
            IF NOT CAN-FIND(FIRST userColumn
                            WHERE userColumn.programName EQ ProgramName
                              AND userColumn.usrID       EQ CurrentUser
                              AND userColumn.colPosition EQ ttUserColumn.colPosition) THEN DO:  
                CREATE bf-userColumn.
                BUFFER-COPY ttUserColumn TO bf-userColumn.
            END.
        END.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID UpdateBrowse( ):
        DEFINE VARIABLE iColumn        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hdBrowseColumn AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cColumnName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hBufferField AS HANDLE NO-UNDO.
        DEFINE VARIABLE iNumExtents AS INT NO-UNDO.
                
        DEFINE VARIABLE iCurrentColPositionList AS INTEGER   NO-UNDO EXTENT.
        DEFINE VARIABLE cCurrentColNameList     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex                  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iPosition               AS INTEGER   NO-UNDO.
        
        EXTENT (iCurrentColPositionList) = BrowseHandle:NUM-COLUMNS.
        
        DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
            ASSIGN 
                hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn) NO-ERROR.
            IF NOT VALID-HANDLE (hdBrowseColumn) THEN
                NEXT.
            hBufferField = hdBrowseColumn:BUFFER-FIELD.
            iNumExtents = IF VALID-HANDLE(hBufferField) AND hBufferField:EXTENT GT 1 THEN hBufferField:EXTENT ELSE 1.
            
            cColumnName = (IF hdBrowseColumn:TABLE EQ ? THEN "" ELSE hdBrowseColumn:TABLE + ".") + hdBrowseColumn:NAME
                        + (IF hdBrowseColumn:INDEX GT 0 THEN "[" + STRING(hdBrowseColumn:INDEX) + "]" ELSE "").

            ASSIGN
                iCurrentColPositionList[iColumn] = iColumn
                cCurrentColNameList              = cCurrentColNameList + "," + cColumnName
                .
                                        
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ cColumnName
                 NO-ERROR.
            IF NOT AVAILABLE ttUserColumn THEN
                NEXT.

            ASSIGN
                hdBrowseColumn:LABEL        = IF ttUserColumn.colLabel   EQ "" THEN hdBrowseColumn:LABEL ELSE 
                                              IF iNumExtents GT 1 THEN hdBrowseColumn:LABEL ELSE ttUserColumn.colLabel 
                hdBrowseColumn:VISIBLE      = IF ttUserColumn.colVisible EQ ?  THEN TRUE ELSE ttUserColumn.colVisible 
                hdBrowseColumn:WIDTH-PIXELS = IF ttUserColumn.colWidth   EQ 0  OR
                                                 ttUserColumn.colWidth   EQ ?  THEN hdBrowseColumn:WIDTH-PIXELS ELSE ttUserColumn.colWidth
                .
                        
            /* Sometimes default width is ?, as width is set using format */
            FIND FIRST ttDefaultUserColumn 
                 WHERE ttDefaultUserColumn.colTable EQ hdBrowseColumn:TABLE
                   AND ttDefaultUserColumn.colName  EQ hdBrowseColumn:NAME
                 NO-ERROR.

            IF AVAILABLE ttDefaultUserColumn AND ttDefaultUserColumn.colWidth EQ ? THEN
                ttDefaultUserColumn.colWidth = hdBrowseColumn:WIDTH-PIXELS.
        END.
    
        cCurrentColNameList = TRIM(cCurrentColNameList,",").
        
        /* The below is to move the columns to the set position  */             
        FOR EACH ttUserColumn
            BY ttUserColumn.colPosition:
            iIndex = LOOKUP (ttUserColumn.colName, cCurrentColNameList).
            IF iIndex GT 0 THEN DO:
                iPosition = iCurrentColPositionList[iIndex].

                IF iPosition GT 0 AND ttUserColumn.colPosition NE iPosition THEN DO:
                    /* Move columns */
                    BrowseHandle:MOVE-COLUMN(iPosition,ttUserColumn.colPosition).
                                        
                    /* The following code is to ensure once a column is moved we need to refetch all the columns in the order of the position */
                    cCurrentColNameList = "".

                    DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
                        hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn) NO-ERROR.

                        cColumnName = (IF hdBrowseColumn:TABLE EQ ? THEN "" ELSE hdBrowseColumn:TABLE + ".") + hdBrowseColumn:NAME
                                    + (IF hdBrowseColumn:INDEX GT 0 THEN "[" + STRING(hdBrowseColumn:INDEX) + "]" ELSE "").

                        ASSIGN
                            iCurrentColPositionList[iColumn] = iColumn
                            cCurrentColNameList              = cCurrentColNameList + "," + cColumnName
                            .
                    END.

                    cCurrentColNameList = TRIM(cCurrentColNameList,",").
                END. 
            END.
        END.
	END METHOD.    

    METHOD PUBLIC VOID SetUserColumnFromBrowse( ):
        pBuildUserColumnFromBrowse().
        
        SetUserColumn(INPUT TABLE ttUserColumn).            
    END METHOD.    
END CLASS.
