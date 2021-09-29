 
 /*------------------------------------------------------------------------
    File        : UserColumn
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
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
                ttDefaultUserColumn.colName     = hdBrowseColumn:NAME     
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
            FIND FIRST ttDefaultUserColumn 
                 WHERE ttDefaultUserColumn.colName EQ ttUserColumn.colName
                 NO-ERROR.
            IF NOT AVAILABLE ttDefaultUserColumn THEN
                DELETE ttUserColumn.
        END.
        
        /* Add any field that is added t browse after user column is saved */
        FOR EACH ttDefaultUserColumn:
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ ttDefaultUserColumn.colName
                 NO-ERROR.
            IF NOT AVAILABLE ttUserColumn THEN DO:
                CREATE ttUserColumn.
                ASSIGN
                    ttUserColumn.programName = ProgramName
                    ttUserColumn.usrID       = CurrentUser
                    ttUserColumn.colName     = ttDefaultUserColumn.colName
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
	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID pBuildUserColumnFromBrowse( ):
        DEFINE VARIABLE iColumn        AS INTEGER NO-UNDO.
        DEFINE VARIABLE hdBrowseColumn AS HANDLE  NO-UNDO.
        
        DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
            hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn).
            
            IF NOT VALID-HANDLE (hdBrowseColumn) THEN
                NEXT.
            
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ hdBrowseColumn:NAME
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
		FOR EACH ttDefaultUserColumn:
		    FIND FIRST ttUserColumn
		         WHERE ttUserColumn.colName EQ ttDefaultUserColumn.colName
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
            CREATE bf-userColumn.
            BUFFER-COPY ttUserColumn TO bf-userColumn.
        END.
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID UpdateBrowse( ):
        DEFINE VARIABLE iColumn        AS INTEGER NO-UNDO.
        DEFINE VARIABLE hdBrowseColumn AS HANDLE  NO-UNDO.
		
        DO iColumn = 1 TO BrowseHandle:NUM-COLUMNS:
            hdBrowseColumn = BrowseHandle:GET-BROWSE-COLUMN(iColumn).
            
            IF NOT VALID-HANDLE (hdBrowseColumn) THEN
                NEXT.
                
            FIND FIRST ttUserColumn 
                 WHERE ttUserColumn.colName EQ hdBrowseColumn:NAME
                 NO-ERROR.
            IF NOT AVAILABLE ttUserColumn THEN
                NEXT.

            ASSIGN
                hdBrowseColumn:LABEL        = IF ttUserColumn.colLabel EQ "" THEN hdBrowseColumn:LABEL ELSE ttUserColumn.colLabel 
                hdBrowseColumn:VISIBLE      = IF ttUserColumn.colVisible EQ ? THEN TRUE ELSE ttUserColumn.colVisible 
                hdBrowseColumn:WIDTH-PIXELS = ttUserColumn.colWidth 
                .
            
            /* Sometimes default width is ?, as width is set using format */
            FIND FIRST ttDefaultUserColumn 
                 WHERE ttDefaultUserColumn.colName EQ hdBrowseColumn:NAME
                 NO-ERROR.
            IF AVAILABLE ttDefaultUserColumn AND ttDefaultUserColumn.colWidth EQ ? THEN
                ttDefaultUserColumn.colWidth = hdBrowseColumn:WIDTH-PIXELS.
                            
            IF ttUserColumn.colPosition NE ? THEN
                BrowseHandle:MOVE-COLUMN(iColumn,ttUserColumn.colPosition).                
        END.		
	END METHOD.    

    METHOD PUBLIC VOID SetUserColumnFromBrowse( ):
        pBuildUserColumnFromBrowse().
        
        SetUserColumn(INPUT TABLE ttUserColumn).            
    END METHOD.    
END CLASS.