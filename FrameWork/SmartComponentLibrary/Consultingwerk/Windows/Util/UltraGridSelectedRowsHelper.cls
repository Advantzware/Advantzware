/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : UltraGridSelectedRowsHelper
    Purpose     : Provides support for processing selected rows in an UltraGrid
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 07 03:05:09 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                FROM PROPATH .
USING Consultingwerk.Assertion.*      FROM PROPATH .
USING Consultingwerk.Util.*           FROM PROPATH .
USING Consultingwerk.Windows.Util.*   FROM PROPATH .   
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY . 
USING Progress.Lang.*                 FROM PROPATH .

CLASS Consultingwerk.Windows.Util.UltraGridSelectedRowsHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraGridSelectedRowsHelper class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE UltraGridSelectedRowsHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Copies selected rows in the Ultragrid's first Band from a source 
                 ProDataset to the target ProDataset 
        Notes:   
        @param poGrid The UltraGrid that contains the selected rows
        @param pcTables The comma delimited list of Tables to process
        @param phSourceDataset The Source ProDataset
        @param phTargetDataset The Target ProDataset 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CopySelectedRowsToProDataset (poGrid AS UltraGrid,
                                                            pcTables AS CHARACTER, 
                                                            phSourceDataset AS HANDLE, 
                                                            phTargetDataset AS HANDLE):
        
        DEFINE VARIABLE i          AS INTEGER      NO-UNDO .
        DEFINE VARIABLE oActiveRow AS UltraGridRow NO-UNDO . 
        DEFINE VARIABLE iPosition  AS INTEGER      NO-UNDO .
        
        DEFINE VARIABLE hSourceBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hTargetBuffer AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cFindString   AS CHARACTER NO-UNDO . 
        
        ObjectAssert:IsValid (poGrid, "poGrid":U) .
        Assert:NotNullOrEmpty (pcTables, "pcTables":U) .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phSourceDataset WidgetTypeEnum:Dataset} .
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTargetDataset WidgetTypeEnum:Dataset} .
        
        DO i = 1 TO NUM-ENTRIES (pcTables):
            DatasetAssert:HasBuffer (phSourceDataset, ENTRY (i, pcTables)) .
            DatasetAssert:HasBuffer (phTargetDataset, ENTRY (i, pcTables)) .
        END.

        ASSIGN oActiveRow = poGrid:ActiveRow 
               iPosition  = CAST (poGrid:DataSource, Progress.Data.BindingSource):Position .
        
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridRow oRow in poGrid:Selected:Rows}

            IF oRow:Band:Index <> 0 THEN 
                NEXT . 

            ASSIGN CAST (poGrid:DataSource, Progress.Data.BindingSource):Position = oRow:ListIndex . 

            DO i = 1 TO NUM-ENTRIES (pcTables):
                ASSIGN hSourceBuffer = phSourceDataset:GET-BUFFER-HANDLE (ENTRY (i, pcTables)) 
                       hTargetBuffer = phTargetDataset:GET-BUFFER-HANDLE (ENTRY (i, pcTables))
                       
                       cFindString   = BufferHelper:UniqueFindPredicate (hSourceBuffer) . 
                    
                hTargetBuffer:FIND-UNIQUE (cFindString) NO-ERROR . 
                
                IF NOT hTargetBuffer:AVAILABLE THEN DO:
                    hTargetBuffer:BUFFER-CREATE () . 
                    hTargetBuffer:BUFFER-COPY (hSourceBuffer) .
                    hTargetBuffer:BUFFER-RELEASE () .  
                END.                    
            END.
        END.

        FINALLY:
            ASSIGN CAST (poGrid:DataSource, Progress.Data.BindingSource):Position = iPosition
                   poGrid:ActiveRow                                               = oActiveRow .
                     
        END FINALLY.

    END METHOD .

END CLASS.
