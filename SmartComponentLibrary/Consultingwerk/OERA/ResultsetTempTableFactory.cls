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
    File        : ResultsetTempTableFactory
    Purpose     : Creates the temp-table that is returned to the caller
                  when multiple ProDataset tables will be joined into a
                  single resultset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 08 12:17:20 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH . 
USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.OERA.OpenAccess.* FROM PROPATH .  
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.ResultsetTempTableFactory: 

    /*------------------------------------------------------------------------------
        Purpose: Creates the temp-table that is returned to the caller
                 when multiple ProDataset tables will be joined into a
                 single resultset
        Notes:   
        @param phDataset The handle of the ProDataset
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @return The temp-table handle
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC HANDLE BuildResultsetTempTable (phDataset AS HANDLE, 
                                                  pcEntityTable AS CHARACTER,
                                                  pcEntityView AS CHARACTER):   
		
        DEFINE VARIABLE hTable  AS HANDLE  NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER NO-UNDO .
        DEFINE VARIABLE cBuffer AS CHARACTER NO-UNDO.

        DatasetAssert:HasBuffer (phDataset, pcEntityTable) .

        CREATE TEMP-TABLE hTable. 
        
        hTable:ADD-FIELDS-FROM (phDataset:GET-BUFFER-HANDLE (pcEntityTable)) .

        DO i = 1 TO NUM-ENTRIES (pcEntityView) ON ERROR UNDO, THROW:

            ASSIGN cBuffer = ENTRY (i, pcEntityView) .

            DatasetAssert:HasBuffer (phDataset, cBuffer) .

            hTable:ADD-FIELDS-FROM (phDataset:GET-BUFFER-HANDLE (cBuffer)) .
        END.

        hTable:TEMP-TABLE-PREPARE (pcEntityTable) . 
        
        RETURN hTable .

	END METHOD .

END CLASS.
