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
    File        : SchemaHelper
    Purpose     : Provides helper methods to manipulate the schema for 
                  Business Entities exposed through the REST Adapter
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 22 20:15:07 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .  
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.SchemaHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a new Dataset based on the schema of the original dataset
                 with additional fields added to every table 
        Notes:   Supports "id" and "seq" as additinal fields 
        @param phDataset The handle of the original dataset
        @param pcFieldNames The comma delimited list of field names to add
        @return The handle of the new dataset with the additional fields
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC HANDLE AddFieldsToDataset (phDataset AS HANDLE,     
	                                         pcFieldNames AS CHARACTER):
		
        DEFINE VARIABLE hDataset  AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hTable    AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hBuffer   AS HANDLE  NO-UNDO .
        DEFINE VARIABLE iBuffer   AS INTEGER NO-UNDO .
        DEFINE VARIABLE hRelation AS HANDLE  NO-UNDO .
        DEFINE VARIABLE iRelation AS INTEGER NO-UNDO .
        
        CREATE DATASET hDataset .  
        ASSIGN hDataset:NAME = phDataset:NAME . 

        DO iBuffer = 1 TO phDataset:NUM-BUFFERS:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (iBuffer) .
            
            CREATE TEMP-TABLE hTable . 
            hTable:ADD-FIELDS-FROM (hBuffer) .
            
            IF ListHelper:EntryIsInList ("id":U, pcFieldNames) THEN 
                hTable:ADD-NEW-FIELD ("id":U, "character":U) .
            
            IF ListHelper:EntryIsInList ("seq":U, pcFieldNames) THEN DO: 
                hTable:ADD-NEW-FIELD ("seq":U, "integer":U) .
                hTable:ADD-NEW-INDEX ("seq":U, TRUE, TRUE).
                hTable:ADD-INDEX-FIELD("seq":U, "seq":U).
            END.
            
            hTable:TEMP-TABLE-PREPARE (hBuffer:NAME) .
            
            hDataset:ADD-BUFFER (hTable:DEFAULT-BUFFER-HANDLE) .
        END.

        DO iRelation = 1 TO phDataset:NUM-RELATIONS:
            ASSIGN hRelation = phDataset:GET-RELATION (iRelation) . 
            
            hDataset:ADD-RELATION (hDataset:GET-BUFFER-HANDLE (hRelation:PARENT-BUFFER:NAME), 
                                   hDataset:GET-BUFFER-HANDLE (hRelation:CHILD-BUFFER:NAME),   
                                   hRelation:RELATION-FIELDS, 
                                   hRelation:REPOSITION , 
                                   hRelation:NESTED, 
                                   hRelation:ACTIVE, 
                                   hRelation:RECURSIVE, 
                                   hRelation:FOREIGN-KEY-HIDDEN) . 
        END. 

        RETURN hDataset . 

	END METHOD .

END CLASS.
