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
    File        : ResultsetQueryBuilder
    Purpose     : Creates and prepares the query used to join (in case multiple 
                  the tables have been chosen) the resultset before returning
                  it to the caller 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwek Ltd.
    Created     : Sat Nov 08 11:53:26 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*       FROM PROPATH .
USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.OpenAccess.* FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .   
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.ResultsetQueryBuilder: 

     /*------------------------------------------------------------------------------
        Purpose: Builds the Resultset Query                                                                       
        Notes:   Creates the Query handle and prepares it       
        @param phDataset The handle of the ProDataset
        @param pcEntityTable The entity table name
        @param pcEntityView The entity view table names
        @param plOuterJoin Logical value indicating if the query should use an OUTER-JOIN
        @return The query handle
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC HANDLE PrepareResultsetQuery (phDataset AS HANDLE, 
                                                pcEntityTable AS CHARACTER,
                                                pcEntityView AS CHARACTER,
                                                plOuterJoin AS LOGICAL):   
                                                              
        DEFINE VARIABLE hQuery           AS HANDLE    NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE j                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cQueryString     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hParentBuffer    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cChildTableQuery AS CHARACTER NO-UNDO .
    
        DatasetAssert:HasBuffer (phDataset, pcEntityTable) .
    
        ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (pcEntityTable) .
    
        IF NOT VALID-HANDLE (hBuffer) THEN 
            UNDO, THROW NEW InvalidValueException (pcEntityTable, "Entity Table":U) .
    
        CREATE QUERY hQuery .
        hQuery:ADD-BUFFER (hBuffer) .

        cQueryString = "FOR EACH ":U + pcEntityTable .

        DO i = 1 TO NUM-ENTRIES(pcEntityView) ON ERROR UNDO, THROW:
            ASSIGN hBuffer = phDataset:GET-BUFFER-HANDLE (ENTRY(i, pcEntityView)) .
            
            IF NOT VALID-HANDLE (hBuffer) THEN 
                UNDO, THROW NEW InvalidValueException (ENTRY(i, pcEntityView), "Entity View":U) .
            
            hQuery:ADD-BUFFER (hBuffer) .

            IF NOT VALID-HANDLE (hBuffer:PARENT-RELATION) THEN DO:
                UNDO, THROW NEW AppError ("Only Child-Tables of a DATA-RELATION can be used in the BindingQuery.~n~n" +
                                          "Please create or activate the DATA-RELATION first.~n"  +
                                          "(" + pcEntityTable + " ->" + pcEntityView + ")", 0) .
            END.

            /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
               Build query predicate for EntityTable based on 
               the DATA-RELATION Query, not a static buffer2 OF buffer1 */     
            IF hBuffer:PARENT-RELATION:REPOSITION = FALSE THEN DO:                                 
                ASSIGN cChildTableQuery = hBuffer:PARENT-RELATION:QUERY:PREPARE-STRING .
                
                IF cChildTableQuery BEGINS "FOR ":U OR cChildTableQuery BEGINS "PRESELECT ":U THEN 
                    ASSIGN ENTRY(1, cChildTableQuery, " ":U) = "":U .
            END.
            ELSE 
                /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
                   The Query of a REPOSITION DATA-RELATION does not contain the 
                   parent table filter condition, so we need to build it manually */
                cChildTableQuery = DatasetHelper:ChildRelationQueryString (hBuffer) . 
                
            cQueryString = cQueryString + ", ":U + cChildTableQuery +  
                           (IF plOuterJoin THEN " OUTER-JOIN ":U ELSE "":U).
        END.

        hQuery:QUERY-PREPARE (cQueryString) .

        RETURN hQuery.    

    END METHOD.

END CLASS.
