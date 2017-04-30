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
    File        : SmartWebGrid
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 23:55:06 CEST 2012
    Notes       : Displays a Table
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.SmartWebGrid 
    INHERITS Control: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of BrowserColumns for this Browser                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Columns AS Consultingwerk.Web.Widgets.ListGridColumn NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Query used by the SmartWebGrid                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Query AS HANDLE NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the title of the Grid                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GridTitle AS CHARACTER NO-UNDO 
    GET.
    SET.  

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebGrid class                                                                       
        Notes:                             
        @param pcName The name of the Control                                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebGrid (pcName AS CHARACTER):
        SUPER (pcName).
        
        THIS-OBJECT:Columns = NEW ListGridColumn () . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the Output for the Web Widget                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE lAlternate        AS LOGICAL   NO-UNDO INIT FALSE.
        DEFINE VARIABLE pcColumnName      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcInitColumnWidth AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColSorting      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColType         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColAlign        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE plcDataXML        AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE lRowTrigger       AS LOGICAL   NO-UNDO INIT FALSE. 
        
        DEFINE VARIABLE iColumn           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cColumnName       AS CHARACTER NO-UNDO INIT "":U.
        
        IF NOT THIS-OBJECT:GridTitle = "":U THEN 
            THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1_Title" class="SmartGridTitle"><a>&2</a></div>':U, THIS-OBJECT:Name, THIS-OBJECT:GridTitle)) .
        
        
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1" style="height:500px" class="SmartGrid">':U, THIS-OBJECT:Name)) .
        THIS-OBJECT:OUTPUT ('<table border="0" cellspacing="0" cellpadding="0">':U) .
        THIS-OBJECT:OUTPUT ('<thead>':U) .
        THIS-OBJECT:OUTPUT ('<tr>':U) .
        {Consultingwerk/foreachABL.i GridColumn oColumn in THIS-OBJECT:Columns}
        
            THIS-OBJECT:OUTPUT (SUBSTITUTE ('<th width="&2" align="left">&1</th>':U, 
                                            oColumn:ColumnLabel,
                                            SUBSTITUTE ("&1px":U, oColumn:WIDTH))) .           
        END.
        THIS-OBJECT:OUTPUT ('</tr>':U) .  
        THIS-OBJECT:OUTPUT ('</thead>':U) .  
        THIS-OBJECT:OUTPUT ('<tbody>':U) .         
        DO WHILE NOT THIS-OBJECT:Query:QUERY-OFF-END ON ERROR UNDO, THROW:
        
            IF NOT lRowTrigger THEN                                                                                  
                THIS-OBJECT:OUTPUT ('<tr>':U) .
            ELSE   
                THIS-OBJECT:OUTPUT ('<tr class="alt">':U) .
        
            ASSIGN lRowTrigger = NOT lRowTrigger.
            
            {Consultingwerk/foreachABL.i GridColumn oColumn in THIS-OBJECT:Columns nodefine}
                oColumn:PerformBinding () .
                THIS-OBJECT:OUTPUT ('<td><div>':U) .
                THIS-OBJECT:OUTPUT (STRING (oColumn:OutputCell ())) .
                THIS-OBJECT:OUTPUT ('</div></td>':U) .  
            END.  
            
            THIS-OBJECT:OUTPUT ('</tr>':U) . 
            
            FINALLY:                                                                                                  
                THIS-OBJECT:Query:GET-NEXT () .
            END FINALLY.
        END.
        THIS-OBJECT:OUTPUT ('</tbody>':U) . 
        THIS-OBJECT:OUTPUT ('</table>':U) .
        THIS-OBJECT:OUTPUT ('</div>':U) .

    END METHOD.

END CLASS.
