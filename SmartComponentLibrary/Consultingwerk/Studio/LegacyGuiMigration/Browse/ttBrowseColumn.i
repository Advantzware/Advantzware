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
    File        : ttBrowseColumn.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Tue Nov 19 07:58:48 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttBrowseColumn NO-UNDO 
    FIELD Order             AS INTEGER 
    FIELD FieldName         AS CHARACTER FORMAT "x(30)":U
    FIELD FieldDataType     AS CHARACTER FORMAT "x(30)":U
    FIELD FieldFormat       AS CHARACTER FORMAT "x(30)":U
    FIELD ViewAs            AS CHARACTER FORMAT "x(30)":U
    FIELD FieldWidth        AS DECIMAL    
    FIELD WidgetLabel       AS CHARACTER FORMAT "x(30)":U
    FIELD ListItems         AS CHARACTER 
    FIELD ListItemPairs     AS CHARACTER
    FIELD Expression        AS CHARACTER
    FIELD ColumnName        AS CHARACTER
    
    INDEX Order IS UNIQUE PRIMARY Order 
    INDEX FieldName FieldName .
    