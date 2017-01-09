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
    File        : ttWidget.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Tue Nov 19 07:58:48 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttWidget NO-UNDO 
    FIELD FieldName         AS CHARACTER FORMAT "x(30)":U
    FIELD FieldDataType     AS CHARACTER FORMAT "x(30)":U
    FIELD WidgetType        AS CHARACTER FORMAT "x(30)":U
    FIELD FieldFormat       AS CHARACTER FORMAT "x(30)":U
    FIELD AtRow             AS DECIMAL 
    FIELD AtColumn          AS DECIMAL 
    FIELD ViewAs            AS CHARACTER FORMAT "x(30)":U
    FIELD ViewAsWidth       AS DECIMAL  
    FIELD ViewAsHeight      AS DECIMAL  
    FIELD WidgetValue       AS CHARACTER FORMAT "x(30)":U
    FIELD WidgetLabel       AS CHARACTER FORMAT "x(30)":U
    FIELD NoLabels          AS LOGICAL INIT FALSE 
    FIELD RadioButtons      AS CHARACTER 
    FIELD ListItems         AS CHARACTER 
    FIELD ListItemPairs     AS CHARACTER
    
    INDEX FieldName IS UNIQUE PRIMARY FieldName .
    