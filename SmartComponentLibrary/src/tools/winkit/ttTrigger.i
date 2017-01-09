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
    File        : ttTrigger.i
    Purpose     : 

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 30 13:48:18 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTrigger NO-UNDO 
    FIELD FileName               AS CHARACTER 
    FIELD LineAnfang             AS INTEGER
    FIELD ColumnAnfang           AS INTEGER 
    FIELD SurroundingStartLine   AS INTEGER 
    FIELD SurroundingStartColumn AS INTEGER 
    FIELD SurroundingEndLine     AS INTEGER 
    FIELD SurroundingEndColumn   AS INTEGER 
    FIELD EnclosedStartLine      AS INTEGER 
    FIELD EnclosedStartColumn    AS INTEGER
    FIELD EnclosedEndLine        AS INTEGER
    FIELD EnclosedEndColumn      AS INTEGER 
    FIELD ProcedureName          AS CHARACTER 
    FIELD PersistentTrigger      AS LOGICAL INIT FALSE 
    FIELD EventList              AS CHARACTER 
    FIELD WidgetRef              AS CHARACTER
    INDEX TriggerPosition IS UNIQUE PRIMARY
        FileName LineAnfang ColumnAnfang 
  .
  