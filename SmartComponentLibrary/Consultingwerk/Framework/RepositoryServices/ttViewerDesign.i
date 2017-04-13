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
    File        : ttViewerDesign.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 09 21:29:32 CET 2010
    Notes       : This file used to reside under Consultingwerk\SmartComponents\Implementation
                  If you still have a copy there, you may delete it
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttViewerDesign NO-UNDO {&REFERENCE-ONLY}
    FIELD ControlName         AS CHARACTER
    FIELD ControlInstanceId   AS CHARACTER
    FIELD ContainerInstanceId AS CHARACTER
    FIELD ControlType         AS CHARACTER
    FIELD ControlText         AS CHARACTER
    FIELD ControlMultiline    AS LOGICAL INITIAL FALSE
    FIELD ControlSize         AS CHARACTER
    FIELD ControlLocation     AS CHARACTER
    FIELD ControlDock         AS CHARACTER
    FIELD ControlAnchor       AS CHARACTER
    FIELD BindingProperties   AS CHARACTER
    FIELD BindingFields       AS CHARACTER
    FIELD DynamicControlName  AS CHARACTER
    FIELD HAlign              AS CHARACTER
    INDEX ControlName IS UNIQUE PRIMARY ControlName ContainerInstanceId
    INDEX ContainerInstanceId ContainerInstanceId .
