/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttDataSources.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 29 13:17:56 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDataSources NO-UNDO
    FIELD Sequence             AS INTEGER    LABEL "Sequence":T
    FIELD TableName            AS CHARACTER  LABEL "Table Name":T
    FIELD DataSourceName       AS CHARACTER  LABEL "Name":T
    FIELD GenerateDataSource   AS LOGICAL    LABEL "Generate":T               INIT TRUE
    FIELD ClientSideProcessing AS LOGICAL    LABEL "Client-side processing":T INIT FALSE
    FIELD CountFunctionName    AS CHARACTER  LABEL "Count Function":T         INIT "count":U

    INDEX Sequence IS PRIMARY UNIQUE Sequence
    INDEX Generate GenerateDataSource Sequence .
