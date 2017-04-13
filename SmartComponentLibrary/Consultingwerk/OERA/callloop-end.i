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
    File        : callloop-end.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 22 14:54:24 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
                LEAVE callloop .

                CATCH callooperr AS Progress.Lang.SysError:
                    IF ListHelper:EntryIsInList(STRING (callooperr:GetMessageNum(1)), "9326,9407,5490,14810,5451,5453":U) THEN DO:
                        IF NOT THIS-OBJECT:HandleDisconnection (pcPartition,
                                                                ErrorHelper:FormattedErrorMessages (callooperr)) THEN DO:
                            System.Windows.Forms.Application:Exit () .
                            LEAVE callloop .
                        END .
                        ELSE DO:
                            ASSIGN hAppServer = hCachedServerHandle .
                        END.
                    END.
                    ELSE
                        UNDO, THROW callooperr .
                END CATCH.
            END .
        &ENDIF
