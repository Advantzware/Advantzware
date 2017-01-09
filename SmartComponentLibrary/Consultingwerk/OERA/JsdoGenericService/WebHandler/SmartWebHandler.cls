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
    File        : SmartWebHandler
    Purpose     : Base class for WebHandler classes
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Jun 01 08:18:13 CEST 2016
    Notes       : Compilation requires %DLC%/{gui|tty}/netlib/OpenEdge.Net.pl
                  in propath and PacificWebSpeed setting in products.i
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*                         FROM PROPATH .
USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.*            FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING OpenEdge.Core.*                                     FROM PROPATH .
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH .
USING OpenEdge.Net.HTTP.Filter.Payload.*                  FROM PROPATH .
USING OpenEdge.Net.HTTP.Filter.Writer.*                   FROM PROPATH .
USING OpenEdge.Web.*                                      FROM PROPATH .
USING Progress.Json.ObjectModel.*                         FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.SmartWebHandler
&IF DEFINED (PacificWebSpeed) NE 0 &THEN
    INHERITS OpenEdge.Web.WebHandler
&ENDIF
    ABSTRACT:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN
    /**
     * Purpose: Handels an error. The error is written into the logfile and returned
     *          to the client in the body.
     * Notes:
     * @param poError An instance of a Progress.Lang.Error
     * @param poResponse The Response instance returned to the client
     */
    METHOD PROTECTED VOID HandleError (poError AS Progress.Lang.Error,
                                       poResponse AS IHttpResponse):

        poResponse:StatusCode = 500 .

        IF TYPE-OF (poError, NotAuthorizedException) THEN
            poResponse:StatusCode = 403 .
        ELSE IF TYPE-OF (poError, UnableToLaunchBusinessServiceException) THEN
                poResponse:StatusCode = 404 .

        LogManager:WriteError (poError).

        poResponse:Entity = ErrorHelper:JsonErrorDetails (poError).
        poResponse:ContentType   = 'application/json':U.

        THIS-OBJECT:WriteResponse (poResponse).

    END METHOD.

    /**
     * Purpose: Handler for unsupported methods. The request being serviced and
     *          an optional status code is returned. A zero or null value means
     *          this method will deal with all errors.
     * Notes:
     * @param poRequest The IWebRequest instance representing the call
     * @return StatusCode of the response sent to the client
     */
    METHOD OVERRIDE PROTECTED INTEGER HandleNotAllowedMethod (poRequest AS OpenEdge.Web.IWebRequest):

        /* Throwing an error from this method results in a 500/Internal Server Error response.
        The web handler will attempt to log this exception.

        See the HandleGet method's comments on choosing a value to return from this method. */

        UNDO, THROW NEW Progress.Lang.AppError ("METHOD NOT IMPLEMENTED":U).

    END METHOD.

    /**
     * Purpose: Handler for unknown methods. The request being serviced and an
     *          optional status code is returned. A zero or null value means
     *          this method will deal with all errors.
     * Notes:
     * @param poRequest The IWebRequest instance representing the call
     * @return StatusCode of the response sent to the client
     */
    METHOD OVERRIDE PROTECTED INTEGER HandleNotImplemented (poRequest AS OpenEdge.Web.IWebRequest):

        /* Throwing an error from this method results in a 500/Internal Server Error response.
        The web handler will attempt to log this exception.

        See the HandleGet method's comments on choosing a value to return from this method. */
        UNDO, THROW NEW Progress.Lang.AppError ("METHOD NOT IMPLEMENTED":U).

    END METHOD.

    /**
     * Purpose: Writes the HttpResponse to the stream
     * Notes:   Does handle OpenEdge.Core.String seperatly due to (PSC00348040)
     * @param poResponse The Response instance to write
     */
    METHOD PROTECTED VOID WriteResponse (poResponse AS IHttpResponse):

        DEFINE VARIABLE oWriter   AS WebResponseWriter NO-UNDO.
        DEFINE VARIABLE oCTWriter AS MessageWriter     NO-UNDO.
        DEFINE VARIABLE oBytes    AS ByteBucket        NO-UNDO.
        DEFINE VARIABLE mEntity   AS MEMPTR            NO-UNDO.

        DEFINE VARIABLE oBody     AS OpenEdge.Core.String NO-UNDO.

        /* dump to stream */
        oWriter = NEW WebResponseWriter (poResponse).
        oWriter:Open ().

        /* Convert from an image to bytes */
        IF VALID-OBJECT (poResponse:Entity) THEN DO:

            oCTWriter = BodyWriterBuilder:Build (poResponse):Writer.

            IF NOT TYPE-OF (poResponse:Entity, OpenEdge.Core.String) THEN DO ON ERROR UNDO, THROW:

                oCTWriter:Open ().
                oCTWriter:Write (poResponse:Entity).
                oCTWriter:Close ().

                oBytes = CAST (oCTWriter:Entity, ByteBucket).

                mEntity = oBytes:GetBytes ():VALUE.

                poResponse:ContentLength = GET-SIZE (mEntity).
                oWriter:Write (mEntity).

                FINALLY:
                    SET-SIZE (mEntity) = 0.
                END FINALLY.

            END.
            ELSE DO:

                oBody = CAST (poResponse:Entity, OpenEdge.Core.String).

                poResponse:ContentLength = oBody:Size.

                oWriter:Open ().

                /** HTTP MESSAGE ENTITY APPROACH **/
                /* The Progress.IO.OutputStream Write() methods take multiple overloads, for
                   a variety of data types. See the doc for more information. */
                oWriter:Write (oBody:Value).

            END.
        END.

        FINALLY:
            oWriter:Close ().
        END FINALLY.

    END METHOD.
&ENDIF

END CLASS.
