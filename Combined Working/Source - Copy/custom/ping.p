/******************************************************************************

    Program:        ping.p
    
    Written By:     Marian EDU
    Written On:     September 2002
    
    Description:    Used to do ping or traceroute to one specific host address. 
                    Host name is not supported, works only with IP address.
    Parameters:     Input   -   IP address
                            -   ping && traceroute options
                            -   show result message flag
                    Output  -   host available flag
    Note:           Options:    You can specify ping && traceroute options 
                                as a comma delimited string.
                                ex: '-t,-n 10,-i 20,-l 32,-w 300'
                                    will send maximum 10 echo requests with
                                    32 bytes of data for each host on trace 
                                    route with the 300 milliseconds time-out
                                    and the maximum TTL is 20 and traceroute
                                    is enabled.
                            -t  enable traceroute
                            -n  number of request to send
                            -i  time to live TTL
                            -l  send packet size
                            -w  time-out in milliseconds to wait for reply
    Examples:       ping.p('66.218.71.86', '-t,-w 300,-n 10,-l 320,-i 20', TRUE, OUTPUT lAvail)
                            Will do a traceroute to yahoo servers using a 
                            320 bytes data packet, with a maximum hops number 
                            of 20 (TTL), for each host in trace route will send 
                            a maximum 10 echo request until will get an answer 
                            using 300 milliseconds time-out. 
                            Cause the show result message flag is true will 
                            display the traceroute result at the end.
                            
                                
    --------------------- Revision History ------------------
    
    Date:     Author        Change Description
    
    23/09/02  M EDU         Initial Release
    24/09/02  M EDU         Traceroute implemented, more options available

******************************************************************************/


DEFINE INPUT    PARAMETER pcHostAddr    AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER pcOptions     AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER plShowResults AS LOGICAL   NO-UNDO.
DEFINE OUTPUT   PARAMETER plAlive       AS LOGICAL   NO-UNDO.


DEFINE VARIABLE iNoRetry                AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPacketSize             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTimeOut                AS INTEGER    NO-UNDO.
DEFINE VARIABLE iMaxHops                AS INTEGER    NO-UNDO.
DEFINE VARIABLE lEnableTrace            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE ReqData                 AS MEMPTR     NO-UNDO.
DEFINE VARIABLE ReplyBuf                AS MEMPTR     NO-UNDO.
DEFINE VARIABLE PIP_OPTION_INFORMATION  AS MEMPTR     NO-UNDO.
DEFINE VARIABLE HopAddr                 AS MEMPTR     NO-UNDO.

DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO   EXTENT 3.
DEFINE VARIABLE iRes        AS INTEGER    NO-UNDO.
DEFINE VARIABLE iIcmpHdl    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iDstAddr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cHostAddr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEntry      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER  NO-UNDO.

/* API definitions                                  */
/* Microsoft has their own proprietary API for ping 
   and tracert implemented in ICMP.DLL.
   The functions in ICMP.DLL are not considered part 
   of the Win32 API and might not be supported in 
   future releases.                                 */

PROCEDURE IcmpCreateFile EXTERNAL 'ICMP.DLL':
    DEFINE RETURN PARAMETER phIcmp      AS LONG.
END PROCEDURE.

PROCEDURE IcmpCloseHandle EXTERNAL 'ICMP.DLL':
    DEFINE INPUT PARAMETER phIcmp       AS LONG.
END PROCEDURE.

PROCEDURE IcmpSendEcho EXTERNAL 'ICMP.DLL':
    DEFINE INPUT PARAMETER phIcmp       AS LONG.
    DEFINE INPUT PARAMETER DstAddr      AS LONG.
    DEFINE INPUT PARAMETER ReqData      AS LONG.
    DEFINE INPUT PARAMETER ReqSize      AS LONG.
    DEFINE INPUT PARAMETER ReqOptions   AS LONG. 
    DEFINE INPUT PARAMETER ReplyBuf     AS LONG.
    DEFINE INPUT PARAMETER ReplySize    AS LONG. 
    DEFINE INPUT PARAMETER Timeout      AS LONG.
    DEFINE RETURN PARAMETER ReplyCount  AS LONG.
END PROCEDURE.

PROCEDURE inet_addr EXTERNAL 'WS2_32.DLL':
    DEFINE INPUT  PARAMETER HostName AS CHARACTER.
    DEFINE RETURN PARAMETER HostAddr AS LONG.
END PROCEDURE.

PROCEDURE inet_ntoa EXTERNAL 'WS2_32.DLL':
    DEFINE INPUT  PARAMETER HostAddr AS LONG.
    DEFINE RETURN PARAMETER HostName AS MEMPTR.
END PROCEDURE.


/* default options                                  */
ASSIGN
    lEnableTrace = FALSE
    iPacketSize  = 32
    iTimeOut     = 5000
    iNoRetry     = 4
    iMaxHops     = 64 NO-ERROR.

/* parse options parameter                          */
DO iCount[1] = 1 TO NUM-ENTRIES(pcOptions):
    cEntry = ENTRY(iCount[1],pcOptions).
    CASE ENTRY(1,cEntry,' ':U):
        WHEN '-t':U THEN lEnableTrace = TRUE.
        WHEN '-w'   THEN iTimeOut     = INTEGER(ENTRY(NUM-ENTRIES(cEntry,' ':U),cEntry,' ':U)) NO-ERROR.
        WHEN '-n'   THEN iNoRetry     = INTEGER(ENTRY(NUM-ENTRIES(cEntry,' ':U),cEntry,' ':U)) NO-ERROR.
        WHEN '-l'   THEN iPacketSize  = INTEGER(ENTRY(NUM-ENTRIES(cEntry,' ':U),cEntry,' ':U)) NO-ERROR.
        WHEN '-i'   THEN iMaxHops     = INTEGER(ENTRY(NUM-ENTRIES(cEntry,' ':U),cEntry,' ':U)) NO-ERROR.
    END CASE.
END.

SET-SIZE(ReqData)  = iPacketSize + 1.
DO iCount[1] = 1 TO iPacketSize:
    PUT-STRING(ReqData,iCount[1]) = CHR(32 + iCount[2]).
    iCount[2] = iCount[2] + 1.
    IF iCount[2] >= 94 THEN iCount[2] = 0.
END.
SET-SIZE(ReplyBuf) = GET-SIZE(ReqData) + 28 + 1.
SET-SIZE(PIP_OPTION_INFORMATION) = 4 + 1.
SET-SIZE(HopAddr)  = 16.
RUN inet_addr(pcHostAddr, OUTPUT iDstAddr) NO-ERROR.
RUN IcmpCreateFile(OUTPUT iIcmpHdl) NO-ERROR.
/* if valid host IP address suplied                 */
IF iDstAddr NE -1 AND iIcmpHdl NE -1 THEN DO: 
    /* traceroute - increment TTL and send a new
                    echo request                    */
    IF lEnableTrace THEN DO iCount[1] = 1 TO iMaxHops:
        PUT-LONG(PIP_OPTION_INFORMATION,1) = iCount[1].
        DO iCount[2] = 1 TO iNoRetry:
            RUN IcmpSendEcho(iIcmpHdl,
                             iDstAddr,
                             GET-POINTER-VALUE(ReqData),
                             GET-SIZE(ReqData),
                             GET-POINTER-VALUE(PIP_OPTION_INFORMATION),
                             GET-POINTER-VALUE(ReplyBuf),
                             GET-SIZE(ReplyBuf),
                             iTimeOut,
                             OUTPUT iRes).
            IF iRes > 0 THEN LEAVE.
        END.
        RUN inet_ntoa(GET-LONG(ReplyBuf,1), OUTPUT HopAddr).
        /* format the treceroute result message     */
        IF plShowResults THEN 
            cMessage = cMessage + 
                        SUBSTITUTE('Reply from &1~t time=&2ms~t TTL=&3~n',
                                   GET-STRING(HopAddr,1),
                                   STRING(GET-LONG(ReplyBuf,9)),
                                   STRING(iCount[1])).
        IF iDstAddr = GET-LONG(ReplyBuf,1) THEN DO:
            plAlive = TRUE.
            LEAVE.
        END.
    END.
    /* ping - send a number of requests using 
              the given TTL, time-out, packet size  */
    ELSE DO iCount[1] = 1 TO iNoRetry:
        PUT-LONG(PIP_OPTION_INFORMATION,1) = iMaxHops.
        RUN IcmpSendEcho(iIcmpHdl,
                         iDstAddr,
                         GET-POINTER-VALUE(ReqData),
                         GET-SIZE(ReqData),
                         GET-POINTER-VALUE(PIP_OPTION_INFORMATION),
                         GET-POINTER-VALUE(ReplyBuf),
                         GET-SIZE(ReplyBuf),
                         iTimeOut,
                         OUTPUT iRes).
        IF iRes = 0 THEN NEXT.
        RUN inet_ntoa(GET-LONG(ReplyBuf,1), OUTPUT HopAddr).
        /* format the ping result message           */
        IF plShowResults THEN 
            cMessage = cMessage + 
                        SUBSTITUTE('Reply from &1~t bytes=&2~t~ttime=&3ms~t TTL=&4~n',
                                   GET-STRING(HopAddr,1),
                                   STRING(GET-LONG(ReplyBuf,13)),
                                   STRING(GET-LONG(ReplyBuf,9)),
                                   STRING(GET-LONG(ReplyBuf,21))).
        IF iRes > 0 THEN plAlive = TRUE.
    END.
END.
RUN IcmpCloseHandle(iIcmpHdl).

SET-SIZE(HopAddr)                   = 0.
SET-SIZE(PIP_OPTION_INFORMATION)    = 0.
SET-SIZE(ReqData)                   = 0.
SET-SIZE(ReplyBuf)                  = 0.
IF cMessage NE '':U THEN
    MESSAGE cMessage VIEW-AS ALERT-BOX.


 
