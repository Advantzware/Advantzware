/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\RMS\I864.P
**       By: Chris Heins, Report Concepts (c) 1997 All Rights Reserved.
** Descript: Inbound Text Message (Sears Store Directory)
**
*****************************************************************************
\***************************************************************************/
DEF STREAM s-in.
DEF SHARED STREAM s-out.
DEF BUFFER DC FOR edshipto.
{ed/sharedv.i}
{ed/edivars.i}
{ed/rms/sharedv.i "new shared"}
{rc/stats.i}
{rc/stringv.i}
{rc/datev.i}
{rc/timev.i}
{rc/ercvars.i 60 "initial ''" "new"}
{rc/fcurrent.i "row 3"}
DEF VAR fid AS CHAR FORMAT 'x(12)' NO-UNDO EXTENT 1 INITIAL
  [ "" ].
DEF VAR temp_fid      AS CHAR NO-UNDO.
DEF VAR err_fid AS CHAR NO-UNDO.
DEF VAR curr_fid     AS CHAR NO-UNDO.  /* used for deletion when done */
DEF VAR n AS INT NO-UNDO FORMAT "9" INITIAL 1.
DEF VAR debug AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR debug_msg AS CHAR NO-UNDO.
DEF VAR ws_version AS CHAR NO-UNDO.
DEF VAR ws_customer LIKE edmast.cust NO-UNDO.
DEF VAR ws_isa  AS INT NO-UNDO.
DEF VAR ws_gs   AS INT NO-UNDO.
DEF VAR ws_st   AS INT NO-UNDO.
DEF VAR archive_fid AS CHAR NO-UNDO.
DEF VAR ws_fext AS CHAR NO-UNDO.
/* set true if we have to skip to the next header */
DEF VAR skip_doc AS LOGICAL NO-UNDO.
DEF VAR ws_message AS CHAR FORMAT 'x(40)' NO-UNDO LABEL "Message".
DEF VAR ws_terminator AS CHAR NO-UNDO.
{ed/getpath.i}
ASSIGN
  temp_fid = "t_" + STRING(TIME,"99999") + ".q"
  .
RUN rc/dt2fext.p (TODAY, OUTPUT ws_fext).
err_fid = edco.path-err + dirsep + "edierr" + ws_fext.
start = TIME.
n = 1.
dtl_rec_code = 30.
{rc/statline.i}
_files:
REPEAT n = 1 TO 1:
  IF debug THEN
  DO:
    DISPLAY
      ws_rec_code WITH FRAME f-debug.
  END.
  curr_fid = ws_edi_path + (IF fid[n] > "" THEN
  dirsep + LC(fid[n]) ELSE ""
  ).
  curr_fid = SEARCH(curr_fid).
  IF SEARCH(curr_fid) = ? THEN
  DO:
    IF n <= 1 /* no header file */ THEN
    DO:
      PUT STREAM s-out UNFORMATTED SKIP(1)
        "Input file: " curr_fid " was not found, cannot continue" SKIP.
      RETURN.
    END.
    ELSE
    NEXT _files.
  END.
  /* assigment required to put the archive in the same directory */
  ELSE
  curr_fid = SEARCH(curr_fid).
  archive_fid = curr_fid.
  ws_int = R-INDEX(archive_fid, ".").
  IF ws_int > 0          /* strip off extension */
    THEN
  archive_fid = SUBSTRING(archive_fid, 1, ws_int - 1).
  archive_fid = archive_fid + ws_fext.  /* add MDD extension */
  IF debug THEN
  DO:
    MESSAGE "running quoter".
    PAUSE.
  END.
  RUN rc/osquoter.p
    (curr_fid, ?, ?, temp_fid).
  INPUT STREAM s-in FROM VALUE(temp_fid) NO-ECHO.
  PUT STREAM s-out UNFORMATTED SKIP(1) "Processing from file: " curr_fid SKIP.
  PAUSE 0.
  _main:
  REPEAT:
    ASSIGN str_buffa = '' erclist = ''.
    IMPORT STREAM s-in str_buffa.
    {rc/incr.i ws_recs_read}.
    IF debug THEN
    DO:
      DISPLAY
        STR_BUFFA FORMAT "X(50)"  LABEL "Current Record (Leading 50 Chars)"
        WITH FRAME f-debug.
      PAUSE 1.
    END.
    IF str_buffa <= " " THEN
    NEXT _main.
    IF SUBSTRING(str_buffa,1,5) = rms_sep_code THEN
    DO:
      ASSIGN
        ws_rec_code = 0
        last_rec_code = 0
        skip_doc = FALSE. /* reset at start of new document */
    END.
    ELSE
    DO:
      IF skip_doc THEN
      NEXT _main.
      ws_rec_code = INTEGER(SUBSTRING(str_buffa,1,2)).
    END.
    IF debug THEN
    DISPLAY ws_rec_code last_rec_code transaction_purpose_code
    WITH FRAME f-debug.
    IF WS_REC_CODE >= dtl_rec_code
      AND (ws_rec_code < LAST_REC_CODE
      OR (last_rec_code = 30 AND transaction_purpose_code = "03")
      OR   ws_rec_code = 35)
      THEN
    DO:
      /* 35 is the last record type, create detail here */
      IF DEBUG THEN DO:
      MESSAGE
       "About to do the IO: ws_rec_code" ws_rec_code
        "last" last_rec_code
        "purpose" transaction_purpose_code.
      PAUSE.
      END.
      FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE-LOCK.
      /*
      IF transaction_purpose_code = "05"  /* replace */
      OR transaction_purpose_code = "03"  /* delete */
      THEN
      DO:    /* location existing if possible */
      */
      FIND edshipto
        WHERE edshipto.partner = eddoc.partner
        AND edshipto.ref-type  = duns_qualifier
        AND edshipto.by-code = ordering_store_number
        EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL edshipto THEN
      DO:
        IF transaction_purpose_code <> "03"
          THEN
        DO:
          DELETE edshipto.
          {rc/incr.i ws_recs_deleted}.
        END.
        ELSE
        DO:
          /* ON DELETES, must stamp the record so that the action
          will pass through to the application in phase 2 ... */
          ASSIGN
            edshipto.cust-region = "DELETED"
            {rc/incr.i ws_recs_changed}.
        END.
      END.
      IF transaction_purpose_code = "00"  /* original */
        OR transaction_purpose_code = "02"  /* add */
        OR transaction_purpose_code = "05"  /* replace */
        THEN
      DO:
        IF sales_region > ""
          AND shipto_store_number = ""
          THEN
        DO:   /* xref to default ST code */
          FIND FIRST dc
            WHERE dc.partner = ws_partner
            AND dc.ref-type = "ST"
            AND dc.dest-zone = sales_region
            and dc.dest-zone > ""
            and dc.CUST-region <> "DELETED"
            NO-LOCK NO-ERROR.
          IF AVAIL dc THEN
          shipto_store_number = dc.by-code.
        END.
        CREATE edshipto.
        IF NAME3 > "" then DO:
            name2 = name2
                + (if name2 > "" then "-" else "")
                + name3.
            name3 = ''.
        end.
        if name2 > '' and address2 = ''
        then assign
            address2 = address1
            address1 = name2.
        else if name2 > "" then do:
            company_name = company_name
                + (if company_name > "" then "-" else "")
                + name2.
        end.
        ASSIGN
          edshipto.Partner     = eddoc.Partner
          edshipto.Ref-type    = duns_qualifier
          edshipto.By-code     = ordering_store_number
          edshipto.St-code     = shipto_store_number
          edshipto.Cust        = edmast.Cust
          edshipto.Ship-to     = ?
          edshipto.Name        = company_name
          edshipto.Addr1       = address1
          edshipto.Addr2       = address2
          edshipto.City        = City
          edshipto.State       = State
          edshipto.Zip         = Zip
          edshipto.Country     = Country
          edshipto.Attention   = contact_name
          edshipto.Phone       = contact_phone_number
          edshipto.Opened      = extra_date#
          edshipto.Description = eddoc.docid  /* second_Description */
          edshipto.Dest-Zone   = sales_region
          edshipto.Fax         = ws_Fax
          edshipto.Comments[1] = note
          .
        /*
        edshipto.Comments[2] = {2}.Comments[2]
        edshipto.Comments[3] = {2}.Comments[3]
        edshipto.Comments[4] = {2}.Comments[4]
        edshipto.Comments[5] = {2}.Comments[5]
        edshipto.Cust-Region = {2}.Cust-Region
        */
        {rc/incr.i ws_recs_added}.
      END.    /* create */
    END.    /* ws_rec_code <= last rec code */
    IF ws_rec_code = 0 THEN
    DO: /* header record */
      ASSIGN
        {rc/incr.i ws_recs_selected}
        {rc/substr.i rms_header_partner         6  5}
        {rc/substr.i rms_header_setid          11  3}
        {rc/substr.i rms_header_std-ver        14 12}
        {rc/substr.i rms_header_int-cd         26 30}
        {rc/substr.i rms_header_company-id     56  5}
        .
      IF debug THEN
      DO:
        DISPLAY
          rms_header_partner
          rms_header_setid
          rms_header_std-ver
          rms_header_int-cd
          rms_header_company-id
          .
      END.
      FIND FIRST edcode
        WHERE edcode.partner =
        (IF rms_header_company-id > "" THEN rms_header_company-id
        ELSE rms_header_partner)
        AND edcode.setid   = rms_header_setid
        AND INTEGER(edcode.version) = INTEGER(rms_header_std-ver)
        AND edcode.direction = "I" NO-LOCK NO-ERROR.
      IF AVAIL edcode THEN
      ws_edcode_rec = RECID(edcode).
      ELSE
      DO:
        DISPLAY STREAM s-out
          rms_header_partner
          rms_header_setid
          rms_header_std-ver
          rms_header_int-cd
          rms_header_company-id
          "No corresponding edcode for RMS header"  @ ws_message
          WITH FRAME f-rms-header 1 DOWN WIDTH 132.
        skip_doc = TRUE. /* unknown partner or set or version */
        NEXT _main.
      END.
      /* resets */
      ASSIGN
        ws_eddoc_rec = ?
        ws_docid = ""
        ws_edcode_rec = RECID(edcode)
        ws_partner = edcode.partner
        ws_setid = edcode.setid
        ordering_store_number = ""
        shipto_store_number = ""
        last_rec_code = 0
        .
    END.    /* separator rec, code = 0  */
    /* check to see if the current record is the first detail line.
    if so, create headers from data stored in variables.
    */
    IF ws_rec_code >= dtl_rec_code THEN
    DO:
      IF last_rec_code < dtl_rec_code THEN
      _doc_header:
      DO:
        if debug then do:
         message "in doc_header" ws_partner ws_setid ws_docid.
         pause.
        end.
        FIND FIRST eddoc
          WHERE eddoc.partner = ws_partner
          AND eddoc.setid = ws_setid
          AND eddoc.docid = ws_docid
          /*
          AND eddoc.docseq = INTEGER(location_number)
          AND eddoc.isa = ws_isa
          AND eddoc.gs  = ws_gs
          AND eddoc.st  = ws_st
          AND eddoc.stat = 0
          */
          NO-ERROR.
        IF NOT AVAIL eddoc THEN
        DO:
          FIND edmast
            WHERE RECID(edmast) = ws_edmast_rec EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL edmast THEN
          DO:
            {rc/listadd.i erclist 201}
            erctoken[1] = ws_partner.
            LEAVE _doc_header.
          END.
          ASSIGN
            ws_customer = edmast.cust
            ws_isa = ?.
          FIND edcode WHERE RECID(edcode) = ws_edcode_rec EXCLUSIVE-LOCK
            NO-ERROR.
          IF NOT AVAIL edcode THEN
          DO:
            {rc/listadd.i erclist 301}
            erctoken[3] = "864".
            LEAVE _doc_header.
          END.
          RUN ed/gendoc.p (RECID(edcode), ws_docid, OUTPUT ws_eddoc_rec).
          FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
          ASSIGN
            eddoc.docseq        = INTEGER(location_number)
            eddoc.st-code       = location_number
            eddoc.status-flag   = "RCV"
            eddoc.isa           = ws_isa
            eddoc.gs            = ws_gs
            eddoc.st            = ws_st
            eddoc.fgsender      = rms_header_partner
            eddoc.setid         = rms_header_setid
            eddoc.version       = rms_header_std-ver
            eddoc.userref       = rms_header_int-cd
            eddoc.fgrecvid      = rms_header_company-id
            .
          IF debug THEN
          DO:
            MESSAGE "after create eddoc".
            PAUSE.
          END.
          {rc/incr.i ws_amt_added}.
        END.
        ELSE
        IF AVAIL eddoc THEN
        DO:
          PUT STREAM s-out UNFORMATTED SKIP
            "Duplicate document detected for partner: " eddoc.partner
            " document-id: " eddoc.docid.
          IF eddoc.stat > 0 THEN
          DO:
            /* duplicate ??? */
            PUT STREAM s-out UNFORMATTED
              " has already been processed - duplicate skipped" SKIP.
            {rc/listadd.i erclist 1002}
            erctoken[7] = ws_docid.
            {rc/incr.i ws_amt_inerror}.
            skip_doc = TRUE.
          END.
          ELSE
          DO:
            PUT STREAM s-out UNFORMATTED
              " not yet processed - duplicate deleted" SKIP.
            RUN ed/fm864del.p (RECID(eddoc)).
            {rc/incr.i ws_amt_changed}.
          END.
        END.
        ws_eddoc_rec = RECID(eddoc).
        IF erclist = '' THEN
        DO:
        END.
      END.  /* first detail line, create headers */
      IF erclist = "" THEN
      DO:
        /* find headers if into detail lines */
        {rc/incr.i ws_recs_selected}.
        FIND eddoc    WHERE RECID(eddoc)    = ws_eddoc_rec EXCLUSIVE.
        IF ws_rec_code = 30 THEN
        DO:
          ASSIGN
            {rc/substr.i  company_name             5 35}
            {rc/substr.i  duns_qualifier          40  2}
            {rc/substr.i  ordering_store_number   42 17}
            {rc/substr.i  name2                   63 35}
            {rc/substr.i  name3                   98 35}
            .
          IF duns_qualifier = "92" THEN
          duns_qualifier = "BY".
          Assign
            address1 = ''
            address2 = ''
            city = ''
            state = ''
            zip = ''
            country = ''
            reference_2_qualifier = ""
            reference_2 = ""
            reference_3 = ""
            sales_region = ""
            contact_name = ""
            contact_number_qualifier = ''
            ws_fax = ''
            contact_phone_number = ''
            note = ''
            ws_terminator = ''
            shipto_store_number = ""
            .
        END.
        ELSE
        IF ws_rec_code = 31 THEN
        DO:
          ASSIGN
            {rc/substr.i address1           5 35}
            {rc/substr.i address2           40 33}
            {rc/substr.i city               73 19}
            {rc/substr.i state              92  2}
            {rc/substr.i zip                94  9}
            {rc/substr.i country            103 2}
            .
            IF LENGTH(ZIP) = 9
            THEN ZIP = SUBSTRING(ZIP,1,5) + '-' + SUBSTRING(ZIP,6,4).
        END.
        ELSE
        IF ws_rec_code = 32 THEN
        DO:
          ASSIGN
            {rc/substr.i reference_2_qualifier   3 02}
            {rc/substr.i reference_2            12 07}   /* dest del zone */
            {rc/substr.i reference_3            22 07}   /* ST or ZZ */
            .
          IF reference_2_qualifier = "ST" THEN
          DO:
            IF ordering_store_number = ""
              THEN
            ordering_store_number = reference_2.
            {rc/xyymmdd.i reference_3 extra_date#}
          END.
          ELSE
          IF reference_2_qualifier = "ZZ" THEN
          DO:
            sales_region = reference_2.
          END.
          ELSE
          DO:
            ASSIGN
              reference_2_qualifier = ""
              reference_2 = ""
              reference_3 = ""
              sales_region = ""
              .
          END.
        END.
        ELSE
        IF ws_rec_code = 33 THEN
        DO:
          ASSIGN
            {rc/substr.i contact_name              5 35}
            {rc/substr.i contact_number_qualifier 40  2} .
            IF contact_number_qualifier = "FX" then
            {rc/substr.i ws_fax                   67 25} .
            else {rc/substr.i contact_phone_number     42 25} .
        END.
        ELSE
        IF ws_rec_code = 35 THEN
        DO:
          ASSIGN
            {rc/substr.i note                     3 64}
            {rc/substr.i ws_terminator           65  2}
            .
        END.
        ELSE
        DO:
          erctoken[9] = STRING(ws_rec_code).
          {rc/listadd.i erclist 2001}.
        END.
      END.
    END.
    ELSE
    DO:    /* header rec types accum into variables */
      IF ws_rec_code = 10 THEN
      DO:
        ASSIGN
          {rc/substr.i transaction_purpose_code              3   2}
          /* 00=Original, 02=ADD, 03=DELETE, 05=REPLACE */
          {rc/substr.i second_description               5  80}
          /* STORE LIST NUMBER */
          {rc/substr.i purchase_order_type                85   2}
          /* 01=LOCATION ADDRESS INFO */
          {rc/substr.i send_date                97   6}
          .
        {rc/xyymmdd.i send_date send_date#}
        ws_docid = send_date + "," + second_description + "," +
        transaction_purpose_code.
        IF DEBUG THEN DO:
        MESSAGE "DocID" ws_docid.
        PAUSE.
        END.
      END.
    END.
    IF erclist > '' THEN
    DO:
      {rc/incr.i ws_recs_inerror}.
      DISPLAY STREAM s-out
        ws_partner
        COLUMN-LABEL "PARTNER"
        ws_docid
        COLUMN-LABEL "DOCUMENT-ID"
        location_number  FORMAT 'X(08)'
        COLUMN-LABEL "LOCATION"
        record_type
        COLUMN-LABEL "RT"
        unique_order_number
        COLUMN-LABEL "UNIQUE ORD#"
        line_sequence_number
        COLUMN-LABEL "SEQ#"
        edi_transmission_control_number
        COLUMN-LABEL "ISA#"
        edi_funct_group_control_number
        COLUMN-LABEL "FG#"
        edi_set_control_number
        COLUMN-LABEL "SET#"
        edi_standard
        COLUMN-LABEL "STD"
        edi_version
        COLUMN-LABEL "VERSION"
        ws_setid
        COLUMN-LABEL "DOC"
        WITH FRAME f-det DOWN WIDTH 185.
      {rc/ercput.i "stream s-out"}
    END.
    last_rec_code = ws_rec_code.
  END. /* repeat */
  PUT STREAM s-out UNFORMATTED
    SKIP(1) "Records added        : " ws_recs_added  FORMAT "-99999" SKIP.
  IF ws_amt_inerror > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicates rejected  : " ws_amt_inerror FORMAT "-99999" SKIP.
  IF ws_amt_changed > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicated recycled  : " ws_amt_changed FORMAT "-99999" SKIP.
  INPUT STREAM s-in CLOSE.
  RUN rc/oscopy.p (curr_fid, archive_fid).  /* save the input file as .MDD */
  ws_char = temp_fid.                       /* delete quoter temp file */
  IF SEARCH(archive_fid) <> ? THEN
  DO:      /* verify file is backed up */
    PUT STREAM s-out UNFORMATTED
      SKIP(1) "Input file saved as: " archive_fid SKIP.
    {rc/listadd.i ws_char curr_fid}         /*  add it to deletion list */
  END.
  RUN rc/osdel.p (ws_char).                 /* deletes list of files */
END.
IF debug THEN
HIDE FRAME f-debug NO-PAUSE.
/*
----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
*****SEARD864003020                                    SEARA
1000GENERIC TEST FILE                                                          01     097  930115         CQ   SEARS MDSE. GROUP
30CQSEARS ROEBUCK & CO.                920124                 PHILADELPHIA
31  4640 ROOSEVELT BLVD                                                 PHILADELPHIA       PA191320000
32ST       0124      670201
33IC                                   TE215-831-4117
30CQSEARS ROEBUCK & CO.                920251                 PHOENIX DIST OFFICE
31  PARADISE VALLEY MALL               4604 E CACTUS RD                 PHOENIX            AZ85032
32ST       0251      920401
33IC                                   TE602-954-7150
30CQSEARS ROEBUCK & CO.                920425                 JACKSONVILLE - RRC
31  10512 BUSCH DR                                                      JACKSONVILLE       FL32298
32ST       0425      881101
32ZZ       DZ06
33IC                                   FX                         904-751-8506
33IC                                   TE904-751-8510
30CQSEARS ROEBUCK & CO.                920440                 MANTENO - RRC
31  1600 BOUDREAU RD                                                    MANTENO            IL60950
32ST       0440      900501
32ZZ       DZ11
33IC                                   FX                         815-468-2156
33IC                                   TE815-468-2000
30CQSEARS ROEBUCK & CO.                920441                 KANSAS CITY - RRC
31  1215 E 12TH AVE                                                     N KANSAS CITY      MO64116
32ST       0441      881101
32ZZ       DZ12
32ST       0444      881101
32ZZ       DZ03
33IC                                   FX                         215-831-3197
33IC                                   TE215-831-3201
30CQSEARS ROEBUCK & CO.                920446                 MEMPHIS - RRC
31  3456 MEYERS AVE                                                     MEMPHIS            TN38108
32ST       0446      881101
32ZZ       DZ08
33IC                                   FX                         901-325-7409
33IC                                   TE901-325-7401
30CQSEARS ROEBUCK & CO.                920447                 GARLAND - RRC
31  2775 W MILLER RD                   BLDG F & G                       GARLAND            TX75041
32ST       0447      881101
32ZZ       DZ14
33IC                                   FX                         214-864-2775
33IC                                   TE800-326-1495
30CQSEARS ROEBUCK & CO.                920448                 LOS ANGELES - RRC
31  825 S VAIL AVE                                                      MONTEBELLO         CA90640
32ST       0448      881101
32ZZ       DZ18
33IC                                   FX                         213-728-3361
33IC                                   TE213-887-6800
30CQSEARS ROEBUCK & CO.                920470                 COLUMBUS-CMDC
31  4711 FISHER RD                                                      COLUMBUS           OH432160000
32ST       0470      720619
32ZZ       DZ09
33IC                                   FX                         614-272-3454
33IC                                   TE614-272-3000
30CQSEARS ROEBUCK & CO.                920475                 JACKSONVILLE
31  1 IMESON PARK BLVD                                                  JACKSONVILLE       FL322970000
32ST       0475      750115
32ZZ       DZ06
33IC                                   FX                         904-696-3103
33IC                                   TE904-696-1000
*/
