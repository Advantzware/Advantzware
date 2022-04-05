/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */

IF fi_job-no NE "" THEN fi_job-no = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', fi_job-no))  .


  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              NO-LOCK           

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
           


