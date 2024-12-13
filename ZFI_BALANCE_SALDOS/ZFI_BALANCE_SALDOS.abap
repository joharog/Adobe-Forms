*&---------------------------------------------------------------------*
*& Report ZETIQUETAS
*&---------------------------------------------------------------------*
REPORT zfi_balance_saldos.

TABLES: acdoca, fagl_011zc, fagl_011qt.

*&---------------------------------------------------------------------*
*&   T Y P E S  -  S T R U C T U R E S  -  T A B L E S
*&---------------------------------------------------------------------*

RANGES: r_racct FOR acdoca-racct.


DATA: ls_header      TYPE zst_balance_header,
      ls_data        TYPE zst_balance_data,
      lt_data        TYPE TABLE OF zst_balance_data,
      ls_clean       TYPE zst_balance_data,             "zst_balance_van_vienen,
      lt_clean       TYPE TABLE OF zst_balance_data,    "zst_balance_van_vienen,
      ls_vavi        TYPE zst_balance_van_vienen,
      lt_vavi        TYPE TABLE OF zst_balance_van_vienen,
      lv_langu       TYPE spras,
      total_deudor   TYPE char25,
      total_acreedor TYPE char25.

DATA: date_first  TYPE sy-datum,
      date_last   TYPE sy-datum,
      month_names TYPE TABLE OF t247,
      lv_balance  TYPE acdoca-hsl,
      lv_total_d  TYPE acdoca-hsl,
      lv_total_a  TYPE acdoca-hsl.

*&---------------------------------------------------------------------*
*&   P A R A M E T R O S   E N T R A D A
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE t01.
  PARAMETERS:
    p_ktopl LIKE acdoca-ktopl OBLIGATORY DEFAULT 'PCGS',
    p_bukrs LIKE acdoca-rbukrs OBLIGATORY DEFAULT '1001',
    p_rldnr LIKE acdoca-rldnr OBLIGATORY DEFAULT '0L',
    p_gjahr LIKE acdoca-gjahr OBLIGATORY DEFAULT sy-datum(4).
  SELECT-OPTIONS:
    s_poper FOR acdoca-poper OBLIGATORY.
SELECTION-SCREEN END OF BLOCK  b01.


START-OF-SELECTION.

  PERFORM get_data.

  PERFORM print_pdf.

END-OF-SELECTION.


FORM get_data.

*&---------------------------------------------------------------------*
*&   H E A D E R
*&---------------------------------------------------------------------*
  SELECT SINGLE * FROM t001 INTO @DATA(ls_t001)
   WHERE bukrs EQ @p_bukrs.

  CONCATENATE p_gjahr s_poper-high+1(2) '01' INTO date_first.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = date_first
    IMPORTING
      last_day_of_month = date_last.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = month_names.

  READ TABLE month_names INTO DATA(ls_month) WITH KEY mnr = s_poper-high+1(2).
  IF sy-subrc EQ 0.
    CONCATENATE 'AL' date_last+6(2) 'DE' ls_month-ltx 'DE' p_gjahr INTO ls_header-periodo SEPARATED BY space.
  ENDIF.

  TRANSLATE ls_header-periodo TO UPPER CASE.
  MOVE ls_t001-butxt TO ls_header-sociedad.
  MOVE 'BALANCE DE SALDOS' TO ls_header-balance.
*  MOVE 'AL DATE_LAST+6(2)PERIODO' TO ls_header-periodo.
  MOVE '(CIFRAS EN QUETZALES)' TO  ls_header-cifras.

*&---------------------------------------------------------------------*
*&   D A T A
*&---------------------------------------------------------------------*
  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'ES'
    IMPORTING
      output = lv_langu.

  SELECT * FROM fagl_011zc INTO TABLE @DATA(lt_011zc)
*    UP TO 20 ROWS
    WHERE versn EQ 'Z010'
      AND ktopl EQ @p_ktopl.


  SELECT * FROM fagl_011qt INTO TABLE @DATA(lt_011qt)
    WHERE versn EQ 'Z010'
      AND spras EQ @lv_langu.

  SORT lt_011zc BY vonkt ergsl.
  SORT lt_011qt BY txt45 ergsl.

  LOOP AT lt_011zc INTO DATA(ls_011zc).
    APPEND VALUE #( sign   = 'I'
                    option = 'BT'
                    low    = ls_011zc-vonkt
                    high   = ls_011zc-biskt )
                    TO r_racct.
  ENDLOOP.

  "start test
*  IF sy-uname EQ 'CONSULABAP05'.
*    REFRESH: r_racct.
*    APPEND VALUE #( sign   = 'I'
*                    option = 'BT'
*                    low    = 1101110001
*                    high   = 1101999999 )
*                    TO r_racct.
*  ENDIF.
*
*  DELETE lt_011zc WHERE ergsl NE 08.
  "end test

  SELECT * FROM acdoca INTO TABLE @DATA(lt_acdoca)
    WHERE rldnr EQ @p_rldnr
      AND rbukrs EQ @p_bukrs
      AND gjahr EQ @p_gjahr
      AND poper IN @s_poper
      AND racct IN @r_racct.
*  BREAK consulabap05 .

  LOOP AT lt_011zc INTO ls_011zc.

    READ TABLE lt_011qt INTO DATA(ls_011qt) WITH KEY ergsl = ls_011zc-ergsl.
    IF sy-subrc EQ 0.
      ls_data-cuenta = ls_011qt-txt45.
    ENDIF.


    REFRESH: r_racct.

    APPEND VALUE #( sign   = 'I'
                    option = 'BT'
                    low    = ls_011zc-vonkt
                    high   = ls_011zc-biskt )
                    TO r_racct.

    LOOP AT lt_acdoca INTO DATA(ls_acdoca) WHERE racct IN r_racct.

      lv_balance = lv_balance + ls_acdoca-hsl.
*      total_deudor = total_deudor + ls_acdoca-hsl.
*      CONDENSE: ls_data-deudor, total_deudor.



*      CASE ls_acdoca-drcrk. "bschl.
*        WHEN 'S'. "40
*          ls_acdoca-hsl = abs( ls_acdoca-hsl ).
*      ls_data-deudor = ls_data-deudor + ls_acdoca-hsl.
*      total_deudor = total_deudor + ls_acdoca-hsl.
*      CONDENSE: ls_data-deudor, total_deudor.
*        WHEN 'H'. "50
*          ls_acdoca-hsl = abs( ls_acdoca-hsl ).
*          ls_data-acreedor = ls_data-acreedor + ls_acdoca-hsl.
*          total_acreedor = total_acreedor + ls_acdoca-hsl.
*          CONDENSE: ls_data-acreedor, total_acreedor.
*      ENDCASE.
    ENDLOOP.

*    lv_totals = lv_balance.

    CASE ls_data-cuenta(1).
      WHEN 1.
        ls_data-deudor = lv_balance.
      WHEN 2.
        ls_data-acreedor = lv_balance.
      WHEN 3.
        ls_data-acreedor = lv_balance.
      WHEN 4.
        ls_data-acreedor = lv_balance.
      WHEN 5.
        ls_data-deudor = lv_balance.
      WHEN 6.
        ls_data-deudor = lv_balance.
      WHEN 7.
        ls_data-acreedor = lv_balance.
      WHEN 8.
        ls_data-deudor = lv_balance.
    ENDCASE.

    ls_clean = ls_data.
    CONDENSE: ls_clean-deudor, ls_clean-acreedor.
    APPEND ls_clean TO lt_clean.

    PERFORM van_vienen.


    lv_total_d = lv_total_d + ls_data-deudor.
    lv_total_a = lv_total_a + ls_data-acreedor.


    CASE ls_data-cuenta(1).
      WHEN 1.
        IF lv_balance < 0.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
          ls_data-deudor = |({ ls_data-deudor })|.
        ELSE.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
        ENDIF.
      WHEN 2.
        IF lv_balance > 0.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
          ls_data-acreedor = |({ ls_data-acreedor })|.
        ELSE.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
        ENDIF.
      WHEN 3.
        IF lv_balance > 0.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
          ls_data-acreedor = |({ ls_data-acreedor })|.
        ELSE.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
        ENDIF.
      WHEN 4.
        IF lv_balance > 0.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
          ls_data-acreedor = |({ ls_data-acreedor })|.
        ELSE.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
        ENDIF.
      WHEN 5.
        IF lv_balance < 0.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
          ls_data-deudor = |({ ls_data-deudor })|.
        ELSE.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
        ENDIF.
      WHEN 6.
        IF lv_balance < 0.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
          ls_data-deudor = |({ ls_data-deudor })|.
        ELSE.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
        ENDIF.
      WHEN 7.
        IF lv_balance > 0.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
          ls_data-acreedor = |({ ls_data-acreedor })|.
        ELSE.
          WRITE lv_balance TO ls_data-acreedor NO-SIGN.
          CONDENSE ls_data-acreedor.
        ENDIF.
      WHEN 8.
        IF lv_balance < 0.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
          ls_data-deudor = |({ ls_data-deudor })|.
        ELSE.
          WRITE lv_balance TO ls_data-deudor NO-SIGN.
          CONDENSE ls_data-deudor.
        ENDIF.
    ENDCASE.





    APPEND ls_data TO lt_data.
    CLEAR: ls_data, lv_balance.

  ENDLOOP.
*    BREAK CONSULABAP05.
  WRITE lv_total_d TO total_deudor NO-SIGN.
  WRITE lv_total_a TO total_acreedor NO-SIGN.

  CONDENSE: total_deudor, total_acreedor.
*
*  BREAK-POINT.
*  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
*    CASE <fs_data>-cuenta(1).
*      WHEN 1.
*        SEARCH <fs_data>-deudor FOR '-'.
*        IF sy-subrc EQ 0.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*          <fs_data>-deudor = |({ <fs_data>-deudor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*        ENDIF.
*      WHEN 2.
*        IF lv_balance > 0.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*          <fs_data>-acreedor = |({ <fs_data>-acreedor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*        ENDIF.
*      WHEN 3.
*        IF lv_balance > 0.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*          <fs_data>-acreedor = |({ <fs_data>-acreedor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*        ENDIF.
*      WHEN 4.
*        IF lv_balance > 0.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*          <fs_data>-acreedor = |({ <fs_data>-acreedor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*        ENDIF.
*      WHEN 5.
*        IF lv_balance < 0.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*          <fs_data>-deudor = |({ <fs_data>-deudor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*        ENDIF.
*      WHEN 6.
*        IF lv_balance < 0.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*          <fs_data>-deudor = |({ <fs_data>-deudor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*        ENDIF.
*      WHEN 7.
*        IF lv_balance > 0.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*          <fs_data>-acreedor = |({ <fs_data>-acreedor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-acreedor NO-SIGN.
*          CONDENSE <fs_data>-acreedor.
*        ENDIF.
*      WHEN 8.
*        IF lv_balance < 0.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*          <fs_data>-deudor = |({ <fs_data>-deudor })|.
*        ELSE.
*          WRITE lv_balance TO <fs_data>-deudor NO-SIGN.
*          CONDENSE <fs_data>-deudor.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.



ENDFORM.


FORM print_pdf.

  DATA: gv_funcname TYPE rs38l_fnam.

  DATA: ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_formoutput   TYPE fpformoutput,
        ls_result       TYPE sfpjoboutput,
        lv_subrc        TYPE string.

  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'ZAF_FI_BALANCE_SALDOS'
        IMPORTING
          e_funcname = gv_funcname.

    CATCH cx_fp_api_internal
          cx_fp_api_repository
          cx_fp_api_usage.
      MOVE sy-subrc TO lv_subrc.
  ENDTRY.

  ls_outputparams = VALUE #(
             dest = 'PDF1'
         nodialog = abap_true
          preview = abap_true ).

* Set parameters (print/send/output device etc)
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.

* Actually print Adobe form
*  ls_docparams-langu     = sy-langu.
*  ls_docparams-fillable  = p_edita.
  ls_docparams-langu     = 'S'.
  ls_docparams-replangu1 = 'S'.
  ls_docparams-replangu2 = 'E'.


  CALL FUNCTION gv_funcname
    EXPORTING
      /1bcdwb/docparams  = ls_docparams
      gs_header          = ls_header
      gt_data            = lt_data
      gt_clean           = lt_clean
      gt_vavi            = lt_vavi
      total_deudor       = total_deudor
      total_acreedor     = total_acreedor
    IMPORTING
      /1bcdwb/formoutput = ls_formoutput
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MOVE sy-subrc TO lv_subrc.
  ENDIF.

* Finish off printing
  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING
      e_result       = ls_result
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form van_vienen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM van_vienen .

  CASE ls_data-cuenta(1).
    WHEN 1.
      ls_vavi-deudor = lv_balance.
    WHEN 2.
      ls_vavi-acreedor = lv_balance.
    WHEN 3.
      ls_vavi-acreedor = lv_balance.
    WHEN 4.
      ls_vavi-acreedor = lv_balance.
    WHEN 5.
      ls_vavi-deudor = lv_balance.
    WHEN 6.
      ls_vavi-deudor = lv_balance.
    WHEN 7.
      ls_vavi-acreedor = lv_balance.
    WHEN 8.
      ls_vavi-deudor = lv_balance.
  ENDCASE.

  ls_vavi-cuenta = ls_data-cuenta.
  APPEND ls_vavi TO lt_vavi.
  CLEAR: ls_vavi.

ENDFORM.
