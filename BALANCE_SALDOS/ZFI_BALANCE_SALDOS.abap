*&---------------------------------------------------------------------*
*& Report ZFI_BALANCE_SALDOS
*&---------------------------------------------------------------------*
REPORT zfi_balance_saldos_2.

TABLES: acdoca, zvs_balance, fagl_011zc, fagl_011qt, sscrfields.

*&---------------------------------------------------------------------*
*&   T Y P E S  -  S T R U C T U R E S  -  T A B L E S
*&---------------------------------------------------------------------*
CONSTANTS: i_folio TYPE char4 VALUE '@98@',
           v_folio TYPE tabname VALUE 'ZTFI_FOLIOS_BALA',
           i_conta TYPE char4 VALUE '@A0@',
           v_conta TYPE tabname VALUE 'ZTFI_CONTAD_BALA'.

DATA: smp_dyntxt TYPE smp_dyntxt.

RANGES: r_racct FOR acdoca-racct,
        r_1201_d FOR acdoca-racct,
        r_1201_a FOR acdoca-racct.

DATA: ls_header      TYPE zst_balance_header,
      ls_data        TYPE zst_balance_data,
      lt_data        TYPE TABLE OF zst_balance_data,
      ls_clean       TYPE zst_balance_data,
      lt_clean       TYPE TABLE OF zst_balance_data,
      ls_vavi        TYPE zst_balance_van_vienen,
      lt_vavi        TYPE TABLE OF zst_balance_van_vienen,
      lv_langu       TYPE spras,
      total_deudor   TYPE char25,
      total_acreedor TYPE char25.

DATA: date_first  TYPE sy-datum,
      date_last   TYPE sy-datum,
      month_names TYPE TABLE OF t247,
      lv_balance  TYPE acdoca-hsl,
      lv_1201_d   TYPE acdoca-hsl,
      lv_1201_a   TYPE acdoca-hsl,
      lv_empty    TYPE acdoca-hsl,
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

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      zcl_abap_tools_2=>show_updateview( tabname = v_folio ).
    WHEN 'FC02'.
      zcl_abap_tools_2=>show_updateview( tabname = v_conta ).
  ENDCASE.

INITIALIZATION.
  CLEAR smp_dyntxt.
  smp_dyntxt-text       = 'Folios'.
  smp_dyntxt-icon_id    = i_folio.
  smp_dyntxt-icon_text  = 'Folios'.
  smp_dyntxt-quickinfo  = 'Folios'.
  sscrfields-functxt_01 = smp_dyntxt.

  CLEAR smp_dyntxt.
  smp_dyntxt-text       = 'Contador'.
  smp_dyntxt-icon_id    = i_conta.
  smp_dyntxt-icon_text  = 'Contador'.
  smp_dyntxt-quickinfo  = 'Contador'.
  sscrfields-functxt_02 = smp_dyntxt.


  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = 1201110001
                  high   = 1201119999 )
                  TO r_1201_d.

  APPEND VALUE #( sign   = 'I'
                option = 'BT'
                low    = 1201210001
                high   = 1201219999 )
                TO r_1201_a.

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

  SELECT * FROM zvs_balance INTO TABLE @DATA(lt_acdoca)
    WHERE rldnr EQ @p_rldnr
      AND rbukrs EQ @p_bukrs
      AND gjahr EQ @p_gjahr
      AND poper IN @s_poper
      AND racct IN @r_racct.


  DATA: stabix TYPE char10,
        total  TYPE i,
        stotal TYPE char10,
        msg    TYPE char200.

  DESCRIBE TABLE lt_011zc LINES total.

  LOOP AT lt_011zc INTO ls_011zc.

    stabix = sy-tabix.

    "Mensaje de procesamiento en tiempo de carga
    WRITE: sy-tabix TO stabix, total TO stotal.
    CONDENSE: stabix, stotal.
    CONCATENATE 'Procesando: cuentas' ls_011zc-vonkt(4) '[' stabix ' de ' stotal ']'  INTO msg SEPARATED BY space.
    cl_progress_indicator=>progress_indicate( EXPORTING
                               i_text               = msg
                               i_processed          = sy-tabix
                               i_total              = total
                               i_output_immediately =  'X' ).


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

      IF ls_acdoca-racct IN r_1201_d.
        lv_1201_d += ls_acdoca-hsl.
      ENDIF.

      IF ls_acdoca-racct IN r_1201_a.
        lv_1201_a += ls_acdoca-hsl.
      ENDIF.

    ENDLOOP.


    CASE ls_data-cuenta(1).
      WHEN 1.
        IF ls_data-cuenta(4) EQ 1201.
          ls_data-deudor   = lv_1201_d.
          ls_data-acreedor = lv_1201_a.
        ELSE.
          ls_data-deudor   = lv_balance.
          ls_data-acreedor = lv_empty.
        ENDIF.
      WHEN 2.
        ls_data-acreedor = lv_balance.
        ls_data-deudor   = lv_empty.
      WHEN 3.
        ls_data-acreedor = lv_balance.
        ls_data-deudor   = lv_empty.
      WHEN 4.
        IF ls_data-cuenta(4) EQ 4201 OR ls_data-cuenta(4) EQ 4202.
          ls_data-deudor   = lv_balance.
          ls_data-acreedor = lv_empty.
        ELSE.
          ls_data-acreedor = lv_balance.
          ls_data-deudor   = lv_empty.
        ENDIF.
      WHEN 5.
        IF ls_data-cuenta(4) EQ 5501.
          ls_data-acreedor = lv_balance.
          ls_data-deudor   = lv_empty.
        ELSE.
          ls_data-deudor   = lv_balance.
          ls_data-acreedor = lv_empty.
        ENDIF.
      WHEN 6.
        ls_data-deudor   = lv_balance.
        ls_data-acreedor = lv_empty.
      WHEN 7.
        ls_data-acreedor = lv_balance.
        ls_data-deudor   = lv_empty.
      WHEN 8.
        ls_data-deudor   = lv_balance.
        ls_data-acreedor = lv_empty.
      WHEN 9.
        ls_data-deudor   = lv_empty.
        ls_data-acreedor = lv_balance.
    ENDCASE.

    ls_clean = ls_data.
    CONDENSE: ls_clean-deudor, ls_clean-acreedor.
    APPEND ls_clean TO lt_clean.

    PERFORM van_vienen.


    lv_total_d = lv_total_d + ls_data-deudor.
    lv_total_a = lv_total_a + ls_data-acreedor.


    CASE ls_data-cuenta(1).
      WHEN 1.
        IF ls_data-cuenta(4) EQ 1201.
          IF lv_1201_d < 0.
            WRITE lv_1201_d TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
            ls_data-deudor = |({ ls_data-deudor })|.
          ELSE.
            WRITE lv_1201_d TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
          ENDIF.

          IF lv_1201_a > 0.
            WRITE lv_1201_a TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
            ls_data-acreedor = |({ ls_data-acreedor })|.
          ELSE.
            WRITE lv_1201_a TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
          ENDIF.
        ELSE.
          IF lv_balance < 0.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
            ls_data-deudor = |({ ls_data-deudor })|.
          ELSE.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
          ENDIF.
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
        IF ls_data-cuenta(4) EQ 4201 OR ls_data-cuenta(4) EQ 4202.
          IF lv_balance < 0.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
            ls_data-deudor = |({ ls_data-deudor })|.
          ELSE.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
          ENDIF.
        ELSE.
          IF lv_balance > 0.
            WRITE lv_balance TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
            ls_data-acreedor = |({ ls_data-acreedor })|.
          ELSE.
            WRITE lv_balance TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
          ENDIF.
        ENDIF.
      WHEN 5.
        IF ls_data-cuenta(4) EQ 5501.
          IF lv_balance > 0.
            WRITE lv_balance TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
            ls_data-acreedor = |({ ls_data-acreedor })|.
          ELSE.
            WRITE lv_balance TO ls_data-acreedor NO-SIGN.
            CONDENSE ls_data-acreedor.
          ENDIF.
        ELSE.
          IF lv_balance < 0.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
            ls_data-deudor = |({ ls_data-deudor })|.
          ELSE.
            WRITE lv_balance TO ls_data-deudor NO-SIGN.
            CONDENSE ls_data-deudor.
          ENDIF.
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
      WHEN 9.
*        IF lv_balance > 0.
        WRITE lv_balance TO ls_data-acreedor NO-SIGN.
        CONDENSE ls_data-acreedor.
*          ls_data-deudor = |({ ls_data-deudor })|.
*        ELSE.
*          WRITE lv_balance TO ls_data-deudor NO-SIGN.
*          CONDENSE ls_data-deudor.
*        ENDIF.
    ENDCASE.


    APPEND ls_data TO lt_data.
    CLEAR: ls_data, lv_balance.

  ENDLOOP.

  WRITE lv_total_d TO total_deudor NO-SIGN.
  WRITE lv_total_a TO total_acreedor NO-SIGN.

  CONDENSE: total_deudor, total_acreedor.


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
         nodialog = abap_false
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
  ELSE.

    SELECT * FROM ztfi_folios_bala INTO TABLE @DATA(lt_folios)
     WHERE bukrs EQ @p_bukrs
       AND folha NE ztfi_folios_bala~folio
       ORDER BY secue.

    READ TABLE lt_folios INTO DATA(ls_folios) INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO ls_folios-folio.
      MODIFY ztfi_folios_bala FROM ls_folios.
    ENDIF.

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
      IF ls_data-cuenta(4) EQ 1201.
        ls_vavi-deudor   = lv_1201_d.
        ls_vavi-acreedor = lv_1201_a.
      ELSE.
        ls_vavi-deudor = lv_balance.
      ENDIF.
    WHEN 2.
      ls_vavi-acreedor = lv_balance.
    WHEN 3.
      ls_vavi-acreedor = lv_balance.
    WHEN 4.
      IF ls_data-cuenta(4) EQ 4201 OR ls_data-cuenta(4) EQ 4202.
        ls_vavi-deudor = lv_balance.
      ELSE.
        ls_vavi-acreedor = lv_balance.
      ENDIF.
    WHEN 5.
      IF ls_data-cuenta(4) EQ 5501.
        ls_vavi-acreedor = lv_balance.
      ELSE.
        ls_vavi-deudor = lv_balance.
      ENDIF.
    WHEN 6.
      ls_vavi-deudor = lv_balance.
    WHEN 7.
      ls_vavi-acreedor = lv_balance.
    WHEN 8.
      ls_vavi-deudor = lv_balance.
    WHEN 9.
*      ls_vavi-deudor = lv_balance.
      ls_vavi-acreedor = lv_balance.
  ENDCASE.

  ls_vavi-cuenta = ls_data-cuenta.
  APPEND ls_vavi TO lt_vavi.
  CLEAR: ls_vavi.

ENDFORM.
