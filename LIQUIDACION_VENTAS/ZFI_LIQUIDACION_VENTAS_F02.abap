*&---------------------------------------------------------------------*
*& Include          ZFI_LIQUIDACION_VENTAS_F02
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form map_bapi
*&---------------------------------------------------------------------*
FORM map_bapi.

  DATA:
    ls_header   TYPE bapiache09,
    lt_acctrec  TYPE STANDARD TABLE OF bapiacar09,
    ls_acctrec  TYPE bapiacar09,
    lt_acctgl   TYPE STANDARD TABLE OF bapiacgl09,
    ls_acctgl   TYPE bapiacgl09,
    lt_currency TYPE STANDARD TABLE OF bapiaccr09,
    ls_currency TYPE bapiaccr09,
    lt_return   TYPE STANDARD TABLE OF bapiret2.

  DATA:
    lv_conct TYPE char255.

  LOOP AT gt_data ASSIGNING <gfs_data> WHERE pass EQ abap_false.
*&---------------------------------------------------------------------*
*&        D O C U M E N T H E A D E R
*&---------------------------------------------------------------------*
    ls_header-bus_act    = 'RFBU'.
    ls_header-username   = sy-uname.
    ls_header-header_txt = ''.            "NO INFO
    ls_header-comp_code  = <gfs_data>-bukrs.
    IF sy-uname EQ 'CONSULABAP05'.
      ls_header-doc_date   = p_budat.
      ls_header-pstng_date = p_budat.
    ELSE.
      ls_header-doc_date   = p_budat. "<gfs_data>-budat. "<gfs_data>-budat.
      ls_header-pstng_date = p_budat. "<gfs_data>-budat. "<gfs_data>-bldat.
    ENDIF.
    ls_header-fisc_year  = sy-datum(4).
    ls_header-doc_type   = 'DX'.
    ls_header-ref_doc_no = <gfs_data>-zncob.      "Valor de Campo Z N° de recibo ??


*&---------------------------------------------------------------------*
*&        A C C O U N T G L
*&---------------------------------------------------------------------*
    ls_acctgl-itemno_acc = '0000000001'.    "VERIFICAR 1ra posición ??
    ls_acctgl-gl_account = s_saknr-low.     "VERIFICAR cambiar a p_saknr?
    ls_acctgl-item_text  = <gfs_data>-sgtxt.   "VERIFICAR llenado por posicion pero es fijo
    ls_acctgl-alloc_nmbr = <gfs_data>-assig.   "Confirmar campo con funcional
    ls_acctgl-tax_code   = ''.              "NO INFO
    ls_acctgl-costcenter = ''.              "NO INFO
    ls_acctgl-profit_ctr = ''.              "NO INFO
    ls_acctgl-orderid    = ''.              "NO INFO

    APPEND ls_acctgl TO lt_acctgl.


*&---------------------------------------------------------------------*
*&        A C C O U N T R E C E I V A B L E
*&---------------------------------------------------------------------*
    ls_acctrec-itemno_acc = '0000000002'.
    ls_acctrec-customer   = <gfs_data>-kunnr.

    SELECT SINGLE akont FROM knb1 INTO ls_acctrec-gl_account WHERE bukrs EQ <gfs_data>-bukrs AND kunnr EQ <gfs_data>-kunnr.
*      ls_acctrec-gl_account

    IF <gfs_data>-dmbtr NE <gfs_data>-zimpc.
      ls_acctrec-paymt_ref = <gfs_data>-payre.
    ENDIF.

    ls_acctrec-alloc_nmbr = <gfs_data>-assig.
    ls_acctrec-item_text  = <gfs_data>-sgtxt.
    ls_acctrec-profit_ctr = ''.

    APPEND ls_acctrec TO lt_acctrec.


*&---------------------------------------------------------------------*
*&        C U R  R E N C Y A M O U N T
*&---------------------------------------------------------------------*
*    accountgl importe positivo
    ls_currency-itemno_acc = '0000000001'.
    ls_currency-currency   = <gfs_data>-waers.
    ls_currency-amt_doccur = <gfs_data>-zimpc.
    APPEND ls_currency TO lt_currency.

*    accountreceivable  importe negativo
    ls_currency-itemno_acc = '0000000002'.
    ls_currency-currency   = <gfs_data>-waers.
    ls_currency-amt_doccur = <gfs_data>-zimpc * -1.
    APPEND ls_currency TO lt_currency.

*    BREAK consulabap05.
    IF <gfs_data>-zfncb EQ abap_true.
      CLEAR: <gfs_data>-zimpc.
    ELSE.
*&---------------------------------------------------------------------*
*&        B A P I   E X E C U T E
*&---------------------------------------------------------------------*
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = ls_header
        TABLES
          accountgl         = lt_acctgl
          accountreceivable = lt_acctrec
          currencyamount    = lt_currency
          return            = lt_return.

      READ TABLE lt_return INTO DATA(ls_return)  WITH KEY type = 'S' id = 'RW' number = '605'.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        <gfs_data>-conta = ls_return-message_v2.
        <gfs_data>-pass = abap_true.

*        PERFORM submit_sapf124.

      ELSE.
        LOOP AT lt_return INTO ls_return WHERE type = 'E'.
          IF ls_return-number NE 609.
            lv_conct = |{ lv_conct }{ ls_return-message }|.
          ENDIF.

        ENDLOOP.
        <gfs_data>-conta = lv_conct.
      ENDIF.

    ENDIF.

*&---------------------------------------------------------------------*
*&        W I P E A L L
*&---------------------------------------------------------------------*
    CLEAR: lv_conct, ls_return, ls_currency, ls_acctrec, ls_acctgl,  ls_header.
    REFRESH: lt_return, lt_currency, lt_acctrec, lt_acctgl.


  ENDLOOP.

  PERFORM save.
  PERFORM submit_sapf124.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form print_form
*&---------------------------------------------------------------------*
FORM print_form.

*  BREAK consulabap05.
  DATA: lt_data         TYPE TABLE OF zst_liquidacion_data,
        gv_funcname     TYPE rs38l_fnam,
        ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_formoutput   TYPE fpformoutput,
        ls_result       TYPE sfpjoboutput,
        lv_subrc        TYPE string.

  lt_data[] = gt_data[].
  DELETE lt_data WHERE pass NE abap_true.

  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'ZAF_FI_LIQUIDACION_VENTAS'
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
          preview = abap_false ).

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
  ls_docparams-langu     = sy-langu.
*  ls_docparams-fillable  = p_edita.
*  ls_docparams-langu     = 'S'.
*  ls_docparams-replangu1 = 'S'.
*  ls_docparams-replangu2 = 'E'.
*  ls_docparams-country   = 'GT'.

  CALL FUNCTION gv_funcname
    EXPORTING
      /1bcdwb/docparams  = ls_docparams
      gs_header          = gs_header
      gt_data            = lt_data "gt_data
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
*& Form submit_sapf124
*&---------------------------------------------------------------------*
FORM submit_sapf124.

  DATA:
    lt_submit TYPE TABLE OF zst_liquidacion_data,
    lt_seltab TYPE TABLE OF rsparams,
    ls_seltab LIKE LINE OF lt_seltab,
    lv_job    TYPE flag.


  lt_submit[] = gt_data.

  DELETE ADJACENT DUPLICATES FROM lt_submit COMPARING kunnr bukrs.

  LOOP AT lt_submit ASSIGNING <gfs_data>.

    APPEND VALUE #( selname = 'BUKRX'
                    kind    = 'S'
                    sign    = 'I'
                    option  = 'EQ'
                    low     = <gfs_data>-bukrs )
                    TO lt_seltab.

    APPEND VALUE #( selname = 'X_KUNNR'
                    kind    = 'P'
                    low     = 'X' )
                    TO lt_seltab.

    APPEND VALUE #( selname = 'AUGDT'
                    kind    = 'P'
*                    sign    = 'I'
*                    option  = 'EQ'
                    low     = <gfs_data>-budat )
                    TO lt_seltab.

    APPEND VALUE #( selname = 'KONTD'
                    kind    = 'S'
                    sign    = 'I'
                    option  = 'EQ'
                    low     = <gfs_data>-kunnr )
                    TO lt_seltab.

    APPEND VALUE #( selname = 'X_TESTL'
                    kind    = 'P'
                    low     = '' )
                    TO lt_seltab.

    APPEND VALUE #( selname = 'XNAUSBEL'
                    kind    = 'P'
                    low     = ' ' )
                    TO lt_seltab.
    APPEND VALUE #( selname = 'X_FEHLER'
                    kind    = 'P'
                    low     = ' ' )
                    TO lt_seltab.

  ENDLOOP.

  UNASSIGN <gfs_data>.

*  cl_salv_bs_runtime_info=>set(
*         EXPORTING display     = abap_false
*                   metadata    = abap_true
*                   data        = abap_true ).



*  BREAK consulabap05.


*  lv_job = abap_true.
*  EXPORT lv_job FROM lv_job TO MEMORY ID 'JOB'.



  SUBMIT sapf124 WITH SELECTION-TABLE lt_seltab AND RETURN.

*  SUBMIT sapf124 WITH SELECTION-TABLE lt_seltab. "EXPORTING LIST TO MEMORY AND RETURN.
*  SUBMIT sapf124  WITH SELECTION-TABLE lt_seltab AND RETURN.

*  DATA gt_alv_table TYPE REF TO data.
*  FIELD-SYMBOLS: <f_alv_data> TYPE ANY TABLE.
*  TRY.
*      cl_salv_bs_runtime_info=>get_data_ref(
*        IMPORTING r_data = gt_alv_table ).
*      ASSIGN gt_alv_table->* TO <f_alv_data>.
*    CATCH cx_salv_bs_sc_runtime_info.
*  ENDTRY.

*  cl_salv_bs_runtime_info=>clear_all( ). "clear the memory.

*  FREE MEMORY ID 'JOB'.

ENDFORM.


*  ls_seltab-selname = 'BUKRX'.
*  ls_seltab-kind    = 'S'.
*  ls_seltab-sign    = 'I'.
*  ls_seltab-option  = 'EQ'.
*  ls_seltab-low     = <gfs_data>-bukrs.
*  APPEND ls_seltab TO lt_seltab.
*
*  ls_seltab-selname = 'X_KUNNR'.
*  ls_seltab-kind    = 'P'.
*  ls_seltab-low     = 'X'.
*  APPEND ls_seltab TO lt_seltab.
*
*  ls_seltab-selname = 'KONTD'.
*  ls_seltab-kind    = 'S'.
*  ls_seltab-sign    = 'I'.
*  ls_seltab-option  = 'EQ'.
*  ls_seltab-low     = <gfs_data>-kunnr.
*  APPEND ls_seltab TO lt_seltab.

*  ls_seltab-selname = 'X_TESTL'.
*  ls_seltab-kind    = 'P'.
*  ls_seltab-low     = ''.
*  APPEND ls_seltab TO lt_seltab.
*
*  ls_seltab-selname = 'XNAUSBEL'.
*  ls_seltab-kind    = 'P'.
*  ls_seltab-low     = ''.
*  APPEND ls_seltab TO lt_seltab.
*
*  ls_seltab-selname = 'X_FEHLER'.
*  ls_seltab-kind    = 'P'.
*  ls_seltab-low     = ''.
*  APPEND ls_seltab TO lt_seltab.

*&---------------------------------------------------------------------*
*& Form save
*&---------------------------------------------------------------------*
FORM save.

  DATA: lt_data TYPE TABLE OF zst_liquidacion_data.

  lt_data[] = gt_data[].
  DELETE lt_data WHERE pass NE abap_true.

  MOVE-CORRESPONDING lt_data TO gt_liq_ventas.

  MODIFY zfi_liq_ventas FROM TABLE gt_liq_ventas.
  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form re_print
*&---------------------------------------------------------------------*
FORM re_print.


  REFRESH: gt_data.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'ES'
    IMPORTING
      output = gv_spras.

  CONCATENATE s_bldat-low+6(2) s_bldat-low+4(2) s_bldat-low(4) INTO gv_date SEPARATED BY '/'.

  SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr)
    WHERE "bukrs IN @s_bukrs.
          bukrs EQ @p_bukrs.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM adrc INTO @DATA(ls_adrc)
      WHERE addrnumber EQ @lv_adrnr.

    DATA(lv_names) = |{ ls_adrc-name1 } { ls_adrc-name2 } { ls_adrc-name3 } { ls_adrc-name4 }|.
  ENDIF.

  SELECT SINGLE bezei FROM tvrot INTO @DATA(lv_bezei)
    WHERE route IN @s_route
      AND spras EQ @gv_spras.

  gs_header-nombre = lv_names.
  gs_header-titulo = 'LIQUIDACIÓN DE VENTAS'.
  CONCATENATE 'FECHA:' gv_date INTO gs_header-fecha SEPARATED BY space.
  CONCATENATE 'RUTA:' s_route-low INTO gs_header-ruta SEPARATED BY space.

  CLEAR: gv_date, lv_adrnr, ls_adrc, lv_names, lv_bezei.

  SELECT * FROM zfi_liq_ventas INTO TABLE gt_liq_ventas
    WHERE bukrs EQ p_bukrs
      AND bldat IN s_bldat
      AND route IN s_route
      AND budat EQ p_budat.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING gt_liq_ventas TO gt_data.
  ELSE.
    MESSAGE 'No se encontraron datos'  TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.



ENDFORM.
