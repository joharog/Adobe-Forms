*&---------------------------------------------------------------------*
*& Report ZFI_RECIBO_TRANSFERENCIA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_recibo_transferencia.


TABLES: bsad, bkpf, kna1, bseg.


DATA: it_alv TYPE ztt_recibo_transferencia,
      wa_alv TYPE zst_recibo_transferencia.

DATA: lv_valor TYPE dmbtr.
DATA: lv_answer TYPE char1.

TYPE-POOLS: slis.



SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_belnr FOR bkpf-belnr,  "DEFAULT '1400000021',
                  s_budat FOR bkpf-budat,
                  s_blart FOR bkpf-blart.

  PARAMETERS: p_bukrs LIKE bkpf-bukrs OBLIGATORY DEFAULT 'P16',
              p_gjahr LIKE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4).



SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION .

  PERFORM get_data.

END-OF-SELECTION.

  PERFORM display_data.


*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT * FROM bkpf INTO TABLE @DATA(lt_bkpf)
    WHERE belnr IN @s_belnr
      AND budat IN @s_budat
      AND bukrs EQ @p_bukrs
      AND gjahr EQ @p_gjahr
      AND blart IN @s_blart.

  IF sy-subrc EQ 0.
    SELECT * FROM bsad INTO TABLE @DATA(lt_bsad)
      FOR ALL ENTRIES IN @lt_bkpf
      WHERE belnr EQ @lt_bkpf-belnr
*      WHERE augbl EQ ( SELECT augbl FROM bsad WHERE belnr EQ @lt_bkpf-belnr )
        AND gjahr EQ @lt_bkpf-gjahr
        AND budat EQ @lt_bkpf-budat
        AND bukrs EQ @lt_bkpf-bukrs.

    IF sy-subrc EQ 0.
      SELECT * FROM kna1 INTO TABLE @DATA(lt_kna1)
        FOR ALL ENTRIES IN @lt_bsad
        WHERE kunnr EQ @lt_bsad-kunnr.

    ENDIF.

    IF sy-subrc EQ 0.
      SELECT * FROM bseg INTO TABLE @DATA(lt_bseg)
        FOR ALL ENTRIES IN @lt_bkpf
        WHERE belnr EQ @lt_bkpf-belnr
          AND bukrs EQ @lt_bkpf-bukrs
          AND gjahr EQ @lt_bkpf-gjahr
*          AND koart EQ 'D'
          AND sgtxt IN ( 'EFECTIVO', 'TJ', 'TRANSFERENCIA' , 'CHEQUE' ).
    ENDIF.

  ENDIF.

  SORT lt_bkpf.


  LOOP AT lt_bkpf INTO DATA(ls_bkpf).

    wa_alv-belnr = ls_bkpf-belnr.   "doc compensado
    wa_alv-budat = ls_bkpf-budat.   "fecha
    wa_alv-bukrs = ls_bkpf-bukrs.   "sociedad
    wa_alv-gjahr = ls_bkpf-gjahr.   "año fiscal
    wa_alv-waers = ls_bkpf-waers.   "moneda
    wa_alv-bktxt = ls_bkpf-bktxt.   "nro. comprob. fiscal
    wa_alv-blart = ls_bkpf-blart.   "clase doc.

*    wa_alv-usnam = ls_bkpf-usnam.   "usuario
*    wa_alv-cputm = ls_bkpf-cputm.   "hora


    READ TABLE lt_bsad INTO DATA(ls_bsad) WITH KEY belnr = ls_bkpf-belnr bukrs = ls_bkpf-bukrs gjahr = ls_bkpf-gjahr.
    IF sy-subrc EQ 0.
      wa_alv-augbl = ls_bsad-augbl.
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_bsad-kunnr.
      IF sy-subrc EQ 0.
        wa_alv-kunnr = ls_kna1-kunnr.  "cliente
*        wa_alv-name1 = ls_kna1-name1.  "recibimos de
*        wa_alv-stras = ls_kna1-stras.  "direccion
*        wa_alv-ort01 = ls_kna1-ort01.  "ciudad
      ENDIF.
    ENDIF.

    CLEAR: ls_bsad.

    LOOP AT lt_bsad INTO ls_bsad WHERE belnr = ls_bkpf-belnr
                                   AND bukrs = ls_bkpf-bukrs
                                   AND gjahr = ls_bkpf-gjahr.

*      IF ls_bsad-blart NE 'DZ' AND ls _bsad-shkzg EQ 'H'.
      wa_alv-dmbtr = wa_alv-dmbtr + ls_bsad-dmbtr.  "valor
*      ENDIF.

    ENDLOOP.


    READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY belnr = ls_bkpf-belnr
                                                   bukrs = ls_bkpf-bukrs
                                                   gjahr = ls_bkpf-gjahr.
    IF sy-subrc EQ 0.
      wa_alv-sgtxt = ls_bseg-sgtxt.
      CASE ls_bseg-sgtxt.
        WHEN 'EFECTIVO'.
          wa_alv-efectivo = 'X'.
        WHEN 'TJ'.
          wa_alv-tarjeta = 'X'.
        WHEN 'TRANSFERENCIA'.
          wa_alv-banco = 'X'.
        WHEN 'CHEQUE'.
          wa_alv-cheque = 'X'.
      ENDCASE.
    ENDIF.
    APPEND wa_alv TO it_alv.

  ENDLOOP.

  SORT it_alv BY belnr.

*  PERFORM print_recibo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  IF it_alv[] IS NOT INITIAL.
    PERFORM alv_report USING it_alv[].      "LLamado al ALV
  ELSE.
    MESSAGE 'No se encontraron datos' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: lf_sp_group  TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
      lf_layout    TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
      it_topheader TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
      wa_top       LIKE LINE OF it_topheader. "LÍNEA PARA CABECERA

DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo


*///////////////////////////////      ALV PERFORMS
FORM alv_report  USING  pp_itab LIKE it_alv[].

  PERFORM sp_group_build USING lf_sp_group[].         "1
  PERFORM alv_ini_fieldcat.                           "2
  PERFORM layout_build USING lf_layout.               "3
  PERFORM alv_listado USING pp_itab[].                "4

ENDFORM.

*///////////////////////////////      ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = TEXT-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.


*///////////////////////////////      ALV PERFORM_2
FORM alv_ini_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'BELNR'.
  alv_git_fieldcat-seltext_l = 'Nº doc.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'BLART'.
  alv_git_fieldcat-seltext_l = 'Clase doc.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'BUDAT'.
  alv_git_fieldcat-seltext_l = 'Fecha doc.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '18'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'DMBTR'.
  alv_git_fieldcat-seltext_l = 'Importe ML'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '8'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'WAERS'.
  alv_git_fieldcat-seltext_l = 'Moneda'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '40'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'BKTXT'.
  alv_git_fieldcat-seltext_l = 'Nº comprobante fiscal'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'AUGBL'.
  alv_git_fieldcat-seltext_l = 'Doc.comp.'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '40'.
  alv_git_fieldcat-tabname   = 'IT_ALV'.
  alv_git_fieldcat-fieldname = 'SGTXT'.
  alv_git_fieldcat-seltext_l = 'Texto'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

ENDFORM.

*///////////////////////////////      ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

  u_lf_layout-box_fieldname = 'CHECKED'.
*  u_lf_layout-box_fieldname       = 'CHK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.

*///////////////////////////////      ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE it_alv[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = lf_layout
      it_fieldcat              = alv_git_fieldcat[]
*     it_special_groups        = lf_sp_group
      i_save                   = 'X'
    TABLES
      t_outtab                 = ppp_itab.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
  ENDIF.

ENDFORM.

FORM pf_status_set USING extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.

FORM user_command USING p_ucomm TYPE sy-ucomm
                         p_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&PRINT'.
      PERFORM popup_print.
  ENDCASE.
ENDFORM.


FORM popup_print.

  CLEAR: lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmación '
      text_question         = '¿Quieres imprimir el recibo de transferencia?'
      text_button_1         = 'Si'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_CANCEL'
      display_cancel_button = ' '
      popup_type            = 'ICON_HINT' "'ICON_MESSAGE_ERROR'
    IMPORTING
      answer                = lv_answer.
  IF lv_answer NE 1.
    LEAVE SCREEN.
  ELSE.
    PERFORM print_recibo.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form print_recibo
*&---------------------------------------------------------------------*
FORM print_recibo.


  READ TABLE it_alv TRANSPORTING NO FIELDS WITH KEY checked = 'X'.
  IF sy-subrc NE 0.

    MESSAGE 'Debe selecionar un N° Documento' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE SCREEN.

  ELSE.

    DELETE it_alv WHERE checked NE 'X'.

    DATA lv_name  TYPE char30.
    DATA lv_function_name   TYPE rs38l_fnam.
    DATA lv_interface_type  TYPE fpinterfacetype.
    DATA lv_subrc           TYPE sy-subrc.
    DATA ls_outputparams    TYPE sfpoutputparams.
    DATA ls_docparams       TYPE sfpdocparams.
    DATA ls_formoutput      TYPE fpformoutput.
    DATA ls_result        TYPE sfpjoboutput.


    lv_name = 'ZAF_RECIBO_TRANSFERENCIA'.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name           = lv_name
          IMPORTING
            e_funcname       = lv_function_name
            e_interface_type = lv_interface_type.

      CATCH cx_fp_api_internal
            cx_fp_api_repository
            cx_fp_api_usage.
        MOVE sy-subrc TO lv_subrc.
    ENDTRY.

*  ls_outputparams-getpdf   = 'X'.

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
    IF sy-subrc <> 0.
      MOVE sy-subrc TO lv_subrc.
    ENDIF.

* Actually print Adobe form
    ls_docparams-langu     = 'S'.
    ls_docparams-replangu1 = 'S'.
    ls_docparams-replangu2 = 'E'.
    ls_docparams-country   = 'DO'.

    CALL FUNCTION lv_function_name
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        cert_data          = it_alv
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
        e_result       = ls_result            " What has been done? 2030195
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3.
    IF sy-subrc <> 0.
      MOVE sy-subrc TO lv_subrc.
    ENDIF.

  ENDIF.

ENDFORM.
