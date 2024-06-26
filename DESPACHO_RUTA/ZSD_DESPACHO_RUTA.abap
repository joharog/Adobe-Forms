*&---------------------------------------------------------------------*
*& Report ZETIQUETAS
*&---------------------------------------------------------------------*
REPORT zsd_despacho_ruta.

TABLES: likp.

DATA: lt_data TYPE TABLE OF zst_despacho_ruta,
      ls_data TYPE zst_despacho_ruta,
      doc_ini TYPE vbeln,
      doc_fin TYPE vbeln.

*&---------------------------------------------------------------------*
*& PARAMETRO DE ENTRADA - PANTALLA
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE t01.

  SELECT-OPTIONS:
    p_vbeln FOR likp-vbeln OBLIGATORY.
  PARAMETERS:
    p_erdat LIKE likp-erdat,
    p_route LIKE likp-route.

SELECTION-SCREEN END OF BLOCK  b01.

START-OF-SELECTION.

  doc_ini = |{ p_vbeln-low ALPHA = OUT }|.
  doc_fin = |{ p_vbeln-high  ALPHA = OUT }|.

  PERFORM get_data.

END-OF-SELECTION.

  PERFORM call_alv.


  TYPE-POOLS: slis.

  DATA: lf_sp_group   TYPE slis_t_sp_group_alv,  "MANEJAR GRUPOS DE CAMPOS
        lf_layout     TYPE slis_layout_alv,      "MANEJAR DISEÑO DE LAYOUT
        it_topheader  TYPE slis_t_listheader,    "MANEJAR CABECERA DEL REP
        wa_top        LIKE LINE OF it_topheader, "LÍNEA PARA CABECERA
        lt_event_exit TYPE slis_t_event_exit,    "Event
        ls_event_exit TYPE slis_event_exit.      "Event

  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo


FORM call_alv.

  IF lt_data[] IS NOT INITIAL.
    PERFORM alv_report USING lt_data[].
  ELSE.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*///////////////////////////////      ALV PERFORMS
FORM alv_report USING pp_itab LIKE lt_data[].

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
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'VBELN'.
  alv_git_fieldcat-seltext_l = 'Entrega'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '10'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'POSNR'.
  alv_git_fieldcat-seltext_l = 'Posición'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'MATKL'.
  alv_git_fieldcat-seltext_l = 'Grupo de Articulos'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'MATNR'.
  alv_git_fieldcat-seltext_l = 'Código de Material'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '50'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'ARKTX'.
  alv_git_fieldcat-seltext_l = 'Descripción del material'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '15'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'LFIMG'.
  alv_git_fieldcat-seltext_l = 'Cantidad'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'VRKME'.
  alv_git_fieldcat-seltext_l = 'UM Unidad de medida'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'CANTIDADX'.
  alv_git_fieldcat-seltext_l = 'Cantidad x UB'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'TOTAL'.
  alv_git_fieldcat-seltext_l = 'TOTAL'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR: alv_git_fieldcat.
  alv_git_fieldcat-outputlen = '20'.
  alv_git_fieldcat-tabname   = 'LT_DATA'.
  alv_git_fieldcat-fieldname = 'REVISION'.
  alv_git_fieldcat-seltext_l = 'Revision'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

ENDFORM.

FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.
*  u_lf_layout-colwidth_optimize   = 'X'.
*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe
ENDFORM.

*///////////////////////////////      ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE lt_data[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = lf_layout
      it_fieldcat              = alv_git_fieldcat[]
      it_event_exit            = lt_event_exit
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
      PERFORM print_pdf.
  ENDCASE.
ENDFORM.

FORM get_data.

  IF p_vbeln IS NOT INITIAL AND p_erdat IS INITIAL AND p_route IS INITIAL.
    SELECT * FROM likp INTO TABLE @DATA(lt_likp)
      WHERE vbeln IN @p_vbeln.

  ELSEIF p_vbeln IS NOT INITIAL AND p_erdat IS NOT INITIAL AND p_route IS INITIAL.
    SELECT * FROM likp INTO TABLE lt_likp
      WHERE vbeln IN p_vbeln
        AND erdat EQ p_erdat.

  ELSEIF p_vbeln IS NOT INITIAL AND p_erdat IS NOT INITIAL AND p_route IS NOT INITIAL.
    SELECT * FROM likp INTO TABLE lt_likp
    WHERE vbeln IN p_vbeln
      AND erdat EQ p_erdat
      AND route EQ p_route.
  ENDIF.


  IF sy-subrc EQ 0.

    SELECT * FROM lips INTO TABLE @DATA(lt_lips)
      FOR ALL ENTRIES IN @lt_likp
       WHERE vbeln EQ @lt_likp-vbeln
         AND erdat EQ @lt_likp-erdat.

  ENDIF.

  LOOP AT lt_lips INTO DATA(ls_lips).

    CLEAR: ls_data.
    MOVE-CORRESPONDING ls_lips TO ls_data.
    ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
    ls_data-vbeln = |{ ls_data-vbeln ALPHA = OUT }|.
    ls_data-matkl = |{ ls_data-matkl ALPHA = OUT }|.
    APPEND ls_data TO lt_data.

  ENDLOOP.


ENDFORM.


FORM print_pdf.
  DATA: lv_name           TYPE char30,
        lv_function_name  TYPE rs38l_fnam,
        lv_interface_type TYPE fpinterfacetype,
        lv_subrc          TYPE sy-subrc.

  DATA: ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_formoutput   TYPE fpformoutput,
        ls_result       TYPE sfpjoboutput.

  lv_name = 'ZAF_DESPACHO_RUTA'.

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

  ls_outputparams-copies = 1.

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
*  ls_docparams-country   = 'GT'.

  CALL FUNCTION lv_function_name
    EXPORTING
      /1bcdwb/docparams  = ls_docparams
      tab_ruta           = lt_data
      doc_ini            = doc_ini
      doc_fin            = doc_fin
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

ENDFORM.
