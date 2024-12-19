*&---------------------------------------------------------------------*
*& Report ZFI_LIQUIDACION_VENTAS
*&---------------------------------------------------------------------*

REPORT zfi_liquidacion_ventas.

INCLUDE zfi_liquidacion_ventas_top.
INCLUDE zfi_liquidacion_ventas_f01.
INCLUDE zfi_liquidacion_ventas_f02.


*---------------------------------------------------------------------
*           S T A R T  -  O F  -  S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.

  IF p_chkbx IS INITIAL.
    PERFORM get_data.
  ELSE.
    PERFORM re_print.
  ENDIF.

  PERFORM show_alv.

*---------------------------------------------------------------------
*           END  -  O F  -  S E L E C T I O N
*---------------------------------------------------------------------
END-OF-SELECTION.
