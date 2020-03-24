*&---------------------------------------------------------------------*
*& Report ZHANA_VIEWER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z92_HANA_VIEWER.

data: container    type ref to cl_gui_docking_container,
      grid         type ref to cl_gui_alv_grid,
      fieldcatalog type lvc_t_fcat.

data: timepassed_seconds type i,
      catalog_name       type string,
      cube_name          type string.

*--------------------------------------------------------------------*

data: editor_container type ref to cl_gui_custom_container,
      text_editor      type ref to cl_gui_textedit.

*--------------------------------------------------------------------*

field-symbols: <tab> type any table.

data: sql  type string,
      rows type i value 1000.

start-of-selection.
  call screen 200.

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_QUERY
*&---------------------------------------------------------------------*
FORM EXECUTE_QUERY.

  data: input_parameters type string,
        data_tab         type ref to data,
        schema           type string.

  free data_tab.
  if <tab> is assigned.
    free: <tab>.
  endif.

*удаляем лишине символы
  call function 'SCP_REPLACE_STRANGE_CHARS'
    exporting
      intext      = sql
      replacement = 32
    importing
      outtext     = sql.

  data comm type string.
  get run time field data(ts_start).

  data_tab = zcl_sql_executor=>result_hana_model( exporting i_query = sql
                                                  importing e_catalog = fieldcatalog
                                                            e_patch = catalog_name
                                                            e_model_name = cube_name
                                                            e_comm = comm ).

  get run time field data(ts_end).
  timepassed_seconds = ts_end - ts_start.

  assign data_tab->* to <tab>.

  if comm is initial.
    call screen 100.
  else.
    message comm type 'I'.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*&      МОДУЛИ
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  case sy-ucomm.
    when 'OUT'.
      leave program.
    when 'BACK'.
      set screen 0.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  GRID_INIT  OUTPUT
*&---------------------------------------------------------------------*
module GRID_INIT output.

  if container is not initial.
    grid->free( ).
    container->free( ).
  endif.

  free: container, grid.

  container = new #( side      = cl_gui_docking_container=>dock_at_top
                     extension = cl_gui_docking_container=>ws_maximizebox ).

  grid = new #( container )." cl_gui_container=>screen0 ).

  data: layout type lvc_s_layo.
  layout-cwidth_opt = abap_true.
  layout-grid_title = |Время исполнения { timepassed_seconds } ms|.

  grid->set_table_for_first_display( exporting is_layout            = layout
                                      changing it_outtab            = <tab>
                                               it_fieldcatalog      = fieldcatalog ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module STATUS_0100 output.
  set pf-status 'STATUS100'.
  set titlebar 'BAR100' with catalog_name cube_name.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS200'.
  SET TITLEBAR 'BAR200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  case sy-ucomm.
    when 'OUT'.
      leave program.
    when 'BACK'.
      leave to screen 0.
    when 'EXECUTE'.
      clear sql.
      text_editor->get_textstream( importing text = sql ).
      cl_gui_cfw=>flush( ).

      perform execute_query.
  endcase.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TEXT_INIT  OUTPUT
*&---------------------------------------------------------------------*
MODULE TEXT_INIT OUTPUT.

  if text_editor is initial.

    editor_container = new #( container_name = 'TEXTEDITOR' ).
    text_editor = new #( parent                     = editor_container
                         wordwrap_mode              = cl_gui_textedit=>wordwrap_off
                         wordwrap_position          = 254
                         wordwrap_to_linebreak_mode = cl_gui_textedit=>false ).
    text_editor->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).
    text_editor->set_statusbar_mode( statusbar_mode = cl_gui_textedit=>false ).

  endif.

ENDMODULE.