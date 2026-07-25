pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__camera_aunit.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__camera_aunit.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E006 : Short_Integer; pragma Import (Ada, E006, "ada__exceptions_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__exception_table_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__soft_links__initialize_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "ada__assertions_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "ada__containers_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ada__io_exceptions_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "ada__numerics_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__strings_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings__utf_encoding_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "gnat_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "interfaces__c_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "interfaces__c__strings_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__os_lib_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__tags_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "ada__strings__text_buffers_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__streams_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "system__file_control_block_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__finalization_root_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "ada__finalization_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "system__file_io_E");
   E474 : Short_Integer; pragma Import (Ada, E474, "ada__streams__stream_io_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "system__storage_pools_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "system__storage_pools__subpools_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "ada__calendar_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "ada__calendar__delays_E");
   E334 : Short_Integer; pragma Import (Ada, E334, "ada__calendar__time_zones_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "ada__real_time_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ada__text_io_E");
   E537 : Short_Integer; pragma Import (Ada, E537, "gnat__secure_hashes_E");
   E539 : Short_Integer; pragma Import (Ada, E539, "gnat__secure_hashes__sha1_E");
   E535 : Short_Integer; pragma Import (Ada, E535, "gnat__sha1_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "ada__strings__maps_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "ada__strings__maps__constants_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "ada__strings__text_buffers__unbounded_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "ada__strings__unbounded_E");
   E422 : Short_Integer; pragma Import (Ada, E422, "system__interrupt_management__operations_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "system__pool_global_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "gnat__sockets_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "gnat__sockets__poll_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "gnat__sockets__thin_common_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "gnat__sockets__thin_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "system__random_seed_E");
   E342 : Short_Integer; pragma Import (Ada, E342, "system__regexp_E");
   E330 : Short_Integer; pragma Import (Ada, E330, "ada__directories_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "system__tasking__initialization_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__tasking__protected_objects_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "system__tasking__protected_objects__entries_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__tasking__queuing_E");
   E424 : Short_Integer; pragma Import (Ada, E424, "system__tasking__stages_E");
   E420 : Short_Integer; pragma Import (Ada, E420, "system__tasking__async_delays_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "command_name_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada_lib_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "ada_lib__os_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "ada_lib__os_strings_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "ada_lib__string_quote_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "ada_lib__strings_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "ada_lib__strings__unlimited_E");
   E209 : Short_Integer; pragma Import (Ada, E209, "ada_lib__options_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "ada_lib__parser_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada_lib__substiture_for_non_alpha_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "ada_lib__time_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "ada_lib__trace_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "ask_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "hex_io_E");
   E312 : Short_Integer; pragma Import (Ada, E312, "ada_lib__help_E");
   E429 : Short_Integer; pragma Import (Ada, E429, "ada_lib__lock_interface_E");
   E416 : Short_Integer; pragma Import (Ada, E416, "ada_lib__lock_E");
   E414 : Short_Integer; pragma Import (Ada, E414, "ada_lib__auto_lock_E");
   E478 : Short_Integer; pragma Import (Ada, E478, "ada_lib__mail_E");
   E454 : Short_Integer; pragma Import (Ada, E454, "ada_lib__object_E");
   E456 : Short_Integer; pragma Import (Ada, E456, "ada_lib__object__handle_E");
   E322 : Short_Integer; pragma Import (Ada, E322, "ada_lib__options__runstring_E");
   E320 : Short_Integer; pragma Import (Ada, E320, "ada_lib__command_line_iterator_E");
   E393 : Short_Integer; pragma Import (Ada, E393, "ada_lib__options__verification_E");
   E636 : Short_Integer; pragma Import (Ada, E636, "ada_lib__options__nested_E");
   E640 : Short_Integer; pragma Import (Ada, E640, "ada_lib__options__template_E");
   E346 : Short_Integer; pragma Import (Ada, E346, "ada_lib__os__environment_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "ada_lib__directory_E");
   E350 : Short_Integer; pragma Import (Ada, E350, "ada_lib__os__run_E");
   E328 : Short_Integer; pragma Import (Ada, E328, "ada_lib__configuration_E");
   E472 : Short_Integer; pragma Import (Ada, E472, "ada_lib__directory__file_compare_E");
   E470 : Short_Integer; pragma Import (Ada, E470, "ada_lib__directory__compare_files_E");
   E476 : Short_Integer; pragma Import (Ada, E476, "ada_lib__directory__file_copy_E");
   E486 : Short_Integer; pragma Import (Ada, E486, "ada_lib__socket_io_E");
   E484 : Short_Integer; pragma Import (Ada, E484, "ada_lib__mail__gmail_E");
   E482 : Short_Integer; pragma Import (Ada, E482, "ada_lib__mail__curl_E");
   E435 : Short_Integer; pragma Import (Ada, E435, "ada_lib__strings__bounded_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "ada_lib__database_E");
   E433 : Short_Integer; pragma Import (Ada, E433, "ada_lib__database__connection_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "ada_lib__database__updater_E");
   E452 : Short_Integer; pragma Import (Ada, E452, "ada_lib__database__event_E");
   E439 : Short_Integer; pragma Import (Ada, E439, "ada_lib__database__subscribe_E");
   E458 : Short_Integer; pragma Import (Ada, E458, "ada_lib__database__subscription_E");
   E450 : Short_Integer; pragma Import (Ada, E450, "ada_lib__database__wild_E");
   E670 : Short_Integer; pragma Import (Ada, E670, "ada_lib__event_E");
   E638 : Short_Integer; pragma Import (Ada, E638, "ada_lib__options__database_E");
   E500 : Short_Integer; pragma Import (Ada, E500, "ada_lib__template_E");
   E642 : Short_Integer; pragma Import (Ada, E642, "ada_lib__trace__options_E");
   E444 : Short_Integer; pragma Import (Ada, E444, "ada_lib__trace_tasks_E");
   E437 : Short_Integer; pragma Import (Ada, E437, "ada_lib__database__server_E");
   E462 : Short_Integer; pragma Import (Ada, E462, "ada_lib__database__server__state_E");
   E492 : Short_Integer; pragma Import (Ada, E492, "ada_lib__timer_E");
   E490 : Short_Integer; pragma Import (Ada, E490, "ada_lib__socket_io__stream_io_E");
   E494 : Short_Integer; pragma Import (Ada, E494, "ada_lib__socket_io__client_E");
   E488 : Short_Integer; pragma Import (Ada, E488, "ada_lib__mail__smtp_E");
   E498 : Short_Integer; pragma Import (Ada, E498, "ada_lib__socket_io__server_E");
   E360 : Short_Integer; pragma Import (Ada, E360, "aunit_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "aunit__memory_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "aunit__memory__utils_E");
   E369 : Short_Integer; pragma Import (Ada, E369, "ada_containers__aunit_lists_E");
   E374 : Short_Integer; pragma Import (Ada, E374, "aunit__tests_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "aunit__time_measure_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "aunit__test_results_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "aunit__assertions_E");
   E448 : Short_Integer; pragma Import (Ada, E448, "ada_lib__database__common_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "aunit__test_filters_E");
   E381 : Short_Integer; pragma Import (Ada, E381, "aunit__simple_test_cases_E");
   E720 : Short_Integer; pragma Import (Ada, E720, "aunit__reporter_E");
   E722 : Short_Integer; pragma Import (Ada, E722, "aunit__reporter__text_E");
   E376 : Short_Integer; pragma Import (Ada, E376, "aunit__test_cases_E");
   E395 : Short_Integer; pragma Import (Ada, E395, "aunit__test_filters__ada_lib_E");
   E406 : Short_Integer; pragma Import (Ada, E406, "aunit__test_suites_E");
   E466 : Short_Integer; pragma Import (Ada, E466, "aunit__test_suites__optional_E");
   E517 : Short_Integer; pragma Import (Ada, E517, "generic_unbounded_array_E");
   E576 : Short_Integer; pragma Import (Ada, E576, "generic_unbounded_ptr_array_E");
   E504 : Short_Integer; pragma Import (Ada, E504, "gnoga_E");
   E630 : Short_Integer; pragma Import (Ada, E630, "gnoga__application_E");
   E511 : Short_Integer; pragma Import (Ada, E511, "gnoga__server_E");
   E583 : Short_Integer; pragma Import (Ada, E583, "gnoga__server__mime_E");
   E589 : Short_Integer; pragma Import (Ada, E589, "gnoga__types_E");
   E587 : Short_Integer; pragma Import (Ada, E587, "gnoga__server__database_E");
   E596 : Short_Integer; pragma Import (Ada, E596, "gnoga__server__model_E");
   E598 : Short_Integer; pragma Import (Ada, E598, "gnoga__server__model__queries_E");
   E585 : Short_Integer; pragma Import (Ada, E585, "gnoga__server__template_parser_E");
   E600 : Short_Integer; pragma Import (Ada, E600, "gnoga__server__template_parser__simple_E");
   E606 : Short_Integer; pragma Import (Ada, E606, "gnoga__types__colors_E");
   E527 : Short_Integer; pragma Import (Ada, E527, "object_E");
   E529 : Short_Integer; pragma Import (Ada, E529, "object__handle_E");
   E531 : Short_Integer; pragma Import (Ada, E531, "object__handle__generic_unbounded_array_E");
   E574 : Short_Integer; pragma Import (Ada, E574, "stack_storage_E");
   E521 : Short_Integer; pragma Import (Ada, E521, "strings_edit_E");
   E547 : Short_Integer; pragma Import (Ada, E547, "strings_edit__base64_E");
   E562 : Short_Integer; pragma Import (Ada, E562, "strings_edit__fields_E");
   E525 : Short_Integer; pragma Import (Ada, E525, "strings_edit__integer_edit_E");
   E519 : Short_Integer; pragma Import (Ada, E519, "gnat__sockets__server_E");
   E515 : Short_Integer; pragma Import (Ada, E515, "gnat__sockets__connection_state_machine_E");
   E545 : Short_Integer; pragma Import (Ada, E545, "gnat__sockets__connection_state_machine__big_endian__unsigneds_E");
   E570 : Short_Integer; pragma Import (Ada, E570, "gnat__sockets__connection_state_machine__expected_sequence_E");
   E572 : Short_Integer; pragma Import (Ada, E572, "gnat__sockets__connection_state_machine__terminated_strings_E");
   E558 : Short_Integer; pragma Import (Ada, E558, "strings_edit__float_edit_E");
   E549 : Short_Integer; pragma Import (Ada, E549, "strings_edit__floats_E");
   E548 : Short_Integer; pragma Import (Ada, E548, "strings_edit__floats_E");
   E560 : Short_Integer; pragma Import (Ada, E560, "strings_edit__quoted_E");
   E564 : Short_Integer; pragma Import (Ada, E564, "strings_edit__utf8_E");
   E602 : Short_Integer; pragma Import (Ada, E602, "strings_edit__utf8__handling_E");
   E578 : Short_Integer; pragma Import (Ada, E578, "tables_E");
   E580 : Short_Integer; pragma Import (Ada, E580, "tables__names_E");
   E533 : Short_Integer; pragma Import (Ada, E533, "gnat__sockets__connection_state_machine__http_server_E");
   E507 : Short_Integer; pragma Import (Ada, E507, "gnoga__gui__base_E");
   E513 : Short_Integer; pragma Import (Ada, E513, "gnoga__server__connection_E");
   E581 : Short_Integer; pragma Import (Ada, E581, "gnoga__server__connection__common_E");
   E620 : Short_Integer; pragma Import (Ada, E620, "gnoga__client__storage_E");
   E604 : Short_Integer; pragma Import (Ada, E604, "gnoga__gui__element_E");
   E613 : Short_Integer; pragma Import (Ada, E613, "gnoga__gui__document_E");
   E698 : Short_Integer; pragma Import (Ada, E698, "gnoga__gui__element__style_block_E");
   E622 : Short_Integer; pragma Import (Ada, E622, "gnoga__gui__location_E");
   E611 : Short_Integer; pragma Import (Ada, E611, "gnoga__gui__view_E");
   E609 : Short_Integer; pragma Import (Ada, E609, "gnoga__gui__element__common_E");
   E615 : Short_Integer; pragma Import (Ada, E615, "gnoga__gui__element__form_E");
   E700 : Short_Integer; pragma Import (Ada, E700, "gnoga__gui__element__list_E");
   E693 : Short_Integer; pragma Import (Ada, E693, "gnoga__gui__element__section_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "gnoga__gui__element__table_E");
   E714 : Short_Integer; pragma Import (Ada, E714, "gnoga__gui__view__card_E");
   E704 : Short_Integer; pragma Import (Ada, E704, "gnoga__gui__view__docker_E");
   E617 : Short_Integer; pragma Import (Ada, E617, "gnoga__gui__window_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "ada_lib__gnoga_E");
   E632 : Short_Integer; pragma Import (Ada, E632, "gnoga__application__multi_connect_E");
   E696 : Short_Integer; pragma Import (Ada, E696, "gnoga__gui__plugin__jqueryui_E");
   E702 : Short_Integer; pragma Import (Ada, E702, "gnoga__gui__plugin__jqueryui__widget_E");
   E712 : Short_Integer; pragma Import (Ada, E712, "gnoga__gui__plugin__message_boxes_E");
   E626 : Short_Integer; pragma Import (Ada, E626, "gnoga_ada_lib_E");
   E628 : Short_Integer; pragma Import (Ada, E628, "gnoga_ada_lib__base_E");
   E468 : Short_Integer; pragma Import (Ada, E468, "ada_lib__directory__test_E");
   E318 : Short_Integer; pragma Import (Ada, E318, "ada_lib__options__program_E");
   E388 : Short_Integer; pragma Import (Ada, E388, "ada_lib__options__unit_test_E");
   E386 : Short_Integer; pragma Import (Ada, E386, "ada_lib__unit_test_E");
   E397 : Short_Integer; pragma Import (Ada, E397, "ada_lib__unit_test__test_cases_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "ada_lib__configuration__tests_E");
   E460 : Short_Integer; pragma Import (Ada, E460, "ada_lib__database__subscription__tests_E");
   E464 : Short_Integer; pragma Import (Ada, E464, "ada_lib__database__unit_test_E");
   E446 : Short_Integer; pragma Import (Ada, E446, "ada_lib__database__server__tests_E");
   E624 : Short_Integer; pragma Import (Ada, E624, "ada_lib__gnoga__unit_test_E");
   E634 : Short_Integer; pragma Import (Ada, E634, "ada_lib__gnoga__unit_test__options_E");
   E480 : Short_Integer; pragma Import (Ada, E480, "ada_lib__mail__tests_E");
   E326 : Short_Integer; pragma Import (Ada, E326, "ada_lib__options__aunit_lib_E");
   E496 : Short_Integer; pragma Import (Ada, E496, "ada_lib__socket_io__stream_io__unit_test_E");
   E719 : Short_Integer; pragma Import (Ada, E719, "ada_lib__unit_test__reporter_E");
   E655 : Short_Integer; pragma Import (Ada, E655, "video__lib_E");
   E657 : Short_Integer; pragma Import (Ada, E657, "configuration_E");
   E658 : Short_Integer; pragma Import (Ada, E658, "configuration__camera_E");
   E683 : Short_Integer; pragma Import (Ada, E683, "widgets_E");
   E689 : Short_Integer; pragma Import (Ada, E689, "widgets__generic_table_E");
   E710 : Short_Integer; pragma Import (Ada, E710, "widgets__video_E");
   E644 : Short_Integer; pragma Import (Ada, E644, "camera_E");
   E646 : Short_Integer; pragma Import (Ada, E646, "camera__lib_E");
   E648 : Short_Integer; pragma Import (Ada, E648, "camera__lib__base_E");
   E650 : Short_Integer; pragma Import (Ada, E650, "camera__commands_E");
   E664 : Short_Integer; pragma Import (Ada, E664, "camera__base_E");
   E678 : Short_Integer; pragma Import (Ada, E678, "camera__command_queue_E");
   E672 : Short_Integer; pragma Import (Ada, E672, "camera__commands__ptz_optics_E");
   E680 : Short_Integer; pragma Import (Ada, E680, "camera__configuration_E");
   E662 : Short_Integer; pragma Import (Ada, E662, "camera__configurations_E");
   E682 : Short_Integer; pragma Import (Ada, E682, "camera__lib__base__command_tests_E");
   E652 : Short_Integer; pragma Import (Ada, E652, "camera__lib__options_E");
   E660 : Short_Integer; pragma Import (Ada, E660, "configuration__camera__state_E");
   E668 : Short_Integer; pragma Import (Ada, E668, "camera__main_E");
   E666 : Short_Integer; pragma Import (Ada, E666, "configuration__camera__setup_E");
   E676 : Short_Integer; pragma Import (Ada, E676, "camera__lib__unit_test_E");
   E674 : Short_Integer; pragma Import (Ada, E674, "camera__lib__options__unit_test_E");
   E706 : Short_Integer; pragma Import (Ada, E706, "widgets__adjust_E");
   E708 : Short_Integer; pragma Import (Ada, E708, "widgets__configured_E");
   E685 : Short_Integer; pragma Import (Ada, E685, "widgets__control_E");
   E691 : Short_Integer; pragma Import (Ada, E691, "widgets__control__unit_test_E");
   E727 : Short_Integer; pragma Import (Ada, E727, "camera__commands__unit_test_E");
   E729 : Short_Integer; pragma Import (Ada, E729, "camera__lib__base__test_E");
   E731 : Short_Integer; pragma Import (Ada, E731, "configuration__camera__setup__unit_tests_E");
   E734 : Short_Integer; pragma Import (Ada, E734, "configuration__camera__state__unit_tests_E");
   E736 : Short_Integer; pragma Import (Ada, E736, "widgets__adjust__unit_test_E");
   E717 : Short_Integer; pragma Import (Ada, E717, "camera__lib__unit_test__run_suite_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "widgets__adjust__unit_test__finalize_body");
      begin
         E736 := E736 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "configuration__camera__state__unit_tests__finalize_body");
      begin
         E734 := E734 - 1;
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "configuration__camera__setup__unit_tests__finalize_body");
      begin
         E731 := E731 - 1;
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "camera__lib__base__test__finalize_body");
      begin
         E729 := E729 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "camera__commands__unit_test__finalize_body");
      begin
         E727 := E727 - 1;
         F5;
      end;
      E676 := E676 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "widgets__control__unit_test__finalize_body");
      begin
         E691 := E691 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "camera__main__finalize_body");
      begin
         E668 := E668 - 1;
         F7;
      end;
      E646 := E646 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "widgets__control__finalize_body");
      begin
         E685 := E685 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "widgets__control__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "widgets__configured__finalize_body");
      begin
         E708 := E708 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "widgets__configured__finalize_spec");
      begin
         F11;
      end;
      E706 := E706 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "widgets__adjust__finalize_spec");
      begin
         F12;
      end;
      E674 := E674 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "camera__lib__options__unit_test__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "camera__lib__base__command_tests__finalize_body");
      begin
         E682 := E682 - 1;
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "camera__lib__unit_test__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "camera__configurations__finalize_body");
      begin
         E662 := E662 - 1;
         F16;
      end;
      E664 := E664 - 1;
      E666 := E666 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "configuration__camera__setup__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "camera__main__finalize_spec");
      begin
         F18;
      end;
      E650 := E650 - 1;
      E660 := E660 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "configuration__camera__state__finalize_spec");
      begin
         F19;
      end;
      E648 := E648 - 1;
      E680 := E680 - 1;
      E672 := E672 - 1;
      E644 := E644 - 1;
      E652 := E652 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "camera__lib__options__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "camera__configurations__finalize_spec");
      begin
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "camera__configuration__finalize_spec");
      begin
         F22;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "camera__commands__ptz_optics__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "camera__base__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "camera__commands__finalize_spec");
      begin
         F25;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "camera__lib__base__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "camera__lib__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "camera__finalize_spec");
      begin
         F28;
      end;
      E710 := E710 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "widgets__video__finalize_spec");
      begin
         F29;
      end;
      E689 := E689 - 1;
      declare
         procedure F30;
         pragma Import (Ada, F30, "widgets__generic_table__finalize_spec");
      begin
         F30;
      end;
      E655 := E655 - 1;
      declare
         procedure F31;
         pragma Import (Ada, F31, "video__lib__finalize_spec");
      begin
         F31;
      end;
      E719 := E719 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "ada_lib__unit_test__reporter__finalize_spec");
      begin
         F32;
      end;
      E397 := E397 - 1;
      E326 := E326 - 1;
      E496 := E496 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "ada_lib__socket_io__stream_io__unit_test__finalize_spec");
      begin
         F33;
      end;
      E388 := E388 - 1;
      E318 := E318 - 1;
      E464 := E464 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "ada_lib__database__server__tests__finalize_body");
      begin
         E446 := E446 - 1;
         F34;
      end;
      declare
         procedure F35;
         pragma Import (Ada, F35, "ada_lib__options__aunit_lib__finalize_spec");
      begin
         F35;
      end;
      E480 := E480 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "ada_lib__mail__tests__finalize_spec");
      begin
         F36;
      end;
      E634 := E634 - 1;
      declare
         procedure F37;
         pragma Import (Ada, F37, "ada_lib__gnoga__unit_test__options__finalize_spec");
      begin
         F37;
      end;
      E624 := E624 - 1;
      declare
         procedure F38;
         pragma Import (Ada, F38, "ada_lib__gnoga__unit_test__finalize_spec");
      begin
         F38;
      end;
      declare
         procedure F39;
         pragma Import (Ada, F39, "ada_lib__directory__test__finalize_body");
      begin
         E468 := E468 - 1;
         F39;
      end;
      declare
         procedure F40;
         pragma Import (Ada, F40, "ada_lib__database__server__tests__finalize_spec");
      begin
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "ada_lib__database__unit_test__finalize_spec");
      begin
         F41;
      end;
      declare
         procedure F42;
         pragma Import (Ada, F42, "ada_lib__database__subscription__tests__finalize_body");
      begin
         E460 := E460 - 1;
         F42;
      end;
      declare
         procedure F43;
         pragma Import (Ada, F43, "ada_lib__database__subscription__tests__finalize_spec");
      begin
         F43;
      end;
      E358 := E358 - 1;
      declare
         procedure F44;
         pragma Import (Ada, F44, "ada_lib__configuration__tests__finalize_spec");
      begin
         F44;
      end;
      declare
         procedure F45;
         pragma Import (Ada, F45, "ada_lib__unit_test__test_cases__finalize_spec");
      begin
         F45;
      end;
      declare
         procedure F46;
         pragma Import (Ada, F46, "ada_lib__unit_test__finalize_body");
      begin
         E386 := E386 - 1;
         F46;
      end;
      declare
         procedure F47;
         pragma Import (Ada, F47, "ada_lib__options__unit_test__finalize_spec");
      begin
         F47;
      end;
      declare
         procedure F48;
         pragma Import (Ada, F48, "ada_lib__options__program__finalize_spec");
      begin
         F48;
      end;
      E628 := E628 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "gnoga_ada_lib__base__finalize_spec");
      begin
         F49;
      end;
      E626 := E626 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "gnoga_ada_lib__finalize_spec");
      begin
         F50;
      end;
      declare
         procedure F51;
         pragma Import (Ada, F51, "gnoga__gui__plugin__message_boxes__finalize_body");
      begin
         E712 := E712 - 1;
         F51;
      end;
      E702 := E702 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "gnoga__gui__plugin__jqueryui__widget__finalize_spec");
      begin
         F52;
      end;
      declare
         procedure F53;
         pragma Import (Ada, F53, "gnoga__application__multi_connect__finalize_body");
      begin
         E632 := E632 - 1;
         F53;
      end;
      E502 := E502 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "ada_lib__gnoga__finalize_spec");
      begin
         F54;
      end;
      E617 := E617 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "gnoga__gui__window__finalize_spec");
      begin
         F55;
      end;
      E704 := E704 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "gnoga__gui__view__docker__finalize_spec");
      begin
         F56;
      end;
      E714 := E714 - 1;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gnoga__gui__view__card__finalize_spec");
      begin
         F57;
      end;
      E687 := E687 - 1;
      declare
         procedure F58;
         pragma Import (Ada, F58, "gnoga__gui__element__table__finalize_spec");
      begin
         F58;
      end;
      E693 := E693 - 1;
      declare
         procedure F59;
         pragma Import (Ada, F59, "gnoga__gui__element__section__finalize_spec");
      begin
         F59;
      end;
      E700 := E700 - 1;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gnoga__gui__element__list__finalize_spec");
      begin
         F60;
      end;
      E615 := E615 - 1;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gnoga__gui__element__form__finalize_spec");
      begin
         F61;
      end;
      E611 := E611 - 1;
      E609 := E609 - 1;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gnoga__gui__element__common__finalize_spec");
      begin
         F62;
      end;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gnoga__gui__view__finalize_spec");
      begin
         F63;
      end;
      E622 := E622 - 1;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gnoga__gui__location__finalize_spec");
      begin
         F64;
      end;
      E698 := E698 - 1;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gnoga__gui__element__style_block__finalize_spec");
      begin
         F65;
      end;
      E613 := E613 - 1;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gnoga__gui__document__finalize_spec");
      begin
         F66;
      end;
      E604 := E604 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gnoga__gui__element__finalize_spec");
      begin
         F67;
      end;
      E620 := E620 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gnoga__client__storage__finalize_spec");
      begin
         F68;
      end;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gnoga__server__connection__finalize_body");
      begin
         E513 := E513 - 1;
         F69;
      end;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gnoga__server__connection__common__finalize_spec");
      begin
         E581 := E581 - 1;
         F70;
      end;
      E507 := E507 - 1;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gnoga__gui__base__finalize_spec");
      begin
         F71;
      end;
      E533 := E533 - 1;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gnat__sockets__connection_state_machine__http_server__finalize_spec");
      begin
         F72;
      end;
      E572 := E572 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gnat__sockets__connection_state_machine__terminated_strings__finalize_spec");
      begin
         F73;
      end;
      E570 := E570 - 1;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gnat__sockets__connection_state_machine__expected_sequence__finalize_spec");
      begin
         F74;
      end;
      E545 := E545 - 1;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gnat__sockets__connection_state_machine__big_endian__unsigneds__finalize_spec");
      begin
         F75;
      end;
      declare
         procedure F76;
         pragma Import (Ada, F76, "gnat__sockets__connection_state_machine__finalize_body");
      begin
         E515 := E515 - 1;
         F76;
      end;
      declare
         procedure F77;
         pragma Import (Ada, F77, "gnat__sockets__connection_state_machine__finalize_spec");
      begin
         F77;
      end;
      E519 := E519 - 1;
      declare
         procedure F78;
         pragma Import (Ada, F78, "gnat__sockets__server__finalize_spec");
      begin
         F78;
      end;
      E574 := E574 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "stack_storage__finalize_spec");
      begin
         F79;
      end;
      E527 := E527 - 1;
      declare
         procedure F80;
         pragma Import (Ada, F80, "object__finalize_spec");
      begin
         F80;
      end;
      E585 := E585 - 1;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gnoga__server__template_parser__finalize_spec");
      begin
         F81;
      end;
      E598 := E598 - 1;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gnoga__server__model__queries__finalize_spec");
      begin
         F82;
      end;
      E596 := E596 - 1;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gnoga__server__model__finalize_spec");
      begin
         F83;
      end;
      E587 := E587 - 1;
      declare
         procedure F84;
         pragma Import (Ada, F84, "gnoga__server__database__finalize_spec");
      begin
         F84;
      end;
      E589 := E589 - 1;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gnoga__types__finalize_spec");
      begin
         F85;
      end;
      E466 := E466 - 1;
      declare
         procedure F86;
         pragma Import (Ada, F86, "aunit__test_suites__optional__finalize_spec");
      begin
         F86;
      end;
      E406 := E406 - 1;
      declare
         procedure F87;
         pragma Import (Ada, F87, "aunit__test_suites__finalize_spec");
      begin
         F87;
      end;
      E395 := E395 - 1;
      declare
         procedure F88;
         pragma Import (Ada, F88, "aunit__test_filters__ada_lib__finalize_spec");
      begin
         F88;
      end;
      E376 := E376 - 1;
      declare
         procedure F89;
         pragma Import (Ada, F89, "aunit__test_cases__finalize_spec");
      begin
         F89;
      end;
      E722 := E722 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "aunit__reporter__text__finalize_spec");
      begin
         F90;
      end;
      E379 := E379 - 1;
      E381 := E381 - 1;
      declare
         procedure F91;
         pragma Import (Ada, F91, "aunit__simple_test_cases__finalize_spec");
      begin
         F91;
      end;
      declare
         procedure F92;
         pragma Import (Ada, F92, "aunit__test_filters__finalize_spec");
      begin
         F92;
      end;
      E364 := E364 - 1;
      declare
         procedure F93;
         pragma Import (Ada, F93, "aunit__assertions__finalize_spec");
      begin
         F93;
      end;
      E373 := E373 - 1;
      declare
         procedure F94;
         pragma Import (Ada, F94, "aunit__test_results__finalize_spec");
      begin
         F94;
      end;
      declare
         procedure F95;
         pragma Import (Ada, F95, "aunit__tests__finalize_spec");
      begin
         E374 := E374 - 1;
         F95;
      end;
      E498 := E498 - 1;
      declare
         procedure F96;
         pragma Import (Ada, F96, "ada_lib__socket_io__server__finalize_spec");
      begin
         F96;
      end;
      E488 := E488 - 1;
      declare
         procedure F97;
         pragma Import (Ada, F97, "ada_lib__mail__smtp__finalize_spec");
      begin
         F97;
      end;
      E494 := E494 - 1;
      declare
         procedure F98;
         pragma Import (Ada, F98, "ada_lib__socket_io__client__finalize_spec");
      begin
         F98;
      end;
      E490 := E490 - 1;
      declare
         procedure F99;
         pragma Import (Ada, F99, "ada_lib__socket_io__stream_io__finalize_spec");
      begin
         F99;
      end;
      E492 := E492 - 1;
      declare
         procedure F100;
         pragma Import (Ada, F100, "ada_lib__timer__finalize_spec");
      begin
         F100;
      end;
      E462 := E462 - 1;
      declare
         procedure F101;
         pragma Import (Ada, F101, "ada_lib__database__server__state__finalize_spec");
      begin
         F101;
      end;
      E437 := E437 - 1;
      declare
         procedure F102;
         pragma Import (Ada, F102, "ada_lib__database__server__finalize_spec");
      begin
         F102;
      end;
      declare
         procedure F103;
         pragma Import (Ada, F103, "ada_lib__trace_tasks__finalize_body");
      begin
         E444 := E444 - 1;
         F103;
      end;
      E642 := E642 - 1;
      declare
         procedure F104;
         pragma Import (Ada, F104, "ada_lib__trace__options__finalize_spec");
      begin
         F104;
      end;
      E500 := E500 - 1;
      declare
         procedure F105;
         pragma Import (Ada, F105, "ada_lib__template__finalize_spec");
      begin
         F105;
      end;
      E638 := E638 - 1;
      declare
         procedure F106;
         pragma Import (Ada, F106, "ada_lib__options__database__finalize_spec");
      begin
         F106;
      end;
      E670 := E670 - 1;
      declare
         procedure F107;
         pragma Import (Ada, F107, "ada_lib__event__finalize_spec");
      begin
         F107;
      end;
      E450 := E450 - 1;
      declare
         procedure F108;
         pragma Import (Ada, F108, "ada_lib__database__wild__finalize_spec");
      begin
         F108;
      end;
      E458 := E458 - 1;
      declare
         procedure F109;
         pragma Import (Ada, F109, "ada_lib__database__subscription__finalize_spec");
      begin
         F109;
      end;
      E439 := E439 - 1;
      declare
         procedure F110;
         pragma Import (Ada, F110, "ada_lib__database__subscribe__finalize_spec");
      begin
         F110;
      end;
      E452 := E452 - 1;
      declare
         procedure F111;
         pragma Import (Ada, F111, "ada_lib__database__event__finalize_spec");
      begin
         F111;
      end;
      E441 := E441 - 1;
      declare
         procedure F112;
         pragma Import (Ada, F112, "ada_lib__database__updater__finalize_spec");
      begin
         F112;
      end;
      E408 := E408 - 1;
      E433 := E433 - 1;
      declare
         procedure F113;
         pragma Import (Ada, F113, "ada_lib__database__connection__finalize_spec");
      begin
         F113;
      end;
      declare
         procedure F114;
         pragma Import (Ada, F114, "ada_lib__database__finalize_spec");
      begin
         F114;
      end;
      E435 := E435 - 1;
      declare
         procedure F115;
         pragma Import (Ada, F115, "ada_lib__strings__bounded__finalize_spec");
      begin
         F115;
      end;
      E482 := E482 - 1;
      declare
         procedure F116;
         pragma Import (Ada, F116, "ada_lib__mail__curl__finalize_spec");
      begin
         F116;
      end;
      E486 := E486 - 1;
      declare
         procedure F117;
         pragma Import (Ada, F117, "ada_lib__socket_io__finalize_spec");
      begin
         F117;
      end;
      E328 := E328 - 1;
      declare
         procedure F118;
         pragma Import (Ada, F118, "ada_lib__configuration__finalize_spec");
      begin
         F118;
      end;
      E640 := E640 - 1;
      declare
         procedure F119;
         pragma Import (Ada, F119, "ada_lib__options__template__finalize_spec");
      begin
         F119;
      end;
      E636 := E636 - 1;
      declare
         procedure F120;
         pragma Import (Ada, F120, "ada_lib__options__nested__finalize_spec");
      begin
         F120;
      end;
      E393 := E393 - 1;
      declare
         procedure F121;
         pragma Import (Ada, F121, "ada_lib__options__verification__finalize_spec");
      begin
         F121;
      end;
      E320 := E320 - 1;
      declare
         procedure F122;
         pragma Import (Ada, F122, "ada_lib__command_line_iterator__finalize_spec");
      begin
         F122;
      end;
      E322 := E322 - 1;
      declare
         procedure F123;
         pragma Import (Ada, F123, "ada_lib__options__runstring__finalize_spec");
      begin
         F123;
      end;
      E454 := E454 - 1;
      declare
         procedure F124;
         pragma Import (Ada, F124, "ada_lib__object__finalize_spec");
      begin
         F124;
      end;
      E414 := E414 - 1;
      declare
         procedure F125;
         pragma Import (Ada, F125, "ada_lib__auto_lock__finalize_spec");
      begin
         F125;
      end;
      E416 := E416 - 1;
      declare
         procedure F126;
         pragma Import (Ada, F126, "ada_lib__lock__finalize_spec");
      begin
         F126;
      end;
      declare
         procedure F127;
         pragma Import (Ada, F127, "ada_lib__lock_interface__finalize_spec");
      begin
         E429 := E429 - 1;
         F127;
      end;
      declare
         procedure F128;
         pragma Import (Ada, F128, "ada_lib__help__finalize_body");
      begin
         E312 := E312 - 1;
         F128;
      end;
      declare
         procedure F129;
         pragma Import (Ada, F129, "ask__finalize_body");
      begin
         E284 := E284 - 1;
         F129;
      end;
      E207 := E207 - 1;
      declare
         procedure F130;
         pragma Import (Ada, F130, "ada_lib__trace__finalize_body");
      begin
         E160 := E160 - 1;
         F130;
      end;
      declare
         procedure F131;
         pragma Import (Ada, F131, "ada_lib__trace__finalize_spec");
      begin
         F131;
      end;
      E209 := E209 - 1;
      declare
         procedure F132;
         pragma Import (Ada, F132, "ada_lib__parser__finalize_spec");
      begin
         F132;
      end;
      declare
         procedure F133;
         pragma Import (Ada, F133, "ada_lib__options__finalize_spec");
      begin
         F133;
      end;
      E228 := E228 - 1;
      declare
         procedure F134;
         pragma Import (Ada, F134, "ada_lib__strings__unlimited__finalize_spec");
      begin
         F134;
      end;
      E156 := E156 - 1;
      declare
         procedure F135;
         pragma Import (Ada, F135, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F135;
      end;
      declare
         procedure F136;
         pragma Import (Ada, F136, "ada__directories__finalize_body");
      begin
         E330 := E330 - 1;
         F136;
      end;
      declare
         procedure F137;
         pragma Import (Ada, F137, "ada__directories__finalize_spec");
      begin
         F137;
      end;
      E342 := E342 - 1;
      declare
         procedure F138;
         pragma Import (Ada, F138, "system__regexp__finalize_spec");
      begin
         F138;
      end;
      declare
         procedure F139;
         pragma Import (Ada, F139, "gnat__sockets__finalize_body");
      begin
         E260 := E260 - 1;
         F139;
      end;
      declare
         procedure F140;
         pragma Import (Ada, F140, "gnat__sockets__finalize_spec");
      begin
         F140;
      end;
      E236 := E236 - 1;
      declare
         procedure F141;
         pragma Import (Ada, F141, "system__pool_global__finalize_spec");
      begin
         F141;
      end;
      E189 := E189 - 1;
      declare
         procedure F142;
         pragma Import (Ada, F142, "ada__strings__unbounded__finalize_spec");
      begin
         F142;
      end;
      E166 := E166 - 1;
      declare
         procedure F143;
         pragma Import (Ada, F143, "ada__strings__text_buffers__unbounded__finalize_spec");
      begin
         F143;
      end;
      E535 := E535 - 1;
      declare
         procedure F144;
         pragma Import (Ada, F144, "gnat__sha1__finalize_spec");
      begin
         F144;
      end;
      E074 := E074 - 1;
      declare
         procedure F145;
         pragma Import (Ada, F145, "ada__text_io__finalize_spec");
      begin
         F145;
      end;
      E280 := E280 - 1;
      declare
         procedure F146;
         pragma Import (Ada, F146, "system__storage_pools__subpools__finalize_spec");
      begin
         F146;
      end;
      E474 := E474 - 1;
      declare
         procedure F147;
         pragma Import (Ada, F147, "ada__streams__stream_io__finalize_spec");
      begin
         F147;
      end;
      declare
         procedure F148;
         pragma Import (Ada, F148, "system__file_io__finalize_body");
      begin
         E087 := E087 - 1;
         F148;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");
      Interrupts_Default_To_System : Integer;
      pragma Import (C, Interrupts_Default_To_System, "__gl_interrupts_default_to_system");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      procedure Tasking_Runtime_Initialize;
      pragma Import (C, Tasking_Runtime_Initialize, "__gnat_tasking_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, True, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, True, False, True, True, True, True, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, True, True, False, True, True, 
           False, True, True, False, True, True, True, True, 
           False, True, False, False, False, True, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, True, True, False, True, True, False, 
           True, False, True, False, True, False, False, False, 
           False, True, False, True, True, True, False, False, 
           True, False, True, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           True, False, False, False, False, True, True, True, 
           True, True, True, False),
         Count => (0, 0, 0, 2, 20, 22, 9, 2, 27, 0),
         Unknown => (False, False, False, False, False, False, True, True, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);
      Tasking_Runtime_Initialize;

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E022 := E022 + 1;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E018 := E018 + 1;
      E011 := E011 + 1;
      E006 := E006 + 1;
      Ada.Assertions'Elab_Spec;
      E214 := E214 + 1;
      Ada.Containers'Elab_Spec;
      E261 := E261 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E077 := E077 + 1;
      Ada.Numerics'Elab_Spec;
      E400 := E400 + 1;
      Ada.Strings'Elab_Spec;
      E052 := E052 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E054 := E054 + 1;
      Gnat'Elab_Spec;
      E231 := E231 + 1;
      Interfaces.C'Elab_Spec;
      E097 := E097 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E276 := E276 + 1;
      System.Os_Lib'Elab_Body;
      E100 := E100 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E062 := E062 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E051 := E051 + 1;
      Ada.Streams'Elab_Spec;
      E076 := E076 + 1;
      System.File_Control_Block'Elab_Spec;
      E108 := E108 + 1;
      System.Finalization_Root'Elab_Spec;
      E090 := E090 + 1;
      Ada.Finalization'Elab_Spec;
      E088 := E088 + 1;
      System.File_Io'Elab_Body;
      E087 := E087 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E474 := E474 + 1;
      System.Storage_Pools'Elab_Spec;
      E240 := E240 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E280 := E280 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E203 := E203 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E268 := E268 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E334 := E334 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E390 := E390 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E074 := E074 + 1;
      E537 := E537 + 1;
      E539 := E539 + 1;
      Gnat.Sha1'Elab_Spec;
      E535 := E535 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E170 := E170 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E173 := E173 + 1;
      Ada.Strings.Text_Buffers.Unbounded'Elab_Spec;
      E166 := E166 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E189 := E189 + 1;
      System.Interrupt_Management.Operations'Elab_Body;
      E422 := E422 + 1;
      System.Pool_Global'Elab_Spec;
      E236 := E236 + 1;
      Gnat.Sockets'Elab_Spec;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E274 := E274 + 1;
      E266 := E266 + 1;
      Gnat.Sockets'Elab_Body;
      E260 := E260 + 1;
      E264 := E264 + 1;
      System.Random_Seed'Elab_Body;
      E404 := E404 + 1;
      System.Regexp'Elab_Spec;
      E342 := E342 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E330 := E330 + 1;
      System.Tasking.Initialization'Elab_Body;
      E144 := E144 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E154 := E154 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E156 := E156 + 1;
      System.Tasking.Queuing'Elab_Body;
      E152 := E152 + 1;
      System.Tasking.Stages'Elab_Body;
      E424 := E424 + 1;
      System.Tasking.Async_Delays'Elab_Body;
      E420 := E420 + 1;
      E258 := E258 + 1;
      Ada_Lib'Elab_Spec;
      Ada_Lib.Os'Elab_Spec;
      E179 := E179 + 1;
      Ada_Lib.Strings'Elab_Spec;
      Ada_Lib.Strings.Unlimited'Elab_Spec;
      Ada_Lib.Strings.Unlimited'Elab_Body;
      E228 := E228 + 1;
      Ada_Lib.Options'Elab_Spec;
      Ada_Lib.Parser'Elab_Spec;
      Ada_Lib.Substiture_For_Non_Alpha'Elab_Body;
      E183 := E183 + 1;
      E181 := E181 + 1;
      Ada_Lib.Time'Elab_Spec;
      Ada_Lib.Time'Elab_Body;
      E205 := E205 + 1;
      Ada_Lib.Options'Elab_Body;
      E209 := E209 + 1;
      Ada_Lib.Trace'Elab_Spec;
      Hex_Io'Elab_Spec;
      Hex_Io'Elab_Body;
      E244 := E244 + 1;
      Ada_Lib.Strings'Elab_Body;
      E201 := E201 + 1;
      Ada_Lib.Trace'Elab_Body;
      E160 := E160 + 1;
      Ada_Lib'Elab_Body;
      E110 := E110 + 1;
      Ada_Lib.Os'Elab_Body;
      E177 := E177 + 1;
      Ada_Lib.Parser'Elab_Body;
      E207 := E207 + 1;
      Ask'Elab_Body;
      E284 := E284 + 1;
      Ada_Lib.Help'Elab_Spec;
      Ada_Lib.Help'Elab_Body;
      E312 := E312 + 1;
      Ada_Lib.Lock_Interface'Elab_Spec;
      E429 := E429 + 1;
      Ada_Lib.Lock'Elab_Spec;
      Ada_Lib.Lock'Elab_Body;
      E416 := E416 + 1;
      Ada_Lib.Auto_Lock'Elab_Spec;
      Ada_Lib.Auto_Lock'Elab_Body;
      E414 := E414 + 1;
      Ada_Lib.Mail'Elab_Spec;
      Ada_Lib.Mail'Elab_Body;
      E478 := E478 + 1;
      Ada_Lib.Object'Elab_Spec;
      Ada_Lib.Object'Elab_Body;
      E454 := E454 + 1;
      E456 := E456 + 1;
      Ada_Lib.Options.Runstring'Elab_Spec;
      Ada_Lib.Options.Runstring'Elab_Body;
      E322 := E322 + 1;
      Ada_Lib.Command_Line_Iterator'Elab_Spec;
      Ada_Lib.Command_Line_Iterator'Elab_Body;
      E320 := E320 + 1;
      Ada_Lib.Options.Verification'Elab_Spec;
      Ada_Lib.Options.Verification'Elab_Body;
      E393 := E393 + 1;
      Ada_Lib.Options.Nested'Elab_Spec;
      Ada_Lib.Options.Nested'Elab_Body;
      E636 := E636 + 1;
      Ada_Lib.Options.Template'Elab_Spec;
      Ada_Lib.Options.Template'Elab_Body;
      E640 := E640 + 1;
      E346 := E346 + 1;
      Ada_Lib.Directory'Elab_Spec;
      Ada_Lib.Os.Run'Elab_Spec;
      Ada_Lib.Directory'Elab_Body;
      E344 := E344 + 1;
      Ada_Lib.Os.Run'Elab_Body;
      E350 := E350 + 1;
      Ada_Lib.Configuration'Elab_Spec;
      Ada_Lib.Configuration'Elab_Body;
      E328 := E328 + 1;
      E472 := E472 + 1;
      E470 := E470 + 1;
      E476 := E476 + 1;
      Ada_Lib.Socket_Io'Elab_Spec;
      Ada_Lib.Socket_Io'Elab_Body;
      E486 := E486 + 1;
      Ada_Lib.Mail.Gmail'Elab_Spec;
      Ada_Lib.Mail.Gmail'Elab_Body;
      E484 := E484 + 1;
      Ada_Lib.Mail.Curl'Elab_Spec;
      Ada_Lib.Mail.Curl'Elab_Body;
      E482 := E482 + 1;
      Ada_Lib.Strings.Bounded'Elab_Spec;
      Ada_Lib.Strings.Bounded'Elab_Body;
      E435 := E435 + 1;
      Ada_Lib.Database'Elab_Spec;
      Ada_Lib.Database.Connection'Elab_Spec;
      Ada_Lib.Database.Connection'Elab_Body;
      E433 := E433 + 1;
      Ada_Lib.Database'Elab_Body;
      E408 := E408 + 1;
      Ada_Lib.Database.Updater'Elab_Spec;
      Ada_Lib.Database.Updater'Elab_Body;
      E441 := E441 + 1;
      Ada_Lib.Database.Event'Elab_Spec;
      Ada_Lib.Database.Event'Elab_Body;
      E452 := E452 + 1;
      Ada_Lib.Database.Subscribe'Elab_Spec;
      Ada_Lib.Database.Subscribe'Elab_Body;
      E439 := E439 + 1;
      Ada_Lib.Database.Subscription'Elab_Spec;
      Ada_Lib.Database.Subscription'Elab_Body;
      E458 := E458 + 1;
      Ada_Lib.Database.Wild'Elab_Spec;
      Ada_Lib.Database.Wild'Elab_Body;
      E450 := E450 + 1;
      Ada_Lib.Event'Elab_Spec;
      Ada_Lib.Event'Elab_Body;
      E670 := E670 + 1;
      Ada_Lib.Options.Database'Elab_Spec;
      Ada_Lib.Options.Database'Elab_Body;
      E638 := E638 + 1;
      Ada_Lib.Template'Elab_Spec;
      Ada_Lib.Template'Elab_Body;
      E500 := E500 + 1;
      Ada_Lib.Trace.Options'Elab_Spec;
      Ada_Lib.Trace.Options'Elab_Body;
      E642 := E642 + 1;
      Ada_Lib.Trace_Tasks'Elab_Spec;
      Ada_Lib.Trace_Tasks'Elab_Body;
      E444 := E444 + 1;
      Ada_Lib.Database.Server'Elab_Spec;
      Ada_Lib.Database.Server'Elab_Body;
      E437 := E437 + 1;
      Ada_Lib.Database.Server.State'Elab_Spec;
      Ada_Lib.Database.Server.State'Elab_Body;
      E462 := E462 + 1;
      Ada_Lib.Timer'Elab_Spec;
      Ada_Lib.Timer'Elab_Body;
      E492 := E492 + 1;
      Ada_Lib.Socket_Io.Stream_Io'Elab_Spec;
      Ada_Lib.Socket_Io.Stream_Io'Elab_Body;
      E490 := E490 + 1;
      Ada_Lib.Socket_Io.Client'Elab_Spec;
      Ada_Lib.Socket_Io.Client'Elab_Body;
      E494 := E494 + 1;
      Ada_Lib.Mail.Smtp'Elab_Spec;
      Ada_Lib.Mail.Smtp'Elab_Body;
      E488 := E488 + 1;
      Ada_Lib.Socket_Io.Server'Elab_Spec;
      Ada_Lib.Socket_Io.Server'Elab_Body;
      E498 := E498 + 1;
      E362 := E362 + 1;
      Aunit'Elab_Body;
      E360 := E360 + 1;
      E371 := E371 + 1;
      E369 := E369 + 1;
      Aunit.Tests'Elab_Spec;
      E374 := E374 + 1;
      Aunit.Time_Measure'Elab_Spec;
      E366 := E366 + 1;
      Aunit.Test_Results'Elab_Spec;
      Aunit.Test_Results'Elab_Body;
      E373 := E373 + 1;
      Aunit.Assertions'Elab_Spec;
      Aunit.Assertions'Elab_Body;
      E364 := E364 + 1;
      Ada_Lib.Database.Common'Elab_Spec;
      E448 := E448 + 1;
      Aunit.Test_Filters'Elab_Spec;
      Aunit.Simple_Test_Cases'Elab_Spec;
      Aunit.Simple_Test_Cases'Elab_Body;
      E381 := E381 + 1;
      Aunit.Test_Filters'Elab_Body;
      E379 := E379 + 1;
      Aunit.Reporter'Elab_Spec;
      E720 := E720 + 1;
      Aunit.Reporter.Text'Elab_Spec;
      Aunit.Reporter.Text'Elab_Body;
      E722 := E722 + 1;
      Aunit.Test_Cases'Elab_Spec;
      Aunit.Test_Cases'Elab_Body;
      E376 := E376 + 1;
      Aunit.Test_Filters.Ada_Lib'Elab_Spec;
      Aunit.Test_Filters.Ada_Lib'Elab_Body;
      E395 := E395 + 1;
      Aunit.Test_Suites'Elab_Spec;
      Aunit.Test_Suites'Elab_Body;
      E406 := E406 + 1;
      Aunit.Test_Suites.Optional'Elab_Spec;
      Aunit.Test_Suites.Optional'Elab_Body;
      E466 := E466 + 1;
      E517 := E517 + 1;
      E576 := E576 + 1;
      Gnoga'Elab_Body;
      E504 := E504 + 1;
      Gnoga.Application'Elab_Body;
      E630 := E630 + 1;
      Gnoga.Server'Elab_Body;
      E511 := E511 + 1;
      E583 := E583 + 1;
      Gnoga.Types'Elab_Spec;
      Gnoga.Types'Elab_Body;
      E589 := E589 + 1;
      Gnoga.Server.Database'Elab_Spec;
      Gnoga.Server.Database'Elab_Body;
      E587 := E587 + 1;
      Gnoga.Server.Model'Elab_Spec;
      Gnoga.Server.Model'Elab_Body;
      E596 := E596 + 1;
      Gnoga.Server.Model.Queries'Elab_Spec;
      Gnoga.Server.Model.Queries'Elab_Body;
      E598 := E598 + 1;
      Gnoga.Server.Template_Parser'Elab_Spec;
      Gnoga.Server.Template_Parser'Elab_Body;
      E585 := E585 + 1;
      Gnoga.Server.Template_Parser.Simple'Elab_Body;
      E600 := E600 + 1;
      Gnoga.Types.Colors'Elab_Spec;
      E606 := E606 + 1;
      Object'Elab_Spec;
      Object'Elab_Body;
      E527 := E527 + 1;
      E529 := E529 + 1;
      E531 := E531 + 1;
      Stack_Storage'Elab_Spec;
      Stack_Storage'Elab_Body;
      E574 := E574 + 1;
      Strings_Edit'Elab_Spec;
      Strings_Edit'Elab_Body;
      E521 := E521 + 1;
      E547 := E547 + 1;
      E562 := E562 + 1;
      E525 := E525 + 1;
      GNAT.SOCKETS.SERVER'ELAB_SPEC;
      GNAT.SOCKETS.SERVER'ELAB_BODY;
      E519 := E519 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_BODY;
      E515 := E515 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_BODY;
      E545 := E545 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_BODY;
      E570 := E570 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_BODY;
      E572 := E572 + 1;
      E558 := E558 + 1;
      Strings_Edit.Floats'Elab_Body;
      E549 := E549 + 1;
      E560 := E560 + 1;
      Strings_Edit.Utf8'Elab_Body;
      E564 := E564 + 1;
      E602 := E602 + 1;
      E578 := E578 + 1;
      E580 := E580 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_BODY;
      E533 := E533 + 1;
      Gnoga.Gui.Base'Elab_Spec;
      Gnoga.Server.Connection'Elab_Spec;
      Gnoga.Gui.Base'Elab_Body;
      E507 := E507 + 1;
      Gnoga.Server.Connection.Common'Elab_Spec;
      E581 := E581 + 1;
      Gnoga.Server.Connection'Elab_Body;
      E513 := E513 + 1;
      Gnoga.Client.Storage'Elab_Spec;
      Gnoga.Client.Storage'Elab_Body;
      E620 := E620 + 1;
      Gnoga.Gui.Element'Elab_Spec;
      Gnoga.Gui.Element'Elab_Body;
      E604 := E604 + 1;
      Gnoga.Gui.Document'Elab_Spec;
      Gnoga.Gui.Document'Elab_Body;
      E613 := E613 + 1;
      Gnoga.Gui.Element.Style_Block'Elab_Spec;
      Gnoga.Gui.Element.Style_Block'Elab_Body;
      E698 := E698 + 1;
      Gnoga.Gui.Location'Elab_Spec;
      Gnoga.Gui.Location'Elab_Body;
      E622 := E622 + 1;
      Gnoga.Gui.View'Elab_Spec;
      Gnoga.Gui.Element.Common'Elab_Spec;
      Gnoga.Gui.Element.Common'Elab_Body;
      E609 := E609 + 1;
      Gnoga.Gui.View'Elab_Body;
      E611 := E611 + 1;
      Gnoga.Gui.Element.Form'Elab_Spec;
      Gnoga.Gui.Element.Form'Elab_Body;
      E615 := E615 + 1;
      Gnoga.Gui.Element.List'Elab_Spec;
      Gnoga.Gui.Element.List'Elab_Body;
      E700 := E700 + 1;
      Gnoga.Gui.Element.Section'Elab_Spec;
      Gnoga.Gui.Element.Section'Elab_Body;
      E693 := E693 + 1;
      Gnoga.Gui.Element.Table'Elab_Spec;
      Gnoga.Gui.Element.Table'Elab_Body;
      E687 := E687 + 1;
      Gnoga.Gui.View.Card'Elab_Spec;
      Gnoga.Gui.View.Card'Elab_Body;
      E714 := E714 + 1;
      Gnoga.Gui.View.Docker'Elab_Spec;
      Gnoga.Gui.View.Docker'Elab_Body;
      E704 := E704 + 1;
      Gnoga.Gui.Window'Elab_Spec;
      Gnoga.Gui.Window'Elab_Body;
      E617 := E617 + 1;
      Ada_Lib.Gnoga'Elab_Spec;
      Ada_Lib.Gnoga'Elab_Body;
      E502 := E502 + 1;
      Gnoga.Application.Multi_Connect'Elab_Body;
      E632 := E632 + 1;
      Gnoga.Gui.Plugin.Jqueryui'Elab_Spec;
      E696 := E696 + 1;
      Gnoga.Gui.Plugin.Jqueryui.Widget'Elab_Spec;
      Gnoga.Gui.Plugin.Jqueryui.Widget'Elab_Body;
      E702 := E702 + 1;
      Gnoga.Gui.Plugin.Message_Boxes'Elab_Body;
      E712 := E712 + 1;
      Gnoga_Ada_Lib'Elab_Spec;
      Gnoga_Ada_Lib'Elab_Body;
      E626 := E626 + 1;
      Gnoga_Ada_Lib.Base'Elab_Spec;
      Gnoga_Ada_Lib.Base'Elab_Body;
      E628 := E628 + 1;
      Ada_Lib.Options.Program'Elab_Spec;
      Ada_Lib.Options.Unit_Test'Elab_Spec;
      Ada_Lib.Unit_Test'Elab_Spec;
      Ada_Lib.Unit_Test'Elab_Body;
      E386 := E386 + 1;
      Ada_Lib.Unit_Test.Test_Cases'Elab_Spec;
      Ada_Lib.Configuration.Tests'Elab_Spec;
      Ada_Lib.Configuration.Tests'Elab_Body;
      E358 := E358 + 1;
      Ada_Lib.Database.Subscription.Tests'Elab_Spec;
      Ada_Lib.Database.Subscription.Tests'Elab_Body;
      E460 := E460 + 1;
      Ada_Lib.Database.Unit_Test'Elab_Spec;
      Ada_Lib.Database.Server.Tests'Elab_Spec;
      Ada_Lib.Directory.Test'Elab_Body;
      E468 := E468 + 1;
      Ada_Lib.Gnoga.Unit_Test'Elab_Spec;
      Ada_Lib.Gnoga.Unit_Test'Elab_Body;
      E624 := E624 + 1;
      Ada_Lib.Gnoga.Unit_Test.Options'Elab_Spec;
      Ada_Lib.Gnoga.Unit_Test.Options'Elab_Body;
      E634 := E634 + 1;
      Ada_Lib.Mail.Tests'Elab_Spec;
      Ada_Lib.Mail.Tests'Elab_Body;
      E480 := E480 + 1;
      Ada_Lib.Options.Aunit_Lib'Elab_Spec;
      Ada_Lib.Database.Server.Tests'Elab_Body;
      E446 := E446 + 1;
      Ada_Lib.Database.Unit_Test'Elab_Body;
      E464 := E464 + 1;
      Ada_Lib.Options.Program'Elab_Body;
      E318 := E318 + 1;
      Ada_Lib.Options.Unit_Test'Elab_Body;
      E388 := E388 + 1;
      Ada_Lib.Socket_Io.Stream_Io.Unit_Test'Elab_Spec;
      Ada_Lib.Socket_Io.Stream_Io.Unit_Test'Elab_Body;
      E496 := E496 + 1;
      Ada_Lib.Options.Aunit_Lib'Elab_Body;
      E326 := E326 + 1;
      Ada_Lib.Unit_Test.Test_Cases'Elab_Body;
      E397 := E397 + 1;
      Ada_Lib.Unit_Test.Reporter'Elab_Spec;
      Ada_Lib.Unit_Test.Reporter'Elab_Body;
      E719 := E719 + 1;
      Video.Lib'Elab_Spec;
      Configuration'Elab_Spec;
      Configuration'Elab_Body;
      E657 := E657 + 1;
      Video.Lib'Elab_Body;
      E655 := E655 + 1;
      Configuration.Camera'Elab_Spec;
      E658 := E658 + 1;
      Widgets'Elab_Spec;
      E683 := E683 + 1;
      Widgets.Generic_Table'Elab_Spec;
      Widgets.Generic_Table'Elab_Body;
      E689 := E689 + 1;
      Widgets.Video'Elab_Spec;
      Widgets.Video'Elab_Body;
      E710 := E710 + 1;
      Camera'Elab_Spec;
      Camera.Lib'Elab_Spec;
      Camera.Lib.Base'Elab_Spec;
      Camera.Commands'Elab_Spec;
      Camera.Base'Elab_Spec;
      Camera.Command_Queue'Elab_Spec;
      Camera.Commands.Ptz_Optics'Elab_Spec;
      Camera.Configuration'Elab_Spec;
      Camera.Configurations'Elab_Spec;
      Camera.Lib.Base.Command_Tests'Elab_Spec;
      Camera.Lib.Options'Elab_Spec;
      Camera.Lib.Options'Elab_Body;
      E652 := E652 + 1;
      Camera'Elab_Body;
      E644 := E644 + 1;
      Camera.Commands.Ptz_Optics'Elab_Body;
      E672 := E672 + 1;
      Camera.Configuration'Elab_Body;
      E680 := E680 + 1;
      Camera.Lib.Base'Elab_Body;
      E648 := E648 + 1;
      Configuration.Camera.State'Elab_Spec;
      Configuration.Camera.State'Elab_Body;
      E660 := E660 + 1;
      Camera.Commands'Elab_Body;
      E650 := E650 + 1;
      Camera.Command_Queue'Elab_Body;
      E678 := E678 + 1;
      Camera.Main'Elab_Spec;
      Configuration.Camera.Setup'Elab_Spec;
      Configuration.Camera.Setup'Elab_Body;
      E666 := E666 + 1;
      Camera.Base'Elab_Body;
      E664 := E664 + 1;
      Camera.Configurations'Elab_Body;
      E662 := E662 + 1;
      Camera.Lib.Unit_Test'Elab_Spec;
      Camera.Lib.Base.Command_Tests'Elab_Body;
      E682 := E682 + 1;
      Camera.Lib.Options.Unit_Test'Elab_Spec;
      Camera.Lib.Options.Unit_Test'Elab_Body;
      E674 := E674 + 1;
      Widgets.Adjust'Elab_Spec;
      Widgets.Adjust'Elab_Body;
      E706 := E706 + 1;
      Widgets.Configured'Elab_Spec;
      Widgets.Configured'Elab_Body;
      E708 := E708 + 1;
      Widgets.Control'Elab_Spec;
      Widgets.Control'Elab_Body;
      E685 := E685 + 1;
      Camera.Lib'Elab_Body;
      E646 := E646 + 1;
      Camera.Main'Elab_Body;
      E668 := E668 + 1;
      Widgets.Control.Unit_Test'Elab_Body;
      E691 := E691 + 1;
      Camera.Lib.Unit_Test'Elab_Body;
      E676 := E676 + 1;
      Camera.Commands.Unit_Test'Elab_Spec;
      Camera.Commands.Unit_Test'Elab_Body;
      E727 := E727 + 1;
      Camera.Lib.Base.Test'Elab_Spec;
      Camera.Lib.Base.Test'Elab_Body;
      E729 := E729 + 1;
      Configuration.Camera.Setup.Unit_Tests'Elab_Body;
      E731 := E731 + 1;
      Configuration.Camera.State.Unit_Tests'Elab_Body;
      E734 := E734 + 1;
      Widgets.Adjust.Unit_Test'Elab_Spec;
      Widgets.Adjust.Unit_Test'Elab_Body;
      E736 := E736 + 1;
      E717 := E717 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_camera_aunit");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/generic_set.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/generic_discrete_set.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/generic_unbounded_array.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/generic_unbounded_ptr_array.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/object.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/object-handle.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/object-handle-generic_unbounded_array.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/stack_storage.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-base64.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-fields.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-integer_edit.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-integers.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-server.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine-big_endian.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine-big_endian-unsigneds.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine-expected_sequence.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine-terminated_strings.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-float_edit.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-floats.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-quoted.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-utf8.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/strings_edit-utf8-handling.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/tables.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/tables-names.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/gnat-sockets-connection_state_machine-http_server.o
   --   /Users/wayne/Project/git/alr/clone-7-23-26/applications/video/camera/unit_test/obj/execute/camera_aunit.o
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/applications/video/camera/unit_test/obj/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/applications/video/camera/unit_test/obj/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/ada_lib/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/ada_lib/aunit/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/aunit/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/ada_lib/ada_lib_base/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/ada_lib/ada_lib_options/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/lib/execute/
   --   -L/Users/wayne/.local/share/alire/builds/gnatcoll_minimal_26.0.0_cc5dd0a1/777cdb68d10566ae68be0fe9ddab945dcac7542f0e4c8620ba10e3136058e1df/minimal/lib/gnatcoll_core/static/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/obj/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/vendor/github.com/gnoga/deps/simple_components/xpm/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/gnoga_lib/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/applications/video/camera/library/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/ada_lib/ada_lib_test_lib/lib/execute/
   --   -L/Users/wayne/Project/git/alr/clone-7-23-26/applications/video/library/lib/execute/
   --   -L/users/wayne/.local/share/alire/toolchains/gnat_native_16.1.0_657cf254/lib/gcc/aarch64-apple-darwin24.6.0/16.1.0/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lm
--  END Object file/option list   

end ada_main;
