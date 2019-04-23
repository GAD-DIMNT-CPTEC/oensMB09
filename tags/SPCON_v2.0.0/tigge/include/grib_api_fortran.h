C
C     grib_api_fortran77.h
C
C     Fortran 77 header file
C
C
C
      integer grib_open_file
      external grib_open_file
C
      integer grib_close_file
      external grib_close_file
C
      integer grib_new_from_message
      external grib_new_from_message
C
      integer grib_new_from_template
      external grib_new_from_template
C
      integer grib_clone
      external grib_clone
C
      integer grib_new_from_file
      external grib_new_from_file
C
      integer grib_multi_support_on
      external grib_multi_support_on
C
      integer grib_multi_support_off
      external grib_multi_support_off
C
      integer grib_release
      external grib_release
C
      integer grib_iterator_new
      external grib_iterator_new
C
      integer grib_iterator_next
      external grib_iterator_next
C
      integer grib_iterator_delete
      external grib_iterator_delete
C
      integer grib_dump
      external grib_dump
C
C     integer grib_check
C     external grib_check
C
      integer grib_print
      external grib_print
C
      integer grib_get_error_string
      external grib_get_error_string
C
      integer grib_get_size
      external grib_get_size
C
      integer grib_get_int
      external grib_get_int
C
      integer grib_get_int_array
      external grib_get_int_array
C
      integer grib_set_int_array
      external grib_set_int_array
C
      integer grib_set_int
      external grib_set_int
C
      integer grib_set_real4
      external grib_set_real4
C
      integer grib_get_real4
      external grib_get_real4
C
      integer grib_get_real4_array
      external grib_get_real4_array
C
      integer grib_set_real4_array
      external grib_set_real4_array
C
      integer grib_set_real8
      external grib_set_real8
C
      integer grib_get_real8
      external grib_get_real8
C
      integer grib_get_real8_array
      external grib_get_real8_array
C
      integer grib_set_real8_array
      external grib_set_real8_array
C
      integer grib_get_string
      external grib_get_string
C
      integer grib_set_string
      external grib_set_string
C
      integer grib_get_message_size
      external grib_get_message_size
C
      integer grib_copy_message
      external grib_copy_message
C
      integer grib_write
      external grib_write
C
      integer grib_keys_iterator_new
      external grib_keys_iterator_new
C
      integer grib_keys_iterator_next
      external grib_keys_iterator_next
C
      integer grib_keys_iterator_delete
      external grib_keys_iterator_delete
C
      integer grib_skip_computed
      external grib_skip_computed
C
      integer grib_skip_coded
      external grib_skip_coded
C
      integer grib_skip_edition_specific
      external grib_skip_edition_specific
C
      integer grib_skip_duplicates
      external grib_skip_duplicates
C
      integer grib_skip_read_only
      external grib_skip_read_only
C
      integer grib_skip_function
      external grib_skip_function
C
      integer grib_keys_iterator_get_name
      external grib_keys_iterator_get_name
C
      integer grib_keys_iterator_rewind
      external grib_keys_iterator_rewind
C

