/*! \file grib_api.h
  \brief grib_api C header file

  This is the only file that must be included to use the grib_api library
  from C.
*/

#ifndef grib_api_H
#define grib_api_H

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#define GRIB_API_VERSION (GRIB_API_MAJOR_VERSION*10000+GRIB_API_MINOR_VERSION*100+GRIB_API_REVISION_VERSION)



/* LOG MODES
Log mode for information for processiong information
*/
/*  Log mode for info */
#define GRIB_LOG_INFO           0
/*  Log mode for warnings */
#define GRIB_LOG_WARNING        1
/*  Log mode for errors */
#define GRIB_LOG_ERROR          2
/*  Log mode for fatal errors */
#define GRIB_LOG_FATAL          3
/*  Log mode for debug */
#define GRIB_LOG_DEBUG          4

/* Types */
/*  undefined */
#define GRIB_TYPE_UNDEFINED 0
/*  long integer */
#define GRIB_TYPE_LONG 1
/*  double */
#define GRIB_TYPE_DOUBLE 2
/*  char*    */
#define GRIB_TYPE_STRING 3
/*  bytes */
#define GRIB_TYPE_BYTES 4
/*  section */
#define GRIB_TYPE_SECTION 5
/*  labe */
#define GRIB_TYPE_LABEL 6
/*  missing */
#define GRIB_TYPE_MISSING 7

/* Missing values */
#define GRIB_MISSING_LONG   0xffffffff
#define GRIB_MISSING_DOUBLE -1e+100

/* Dump option flags*/
#define GRIB_DUMP_FLAG_READ_ONLY    (1<<0)
#define GRIB_DUMP_FLAG_OPTIONAL     (1<<1)
#define GRIB_DUMP_FLAG_VALUES       (1<<2)
#define GRIB_DUMP_FLAG_CODED        (1<<3)
#define GRIB_DUMP_FLAG_OCTECT       (1<<4)
#define GRIB_DUMP_FLAG_ALIASES      (1<<5)
#define GRIB_DUMP_FLAG_TYPE         (1<<6)
#define GRIB_DUMP_FLAG_HEXADECIMAL  (1<<7)

/*! Iteration is carried out on all the keys available in the message
\ingroup keys_iterator
\see grib_keys_iterator_new
*/
#define GRIB_KEYS_ITERATOR_ALL_KEYS            0

/*! read only keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new
*/
#define GRIB_KEYS_ITERATOR_SKIP_READ_ONLY          (1<<0)

/*! optional keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_OPTIONAL           (1<<1)

/*! edition specific keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_EDITION_SPECIFIC   (1<<2)

/*! coded keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_CODED              (1<<3)

/*! computed keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_COMPUTED           (1<<4)

/*! duplicates of a key are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_DUPLICATES         (1<<5)

/*! function keys are skipped by keys iterator.
\ingroup keys_iterator
\see grib_keys_iterator_new */
#define GRIB_KEYS_ITERATOR_SKIP_FUNCTION           (1<<6)

typedef struct grib_values {
  const char* name;
  int         type;
  long        long_value;
  double      double_value;
  const char* string_value;
  int         error;
  int         has_value;
  int         equal;
} grib_values;

/*! Grib handle,   structure giving access to parsed grib values by keys
    \ingroup grib_handle
*/
typedef struct grib_handle    grib_handle;

/*! Grib context,  structure containing the memory methods, the parsers and the formats.
    \ingroup grib_context
*/
typedef struct grib_context   grib_context;

/*! Grib iterator, structure supporting a geographic iteration of values on a grib message.
    \ingroup grib_iterator
*/
typedef struct grib_iterator  grib_iterator;

/*! Grib keys iterator. Iterator over keys.
    \ingroup keys_iterator
*/
typedef struct grib_keys_iterator    grib_keys_iterator;

/*! \defgroup grib_handle The grib_handle
The grib_handle is the structure giving access to parsed grib values by keys.
*/
/*! @{*/
/**
*  Create a handle from a file resource.
*  The file is read until a message is found. The message is then copied.
*
* @param c           : the context from wich the handle will be created (NULL for default context)
* @param f           : the file resource
* @param error     : error code set if the returned handle is NULL and the end of file is not reached
* @return            the new handle, NULL if the resource is invalid or a problem is encountered
*/
grib_handle* grib_handle_new_from_file     (grib_context* c, FILE* f, int* error)        ;

/**
*  Create a handle from a user message. The message will not be freed at the end.
*  The message will be copied as soon as a modification is needed.
*
* @param c           : the context from which the handle will be created (NULL for default context)
* @param data        : the actual message
* @param data_len    : the length of the message in number of bytes
* @return            the new handle, NULL if the message is invalid or a problem is encountered
*/
grib_handle* grib_handle_new_from_message(grib_context* c, void* data, size_t data_len);


/**
*  Create a handle from a user message. The message is copied and will be freed with the handle
*
* @param c           : the context from wich the handle will be created (NULL for default context)
* @param data        : the actual message
* @param data_len    : the length of the message in number of bytes
* @return            the new handle, NULL if the message is invalid or a problem is encountered
*/
grib_handle* grib_handle_new_from_message_copy(grib_context* c, const void* data, size_t data_len);


/**
*  Create a handle from a read_only template resource.
*  The message is copied at the creation of the handle
*
* @param c           : the context from wich the handle will be created (NULL for default context)
* @param res_name    : the resource name
* @return            the new handle, NULL if the resource is invalid or a problem is encountered
*/
grib_handle* grib_handle_new_from_template (grib_context* c, const char* res_name)  ;



/**
*  Clone an existing handle using the context of the original handle,
*  The message is copied and reparsed
*
* @param h           : The handle to be cloned
* @return            the new handle, NULL if the message is invalid or a problem is encountered
*/
grib_handle* grib_handle_clone             (grib_handle* h)                 ;

/**
*  Frees a handle, also frees the message if it is not a user message
*  @see  grib_handle_new_from_message
* @param h           : The handle to be deleted
* @return            0 if OK, integer value on error
*/
int   grib_handle_delete   (grib_handle* h);
/*! @} */

/*! \defgroup handling_coded_messages Handling coded messages */
/*! @{ */
/**
* getting the message attached to a handle
*
* @param h              : the grib handle to wich the buffer should be gathered
* @param message        : the pointer to be set to the handle's data
* @param message_length : at exist, the message size in number of bytes
* @return            0 if OK, integer value on error
*/
int grib_get_message(grib_handle* h ,const void** message, size_t *message_length  );


/**
* getting a copy of the message attached to a handle
*
* @param h              : the grib handle to wich the buffer should be returned
* @param message        : the pointer to the data buffer to be filled
* @param message_length : at entry, the size in number of bytes of the allocated empty message.
*                         At exist, the actual message length in number of bytes
* @return            0 if OK, integer value on error
*/
int grib_get_message_copy(grib_handle* h ,  void* message,size_t *message_length );
/*! @} */

/*! \defgroup iterators Iterating on latitude/longitude/values */
/*! @{ */

/*!
* \brief Create a new iterator from a handle, using current geometry and values.
*
* \param h           : the handle from which the iterator will be created
* \param flags       : flags for future use.
* \param error       : error code
* \return            the new iterator, NULL if no iterator can be created
*/
grib_iterator*      grib_iterator_new      (grib_handle*   h, unsigned long flags,int* error);

/**
* Get the next value from an iterator.
*
* @param i           : the iterator
* @param lat         : on output latitude in degree
* @param lon         : on output longitude in degree
* @param value       : on output value of the point
* @return            positive value if successful, 0 if no more data are available
*/
int                 grib_iterator_next     (grib_iterator *i, double* lat,double* lon,double* value);

/**
* Get the previous value from an iterator.
*
* @param i           : the iterator
* @param lat         : on output latitude in degree
* @param lon         : on output longitude in degree
* @param value       : on output value of the point*
* @return            positive value if successful, 0 if no more data are available
*/
int                 grib_iterator_previous (grib_iterator *i, double* lat,double* lon,double* value);

/**
* Test procedure for values in an iterator.
*
* @param i           : the iterator
* @return            boolean, 1 if the iterator still nave next values, 0 otherwise
*/
int                 grib_iterator_has_next (grib_iterator *i);

/**
* Test procedure for values in an iterator.
*
* @param i           : the iterator
* @return            0 if OK, integer value on error
*/
int                 grib_iterator_reset    (grib_iterator *i);

/**
*  Frees an iterator from memory
*
* @param i           : the iterator
* @return            0 if OK, integer value on error
*/
int                 grib_iterator_delete   (grib_iterator *i);
/* @} */

/*! \defgroup get_set Accessing header and data values   */
/*! @{ */
/**
*  Reads the next message from a resource and allocate an appropriate buffer
*
* @param c           : the context (NULL for default)
* @param res         : the resource to read from (typically a FILE*)
* @param mesg        : a pointer to the memory space to be allocated
* @param size        : the size of the allocated message on exit
* @return            0 if OK, integer value on error
*/
int grib_read_any_alloc(grib_context *c, void* res, unsigned char** mesg, size_t* size);

/**
*  Reads the next message from a resource in the user given buffer
*
* @param c           : the context (NULL for default)
* @param res         : the resource to read from (typically a FILE*)
* @param mesg        : a pointer to the memory already allocated
* @param size        : on entry, the size of the user allocated buffer, on exit, the actual size of the message
* @return            0 if OK, integer value on error
*/
int grib_read_any      (grib_context *c, void* res, unsigned char* mesg , size_t* size);


/**
*  Get the number offset of a key, in a message if several keys of the same name
*  are present, the offset of the last one is returned
*
* @param h           : the handle to get the offset from
* @param key         : the key to be searched
* @param offset      : the address of a size_t where the offset will be set
* @return            0 if OK, integer value on error
*/
int                  grib_get_offset(grib_handle* h, const char* key, size_t* offset);

/**
*  Get the number of coded value from a key, if several keys of the same name are present, the total sum is returned
*
* @param h           : the handle to get the offset from
* @param key         : the key to be searched
* @param size        : the address of a size_t where the size will be set
* @return            0 if OK, integer value on error
*/
int         grib_get_size(grib_handle* h, const char* key,size_t *size);

/**
*  Get a long value from a key, if several keys of the same name are present, the last one is returned
*  @see  grib_set_long
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param value       : the address of a long where the data will be retreived
* @return            0 if OK, integer value on error
*/
int          grib_get_long         (grib_handle* h, const char* key, long*   value  );

/**
*  Get a double value from a key, if several keys of the same name are present, the last one is returned
*  @see  grib_set_double
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param value       : the address of a double where the data will be retreived
* @return            0 if OK, integer value on error
*/
int grib_get_double       (grib_handle* h, const char* key, double* value                             );

/**
*  Get a string value from a key, if several keys of the same name are present, the last one is returned
* @see  grib_set_string
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param mesg       : the address of a string where the data will be retreived
* @param length      : the address of a size_t that contains allocated length of the string on input, and that contains the actual length of the string on output
* @return            0 if OK, integer value on error
*/
int grib_get_string       (grib_handle* h, const char* key, char*   mesg,             size_t *length  );

/**
*  Get raw bytes values from a key. If several keys of the same name are present, the last one is returned
* @see  grib_set_bytes
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param bytes       : the address of a byte array where the data will be retreived
* @param length      : the address of a size_t that contains allocated length of the byte array on input, and that contains the actual length of the byte array on output
* @return            0 if OK, integer value on error
*/
int grib_get_bytes        (grib_handle* h, const char* key, unsigned char*  bytes,    size_t *length  );
/**
*  Get double array values from a key. If several keys of the same name are present, the last one is returned
* @see  grib_set_double_array
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param vals       : the address of a double array where the data will be retreived
* @param length      : the address of a size_t that contains allocated length of the double array on input, and that contains the actual length of the double array on output
* @return            0 if OK, integer value on error
*/
int grib_get_double_array (grib_handle* h, const char* key, double* vals,           size_t *length  );

/**
*  Get long array values from a key. If several keys of the same name are present, the last one is returned
* @see  grib_set_long_array
*
* @param h           : the handle to get the data from
* @param key         : the key to be searched
* @param vals       : the address of a long array where the data will be retreived
* @param length      : the address of a size_t that contains allocated length of the long array on input, and that contains the actual length of the long array on output
* @return            0 if OK, integer value on error
*/int grib_get_long_array   (grib_handle* h, const char* key, long*   vals,           size_t *length  );



/*   setting      data         */
/**
*  Set a long value from a key. If several keys of the same name are present, the last one is set
*  @see  grib_get_long
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param val         : a long where the data will be read
* @return            0 if OK, integer value on error
*/
int grib_set_long         (grib_handle* h, const char*  key , long val     );

/**
*  Set a double value from a key. If several keys of the same name are present, the last one is set
*  @see  grib_get_double
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param val       : a double where the data will be read
* @return            0 if OK, integer value on error
*/
int grib_set_double       (grib_handle* h, const char*  key , double   val   );

/**
*  Set a string value from a key. If several keys of the same name are present, the last one is set
*  @see  grib_get_string
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param mesg       : the address of a string where the data will be read
* @param length      : the address of a size_t that contains the length of the string on input, and that contains the actual packed length of the string on output
* @return            0 if OK, integer value on error
*/
int grib_set_string       (grib_handle* h, const char*  key , const char* mesg, size_t *length );

/**
*  Set a bytes array from a key. If several keys of the same name are present, the last one is set
*  @see  grib_get_bytes
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param bytes       : the address of a byte array where the data will be read
* @param length      : the address of a size_t that contains the length of the byte array on input, and that contains the actual packed length of the byte array  on output
* @return            0 if OK, integer value on error
*/
int grib_set_bytes        (grib_handle* h, const char*  key, const unsigned char* bytes, size_t *length  );

/**
*  Set a double array from a key. If several keys of the same name are present, the last one is set
*   @see  grib_get_double_array
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param vals        : the address of a double array where the data will be read
* @param length      : a size_t that contains the length of the byte array on input
* @return            0 if OK, integer value on error
*/
int grib_set_double_array (grib_handle* h, const char*  key , const double*        vals   , size_t length  );

/**
*  Set a long array from a key. If several keys of the same name are present, the last one is set
*  @see  grib_get_long_array
*
* @param h           : the handle to set the data to
* @param key         : the key to be searched
* @param vals        : the address of a long array where the data will be read
* @param length      : a size_t that contains the length of the long array on input
* @return            0 if OK, integer value on error
*/
int grib_set_long_array   (grib_handle* h, const char*  key , const long*          vals   , size_t length  );
/*! @} */


/**
*  Print all keys, with the context print procedure and dump mode to a resource
*
* @param h            : the handle to be printed
* @param out          : output file handle
* @param mode         : available dump modes are: debug wmo c_code
* @param option_flags : all the GRIB_DUMP_FLAG_x flags can be used
* @param arg          : used to provide a format to output data (experimental)
*/
void   grib_dump_content(grib_handle* h,FILE* out,const char* mode, unsigned long option_flags,void* arg);

/**
*  Gather all names available in a handle to a string, using a space as separator
*
* @param h           : the handle used to gather the keys
* @param names       : the sting to be filled with the names
*/
void     grib_get_all_names(grib_handle* h,char* names);

/**
*  Print all keys from the parsed definition files available in a context
*
* @param f           : the File used to print the keys on
* @param c           : the context that containd the cached definition files to be printed
*/
 void     grib_dump_action_tree(grib_context* c,  FILE* f) ;

/*! \defgroup context The context object
 The context is a long life configuration object of the grib_api.
 It is used to define special allocation and free routines or
 to set special grib_api behaviours and variables.
 */
/*! @{ */
/**
* Grib free procedure, format of a procedure referenced in the context that is used to free memory
*
* @param c           : the context where the memory freeing will apply
* @param data        : pointer to the data to be freed
* must match @see grib_malloc_proc
*/
typedef void  (*grib_free_proc)     (const grib_context* c, void* data);

/**
* Grib malloc procedure, format of a procedure referenced in the context that is used to allocate memory
* @param c             : the context where the memory allocation will apply
* @param length        : length to be allocated in number of bytes
* @return              a pointer to the alocated memory, NULL if no memory can be allocated
* must match @see grib_free_proc
*/
typedef void* (*grib_malloc_proc)   (const grib_context* c, size_t length);

/**
* Grib loc proc, format of a procedure referenced in the context that is used to log internal messages
*
* @param c             : the context where the logging will apply
* @param level         : the log level, as defined in log modes
* @param mesg          : the message to be logged
*/
typedef void  (*grib_log_proc)      (const grib_context* c, int level, const char* mesg);

/**
* Grib print proc, format of a procedure referenced in the context that is used to print external messages
*
* @param c             : the context where the logging will apply
* @param descriptor    : the structure to be printed on, must match the implementation
* @param mesg          : the message to be printed
*/
typedef void  (*grib_print_proc)    (const grib_context* c, void* descriptor, const char* mesg);


/**
* Grib data read proc, format of a procedure referenced in the context that is used to read from a stream in a resource
*
* @param c             : the context where the read will apply
* @param *ptr          : the resource
* @param size          : size to read
* @param *stream       : the stream
* @return              size read
*/
typedef size_t  (*grib_data_read_proc) (const grib_context* c,void *ptr, size_t size, void *stream);

/**
* Grib data read write, format of a procedure referenced in the context that is used to write to a stream from a resource
*
* @param c             : the context where the write will apply
* @param *ptr          : the resource
* @param size          : size to read
* @param *stream       : the stream
* @return              size written
*/
typedef size_t  (*grib_data_write_proc)(const grib_context* c,const void *ptr, size_t size,  void *stream);

/**
* Grib data tell, format of a procedure referenced in the context that is used to tell the current position in a stream
*
* @param c             : the context where the tell will apply
* @param *stream       : the stream
* @return              the position in the stream
*/
typedef long    (*grib_data_tell_proc) (const grib_context* c, void *stream);

/**
* Grib data eof, format of a procedure referenced in the context that is used to test end of file
*
* @param c             : the context where the tell will apply
* @param *stream       : the stream
* @return              the position in the stream
*/
typedef int    (*grib_data_eof_proc) (const grib_context* c, void *stream);

/**
*  Retreive the context from a handle
*
* @param h           : the handle used to retreive the context from
* @return            The handle's context, NULL it the handle is invalid
*/
grib_context*    grib_get_context       (grib_handle* h);

/**
*  Get the static default context
*
* @return            the default context, NULL it the context is not available
*/
grib_context*    grib_context_get_default();

/**
*  Create and allocate a new context from a parent context.
*
* @param c           : the context to be cloned, NULL for default context
* @return            the new and empty context, NULL if error
*/
grib_context*    grib_context_new                        (grib_context* c);

/**
*  Frees the cached definition files of the context
*
* @param c           : the context to be deleted
*/
void             grib_context_delete                     (grib_context* c);

/**
*  Sets user data in a context
*
* @param c           : the context to be modified
* @param udata       : the user data to set
*/
void             grib_context_set_user_data              (grib_context* c, void* udata);

/**
*  get userData from a context
*
* @param c           : the context from which the user data will be retreived
* @return            the user data referenced in the context
*/
void*            grib_context_get_user_data              (grib_context* c);

/**
*  Sets memory procedures of the context
*
* @param c           : the context to be modified
* @param griballoc   : the memory allocation procedure to be set @see grib_malloc_proc
* @param gribfree    : the memory freeing procedure to be set @see grib_free_proc
*/
void grib_context_set_memory_proc(grib_context* c, grib_malloc_proc griballoc, grib_free_proc gribfree);

/**
*  Sets memory procedures of the context for persistent data
*
* @param c           : the context to be modified
* @param griballoc   : the memory allocation procedure to be set @see grib_malloc_proc
* @param gribfree    : the memory freeing procedure to be set @see grib_free_proc
*/
void             grib_context_set_persistent_memory_proc            (grib_context* c, grib_malloc_proc griballoc, grib_free_proc gribfree);

/**
*  Sets the context search path for definition files
*
* @param c           : the context to be modified
* @param path        : the search path to be set
*/
void   grib_context_set_path(grib_context* c, const char* path);

/**
*  Sets context dump mode
*
* @param c           : the context to be modified
* @param mode        : the log mode to be set
*/
void    grib_context_set_dump_mode(grib_context* c, int mode);

/**
*  Sets the context printing procedure used for user interaction
*
* @param c            : the context to be modified
* @param printp       : the printing procedure to be set @see grib_print_proc
*/
void   grib_context_set_print_proc(grib_context* c, grib_print_proc printp);

/**
*  Sets the context logging procedure used for system (warning, errors, infos ...) messages
*
* @param c            : the context to be modified
* @param logp         : the logging procedure to be set @see grib_log_proc
*/
void    grib_context_set_logging_proc(grib_context* c, grib_log_proc logp);

/**
*  Turn on support for multiple fields in single grib messages
*
* @param c            : the context to be modified
*/
void grib_multi_support_on(grib_context* c);

/**
*  Turn off support for multiple fields in single grib messages
*
* @param c            : the context to be modified
*/
void grib_multi_support_off(grib_context* c);
/*! @} */

/**
*  Get the api version
*
*  @return        api version
*/
long grib_get_api_version();

/**
*  Prints the api version
*
*
*/
void grib_print_api_version(FILE* out);

/*! \defgroup keys_iterator Iterating on keys names
The keys iterator is designed to get the key names defined in a message.
Key names on which the iteration is carried out can be filtered through their
attributes or by the namespace they belong to.
*/
/*! @{ */
/*! Create a new iterator from a valid and initialized handle.
*  @param h             : the handle whose keys you want to iterate
*  @param filter_flags  : flags to filter out some of the keys through their attributes
*  @param name_space     : if not null the iteration is carried out only on
*                         keys belongin to the namespace passed. (NULL for all the keys)
*  @return              keys iterator ready to iterate through keys according to filter_flags
*                         and namespace
*/
grib_keys_iterator* grib_keys_iterator_new(grib_handle* h,unsigned long filter_flags, char* name_space);

/*! Step to the next iterator.
*  @param kiter         : valid grib_keys_iterator
*  @return              1 if next iterator exitsts, 0 if no more elements to iterate on
*/
int grib_keys_iterator_next(grib_keys_iterator *kiter);



/*! get the key name from the iterator
*  @param kiter         : valid grib_keys_iterator
*  @return              key name
*/
const char* grib_keys_iterator_get_name(grib_keys_iterator *kiter);

/*! Delete the iterator.
*  @param kiter         : valid grib_keys_iterator
*  @return              0 if OK, integer value on error
*/
int grib_keys_iterator_delete( grib_keys_iterator* kiter);

/*! Rewind the iterator.
*  @param kiter         : valid grib_keys_iterator
*  @return              0 if OK, integer value on error
*/
int grib_keys_iterator_rewind(grib_keys_iterator* kiter);


int grib_keys_iterator_set_flags(grib_keys_iterator *kiter,unsigned long flags);
/* @} */

void grib_update_sections_lengths(grib_handle* h);


/**
* Convert an error code into a string
* @param code       : the error code
* @return           the error message
*/
const char* grib_get_error_message(int code);
const char* grib_get_type_name(int type);

int grib_get_native_type(grib_handle* h, const char* name,int* type);

void grib_check(const char* call,const char*  file,int line,int e,const char* msg);
#define GRIB_CHECK(a,msg) grib_check(#a,__FILE__,__LINE__,a,msg)


int grib_set_values(grib_handle* h,grib_values*  grib_values , size_t arg_count);
grib_handle* grib_handle_new_from_partial_message_copy(grib_context* c, const void* data, size_t size);
grib_handle* grib_handle_new_from_partial_message(grib_context* c,void* data, size_t buflen);
int grib_is_missing(grib_handle* h, const char* key, int* err);
int grib_set_missing(grib_handle* h, const char* key);
int grib_get_gaussian_latitudes(long truncation,double* latitudes);


#ifdef __cplusplus
}
#endif
#endif
/* This part is automatically generated by ./errors.pl, do not edit */
#ifndef grib_errors_H
#define grib_errors_H
/** No error */
#define GRIB_SUCCESS		0
/** End of ressource reached */
#define GRIB_END_OF_FILE		-1
/** Internal error */
#define GRIB_INTERNAL_ERROR		-2
/** Passed buffer is too small */
#define GRIB_BUFFER_TOO_SMALL		-3
/** Function not yet implemented */
#define GRIB_NOT_IMPLEMENTED		-4
/** Missing 7777 at end of message */
#define GRIB_7777_NOT_FOUND		-5
/** Passed array is too small */
#define GRIB_ARRAY_TOO_SMALL		-6
/** File not found */
#define GRIB_FILE_NOT_FOUND		-7
/** Code not found in code table */
#define GRIB_CODE_NOT_FOUND_IN_TABLE		-8
/** Code cannot unpack because of string too small */
#define GRIB_STRING_TOO_SMALL_FOR_CODE_NAME		-9
/** Array size mismatch */
#define GRIB_WRONG_ARRAY_SIZE		-10
/** Key/value not found */
#define GRIB_NOT_FOUND		-11
/** Input output problem */
#define GRIB_IO_PROBLEM		-12
/** Message invalid */
#define GRIB_INVALID_MESSAGE		-13
/** Decoding invalid */
#define GRIB_DECODING_ERROR		-14
/** Encoding invalid */
#define GRIB_ENCODING_ERROR		-15
/** Code cannot unpack because of string too small */
#define GRIB_NO_MORE_IN_SET		-16
/** Problem with calculation of geographic attributes */
#define GRIB_GEOCALCULUS_PROBLEM		-17
/** Out of memory */
#define GRIB_OUT_OF_MEMORY		-18
/** Value is read only */
#define GRIB_READ_ONLY		-19
/** Invalid argument */
#define GRIB_INVALID_ARGUMENT		-20
/** Null handle */
#define GRIB_NULL_HANDLE		-21
/** Invalid section number */
#define GRIB_INVALID_SECTION_NUMBER		-22
/** Value cannot be missing */
#define GRIB_VALUE_CANNOT_BE_MISSING		-23
#endif
