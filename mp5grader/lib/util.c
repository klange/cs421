/* $Id: util.c,v 1.1 2003/09/08 18:56:59 kircher Exp $ */

/*
 * util.c
 *
 * defines utilities that are difficult / impossible to write
 * in native Ocaml code.
 */

#include <unistd.h>
#include <limits.h>
#include <caml/mlvalues.h>

/*
 * this is somewhat of a hack.  oh well.
 * it looks like POSIX is moving away from having an
 * absolute maximum number of files open for a given
 * process.
 *
 * there probably should be a portable way to figure
 * out just what files a process has open.  but until
 * then . . .
 */
#ifndef OPEN_MAX
	#define OPEN_MAX 8192
#endif

CAMLprim value close_all_files (value unit)
{
	int i;
	
	for (i = 3; i < OPEN_MAX; i++)
	{
		close(i);
	}
	
	return Val_unit;
}
