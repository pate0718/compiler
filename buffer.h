#pragma once

/************************************************************************************************************************
File name:		buffer.h
Compiler:		Visual Studio 2019
Author:			Vidhi Patel, ID# 040955982
Version:        1.20.2
Course:			CST 8152 - Compilers, Lab Section: 11
Assignment:		1
Date:			June 4, 2020
Professor:		Svillen Ranev
Purpose:		The file contains all of the macro definitions and function declarations that are used in the buffer.c file.
Function list:	-
************************************************************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */



/*For B_FULL macro function*/
#ifdef B_FULL
#undef B_FULL
#define B_FULL(a) ((a == NULL)? -1:((short)(a->addc_offset*sizeof(char)) == a->capacity)? 1:0)
#endif
#ifndef B_FULL
#define B_FULL(a) b_isfull(a)
#endif

/*Maximum capacity*/
#define MAX_CAP (SHRT_MAX -1)

/*Return b_incfactor failure value*/
#define INC_FACTOR_FAIL 0x100



/*For inc_factor value boundaries*/
#define INC_FACTOR_ADD_MAX 255
#define INC_FACTOR_ADD_MIN 1
#define INC_FACTOR_MULTI_MAX 100
#define INC_FACTOR_MULTI_MIN 1
#define INC_FACTOR_A_M 15
#define INC_FACTOR_F 0

/*For buffer operational mode indicator*/
#define MODE_FIXED 0
#define MODE_ADD 1
#define MODE_MULTI (-1)



/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFF9 /* 1111 1111 1111 1001*/
#define DEFAULT_ZERO   0x0000 /* 0000 0000 0000 0000 */

#define SET_EOB     0x0002
#define RESET_EOB   0xFFFD
#define CHECK_EOB   0x0002

#define SET_R_FLAG      0x0004
#define RESET_R_FLAG    0xFFFB /* 1111 1111 1111 1011 */
#define CHECK_R_FLAG    0x0004

/* user data type declarations */
typedef struct BufferDescriptor {
    char* cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, * pBuffer;

/*typedef Buffer *pBuffer;*/


/* function declarations */
Buffer* b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_clear(Buffer* const);
void b_free(Buffer* const);
int b_isfull(Buffer* const);
short b_addcoffset(Buffer* const);
short b_capacity(Buffer* const);
short b_markc(pBuffer const, short);
int b_mode(Buffer* const);
size_t b_incfactor(Buffer* const);
int b_load(FILE* const, Buffer* const);
int b_isempty(Buffer* const);
char b_getc(Buffer* const);
int b_eob(Buffer* const);
int b_print(Buffer* const, char);
Buffer* b_compact(Buffer* const, char);
char b_rflag(Buffer* const);
short b_retract(Buffer* const);
short b_reset(Buffer* const);
short b_getcoffset(Buffer* const);
int b_rewind(Buffer* const);
char* b_location(Buffer* const, short);

/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif
