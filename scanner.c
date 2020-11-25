/******************************************************************************************
File name    : scanner.c
Compiler     : MS Visual Studio 2019
Author       : Nishant Savaliya (040952053), Vidhi Patel(040955982)
Course       : CST 8152 – Compilers , Lab Section : 011
Assignment   : 02
Date	     : 19 July 2020
Professor    : Svillen Ranev and Paulo Sousa.
Purpose      : Creating a Lexical Analyzer(scanner) for the PLATYPUS programming language.
Function list: scanner_init(), malar_next_token(), get_next_state(), char_class(), aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func11(), aa_func12(), trimStringAndGetToken()
******************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;   /*pointer to temporary lexeme buffer*/
static pBuffer sc_buf;    /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
Token trimStringAndGetToken(char* str, int len, int code); /* Truncates string to given length */
/*****************************************************************************************
Function name :  scanner_init.
Purpose : It performs the initialization of the scanner input buffer and some other scanner components.
Author: Svillen Ranev
History/Versions: 1.0
Called functions: b_isempty(), b_rewind(), b_clear().
Parameters: pBuffer psc_buf
Return value: EXIT_FAILURE (1), EXIT_SUCCESS (0)
*****************************************************************************************/
/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/******************************************************************************************
Purpose :  Performs Token Recognition.
Author  : Nishant Savaliya (040952053), Vidhi Patel(040955982)
History/Versions: 1.0
Called functions: b_getc(), b_retract(),  b_markc(), b_reset(), b_addc(),
b_getcoffset, b_allocate, b_addcoffset, aa_table[](), get_next_state, aa_func11, b_free.
Return value: Token (t)
*****************************************************************************************/
Token malar_next_token() {
	Token t = { 0 };   /* token to return after pattern recognition. Set all structure members to 0 */
	char c;   /* input symbol */
	int state = 0;     /* initial state of the FSM */
	short lexstart;    /* start offset of a lexeme in the input char buffer (array) */
	short lexend;      /* end offset of a lexeme in the input char buffer (array) */
	int accept = NOAS; /* type of state - initially not accepting */
	int i = 0;		   /* counter for loop*/
	int err_index;
	int num_of_retract;

	while (1) { /* endless loop broken by token returns it will generate a warning */
		c = b_getc(sc_buf);
		switch (c) {

			/* White space and tab */
		case ' ':
		case '\t':
			continue;
			/* New line */
		case '\n':
			line++;
			continue;
			/* Left bracket */
		case'{':
			t.code = LBR_T;
			return t;
			/* Right bracket */
		case '}':
			t.code = RBR_T;
			return t;
			/* Left round bracket */
		case '(':
			t.code = LPR_T;
			return t;
			/* Right round bracket */
		case ')':
			t.code = RPR_T;
			return t;
			/* Comma */
		case ',':
			t.code = COM_T;
			return t;
			/* End of statement */
		case ';':
			t.code = EOS_T;
			return t;
			/* Arithmetic operators */
			/* Addition Operator */
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
			/* Subtraction Operator */
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
			/* Multiplication Operator */
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
			/* Division Operator */
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
			/* Relational operator */
			/* Greater than operator */
		case'>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
			/* Less than operator */
		case '<':
			c = b_getc(sc_buf);
			if (c == '>') {
				/* Check if it in not operator */
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else {
				/* Retract by one one and set less than operator */
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			}
			/* Equals to operator */
		case '=':
			c = b_getc(sc_buf);
			/* If it is relational operator  */
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			else {
				/* Retract by one index and return assignment opertor*/
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;
			}
			/* Logical operators */
			/* Logical operator followed by . */
		case'.':

			i = 0;
			err_index = 0;
			num_of_retract = 0;

			t.attribute.err_lex[err_index++] = c;
			c = b_getc(sc_buf);
			num_of_retract++;
			if (c == 'A') {
				c = b_getc(sc_buf);
				num_of_retract++;
				if (c == 'N') {
					c = b_getc(sc_buf);
					num_of_retract++;
					if (c == 'D') {
						c = b_getc(sc_buf);
						num_of_retract++;
					}
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = AND;
						return t;
					}
				}
			}
			else if (c == 'O') {
				c = b_getc(sc_buf);
				num_of_retract++;
				if (c == 'R') {
					c = b_getc(sc_buf);
					num_of_retract++;
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}
			/* Retract by all indexes */
			for (i = 0; i < num_of_retract; i++) {
				b_retract(sc_buf);
			}
			t.code = ERR_T;
			t.attribute.err_lex[err_index] = '\0';
			return t;
		case '#':
			c = b_getc(sc_buf);

			if (c == '#') {
				t.code = SCC_OP_T;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '#';
				t.attribute.err_lex[1] = '\0';
				return t;
			}
			/* Comment */
		case '!':
			/* Set mark to current location  */
			/*b_markc(sc_buf, b_getcoffset(sc_buf));*/
			/* Get next character */
			c = b_getc(sc_buf);
			/* If there is another ! then it is comment */
			if (c == '!') {
				/* Get next character in buffer */
				c = b_getc(sc_buf);
				/* If this is end of file return SEOF_T */
				if (c == (unsigned char)SEOF) {
					t.code = SEOF_T;
					t.attribute.seof = SEOF_EOF;
					return t;
				}
				/* Else return comment */
				else {
					t.code = COM_T;
				}
				/* Loop until end of line */
				while (c != '\n') {
					/* Get next character in buffer */
					c = b_getc(sc_buf);
					/* If it is end of file then return SEOF_T */
					if (c == (unsigned char)SEOF) {
						t.code = SEOF_T;
						t.attribute.seof = SEOF_EOF;
						return t;
					}
				}
				/* Move to next line */
				line++;
				continue;
			}
			/* Else return an error token */
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
				while (c != '\n') /* Discard the remaining characters */
					c = b_getc(sc_buf);
				line++;
				return t;
			}
		case '"':
			lexstart = b_markc(sc_buf, b_getcoffset(sc_buf));

			while (1) {
				c = b_getc(sc_buf);

				/* go to next line if the line ends*/
				if (c == '\n') {
					line += 1;
				}
				/*check for the ending " */
				if (c == '"') {
					t.code = STR_T;
					t.attribute.str_offset = b_addcoffset(str_LTBL);
					lexend = b_getcoffset(sc_buf) - 1;

					/* add the string to the str_LTBL buffer */
					if (lexend != lexstart) {
						/*reset buffer to the start of hte string */
						b_reset(sc_buf);
						/* go through the string and store it into string literal table */
						while (lexstart < lexend) {
							b_addc(str_LTBL, b_getc(sc_buf));
							lexstart++;
						}
						b_getc(sc_buf);
					}
					/* makr the end of hte string*/
					b_addc(str_LTBL, '\0');
					return t;
				}

				/* when reaching end of the file*/
				if (c == SEOF) {
					/* mark the end of the buffer */
					lexend = b_getcoffset(sc_buf);
					lex_buf = b_allocate(lexend - lexstart, 0, 'f');

					/* if buffer allocation fails, then set the token attribute to error */
					if (!lex_buf) {
						t.code = ERR_T;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						return t;
					}

					b_markc(sc_buf, lexstart - 1);
					b_reset(sc_buf);

					/* go through the string and store it into buffer */
					for (i = lexstart; i < lexend; ++i) {
						b_addc(lex_buf, b_getc(sc_buf));
					}

					b_addc(lex_buf, '\0');
					b_getc(sc_buf);
					t = aa_table[12](b_location(lex_buf, 0));
					free(lex_buf);
					return t;
				}
			}

		case EOF:
		case SEOF:
			t.code = SEOF_T;
			t.attribute.seof = SEOF_EOF;
			return t;

		default:
			/*Transition Table Token */
			if (isalpha(c) || isdigit(c)) {
				b_retract(sc_buf);				 /* Retract buffer by one index */
				lexstart = b_getcoffset(sc_buf); /* Set lexstart to first character in buffer  */
				b_markc(sc_buf, lexstart);		 /* Set mark at first character in buffer */
				c = b_getc(sc_buf);				 /* Retrieve next character in buffer */

				while (accept == NOAS) {	     /* Get next char until it is in Accaptable State */
					state = get_next_state(state, c);	/* Get next state */
					accept = as_table[state];			/* Get from state table if it is  */

					if (accept != NOAS)
						break;

					c = b_getc(sc_buf);			  /* Get next character in buffer */
				}

				if (accept == ASWR) {
					b_retract(sc_buf);			/* Retracts by one index */
				}

				lexend = b_getcoffset(sc_buf);  /* Set lexend to last character */

				/* Allocate fixed buffer with size of string */
				lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');
				/* If could not create new buffer then return RUN_TIME_ERROR */
				if (lex_buf == NULL) {
					scerrnum = 1;
					t = aa_func12("RUN TIME ERROR");
					return t;
				}
				b_reset(sc_buf);				/* Reset buffer to first character */
				for (i = lexstart; i < lexend; i++) {
					b_addc(lex_buf, b_getc(sc_buf));  /* Add character to buffer */
				}
				b_addc(lex_buf, '\0');			/* Set end of string */
				t = aa_table[state](b_location(lex_buf, 0));
				b_free(lex_buf);				/* Free lex_buf memory */
				return t;						/* Return lexeme token */
			}
			else if (!isspace(c)) {

				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;

			}
		}

	}
}

/*****************************************************************************************
Purpose : Computes the next state depending on the current state.
Author: Svillen Ranev
History/Versions: 1.0
Called functions: char_class().
Parameters: int state , char c
Return value: int next
*****************************************************************************************/

int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*****************************************************************************************
Function Name : char_class.
Purpose: It returns the column number in the transition table.
Author  : Vidhi Patel(040955982)
History/Versions: 1.0
Called functions: isalpha(), isdigit()
Parameters: char c
Return value: int returnVal
*****************************************************************************************/
int char_class(char c) {
	if (isalpha(c)) {
		return 0;
	}
	else if (c == '0') {
		return 1;
	}
	else if (isdigit(c)) {
		return  2;
	}
	else if (c == '.') {
		return 3;
	}
	else if (c == '#') {
		return 4;
	}
	return 5;
}

/*****************************************************************************************
Function Name : aa_func02.
Purpose :It checks if the lexeme is a keyword.
Author  :Nishant Savaliya (040952053)
History/Versions: 1.0
Called functions: strcmp(), trimStringAndGetToken()
Parameters: char lexeme[]
Return value: Token t
*****************************************************************************************/

Token aa_func02(char* lexeme)
{
	Token t = { 0 };
	int i = 0;

	for (i = 0; i < KWT_SIZE; i++) {
		/* Check if the lexeme is keyword */
		if (strcmp(lexeme, kw_table[i]) == 0) {
			t.code = KW_T;
			t.attribute.kwt_idx = i;
			return t;
		}
	}

	return trimStringAndGetToken(lexeme, VID_LEN, AVID_T);
}

/*****************************************************************************************
Function Name : aa_func03.
Purpose :It Checks for SVID
Author  : Nishant Savaliya (040952053)
History/Versions: 1.0
Called functions: strlen()
Parameters: char lexeme[]
Return value: Token t
*****************************************************************************************/


Token aa_func03(char* lexeme)
{
	Token t = { 0 };
	unsigned int i = 0;

	t.code = SVID_T; /* Set token to SVID */
	/* If lexeme length is greater than VID_LEN then only only take string upto VID_LEN */
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN - 1; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	/* Otherwise store the value directly */
	else {
		for (i = 0; i <= strlen(lexeme); i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	t.attribute.vid_lex[i++] = '#';
	t.attribute.vid_lex[i] = SEOF; /* Set end of string */
	return t; /* retuen SVID token */
}

/*****************************************************************************************
Function Name : aa_func05.
Purpose :It sets the token to IL.
Author  : Nishant Savaliya (040952053)
History/Versions: 1.0
Called functions: atol(), trimStringAndGetToken()
Parameters: char lexeme[]
Return value: INL token
*****************************************************************************************/
Token aa_func05(char* lexeme)
{
	Token t = { 0 };
	unsigned long lex_deciaml = atoi(lexeme);

	/* If value of lexeme  is not in range return error (2 Bytes)*/
	if ((int)lex_deciaml < SHRT_MIN || (int)lex_deciaml > SHRT_MAX) {
		return trimStringAndGetToken(lexeme, ERR_LEN, ERR_T);
	}
	else {
		t.code = INL_T; /* Set token to integer literal */
		t.attribute.int_value = (int)lex_deciaml; /* Case value to int */
		return t; /* Return DIL token */
	}
}

/*****************************************************************************************
Function Name : aa_func08.
Purpose : It converts the lexeme to floating point value
Author  : Vidhi Patel(040955982)
History/Versions: 1.0
Called functions: strtod(),trimStringAndGetToken()
Parameters: lexeme
Return value: Token t
*****************************************************************************************/

Token aa_func08(char* lexeme)
{
	Token t = { 0 };
	double Lex_value = strtod(lexeme, NULL);
	/* If value of lexeme is not valid float return error token */
	if ((Lex_value < FLT_MIN || Lex_value > FLT_MAX) && Lex_value != 0) {
		return trimStringAndGetToken(lexeme, ERR_LEN, ERR_T);
	}
	t.code = FPL_T; /* Set token to floating point literal */
	t.attribute.flt_value = (float)Lex_value; /* Cast value to float */
	return t; /* Return FPL token*/
}

/*****************************************************************************************
Function Name : aa_func11.
Purpose : Return error token
Author  :Vidhi Patel(040955982)
History/Versions: 1.0
Called functions: trimStringAndGetToken()
Parameters: char lexeme[]
Return value: Token (t)
*****************************************************************************************/
Token aa_func11(char* lexeme)
{
	return trimStringAndGetToken(lexeme, ERR_LEN, ERR_T);
}

/*****************************************************************************************
Function Name : aa_func12.
Purpose : Return error token
Author  :  Vidhi Patel(040955982)
History/Versions: 1.0
Called functions: trimStringAndGetToken()
Parameters: char lexeme[]
Return value: Token t
*****************************************************************************************/
Token aa_func12(char* lexeme)
{
	return trimStringAndGetToken(lexeme, ERR_LEN, ERR_T);
}

/*****************************************************************************************
Function Name : trimStringAndGetToken.
Purpose : Return the string truncated to valid length
Author  : Nishant Savaliya (040952053)
History/Versions: 1.0
Called functions: strcmp()
Parameters: char* str, int len, int tokenCode
Return value: Token t
*****************************************************************************************/

Token trimStringAndGetToken(char* str, int len, int tokenCode) {
	Token t = { 0 };
	int i = 0;
	t.code = tokenCode;
	if ((int)strlen(str) > len) {
		for (i = 0; i < len; i++) {
			t.attribute.err_lex[i] = str[i];
		}
		/* Set last 3 characters to . */
		if (strlen(str) > ERR_LEN) {
			t.attribute.err_lex[17] = '.';
			t.attribute.err_lex[18] = '.';
			t.attribute.err_lex[19] = '.';
		}
		/*t.attribute.err_lex[i] = SEOF;  Set end of string */
		return t;
	}
	/* Otherwise store the whole string */
	else {
		for (i = 0; i < (int)strlen(str); i++) {
			t.attribute.err_lex[i] = str[i];
		}
	}
	t.attribute.err_lex[i] = SEOF;
	return t;
}
