/***************************************************************************************************************************************
File name:			parser.h
Compiler:			MS Visual Studio 2019
Author:				Vidhi Patel and Nishant Savaliya
Student Number:     040955982, 040952053.
Date:				08 Aug 2020
Course:				CST 8152 - Compilers, Lab Section: 011
Assignment:			03
Professor:			Svillen Ranev and Paulo Sousa.
Purpose:			This file contains all the constants, global variables and function declarations that has been used in parser.c file.
****************************************************************************************************************************************/

#ifndef PARSER_H_
#define PARSER_H_

/*include buffer header file if not already included */
#ifndef BUFFER_H_
#include "buffer.h"
#endif

/*include token header file if not already included */
#ifndef TOKEN_H_
#include "token.h"
#endif

/*define constants here*/
#define NO_ATTR -1

#define FLAG1  1
#define FLAG0 0
/* index table constants */
#define ELSE 0	
#define FALSE 1 
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

/* operator constant from enum index*/
#define AND 0
#define OR 1

#define EQ 0
#define NE 1
#define GT 2
#define LT 3

#define PLUS 0
#define MINUS 1 
#define MULT 2 
#define DIV 3

#define SEOF_0 0
#define SEOF_EOF 1

extern Buffer* str_LTBL;
extern Token malar_next_token(Buffer*);
extern char* kw_table[]; /* keyword lookup table*/
extern int line;  /*current line in the buffer */

/*global varibales*/

int synerrno;
static Token lookahead;
static pBuffer sc_buf;


/*function declarations*/

void parser(Buffer*);
void match(int, int);
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program();
void opt_statements();
void statements();
void statement();
void statements_p();
void selectionStatement();
void ifCondition();
void thenCondition();
void elseCondition();
void conditionalExpression();
void orExpression();
void orExpression_p();
void andExpression();
void andExpression_p();
void relationalExpression();
void avidExpression();
void svidExpression();
void primaryAVID();
void primaryAVID_p();
void primarySVID();
void primarySVID_p();
void loopStatement();
void checkifempty();
void preCondition();
void assignStatement();
void arithmeticExpression();
void unaryArithmeticOperation();
void additionArithmeticOperation();
void additionArithmeticOperation_p();
void multiplyArithmeticOperation();
void multiplyArithmeticOperation_p();
void primaryArithmeticOperation();
void stringExpression();
void stringExpression_p();
void primaryStringExpression();
void variable_list();
void outputList();
void variableIdentify();
void variable_list_p();
void input_statement();
void outputStatement();








#endif