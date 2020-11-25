/****************************************************************************************************************************
File name:			parser.c
Compiler:			MS Visual Studio 2019
Author:				Vidhi Patel and Nishant Savaliya
Student Number:     040955982, 040952053.
Date:				08 Aug 2020
Course:				CST 8152 - Compilers, Lab Section: 011
Assignment:			03
Professor:			Svillen Ranev and Paulo Sousa.
Purpose:			To create and implement parser using PLATYPUS programming language.
Function list:		parser(), match(), syn_eh()(error handling function), syn_printe()(error printing function),
                    gen_incode(), program(), opt_statements(), statements(), statement(), selectionStatement(),
                    ifCondition(), thenCondition(), elseCondition(), conditionalExpression(), orExpression(),
                    orExpression_p(), andExpression(), andExpression_p(), relationalExpression(), avidExpression(), 
                    svidExpression(), primaryAVID(), primaryAVID_p(), primarySVID(), primarySVID_p(), loopStatement(), 
                    checkifempty(), preCondition(), assignStatement(), arithmeticExpression(), unaryArithmeticOperation(),
                    additionArithmeticOperation(), additionArithmeticOperation_p(), multiplyArithmeticOperation(),
                    multiplyArithmeticOperation_p(), primaryArithmeticOperation(), stringExpression(), stringExpression_p(),
                    primaryStringExpression(), statements_p(), input_statement(), outputStatement(), variable_list(),
                    variable_list_p(), variableIdentify(), outputList().
*****************************************************************************************************************************/

#include "parser.h"
#include <stdlib.h>


char flag1 = FLAG0;

/* function to start the parser (provided in the assignment specs) */
void parser(Buffer* in_buf) {
    sc_buf = in_buf;
    lookahead = malar_next_token(sc_buf);
    program(); match(SEOF_T, NO_ATTR);
    gen_incode("PLATY: Source file parsed");

}
/* function to match parser token with the lookahead token code */
void match(int pr_token_code, int pr_token_attribute) {

    if (lookahead.code != pr_token_code) {

        syn_eh(pr_token_code);
        return;
    }
    switch (pr_token_code) {
    case SEOF_T:
        if (lookahead.code == SEOF_T) {
            return;
        }
        break;
    case KW_T:
        if (pr_token_attribute != lookahead.attribute.kwt_idx) {

            syn_eh(pr_token_code);
            return;
        }
        break;
    case LOG_OP_T:
    case ART_OP_T:
    case REL_OP_T:
        if (pr_token_attribute != lookahead.attribute.get_int) {

            syn_eh(pr_token_code);
            return;
        }
        break;

    }

    lookahead = malar_next_token(sc_buf);

    if (lookahead.code == ERR_T) {

        syn_printe();
        lookahead = malar_next_token(sc_buf);
        synerrno++;
        return;
    }

}
/* error handling function */
void syn_eh(int sync_token_code) {
    /*first call sync_print() function n increment counter*/

    syn_printe();
    synerrno++;
    while (1) {
        /* */
        lookahead = malar_next_token(sc_buf);
        /* if end of file has reached then exit with errcounter*/
        if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) {
            exit(synerrno);
            return;
        }
        /* forward the input token and return */
        if (lookahead.code == sync_token_code && lookahead.code != SEOF_T) {
            lookahead = malar_next_token(sc_buf);
            return;
        }
        if (lookahead.code == sync_token_code && sync_token_code == SEOF_T) {
            return;
        }
        /* exit if synctoken is different than code*/
    }


}
/* error printing function for Assignment 3 (Parser), S20 */
void syn_printe() {
    Token t = lookahead;

    printf("PLATY: Syntax error:  Line:%3d\n", line);
    printf("*****  Token code:%3d Attribute: ", t.code);
    switch (t.code) {
    case  ERR_T: /* ERR_T     0   Error token */
        printf("%s\n", t.attribute.err_lex);
        break;
    case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
        printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
        break;
    case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
    case  SVID_T:/* SVID_T    3  String Variable identifier token */
        printf("%s\n", t.attribute.vid_lex);
        break;
    case  FPL_T: /* FPL_T     4  Floating point literal token */
        printf("%5.1f\n", t.attribute.flt_value);
        break;
    case INL_T: /* INL_T      5   Integer literal token */
        printf("%d\n", t.attribute.get_int);
        break;
    case STR_T:/* STR_T     6   String literal token */
        printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
        break;

    case SCC_OP_T: /* 7   String concatenation operator token */
        printf("NA\n");
        break;

    case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
        printf("NA\n");
        break;
    case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
        printf("%d\n", t.attribute.get_int);
        break;

    case  LPR_T: /*LPR_T    12  Left parenthesis token */
        printf("NA\n");
        break;
    case  RPR_T: /*RPR_T    13  Right parenthesis token */
        printf("NA\n");
        break;
    case LBR_T: /*    14   Left brace token */
        printf("NA\n");
        break;
    case RBR_T: /*    15  Right brace token */
        printf("NA\n");
        break;

    case KW_T: /*     16   Keyword token */
        printf("%s\n", kw_table[t.attribute.get_int]);
        break;

    case COM_T: /* 17   Comma token */
        printf("NA\n");
        break;
    case EOS_T: /*    18  End of statement *(semi - colon) */
        printf("NA\n");
        break;
    default:
        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/

/*function to print the string */
void gen_incode(char* str) {
    printf("%s \n", str);
}

/*function provided by prof in assign_specs*/
/*function checks for the PLATYPUS word in the token */
/*FIRST(<program>) = {KW_T(PLATYPUS)} */
/* Production set :<program> -> PLATYPUS<opt_statments> */
void program(void) {
    match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    gen_incode("PLATY: Program parsed");
}

/*function provided by prof in assign_specs*/
/*Production set : <opt_statements> -> <statements>| e*/
/* FIRST(<opt_statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}*/
void opt_statements() {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T: statements(); break;
    case KW_T:
        /* check for IF,WHILE,READ,WRITE in statements_p() as well*/
        if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE || lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE) {
            statements();
            break;
        }

    default:
        gen_incode("PLATY: Opt_statements parsed");
    }
}

/*
* Production set :<statements> -> <statement><statements_p>*/
/* FIRST(<statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }*/
void statements() {
    statement();
    statements_p();

}

/* FIRST(<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }*/
/* Production set:<statement> -> <assignment statement> | <selection statement>| <iteration statement>| <input statement>| <output statement> */

void statement() {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T:
        assignStatement();
        break;
    case KW_T:
        if (lookahead.attribute.get_int == IF) {
            selectionStatement();


        }
        else if (lookahead.attribute.get_int == WHILE) {
            loopStatement();


        }
        else if (lookahead.attribute.get_int == READ) {
            input_statement();


        }
        else if (lookahead.attribute.get_int == WRITE) {
            outputStatement();


        }
        else {
            syn_printe();
        }
        break;

    default:

        /*empty string – optional statements*/
        break;


    }

}

/*function to check for if conditional statements
FIRST(<selection statement>) = { KW_T({ IF , TRUE , THEN , ELSE}) } */
/* production set: <selection statement> ->
IF <pre-condition> (<conditional expression>) THEN { <opt_statements> }
ELSE { <opt_statements> } */
void selectionStatement() {
    ifCondition();
    thenCondition();
    elseCondition();
    gen_incode("PLATY: Selection statement parsed");

}

/* helper function to match IF TRUE keyword */
void ifCondition() {
    match(KW_T, IF);
    preCondition();
    match(LPR_T, NO_ATTR);
    conditionalExpression();
    match(RPR_T, NO_ATTR);
}

/* helper function to match THEN keyword */
void thenCondition() {
    match(KW_T, THEN);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);

}

/* helper function to match ELSE keyword */
void elseCondition() {
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

}

/* production set: <conditional expression> -> <logical OR expression>
   First(<conditional expression>) =  {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/

void conditionalExpression() {
    orExpression();
}

/* Produciton set:  <logical OR expression> -> <logical AND expression> <logical OR expression_p> */
/*FIRST(<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }*/
void orExpression() {
    andExpression();
    orExpression_p();
    gen_incode("PLATY: Conditional expression parsed");
}

/*Produciton set:  <logical OR expression_p> -> <logical AND expression> .OR. <logical OR expression_p>*/
/* FIRST(<logical OR expression_p) = {.OR.,e}*/
void orExpression_p() {
    if (lookahead.code == LOG_OP_T) {
        if (lookahead.attribute.log_op == OR) {
            match(LOG_OP_T, OR);
            andExpression();
            orExpression_p();
            gen_incode("PLATY: Logical OR expression parsed");
        }
    }

}

/*Production set: <logical AND expression> -> <relational expression> | <logical AND expression_p>
FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }*/
void andExpression() {
    relationalExpression();
    andExpression_p();
}

/* Production set: <logical AND expression_p> -> <relational expression> .AND. <logical AND expression_p>*/
/* FIRST(<logical AND expression_p>) = { .AND., e }*/
void andExpression_p() {
    if (lookahead.code == LOG_OP_T) {
        if (lookahead.attribute.log_op == AND) {
            match(LOG_OP_T, AND);
            relationalExpression();
            andExpression_p();
            gen_incode("PLATY: Logical AND expression parsed");
        }

    }


}

/* Prodcution set: <relational expression> -> <AVID expression> | <SVID expression>*/
/*FIRST(<relational Expression>) = { AVID_T, INL_T, FPL_T, SVID_T, STR_T } */
void relationalExpression() {
    switch (lookahead.code) {
    case AVID_T:
    case INL_T:
    case FPL_T:

        avidExpression();
        break;
    case SVID_T:
    case STR_T:
        svidExpression();
        break;
    default:
        syn_printe();
        break;

    }

    gen_incode("PLATY: Relational expression parsed");
}

/*Prodcution set:<AVID Expression> -> <primary a_relational expression>  <primary a_relational expression_p>*/
/*FIRST(<AVID Expression> = { AVID_T, FPL_T, INL_T ,relational operator})*/

void avidExpression() {
    primaryAVID();
    primaryAVID_p();

}

/*Prodcution set:<SVID Expression> -><primary s_relational expression> <primary s_relational expression_p>*/
/*FIRST(<SVID Expression> = {SVID_T , STR_T , relational operator} */
void svidExpression() {
    primarySVID();
    primarySVID_p();

}

/* Produciton set:<primary a_relational expression> -> <floating-point literal> | <integer literal> | <arithmetic variable identifier>*/
/*FIRST(<primary a_relational expression>) = { FPL_T, INL_T, AVID_T }*/
void primaryAVID() {
    switch (lookahead.code) {
    case AVID_T:
    case INL_T:
    case FPL_T:
        match(lookahead.code, NO_ATTR);
        gen_incode("PLATY: Primary a_relational expression parsed");
        break;

    case SVID_T:
    case STR_T:
        syn_printe();
        gen_incode("PLATY: Primary a_relational expression parsed");
        break;

    default:
        /*no synprint*/
        break;

    }

}

/*Production set: <primary a_relational expression_p> == <primary a_relational expression> | <primary a_relational expression> <> <primary a_relational expression>
| <primary a_relational expression> > <primary a_relational expression> | <primary a_relational expression> < <primary a_relational expression>*/
/*FIRST(<primary a_relational expression_ p>) = { EQ , NE , GT, LT }*/
void primaryAVID_p() {
    switch (lookahead.code) {
    case REL_OP_T:
        if (lookahead.attribute.rel_op == EQ || lookahead.attribute.rel_op == NE || lookahead.attribute.rel_op == GT || lookahead.attribute.rel_op == LT) {
            match(REL_OP_T, lookahead.attribute.rel_op);
            primaryAVID();
        }

        break;
    default:
        syn_printe();
        break;
    }

}

/*Production set:<primary s_relational expression> -> <primary string expression> */
/*FIRST(primary s_relational expression) = {SVID_T,STR_T}*/
void primarySVID() {
    switch (lookahead.code) {
    case AVID_T:
    case FPL_T:
    case INL_T:
        syn_printe();
        gen_incode("PLATY: Primary s_relational expression parsed");
        break;
    case SVID_T:
    case STR_T:
        primaryStringExpression();
        gen_incode("PLATY: Primary s_relational expression parsed");
        break;

    default:
        syn_printe();
        break;
    }

}

/*Production set:<primary s_relational expression_p> -> <primary s_relational expression> == <primary s_relational expression>
| <primary s_relational expression> <> <primary s_relational expression> | <primary s_relational expression> > <primary s_relational expression>
| <primary s_relational expression> < <primary s_relational expression> */
/*FIRST(primary s_relational expression_p) ={ EQ,NE,GT,LT}*/
void primarySVID_p() {
    switch (lookahead.code) {
    case REL_OP_T:
        if (lookahead.attribute.rel_op == EQ || lookahead.attribute.rel_op == NE || lookahead.attribute.rel_op == GT || lookahead.attribute.rel_op == LT) {
            match(REL_OP_T, lookahead.attribute.rel_op);
            primarySVID();
        }
        else {
            syn_printe();
        }

    default:
        /*maybe sync print*/
        break;
    }

}

/*<iteration statement> WHILE <pre-condition> (<conditional Expression>) REPEAT {<statements>}
<pre-condition> -> TRUE | FALSE */
/*FIRST( <iteration statement>) = { KW_T(WHILE), KW_T(REPEAT)} */
void loopStatement() {
    /* check for while loop*/
    match(KW_T, WHILE);
    preCondition();
    match(LPR_T, NO_ATTR);
    conditionalExpression();
    match(RPR_T, NO_ATTR);

    /* check for repeat */
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    checkifempty();
    statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Iteration statement parsed");

}

/*function to check if there is no statement in repeat */
void checkifempty() {
    if (lookahead.code == RBR_T) {
        syn_printe();
    }
}
void preCondition() {
    switch (lookahead.code) {
    case KW_T:
        if (lookahead.attribute.kwt_idx == TRUE) {
            match(KW_T, TRUE);
        }
        else if (lookahead.attribute.kwt_idx == FALSE) {
            match(KW_T, FALSE);
        }
        else {
            syn_printe();
        }

    default:
        break;
    }
}

/*produciton set: <assignment statement> -> <assignments expression> -> AVID | SVID*/
/*  AVID = <arithmetic expression> , SVID_T = <string expression> */
/*FIRST(<assignment statement>) = { AVID_T, SVID_T }*/
void assignStatement() {
    switch (lookahead.code) {
    case AVID_T:
        match(AVID_T, NO_ATTR);
        match(ASS_OP_T, EQ);
        arithmeticExpression();
        gen_incode("PLATY: Assignment expression (arithmetic) parsed");
        match(EOS_T, NO_ATTR);
        break;
    case SVID_T:
        match(SVID_T, NO_ATTR);
        match(ASS_OP_T, EQ);
        stringExpression();
        gen_incode("PLATY: Assignment expression (string) parsed");
        match(EOS_T, NO_ATTR);
        break;

    default:
        syn_printe();
        break;

    }
    gen_incode("PLATY: Assignment statement parsed");
}

/* Production set : <arithmetic expression> -> <unary_arithmetic_expression> | <additive arithmetic expression>*/
/*FIRST(<arithmetic expression>) = {PLUS,MINUS, AVID_T,LPR_T,FPL_T }*/
void arithmeticExpression() {
    switch (lookahead.code) {
    case ART_OP_T:
        if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) {
            unaryArithmeticOperation();
        }
        else {
            syn_printe();
        }
        gen_incode("PLATY: Arithmetic expression parsed");
        break;

    case AVID_T:
    case LPR_T:
    case INL_T:
    case FPL_T:
        additionArithmeticOperation();

        break;
    default:

        syn_printe();
        break;
    }
}

/* Production set : <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression> */

/* FIRST(<unary arithmetic expression>) = {PLUS, MINUS}*/
void unaryArithmeticOperation() {
    if (lookahead.code == ART_OP_T) {
        if (lookahead.attribute.arr_op == PLUS) {
            match(ART_OP_T, PLUS);
            primaryArithmeticOperation();
            gen_incode("PLATY: Unary arithmetic expression parsed");
        }
        else if (lookahead.attribute.arr_op == MINUS) {
            match(ART_OP_T, MINUS);
            primaryArithmeticOperation();
            gen_incode("PLATY: Unary arithmetic expression parsed");
        }
        else {

            syn_printe();/*new*/

        }
    }

}

/*Production set : <additive arithmetic expression> -> <multiplicative arithmetic expression>  <additive arithmetic expression_p>*/
/* FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T }*/
void additionArithmeticOperation() {
    multiplyArithmeticOperation();
    additionArithmeticOperation_p();
    gen_incode("PLATY: Arithmetic expression parsed");
}

/*Production set :<additive arithmetic expression_p> -> <additive arithmetic expression> + <multiplicative arithmetic expression>
| <additive arithmetic expression_p> - <multiplicative arithmetic expression> */
/*FIRST(<additive arithmetic expression_p>) = { PLUS, MINUS, e } */
void additionArithmeticOperation_p() {
    if (lookahead.code == ART_OP_T) {
        if (lookahead.attribute.arr_op == PLUS) {
            match(ART_OP_T, PLUS);
            multiplyArithmeticOperation();
            additionArithmeticOperation_p();
            gen_incode("PLATY: Additive arithmetic expression parsed");

        }
        else if (lookahead.attribute.arr_op == MINUS) {
            match(ART_OP_T, MINUS);
            multiplyArithmeticOperation();
            additionArithmeticOperation_p();
            gen_incode("PLATY: Additive arithmetic expression parsed");
        }
        else {

            syn_printe();/*new*/
        }

    }
}

/* production set : <multiplicative arithmetic expression> -> <primary arithmetic expression> < multiplicative arithmetic expression_p>*/
/*FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T }*/
void multiplyArithmeticOperation() {
    primaryArithmeticOperation();
    multiplyArithmeticOperation_p();

}

/* production set :<multiplicative arithmetic expression_p> -> <multiplicative arithmetic expression_p> * <primary arithmetic expression>
| <multiplicative arithmetic expression_p> / <primary arithmetic expression> | <primary arithmetic expression> */
/*FIRST(<multiplicative arithmetic expression_p>) = { MULT, DIV, e }*/
void multiplyArithmeticOperation_p() {
    if (lookahead.code == ART_OP_T) {
        if (lookahead.attribute.arr_op == MULT) {
            match(ART_OP_T, MULT);
            primaryArithmeticOperation();
            multiplyArithmeticOperation_p();
            gen_incode("PLATY: Multiplicative arithmetic expression parsed");

        }
        else if (lookahead.attribute.arr_op == DIV) {
            match(ART_OP_T, DIV);
            primaryArithmeticOperation();
            multiplyArithmeticOperation_p();
            gen_incode("PLATY: Multiplicative arithmetic expression parsed");

        }
        else {

        }

    }
}

/*Production set: <primary arithmetic expression> -> <arithmetic variable identifier> | <floating-point literal> | <integer literal> | (<arithmetic expression>)*/
/*FIRST(<primary arithmetic expression>) = { AVID_T, INL_T, FPL_T, LPR_T  }*/
void primaryArithmeticOperation() {
    if (lookahead.code == AVID_T || lookahead.code == INL_T || lookahead.code == FPL_T) {
        match(lookahead.code, NO_ATTR);
    }
    else if (lookahead.code == LPR_T) {
        match(LPR_T, NO_ATTR);
        arithmeticExpression();
        match(RPR_T, NO_ATTR);

    }
    gen_incode("PLATY: Primary arithmetic expression parsed");

}

/*Production set: <string expression> -> <primary string expression> | <string expression_p> ## <primary string expression>*/
/*FIRST(<string expression> ) = {SVID_T,STR_T}*/
void stringExpression() {
    primaryStringExpression();
    stringExpression_p();
    gen_incode("PLATY: String expression parsed");

}

/* Produciton set: <string expression_p> -> # <primary string expression> <string expression_p> | e*/
/*FIRST(<string expression>) = { SCC_OP_T, e }*/
void stringExpression_p() {
    if (lookahead.code == SCC_OP_T) {
        match(SCC_OP_T, NO_ATTR);
        primaryStringExpression();
        stringExpression_p();
    }


}

/* Produciton set: <primary string expression> -> <string variable identifier> | <string literal>*/
/*FIRST(<primary string expression>) = { SVID_T, STR_T }*/
void primaryStringExpression() {
    if (lookahead.code == SVID_T) {
        match(SVID_T, NO_ATTR);

    }
    if (lookahead.code == STR_T) {
        match(STR_T, NO_ATTR);
    }
    gen_incode("PLATY: Primary string expression parsed");

}

/*produciton set: <statements_p> -> <statement> <statements_p> | e */
/*FIRST(<statements_p>)={AVID_T,SVID_T, KW_T( { IF,WHILE,READ,WRITE }),e }*/
void statements_p() {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T: statement(); statements(); break;
    case KW_T:
        if (lookahead.attribute.get_int == IF || lookahead.attribute.get_int == WHILE || lookahead.attribute.get_int == READ || lookahead.attribute.get_int == WRITE) {
            statements();

        }
        break;

    default:
        break;
    }


}

/*produciton set:<input statement> -> READ (<variable list>); */
/*FIRST(<input statement>) = { KW_T(READ) }*/
void input_statement(void) {
    match(KW_T, READ); match(LPR_T, NO_ATTR); variable_list();
    match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Input statement parsed");
}

/*produciton set:<output statement> -> WRITE (<outputList>); */
/*FIRST(<output statement>) = { KW_T(WRITE) }*/
void outputStatement() {
    match(KW_T, WRITE);
    match(LPR_T, NO_ATTR);
    outputList();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Output statement parsed");
}

/* produciton set: <variable list> -> <variable identifier> <variable list_p>*/
/* FIRST(<variable list>) = { AVID_T, SVID_T }*/
void variable_list() {
    variableIdentify();
    variable_list_p();
    gen_incode("PLATY: Variable list parsed");
}

/* produciton set: <variable list_p> -> <variable identifier>, <variable list _p>*/
/* FIRST(<variable list_p>) = { COM_T, AVID_T, SVID_T }*/
void variable_list_p() {
    if (lookahead.code == COM_T) {
        match(COM_T, NO_ATTR);
        variableIdentify();
        variable_list_p();
    }
}

/* produciton set: <variable identifier> ->  <string variable identifier> | <Arithmetic variable identifier>
/* FIRST(<variable identifier>) = { COM_T, AVID_T, SVID_T }*/
void variableIdentify() {
    if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
        match(lookahead.code, NO_ATTR);
    }
    else {

        syn_printe();
    }
}

/* produciton set: <output list> -> <variable list> | <string literal>*/
/*FIRST(<output list>) = { AVID_T, SVID_T, STR_T }*/
void outputList() {
    if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
        variable_list();
    }
    else if (lookahead.code == STR_T) {
        match(STR_T, NO_ATTR);
        gen_incode("PLATY: Output list (string literal) parsed");
    }
    else {
        gen_incode("PLATY: Output list (empty) parsed");
    }

}