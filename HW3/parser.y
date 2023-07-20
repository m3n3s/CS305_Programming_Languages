%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "hw3.h"
void yyerror (const char *s) 
{
}

Info * node;
%}
%token tPRINT tGET tSET tFUNCTION tRETURN tIDENT tEQUALITY tIF tGT tLT tGEQ tLEQ tINC tDEC

%union {
char * string_value;
NumNode numNode;
ExprNode exprNode;
int lineno;
}

%token <numNode> tNUM
%token <string_value> tSTRING
%token <lineno> tADD tSUB tMUL tDIV
%type <exprNode> operation
%type <exprNode> expr
%start prog

%%
prog:		'[' stmtlst ']'
;

stmtlst:	stmtlst stmt |
;

stmt:		setStmt | if | print | unaryOperation | expr {info_adder(node, $1,0); } 
		| returnStmt

;

getExpr:	'[' tGET ',' tIDENT ',' '[' exprList ']' ']'
		| '[' tGET ',' tIDENT ',' '[' ']' ']'
		| '[' tGET ',' tIDENT ']'
;

setStmt:	'[' tSET ',' tIDENT ',' expr ']' { info_adder(node, $6 ,0); }

;

if:		'[' tIF ',' condition ',' '[' stmtlst ']' ']'
		| '[' tIF ',' condition ',' '[' stmtlst ']' '[' stmtlst ']' ']'
;

print:		'[' tPRINT ',' '[' expr ']' ']' { info_adder(node, $5,0); }
;

operation:	'[' tADD ',' expr ',' expr ']' { $$ = mkAdd ( $4, $6, $2);}
		| '[' tSUB ',' expr ',' expr ']' { $$ = mkSub ( $4, $6, $2); }
		| '[' tMUL ',' expr ',' expr ']' { $$ = mkMul ( $4, $6, $2); } 
		| '[' tDIV ',' expr ',' expr ']' { $$ = mkDiv ( $4, $6, $2); }
;	

unaryOperation: '[' tINC ',' tIDENT ']'
		| '[' tDEC ',' tIDENT ']'
;

expr:		tNUM { $$ = mkNum ($1, 0, 0); }  | tSTRING {  $$ = mkStr ($1, 0, 0);} 
		| getExpr { $$ = mkOther();} | function { $$ = mkOther();}| operation | condition { $$ = mkOther();};

function:	 '[' tFUNCTION ',' '[' parametersList ']' ',' '[' stmtlst ']' ']'
		| '[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']'
;

condition:	'[' tEQUALITY ',' expr ',' expr ']'{ info_adder(node, $4,0); info_adder(node, $6,0);}
						     
		
		| '[' tGT ',' expr ',' expr ']'{   info_adder(node, $4,0); info_adder(node, $6,0);}

		| '[' tLT ',' expr ',' expr ']'{    info_adder(node, $4,0); info_adder(node, $6,0);}
                                                     

		| '[' tGEQ ',' expr ',' expr ']'{    info_adder(node, $4,0); info_adder(node, $6,0);
                                             
                                                     }

		| '[' tLEQ',' expr ',' expr ']'{     info_adder(node, $4,0); info_adder(node, $6,0);
                                                     }


;

returnStmt:	'[' tRETURN ',' expr ']'  { info_adder(node, $4,0);}
		| '[' tRETURN ']'
;

parametersList: parametersList ',' tIDENT | tIDENT
;

exprList:	exprList ',' expr {    info_adder(node, $3,0);}
		| expr { info_adder(node, $1,0);}
;

%%

Info *initial () {
	Info *head = malloc(sizeof(Info));
	ExprNode dum;
	head->next = NULL;
	head->exprNode = dum;
	return head;
}

void info_adder (struct Info *head, ExprNode node, int m){
	Info *ptr = malloc(sizeof(head));
	Info *q = malloc(sizeof(head));
	ptr->exprNode = node;
	ptr->line = node.line;
	ptr->mismatch = m;
	q = head;
	while (q->next != NULL){
		q = q->next;
	}
	q->next = ptr;
}

void printer (struct Info *head){
	while (head != NULL){
	ExprNode node = head->exprNode;
	int line = head->line;
	int m = head->mismatch;
	if (node.pr == 1 && m == 0){
        	if(node.exprType == NUM && node.numNode.trail == 1){
                double val = rounding(node.numNode.numValue);
                printf("Result of expression on %d is (%.1f)\n", line, val);
                }
                else if(node.exprType == NUM && node.numNode.trail == 0){
                int number = node.numNode.numValue;
                printf("Result of expression on %d is (%d)\n", line, number);
                }
                else if (node.exprType == STRING){
                printf("Result of expression on %d is (%s)\n", line, node.stringValue);
                }}
	else if (m == 1)
		printf("Type mismatch on %d\n", line);		
	head = head->next;
}
}


double rounding (double var) {
	if (var >= 0){
	double value = (int) (var * 10 + 0.5);
	return (double) value / 10;
	}
	else {
	double value = (int) (var * 10 - 0.5);
        return (double) value / 10;	
	}
}

char *concat(const char *str1, const char *str2) {
	const size_t l1 = strlen(str1);
	const size_t l2 = strlen(str2);
	char *result = malloc(l1+l2+1);
   	memcpy(result, str1, l1);
    memcpy(result+l1, str2, l2+1);
    return result;
}


ExprNode  mkStr(char * str, int pr, int line) {
	 ExprNode result;
         result.exprType = STRING;
	 result.stringValue = str;
         result.pr = pr;
	 if (pr == 1)
		result.line = line;
	 return result;
}

ExprNode mkNum(NumNode nmbr, int pr, int line) {
	ExprNode result;
    result.exprType = NUM;		
	result.numNode.numValue = nmbr.numValue;
	result.numNode.trail = nmbr.trail;
	result.pr = pr;
	if (pr == 1)
                result.line = line;
	return result;
}

ExprNode mkOther() {
	ExprNode result;
        result.exprType = OTHER;
        return result;
}

ExprNode mkAdd(ExprNode left, ExprNode right, int line) {
	if( left.exprType == NUM && right.exprType == NUM){
       		if (left.numNode.trail == 1 || right.numNode.trail == 1){
			NumNode numNode;
			numNode.numValue = left.numNode.numValue + right.numNode.numValue;
			numNode.trail= 1;
			return mkNum(numNode,1, line);
			}
		else{
			NumNode numNode;
                        numNode.numValue = left.numNode.numValue + right.numNode.numValue;
                        numNode.trail= 0;
                        return mkNum(numNode,1, line);
			}
		}
	
	else if (left.exprType == STRING && right.exprType == STRING ){
		char * stringValue = concat ( left.stringValue, right.stringValue);
		return mkStr(stringValue,1, line);
		}

	else if((left.exprType == STRING && right.exprType == NUM) || (left.exprType == NUM && right.exprType == STRING)){
		ExprNode hold;
		hold.line = line;
		info_adder(node, hold, 1);
		return mkOther();
	}
	
	else {return mkOther();}
}


ExprNode mkSub(ExprNode left, ExprNode right, int line) {
         if( left.exprType == NUM && right.exprType == NUM){
		if (left.numNode.trail == 1 || right.numNode.trail == 1){
         	        NumNode numNode;
                        numNode.numValue = left.numNode.numValue - right.numNode.numValue;
                        numNode.trail= 1;
                        return mkNum(numNode,1,line);
			}
                else {
                	NumNode numNode;
                        numNode.numValue = left.numNode.numValue - right.numNode.numValue;
                        numNode.trail= 0;
                        return mkNum(numNode,1, line);
		}}

	else if ((left.exprType == STRING && right.exprType == NUM) || (right.exprType == STRING && left.exprType == NUM) || (right.exprType == STRING && left.exprType == STRING) ){
		ExprNode hold;
		hold.line = line;
                info_adder(node, hold, 1);
		return mkOther();
	}

	else {return mkOther();}
}


ExprNode mkMul(ExprNode left, ExprNode right, int line) {
	if( left.exprType == NUM && right.exprType == NUM){
		NumNode numNode;
                numNode.numValue = left.numNode.numValue * right.numNode.numValue;
		if (left.numNode.trail == 1 || right.numNode.trail == 1)
                        numNode.trail= 1;
		else 
                        numNode.trail= 0;
                return mkNum(numNode,1,line);
		}
	else if (left.exprType == NUM && right.exprType == STRING) {
		if (left.numNode.trail == 0 && left.numNode.numValue >= 0){		
		if (left.numNode.numValue == 0){
			char * empty = "";
			return mkStr(empty,1, line); }
		int times = left.numNode.numValue;
		char *result = malloc(times * strlen(right.stringValue));
		memcpy(result, right.stringValue, strlen(right.stringValue));
		while (times > 1) { 
			result = concat ( result , right.stringValue);	
			times--;
		}		
		return mkStr(result,1, line);
		}
		else {
			ExprNode hold;
                	hold.line = line;
                	info_adder(node, hold, 1);
                	return mkOther();
		}}
		
	else if ((left.exprType == STRING && right.exprType == STRING) || (left.exprType == STRING && right.exprType == NUM)){
		ExprNode hold;
		hold.line = line;
                info_adder(node, hold, 1);
		return mkOther();
	}
	
	else {return mkOther();}
}


ExprNode mkDiv(ExprNode left, ExprNode right, int line) {
	if( left.exprType == NUM && right.exprType == NUM){
		NumNode numNode;
                numNode.numValue = left.numNode.numValue / right.numNode.numValue;
		if (left.numNode.trail == 1 || right.numNode.trail == 1)
                        numNode.trail= 1;
			
                else 
                        numNode.trail= 0;
                return mkNum(numNode,1, line);        
			}

	else if ((left.exprType == STRING && right.exprType == NUM) || (left.exprType == NUM && right.exprType == STRING) || (left.exprType == STRING  && right.exprType == STRING)){
		ExprNode hold;
		hold.line = line;
                info_adder(node, hold, 1);
		return mkOther();
		}

	else {return mkOther();}
}

int main ()
{
node = initial();
if (yyparse()) {
// parse error
printf("ERROR\n");
return 1;
}
else {
printer(node);
// successful parsing
return 0;
}
}

